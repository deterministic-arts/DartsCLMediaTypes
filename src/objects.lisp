#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Media Type Descriptors
  Copyright (c) 2020 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package #:darts.lib.media-types)

(defstruct (format-descriptor (:copier nil) (:predicate format-descriptor-p)
                              (:constructor make-format-descriptor-1 (string type subtype hash)))
  (plist nil :type list)
  (string "*/*" :type string :read-only t)
  (type "*" :type simple-mime-token :read-only t)
  (subtype "*" :type simple-mime-token :read-only t)
  (hash 0 :type fixnum :read-only t))

(define-structure-property-list format-descriptor format-descriptor-plist)

(defvar *format-lock* (make-lock "Media Format Table Lock"))
(defvar *format-table* (make-weak-hash-table :test 'equal :weakness :value
                                             :weakness-matters t))

(defun make-format-descriptor (type &optional (subtype "*"))
  (let* ((type (string-downcase (simple-mime-token type)))
         (subtype (string-downcase (simple-mime-token subtype)))
         (key (concatenate 'string type "/" subtype)))
    (when (and (string= type "*") (not (string= subtype "*")))
      (error "the format ~S allows only the variant ~S"
             type "*"))
    (with-lock-held (*format-lock*)
      (or (gethash key *format-table*)
          (setf (gethash key *format-table*)
                (make-format-descriptor-1 key type subtype (sxhash key)))))))

(declaim (inline format-descriptor-equal format-descriptor= format-descriptor<=
                 format-descriptor>= format-descriptor> format-descriptor/=))

(defun format-descriptor-equal (object1 object2)
  (eq object1 object2))

(defun print-format-descriptor (object &optional (stream *standard-output*))
  (write-string (format-descriptor-string object) stream)
  object)

(defun format-descriptor< (obj1 obj2)
  (let ((t1 (format-descriptor-type obj1))
        (t2 (format-descriptor-type obj2)))
    (or (string< t1 t2)
        (and (string= t1 t2)
             (string< (format-descriptor-subtype obj1)
                      (format-descriptor-subtype obj2))))))

(defun format-descriptor/= (obj1 obj2) (not (eq obj1 obj2)))
(defun format-descriptor= (obj1 obj2) (eq obj1 obj2))
(defun format-descriptor> (obj1 obj2) (format-descriptor< obj2 obj1))
(defun format-descriptor<= (obj1 obj2) (not (format-descriptor< obj2 obj1)))
(defun format-descriptor>= (obj1 obj2) (not (format-descriptor< obj1 obj2)))

(defmethod print-object ((object format-descriptor) stream)
  (if (not *print-escape*)
      (write-string (string-capitalize (format-descriptor-string object)) stream)
      (print-unreadable-object (object stream :type t :identity nil)
        (print-format-descriptor object stream)))
  object)

(defgeneric format-descriptor (object)
  (:method ((object format-descriptor)) object)
  (:method ((object t))
    (error 'simple-type-error
           :datum object :expected-type 'format-descriptor
           :format-control "~S is not a supported media format descriptor"
           :format-arguments (list object))))

(defun parse-format-descriptor (string &key (start 0) end)
  (multiple-value-bind (type subtype parameters) (parse-mime-type-1 string :start start :end end :junk-allowed t)
    (and type subtype (null parameters) (make-format-descriptor type subtype))))

(defmethod format-descriptor ((object string))
  (or (parse-format-descriptor object)
      (call-next-method)))

(defun format-descriptor-wild-p (object)
  (or (string= "*" (format-descriptor-type object))
      (string= "*" (format-descriptor-subtype object))))



(defvar *any* (make-format-descriptor "*" "*"))

(defun format-precedence-list (object)
  (let* ((format (format-descriptor object))
         (type (format-descriptor-type format))
         (subtype (format-descriptor-subtype format)))
    (cond
      ((string= type "*") (list object))
      ((string= subtype "*") (list format *any*))
      (t (list format
               (make-format-descriptor type "*")
               *any*)))))

(defun subformatp (format super)
  (let* ((format (format-descriptor format))
         (super (format-descriptor super))
         (ftype (format-descriptor-type format))
         (stype (format-descriptor-type super)))
    (cond
      ((string= stype "*") t)
      ((not (string= ftype stype)) nil)
      (t (let ((fsub (format-descriptor-subtype format))
               (ssub (format-descriptor-subtype super)))
           (or (string= ssub "*")
               (string= fsub ssub)))))))


(defstruct (media-type (:copier nil) (:conc-name media-type-) (:predicate media-type-p)
                       (:constructor make-media-type-1 (format parameters)))
  (format (error "missing format") :type format-descriptor :read-only t)
  (parameters nil :type list :read-only t))

(defun make-media-type (format &optional parameters)
  (make-media-type-1 (format-descriptor format) parameters))

(defun parse-media-type (string &key (start 0) end)
  (multiple-value-bind (type subtype parameters) (parse-mime-type-1 string :start start :end end :junk-allowed t)
    (if (and type subtype)
        (make-media-type-1 (make-format-descriptor type subtype) parameters)
        nil)))

(defun media-type-parameter (key object &optional default)
  (let ((pair (assoc key (media-type-parameters object) :test #'string-equal)))
    (if pair (values (cdr pair) t) (values default nil))))

(defgeneric media-type (object)
  (:method ((object media-type)) object)
  (:method ((object format-descriptor)) (make-media-type-1 object nil))
  (:method ((object t))
    (error 'simple-type-error
           :datum object :expected-type 'media-type
           :format-control "~S is not a supported media type designator"
           :format-arguments (list object))))

(defmethod media-type ((object string))
  (or (parse-media-type object)
      (call-next-method)))

(defmethod format-descriptor ((object media-type))
  (media-type-format object))

(defmethod print-object ((object media-type) stream)
  (let* ((format (media-type-format object))
         (type (format-descriptor-type format))
         (subtype (format-descriptor-subtype format))
         (parameters (media-type-parameters object)))
    (if (not *print-escape*)
        (format-mime-type-1 type subtype parameters :stream stream :case :capitalize)
        (print-unreadable-object (object stream :type t :identity t)
          (format-mime-type-1 type subtype parameters :stream stream :case :preserve)))
    object))

