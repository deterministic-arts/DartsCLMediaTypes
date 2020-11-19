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

(define-condition mime-parse-error (parse-error)
  ((string
     :initarg :string :reader mime-parse-error-string
     :documentation "The string being parsed")
   (start
     :initarg :start :initform 0 :reader mime-parse-error-start
     :documentation "Start index of the first character to be parsed")
   (end
     :initarg :end :initform nil :reader mime-parse-error-end
     :documentation "Index of the first character after the part to parsed")
   (position
     :initarg :position :initform nil :reader mime-parse-error-position
     :documentation "Last valid position in the input string")
   (decoded-format 
     :initarg :decoded-format :initform nil 
     :reader mime-parse-error-decoded-format)
   (decoded-variant 
     :initarg :decoded-variant :initform nil 
     :reader mime-parse-error-decoded-variant)
   (decoded-parameters 
     :initarg :decoded-parameters :initform nil 
     :reader mime-parse-error-decoded-parameters))
  (:report (lambda (object stream)
             (let* ((string (mime-parse-error-string object))
                    (start (mime-parse-error-start object))
                    (end (or (mime-parse-error-end object) (length string)))
                    (position (mime-parse-error-position object)))
               (format stream "could not parse ~S as MIME type specification~@[; parsing should have stopped at index ~D~]"
                       (subseq string start end) position))))
  (:documentation "Condition signalled by `parse-mime-type', if the input
    string cannot be parsed as MIME type specification. Note, that `position' 
    slot does not hold the position, where the error was detected, but the
    last valid index into the string, at which parsing could have \"legally\"
    stopped (FIXME: how to explain this better)"))

(define-condition simple-mime-parse-error (simple-condition mime-parse-error) ())

(defun make-mime-type-lexer (string start end)
  (labels
      ((whitep (char) 
         (find char #.(concatenate 'string '(#\space #\tab #\newline #\return))))
       (atom-char-p (char)
         (or (eql char #\!) (char<= #\# char #\')
             (char<= #\* char #\+) (eql char #\.)
             (char<= #\0 char #\9) (char<= #\A char #\Z)
             (char<= #\^ char #\~) (eql char #\-)))
       (read-atom ()
         (loop
            :with first := start
            :while (< start end)
            :do (let ((char (char string start)))
                  (if (atom-char-p char)
                      (incf start)
                      (return (values :atom (coerce (subseq string first start) 'simple-string)
                                      start))))
            :finally (return (values :atom (coerce (subseq string first) 'simple-string)
                                     start))))
       (skip-comment (position depth)
         (cond
           ((>= position end) end)
           ((eql (char string position) #\() (skip-comment (1+ position) (1+ depth)))
           ((eql (char string position) #\)) (if (eql depth 1) (1+ position) (skip-comment (1+ position) (1- depth))))
           ((eql (char string position) #\\) (skip-comment (+ position 2) depth))
           (t (skip-comment (1+ position) depth))))
       (read-quoted ()
         (loop
            :with buffer := (make-array (- end start) :element-type 'character :fill-pointer 0)
            :and escaped := nil :and string-start := (- start 1)
            :while (< start end)
            :do (let ((char (char string start)))
                  (if escaped
                      (progn
                        (incf start)
                        (vector-push-extend char buffer)
                        (setf escaped nil))
                      (cond
                        ((eql char #\\) (incf start) (setf escaped t))
                        ((eql char #\") (incf start) (return (values :quoted (coerce buffer 'simple-string) start)))
                        (t (incf start)
                           (vector-push-extend char buffer)))))
            :finally (return (values :error string-start string-start)))))
    (lambda ()
      (block scan
        (loop
          (if (>= start end) 
              (return-from scan (values nil nil start))
              (let ((char (char string start)))
                (cond
                  ((whitep char) (incf start))
                  ((atom-char-p char) (return-from scan (read-atom)))
                  ((eql char #\() (setf start (skip-comment (1+ start) 1)))
                  ((eql char #\") (incf start) (return-from scan (read-quoted)))
                  ((eql char #\=) (incf start) (return-from scan (values :equal nil start)))
                  ((eql char #\;) (incf start) (return-from scan (values :semicolon nil start)))
                  ((eql char #\/) (incf start) (return-from scan (values :slash nil start)))
                  ((eql char #\,) (incf start) (return-from scan (values :comma nil start)))
                  (t (return-from scan (values :error start start)))))))))))


(defun parse-mime-type-1 (string 
                          &key (start 0) (end nil) (junk-allowed nil))
  (let* ((end (or end (length string)))
         (lexer (make-mime-type-lexer string start end)))
    (labels
        ((next-token () (funcall lexer))
         (fail (after format variant params &optional ctl &rest args)
           (declare (dynamic-extent args))
           (let* ((params (nreverse params))
                  (condition (if (not ctl)
                                (make-condition 'mime-parse-error :string string :start start :end end 
                                                                  :position after :decoded-format format
                                                                  :decoded-variant variant 
                                                                  :decoded-parameters params)
                                (make-condition 'simple-mime-parse-error :string string :start start :end end 
                                                                         :position after :decoded-format format
                                                                         :decoded-variant variant 
                                                                         :decoded-parameters params
                                                                         :format-control ctl 
                                                                         :format-arguments (copy-list args)))))
             (if (not junk-allowed)
                 (error condition)
                 (return-from parse-mime-type-1 (values format variant params condition)))))
         (parse-media-type (after-1)
           (multiple-value-bind (token format after) (next-token)
             (declare (ignore after))
             (if (not (eq token :atom))
                 (fail after-1 nil nil nil "media type format expected")
                 (multiple-value-bind (token unused after) (next-token)
                   (declare (ignore unused after))
                   (if (not (eq token :slash))
                       (fail after-1 format nil nil "missing ~C separator after the media type format ~S" #\/ format)
                       (multiple-value-bind (token variant after-2) (next-token)
                         (if (not (eq token :atom))
                             (fail after-1 format nil nil "missing format variant after ~C separator" #\/)
                             (parse-parameters after-2 format variant nil))))))))
         (parse-parameters (after-1 format variant parameters)
           (multiple-value-bind (token unused) (next-token)
             (declare (ignore unused))
             (cond
               ((null token) (values format variant (nreverse parameters) nil))
               ((not (eq token :semicolon)) (fail after-1 format variant parameters "junk found after parameter list"))
               (t (multiple-value-bind (token key) (next-token)
                    (cond
                      ((null token) (values format variant (nreverse parameters) nil))
                      ((not (eq token :atom)) (fail after-1 format variant parameters "parameter name expected; found ~S instead" token))
                      (t (multiple-value-bind (token unused after) (next-token)
                           (declare (ignore unused after))
                           (if (not (eq token :equal))
                               (fail after-1 format variant parameters "expected ~C, but found ~S instead" #\= token)
                               (multiple-value-bind (token value after-2) (next-token)
                                 (if (not (or (eq token :atom) (eq token :quoted)))
                                     (fail after-1 format variant parameters "expected the parameter value for ~S, but got ~S instead" key token)
                                     (parse-parameters after-2 format variant 
                                                       (cons (cons key value) 
                                                             parameters))))))))))))))
      (parse-media-type start))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun simple-mime-token-p (string)
    (and (stringp string)
         (cl-ppcre:scan "^[!#-'*-+.0-9A-Z^-~-]+$" string)
         t)))

(deftype simple-mime-token ()
  '(and string (satisfies simple-mime-token-p)))

(defun simple-mime-token (value &key (case :preserve))
  (labels
      ((from-string (string)
         (if (simple-mime-token-p string)
             (ecase case
               ((:preserve) string)
               ((:capitalize) (string-capitalize string))
               ((:upcase) (string-upcase string))
               ((:downcase) (string-downcase string)))
             (error 'simple-type-error
                    :datum value :expected-type 'simple-mime-token
                    :format-control "~S is not a well-formed simple MIME token"
                    :format-arguments (list value)))))
    (cond
      ((stringp value) (from-string value))
      ((symbolp value) (from-string (symbol-name value)))
      ((characterp value) (from-string (string value)))
      (t (error 'simple-type-error
                :datum value :expected-type 'simple-mime-token
                :format-control "~S is not a well-formed simple MIME token"
                :format-arguments (list value))))))

(defun format-mime-type-1 (format variant parameters 
                           &key (case :preserve) (stream *standard-output*) (dense t))
  (write-string (simple-mime-token format :case case) stream)
  (write-char #\/ stream)
  (write-string (simple-mime-token variant :case case) stream)
  (dolist (pair parameters)
    (write-char #\; stream)
    (unless dense (write-char #\space stream))
    (write-string (simple-mime-token (car pair) :case case) stream)
    (write-char #\= stream)
    (let ((value (string (cdr pair))))
      (if (simple-mime-token-p value)
          (write-string value stream)
          (progn
            (write-char #\" stream)
            (loop
              :for char :across value
              :do (when (eql char #\") (write-char #\\ stream))
                  (write-char char stream))
            (write-char #\" stream)))))
  nil)

