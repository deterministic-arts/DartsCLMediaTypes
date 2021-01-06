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

(defpackage #:darts.lib.media-types
  (:import-from #:trivial-garbage #:make-weak-hash-table)
  (:use #:common-lisp #:darts.lib.tools #:bordeaux-threads #:split-sequence)
  (:local-nicknames (#:regex #:cl-ppcre))
  (:export #:mime-parse-error #:mime-parse-error-string
           #:mime-parse-error-start #:mime-parse-error-end #:mime-parse-error-position
           #:mime-parse-error-decoded-format #:mime-parse-error-decoded-variant
           #:mime-parse-error-decoded-parameters #:format-descriptor #:format-descriptor-p
           #:make-format-descriptor #:format-descriptor-equal #:format-descriptor-hash
           #:format-descriptor< #:format-descriptor<= #:format-descriptor>= #:format-descriptor>
           #:format-descriptor= #:format-descriptor/= #:parse-format-descriptor
           #:format-descriptor-wild-p #:subformatp #:media-type #:media-type-p
           #:media-type-format #:media-type-parameters #:parse-media-type #:make-media-type
           #:format-descriptor-subtype #:format-descriptor-type #:media-type-parameter
           #:media-type-string))
