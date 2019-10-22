;; Copyright (C) 2004 Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: ansi-tests.lisp,v 1.3 2004/09/28 01:53:23 yuji Exp $
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;;  * Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Support routines for Paul Dietz's ANSI testsuite.
;;
;; When testing loop.lisp, do the following.
;; (load "loop.lisp")
;; (load "tests/ansi-tests.lisp")
;; (in-package "CL-TEST")
;; (shadowing-import '(sacla-loop:loop sacla-loop:loop-finish))


(defpackage "CL-TEST"
  (:use "COMMON-LISP"))

(in-package "CL-TEST")

(defmacro deftest (name form &rest values)
  `(equal (multiple-value-list ,form) ',values))



;;;; from ansi-aux.lsp of GCL's ANSI-TESTS
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 17:10:18 1998
;;;; License:  GPL
(defmacro classify-error* (form)
"Evaluate form in safe mode, returning its value if there is no error.
If an error does occur, return a symbol classify the error, or allow
the condition to go uncaught if it cannot be classified."
`(locally (declare (optimize (safety 3)))
  (handler-case ,form
     (undefined-function () 'undefined-function)
     (program-error () 'program-error)
     (package-error () 'package-error)
     (type-error    () 'type-error)
     (control-error () 'control-error)
     (stream-error  () 'stream-error)
     (reader-error  () 'reader-error)
     (file-error    () 'file-error)
     (control-error () 'control-error)
     (cell-error    () 'cell-error)
     (error         () 'error)
  )))

(defun classify-error** (form)
  (handler-bind ((warning #'(lambda (c) (declare (ignore c))
                                    (muffle-warning))))
    (classify-error* (eval form))))

(defmacro classify-error (form)
  `(classify-error** ',form))

(defun notnot (x) (not (not x)))
(defun eqlt (x y)
  "Like EQL, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (eql x y)))))
(defun equalt (x y)
  "Like EQUAL, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (equal x y)))))
(defun symbol< (x &rest args)
  (apply #'string< (symbol-name x) (mapcar #'symbol-name args)))
