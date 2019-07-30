;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: must-symbol.lisp,v 1.7 2004/02/20 07:23:42 yuji Exp $
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

(symbolp 'elephant)
(not (symbolp 12))
(symbolp nil)
(symbolp '())
(symbolp :test)
(not (symbolp "hello"))

(not (keywordp 'elephant))
(not (keywordp 12))
(keywordp :test)
(keywordp ':test)
(not (keywordp nil))
(keywordp :nil)
(not (keywordp '(:test)))
(not (keywordp "hello"))
(not (keywordp ":hello"))
(not (keywordp '&optional))


(let ((new (make-symbol "symbol")))
  (string= (symbol-name new) "symbol"))

(let ((new (make-symbol "symbol")))
  (not (boundp new)))

(let ((new (make-symbol "symbol")))
  (not (fboundp new)))
  
(let ((new (make-symbol "symbol")))
  (null (symbol-plist new)))

(let ((new (make-symbol "symbol")))
  (null (symbol-package new)))

(let ((new (make-symbol "symbol")))
  (not (member new (find-all-symbols "symbol"))))

(every #'identity
       (mapcar
	#'(lambda (name)
	    (let ((new (make-symbol name)))
	      (and (string= (symbol-name new) name)
		   (not (boundp new))
		   (not (fboundp new))
		   (null (symbol-plist new))
		   (not (member new (find-all-symbols name))))))
	'("" "Symbol" "eat-this" "SYMBOL" ":S:Y:M:B:O:L:")))


(let ((copy (copy-symbol 'cl:car)))
  (string= (symbol-name copy) (symbol-name 'cl:car)))

(let ((copy (copy-symbol 'cl:car)))
  (not (boundp copy)))

(let ((copy (copy-symbol 'cl:car)))
  (not (fboundp copy)))

(let ((copy (copy-symbol 'cl:car)))
  (null (symbol-plist copy)))

(let ((copy (copy-symbol 'cl:car)))
  (null (symbol-package copy)))


(let ((copy (copy-symbol 'cl:car "copy properties too")))
  (string= (symbol-name copy) (symbol-name 'cl:car)))

(let ((copy (copy-symbol 'cl:car "copy properties too")))
  (if (boundp 'cl:car)
      (boundp copy)
    (not (boundp copy))))

(let ((copy (copy-symbol 'cl:car "copy properties too")))
  (eq (symbol-function copy) (symbol-function 'cl:car)))

(let ((copy (copy-symbol 'cl:car "copy properties too")))
  (equal (symbol-plist copy) (symbol-plist 'cl:car)))

(let ((copy (copy-symbol 'cl:car "copy properties too")))
  (null (symbol-package copy)))



(every #'identity
       (mapcar
	#'(lambda (symbol)
	    (let ((copy1 (copy-symbol symbol))
		  (copy2 (copy-symbol symbol "copy-properties")))
	      (and (string= (symbol-name copy1) (symbol-name symbol))
		   (string= (symbol-name copy2) (symbol-name symbol))
		   (not (boundp copy1))
		   (if (boundp symbol)
		       (boundp copy2)
		     (not (boundp copy2)))
		   (not (fboundp copy1))
		   (if (fboundp symbol)
		       (fboundp copy2)
		     (not (fboundp copy2)))
		   (null (symbol-plist copy1))
		   (equal (symbol-plist copy2) (symbol-plist symbol))
		   (null (symbol-package copy1))
		   (null (symbol-package copy2))
		   (not (member copy1 (find-all-symbols symbol)))
		   (not (member copy2 (find-all-symbols symbol))))))
	'(nil cl:cdr cl:*package* cl:list symbol weird-symbol)))


(let ((new (gensym)))
  (not (boundp new)))

(let ((new (gensym)))
  (not (fboundp new)))

(let ((new (gensym)))
  (null (symbol-plist new)))

(let ((new (gensym)))
  (null (symbol-package new)))


(let ((new (gensym "How about this")))
  (not (boundp new)))

(let ((new (gensym "How about this")))
  (not (fboundp new)))

(let ((new (gensym "How about this")))
  (null (symbol-plist new)))

(let ((new (gensym "How about this")))
  (null (symbol-package new)))


(let ((new (gensym 100)))
  (not (boundp new)))

(let ((new (gensym 10)))
  (not (fboundp new)))

(let ((new (gensym 9)))
  (null (symbol-plist new)))

(let ((new (gensym 8)))
  (null (symbol-package new)))


(let* ((counter *gensym-counter*)
       (new (gensym)))
  (string= (symbol-name new)
	   (with-output-to-string (stream)
              (format stream "G~D" counter))))

(let* ((counter *gensym-counter*)
       (new (gensym "JJ")))
  (string= (symbol-name new)
	   (with-output-to-string (stream)
              (format stream "JJ~D" counter))))

(let* ((counter *gensym-counter*)
       (new (gensym "")))
  (string= (symbol-name new)
	   (with-output-to-string (stream)
              (format stream "~D" counter))))

(let ((new (gensym 0)))
  (string= (symbol-name new) "G0"))

(let ((new (gensym 1000)))
  (string= (symbol-name new) "G1000"))



(let ((symbol (gentemp)))
  (char= (aref (symbol-name symbol) 0) #\T))

(let ((symbol (gentemp)))
  (not (boundp symbol)))

(let ((symbol (gentemp)))
  (not (fboundp symbol)))

(let ((symbol (gentemp)))
  (null (symbol-plist symbol)))

(let ((symbol (gentemp)))
  (multiple-value-bind (symbol-found status)
      (find-symbol (symbol-name symbol))
    (and (eq symbol-found symbol)
	 (if (eq *package* (find-package "KEYWORD"))
	     (eq status :external)
	   (eq status :internal)))))

(let ((symbol-1 (gentemp))
      (symbol-2 (gentemp)))
  (not (string= (symbol-name symbol-1) (symbol-name symbol-2))))

(let ((symbol (gentemp "prefix")))
  (string= (subseq (symbol-name symbol) 0 6) "prefix"))

(let ((symbol (gentemp "prefix")))
  (not (boundp symbol)))

(let ((symbol (gentemp "prefix")))
  (not (fboundp symbol)))

(let ((symbol (gentemp "prefix")))
  (null (symbol-plist symbol)))

(let ((symbol (gentemp "prefix")))
  (multiple-value-bind (symbol-found status)
      (find-symbol (symbol-name symbol))
    (and (eq symbol-found symbol)
	 (if (eq *package* (find-package "KEYWORD"))
	     (eq status :external)
	   (eq status :internal)))))


(let* ((package (defpackage "TEST-PACKAGE-FOR-GENTEMP"))
       (symbol (gentemp "prefix" package)))
  (string= (subseq (symbol-name symbol) 0 6) "prefix"))

(let* ((package (defpackage "TEST-PACKAGE-FOR-GENTEMP"))
       (symbol (gentemp "prefix" package)))
  (not (boundp symbol)))

(let* ((package (defpackage "TEST-PACKAGE-FOR-GENTEMP"))
       (symbol (gentemp "prefix" package)))
  (not (fboundp symbol)))

(let* ((package (defpackage "TEST-PACKAGE-FOR-GENTEMP"))
       (symbol (gentemp "prefix" package)))
  (null (symbol-plist symbol)))

(let* ((package (defpackage "TEST-PACKAGE-FOR-GENTEMP"))
       (symbol (gentemp "prefix" package)))
  (multiple-value-bind (symbol-found status)
      (find-symbol (symbol-name symbol) package)
    (and (eq symbol-found symbol)
	 (eq status :internal))))



(functionp (symbol-function 'cl:car))
(eq (symbol-function 'cl:car) (fdefinition 'cl:car))
(progn (setf (symbol-function 'symbol-for-test) #'car)
       (eq (symbol-for-test '(a)) 'a))

(let ((f #'(lambda (a) a)))
  (setf (symbol-function 'symbol-for-test) f)
  (eq (symbol-function 'symbol-for-test) f))


(stringp (symbol-name 'symbol))
(string= (symbol-name (intern "TEST-SYMBOL")) "TEST-SYMBOL")


(eq (symbol-package 'cl:car) (find-package "COMMON-LISP"))
(eq (symbol-package ':key) (find-package "KEYWORD"))
(null (symbol-package (make-symbol "temp")))
(null (symbol-package (gensym)))
(packagep (symbol-package 'a))
(packagep (symbol-package 'my-symbol))


(listp (symbol-plist 'car))
(listp (symbol-plist 'cdr))
(null (symbol-plist (gensym)))
(null (symbol-plist (gentemp)))

(let ((symbol (gensym)))
  (setf (symbol-plist symbol) (list 'a 1 'b 2 'c 3))
  (equal (symbol-plist symbol) '(a 1 b 2 c 3)))

(let ((symbol (gensym)))
  (setf (symbol-plist symbol) (list 'a 1 'b 2 'c 3))
  (setf (symbol-plist symbol) '())
  (null (symbol-plist symbol)))


(progn (setf (symbol-value 'a) 1)
       (eql (symbol-value 'a) 1))

(progn
  (setf (symbol-value 'a) 1)
  (let ((a 2))
    (eql (symbol-value 'a) 1)))

(progn
  (setf (symbol-value 'a) 1)
  (let ((a 2))
    (setq a 3)
    (eql (symbol-value 'a) 1)))

(progn
  (setf (symbol-value 'a) 1)
  (let ((a 2)) 
    (declare (special a)) 
    (eql (symbol-value 'a) 2)))

(progn
  (setf (symbol-value 'a) 1)
  (let ((a 2)) 
    (declare (special a)) 
    (setq a 3)
    (eql (symbol-value 'a) 3)))

(progn
  (setf (symbol-value 'a) 1)
  (and (eql (let ((a 2))
	      (setf (symbol-value 'a) 3)
	      a)
	    2)
       (eql a 3)))

(progn
  (setf (symbol-value 'a) 1)
  (let ((a 4))
    (declare (special a))
    (let ((b (symbol-value 'a)))
      (setf (symbol-value 'a) 5)
      (and (eql a 5)
	   (eql b 4)))))

(eq (symbol-value :any-keyword) :any-keyword)
(eq (symbol-value 'nil) nil)
(eq (symbol-value '()) nil)
(eq (symbol-value t) t)


(let ((symbol (gensym)))
  (setf (symbol-plist symbol) (list 'a 1 'b 2 'c 3))
  (and (eql (get symbol 'a) 1)
       (eql (get symbol 'b) 2)
       (eql (get symbol 'c) 3)
       (eql (get symbol 'd) nil)
       (eql (get symbol 'e 9) 9)))

(let ((symbol (gensym)))
  (setf (symbol-plist symbol) (list 'a 1 'b 2 'c 3))
  (and (eql (setf (get symbol 'a) 9) 9)
       (eql (get symbol 'a) 9)
       (eql (setf (get symbol 'b) 8) 8)
       (eql (get symbol 'b) 8)
       (eql (setf (get symbol 'c) 7) 7)
       (eql (get symbol 'c) 7)
       (eql (setf (get symbol 'd) 6) 6)
       (eql (get symbol 'd) 6)
       (eql (setf (get symbol 'e) 5) 5)
       (eql (get symbol 'e) 5)))

(let ((symbol (gensym))
      tmp)
  (and (null (get symbol 'a))
       (setf (get symbol 'a (setq tmp 1)) tmp)
       (eql (get symbol 'a) 1)))

(let ((symbol (gensym)))
  (setf (symbol-plist symbol) (list 'a 1 'b 2 'c 3 'a 9))
  (and (eql (setf (get symbol 'a) 5) 5)
       (eql (get symbol 'a) 5)))


(let ((symbol (gensym)))
  (setf (symbol-plist symbol) (list 'a 1 'b 2 'c 3))
  (and (remprop symbol 'a)
       (eq (get symbol 'a 'not-found) 'not-found)))

(let ((symbol (gensym)))
  (not (remprop symbol 'a)))

(let ((symbol (gensym)))
  (setf (symbol-plist symbol) (list 'a 1 'b 2 'c 3 'a 9))
  (and (remprop symbol 'a)
       (eql (get symbol 'a) 9)))

(let ((symbol (gensym)))
  (setf (symbol-plist symbol) (list 'a 1 'b 2 'c 3 'a 9))
  (and (remprop symbol 'a)
       (eql (get symbol 'a) 9)
       (remprop symbol 'a)
       (eq (get symbol 'a 'not-found) 'not-found)))


(not (boundp (gensym)))
(let ((symbol (gensym)))
  (set symbol 1)
  (boundp symbol))

(let ((test-symbol 1))
  (not (boundp 'test-symbol)))

(let ((test-symbol 1))
  (declare (special test-symbol))
  (boundp 'test-symbol))


(not (boundp (makunbound (gensym))))

(let ((test-symbol 0))
  (declare (special test-symbol))
  (and (let ((test-symbol 1))
	 (declare (special test-symbol))
	 (not (boundp (makunbound 'test-symbol))))
       (boundp 'test-symbol)))


(let ((test-symbol 0))
  (declare (special test-symbol))
  (and (let ((test-symbol 1))
	 (makunbound 'test-symbol)
	 (eql test-symbol 1))
       (not (boundp 'test-symbol))))


(let ((test-symbol 0))
  (declare (special test-symbol))
  (and
   (eql test-symbol 0)
   (setf (symbol-value 'test-symbol) 1)
   (eql test-symbol 1)
   (eql (set 'test-symbol 10) 10)
   (eql test-symbol 10)))

(let ((test-symbol 0))
  (declare (special test-symbol))
  (and (let ((test-symbol 1))
	 (set 'test-symbol 100)
	 (eql test-symbol 1))
       (eql test-symbol 100)))

