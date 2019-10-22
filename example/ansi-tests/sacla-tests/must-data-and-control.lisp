;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: must-data-and-control.lisp,v 1.15 2004/02/20 07:23:42 yuji Exp $
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

(let (a b c)
  (and (null (psetq a 1 b 2 c 3))
       (eql a 1)
       (eql b 2)
       (eql c 3)))

(let ((a 1)
      (b 2)
      (c 3))
  (and (null (psetq a (1+ b) b (1+ a) c (+ a b)))
       (eql a 3)
       (eql b 2)
       (eql c 3)))

(let ((x (list 10 20 30)))
  (symbol-macrolet ((y (car x)) (z (cadr x)))
    (psetq y (1+ z) z (1+ y))
    (equal (list x y z) '((21 11 30) 21 11))))

(let ((a 1) (b 2))
  (and (null (psetq a b  b a))
       (eql a 2)
       (eql b 1)))


(null (psetq))
(let ((a nil))
  (and (null (psetq a t))
       (eq a t)))
(let ((a 0)
      (b 1))
  (and (null (psetq a b
		    b a))
       (eq a 1)
       (eq b 0)))

(let ((a 0)
      (b 1)
      (c 2))
  (and (null (psetq a b
		    b c
		    c a))
       (eq a 1)
       (eq b 2)
       (eq c 0)))

(let ((a 0)
      (b 1)
      (c 2)
      (d 3))
  (and (null (psetq a b
		    b c
		    c d
		    d a))
       (eq a 1)
       (eq b 2)
       (eq c 3)
       (eq d 0)))



(null (block nil (return) 1))
(eql (block nil (return 1) 2) 1)
(equal (multiple-value-list (block nil (return (values 1 2)) 3)) '(1 2))
(eql (block nil (block alpha (return 1) 2)) 1)
(eql (block alpha (block nil (return 1)) 2) 2)
(eql (block nil (block nil (return 1) 2)) 1)

(eq (dotimes (i 10 nil)
      (return t))
    t)

(eq (dolist (elt (list 0 1 2 3) nil)
      (when (numberp elt)
	(return t)))
    t)

(not nil)
(not '())
(not (integerp 'sss))
(null (not (integerp 1)))
(null (not 3.7))
(null (not 'apple))

(not nil)
(null (not t))
(not (cdr '(a)))


(equal 'a 'a)
(not (equal 'a 'b))
(equal 'abc 'abc)
(equal 1 1)
(equal 2 2)
(equal 0.1 0.1)
(equal 1/3 1/3)
(not (equal 0 1))
(not (equal 1 1.0))
(not (equal 1/3 1/4))
(equal #\a #\a)
(equal #\b #\b)
(not (equal #\b #\B))
(not (equal #\C #\c))
(equal '(0) '(0))
(equal '(0 #\a) '(0 #\a))
(equal '(0 #\a x) '(0 #\a x))
(equal '(0 #\a x (0)) '(0 #\a x (0)))
(equal '(0 #\a x (0 (#\a (x "abc" #*0101))))
       '(0 #\a x (0 (#\a (x "abc" #*0101)))))
(not (equal (make-array '(2 2) :initial-contents '((a b) (c d)))
	    (make-array '(2 2) :initial-contents '((a b) (c d)))))
(let ((array (make-array '(2 2) :initial-contents '((a b) (c d)))))
  (equal array array))


(eql (identity 101) 101)
(equal (mapcan #'identity (list (list 1 2 3) '(4 5 6))) '(1 2 3 4 5 6))
(eq (identity 'x) 'x)



(funcall (complement #'zerop) 1)
(not (funcall (complement #'characterp) #\A))
(not (funcall (complement #'member) 'a '(a b c)))
(funcall (complement #'member) 'd '(a b c))



(equal (mapcar (constantly 3) '(a b c d)) '(3 3 3 3))
(let ((const-func (constantly 'xyz)))
  (every #'(lambda (arg) (eq arg 'xyz))
	 (list (funcall const-func)
	       (funcall const-func 'a)
	       (funcall const-func 'a 'b)
	       (funcall const-func 'a 'b 'c)
	       (funcall const-func 'a 'b 'c 'd))))



(let ((temp1 1)
      (temp2 1)
      (temp3 1))
  (and (eql (and (incf temp1) (incf temp2) (incf temp3)) 2)
       (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3))
       (eql (decf temp3) 1)
       (null (and (decf temp1) (decf temp2) (eq temp3 'nil) (decf temp3)))
       (and (eql temp1 temp2) (eql temp2 temp3))
       (and)))

(eq (and) t)
(equal (multiple-value-list (and 't 't 't (values 'a 'b 'c)))
       '(a b c))
(null (and 't 't (cdr '(a)) (error "error")))



(let ((temp0 nil)
      (temp1 10)
      (temp2 20)
      (temp3 30))
  (and (eql (or temp0 temp1 (setq temp2 37)) 10)
       (eql temp2 20)
       (eql (or (incf temp1) (incf temp2) (incf temp3)) 11)
       (eql temp1 11)
       (eql temp2 20)
       (eql temp3 30)
       (equal (multiple-value-list (or (values) temp1)) '(11))
       (equal (multiple-value-list (or (values temp1 temp2) temp3)) '(11))
       (equal (multiple-value-list (or temp0 (values temp1 temp2))) '(11 20))
       (equal (multiple-value-list (or (values temp0 temp1)
				       (values temp2 temp3)))
	      '(20 30))))

(zerop (or '0 '1 '2))
(let ((a 0))
  (and (eql (or (incf a) (incf a) (incf a)) 1)
       (eql a 1)))
(equal (multiple-value-list (or (values) 1)) '(1))
(equal (multiple-value-list (or (values 1 2) 3)) '(1))

(null (or))
(equal (multiple-value-list (or (values 0 1 2))) '(0 1 2))
(equal (multiple-value-list (or nil (values 0 1 2))) '(0 1 2))
(equal (multiple-value-list (or nil nil (values 0 1 2))) '(0 1 2))
(equal (multiple-value-list (or nil nil nil (values 0 1 2))) '(0 1 2))


(let ((a nil))
  (flet ((select-options ()
           (cond ((= a 1) (setq a 2))
		 ((= a 2) (setq a 3))
		 ((and (= a 3) (floor a 2)))
		 (t (floor a 3)))))
    (and (eql (setq a 1) 1)
	 (eql (select-options) 2)
	 (eql a 2)
	 (eql (select-options) 3)
	 (eql a 3)
	 (eql (select-options) 1)
	 (setq a 5)
	 (equal (multiple-value-list (select-options)) '(1 2)))))

(null (cond))
(equal (multiple-value-list (cond ((values 1 2 3)))) '(1))
(equal (multiple-value-list (cond (t (values 1 2 3)))) '(1 2 3))
(equal (multiple-value-list (cond (t (values 1)
				     (values 1 2)
				     (values 1 2 3)))) '(1 2 3))
(let ((a 0))
  (and (eql (cond
	     ((incf a))
	     ((incf a))
	     ((incf a)))
	    1)
       (eql a 1)))

(let ((a 0))
  (and (eql (cond
	     ((incf a) (incf a) (incf a))
	     ((incf a) (incf a) (incf a))
	     ((incf a) (incf a) (incf a)))
	    3)
       (eql a 3)))



(eq (when t 'hello) 'HELLO)
(null (unless t 'hello))
(null (when nil 'hello))
(eq (unless nil 'hello) 'HELLO)
(null (when t))
(null (unless nil))
(let ((x 3))
  (equal (list (when (oddp x) (incf x) (list x))
	       (when (oddp x) (incf x) (list x))
	       (unless (oddp x) (incf x) (list x))
	       (unless (oddp x) (incf x) (list x))
	       (if (oddp x) (incf x) (list x)) 
	       (if (oddp x) (incf x) (list x)) 
	       (if (not (oddp x)) (incf x) (list x)) 
	       (if (not (oddp x)) (incf x) (list x)))
	 '((4) NIL (5) NIL 6 (6) 7 (7))))




(equal (let ((list nil))
	 (dolist (k '(1 2 3 :four #\v () t 'other))
	   (push (case k
		   ((1 2) 'clause1)
		   (3 'clause2)
		   (nil 'no-keys-so-never-seen)
		   ((nil) 'nilslot)
		   ((:four #\v) 'clause4)
		   ((t) 'tslot)
		   (otherwise 'others))
		 list))
	 list)
       '(OTHERS TSLOT NILSLOT CLAUSE4 CLAUSE4 CLAUSE2 CLAUSE1 CLAUSE1))


(macro-function 'case)
(macro-function 'ccase)
(macro-function 'ecase)

(eql (case 'a
       ((a b c) 0)
       (x 1)
       (y 2)
       (z 3))
     0)

(eql (case 'j
       ((a b c) 0)
       (x 1)
       (y 2)
       (z 3)
       (t 9))
     9)

(eql (case 'j
       ((a b c) 0)
       (x 1)
       (y 2)
       (z 3)
       (otherwise 9))
     9)

(eql (case 'j
       ((a b c) 0)
       (x 1)
       (y 2)
       (z 3))
     nil)

(null (case 'x))

(let ((x #\a))
  (equal (case x
	   ((#\x #\y #\z) "xyz")
	   (#\a "a")
	   (t "-"))
	 "a"))

(let ((x #\A))
  (equal (case x
	   ((#\x #\y #\z) "xyz")
	   (#\a "a")
	   (t "-"))
	 "-"))

(let ((x t))
  (eql (case x
	 ((t) 0)
	 (t 1))
       0))

(let ((x nil))
  (eql (case x
	 ((t) 0)
	 (t 1))
       1))

(let ((x 'a))
  (eql (case x
	 ((t) 0))
       nil))

(let ((x 'otherwise))
  (eql (case x
	 ((otherwise) 0)
	 (otherwise 1))
       0))

(let ((x nil))
  (eql (case x
	 ((otherwise) 0)
	 (otherwise 1))
       1))

(let ((x 'a))
  (eql (case x
	 ((otherwise) 0))
       nil))


(let ((x 'a))
  (and (eql (case x
	      ((a b c) (setq x 0) 'a)
	      ((x y z) (setq x 1) 'x))
	    'a)
       (eql x 0)))

(let ((x 'x))
  (and (eql (case x
	      ((a b c) (setq x 0) 'a)
	      ((x y z) (setq x 1) 'x))
	    'x)
       (eql x 1)))


(equal (mapcar #'(lambda (x)  (case x (a 0) (b 1) (c 2) (d 3) (e 4)))
	       '(a b c d e f))
       '(0 1 2 3 4 nil))

(case 'a (otherwise t))

(eql (case 'a (otherwise 10)) 10)

(let ((a 0)
      (b 1))
  (and (eq (case (progn (incf a) (incf b))
	     (0 'a)
	     (1 'b)
	     (2 'c))
	   'c)
       (eql a 1)
       (eql b 2)))

(let ((a 0)
      (b 1))
  (and (eq (case (progn (incf a) (incf b))
	     (0 'a)
	     (1 'b)
	     (2 (incf a) (incf b) 'c))
	   'c)
       (eql a 2)
       (eql b 3)))

(let ((a (list 0 1 2 3)))
  (eq (case (caddr a)
	(0 'x)
	(1 'y)
	(2 'z)
	(3 t))
      'z))


(equal (multiple-value-list (case 2
			      (0 (values 0 'x))
			      (1 (values 1 'y))
			      (2 (values 2 'z))
			      (3 (values 3 't))))
       '(2 z))



(let ((a 'c))
  (eql (ccase a
         ((a b c) 0)
	 (x 1)
	 (y 2)
	 (z 3))
       0))

(HANDLER-CASE
    (PROGN
      (LET ((A 'J))
	(CCASE A ((A B C) 0) (X 1) (Y 2) (Z 3))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(HANDLER-CASE
    (PROGN
      (LET ((A NIL))
	(CCASE A)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(HANDLER-CASE
    (PROGN
      (LET ((A #\a))
	(CCASE A ((#\A #\B #\C) 0) ((#\X #\Y #\Z) 1))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(let ((a (list 0 1 2 3)))
  (eq (ccase (caddr a)
	     (0 'x)
	     (1 'y)
	     (2 'z)
	     (3 t))
      'z))

(let ((x #\a))
  (equal (ccase x
		((#\x #\y #\z) "xyz")
		(#\a "a"))
	 "a"))

(let ((x 'a))
  (and (eql (ccase x
		   ((a b c) (setq x 0) 'a)
		   ((x y z) (setq x 1) 'x))
	    'a)
       (eql x 0)))

(let ((x 'x))
  (and (eql (ccase x
		   ((a b c) (setq x 0) 'a)
		   ((x y z) (setq x 1) 'x))
	    'x)
       (eql x 1)))

(equal (mapcar #'(lambda (x)  (ccase x (a 0) (b 1) (c 2) (d 3) (e 4)))
	       '(a b c d e))
       '(0 1 2 3 4))


(equal (multiple-value-list (let ((a 2))
			      (ccase a
				     (0 (values 0 'x))
				     (1 (values 1 'y))
				     (2 (values 2 'z))
				     (3 (values 3 't)))))
       '(2 z))

(let ((a 'c))
  (eql (ecase a
         ((a b c) 0)
	 (x 1)
	 (y 2)
	 (z 3))
       0))

(HANDLER-CASE
    (PROGN
      (LET ((A 'J))
	(ECASE A ((A B C) 0) (X 1) (Y 2) (Z 3))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(HANDLER-CASE
    (PROGN
      (LET ((A NIL))
	(ECASE A)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(HANDLER-CASE
    (PROGN
      (LET ((A #\a))
	(ECASE A ((#\A #\B #\C) 0) ((#\X #\Y #\Z) 1))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(let ((a (list 0 1 2 3)))
  (eq (ecase (caddr a)
	     (0 'x)
	     (1 'y)
	     (2 'z)
	     (3 t))
      'z))

(let ((x #\a))
  (equal (ecase x
		((#\x #\y #\z) "xyz")
		(#\a "a"))
	 "a"))

(let ((x 'a))
  (and (eql (ecase x
		   ((a b c) (setq x 0) 'a)
		   ((x y z) (setq x 1) 'x))
	    'a)
       (eql x 0)))

(let ((x 'x))
  (and (eql (ecase x
		   ((a b c) (setq x 0) 'a)
		   ((x y z) (setq x 1) 'x))
	    'x)
       (eql x 1)))

(equal (mapcar #'(lambda (x)  (ecase x (a 0) (b 1) (c 2) (d 3) (e 4)))
	       '(a b c d e))
       '(0 1 2 3 4))

(equal (multiple-value-list (let ((a 2))
			      (ecase a
				(0 (values 0 'x))
				(1 (values 1 'y))
				(2 (values 2 'z))
				(3 (values 3 't)))))
       '(2 z))


(let ((x 'a))
  (equal (typecase x
	   (cons "cons")
	   (symbol "symbol")
	   (number "number")
	   (otherwise "unknown"))
	 "symbol"))

(let ((x (list 'a)))
  (equal (typecase x
	   (cons "cons")
	   (symbol "symbol")
	   (number "number")
	   (otherwise "unknown"))
	 "cons"))

(let ((x 0))
  (equal (typecase x
	   (cons "cons")
	   (symbol "symbol")
	   (number "number")
	   (otherwise "unknown"))
	 "number"))

(let ((x (make-array '(3 3))))
  (equal (typecase x
	   (cons "cons")
	   (symbol "symbol")
	   (number "number")
	   (otherwise "unknown"))
	 "unknown"))


(null (typecase 'a))
(typecase 'a (otherwise t))
(typecase 'a (t t))

(let ((x (make-array '(3 3))))
  (equal (typecase x
	   (cons "cons")
	   (symbol "symbol")
	   (number "number"))
	 nil))

(let ((x ""))
  (equal (typecase x
	   (t "anything")
	   (otherwise nil))
	 "anything"))

(let ((x ""))
  (and (eql (typecase x
	      (string (setq x 'string) 0)
	      (cons (setq x 'cons) 1)
	      (array (setq x 'array) 2)
	      (t (setq x 't) 9))
	    0)
       (eq x 'string)))

(let ((x (list nil)))
  (and (eql (typecase x
	      (string (setq x 'string) 0)
	      (cons (setq x 'cons) 1)
	      (array (setq x 'array) 2)
	      (t (setq x 't) 9))
	    1)
       (eq x 'cons)))


(let ((x #*01))
  (and (eql (typecase x
	      (string (setq x 'string) 0)
	      (cons (setq x 'cons) 1)
	      (array (setq x 'array) 2)
	      (t (setq x 't) 9))
	    2)
       (eq x 'array)))

(let ((x #\a))
  (and (eql (typecase x
	      (string (setq x 'string) 0)
	      (cons (setq x 'cons) 1)
	      (array (setq x 'array) 2)
	      (t (setq x 't) 9))
	    9)
       (eq x 't)))

(let ((x #*01))
  (and (equal (multiple-value-list (typecase x
				     (string (setq x 'string) (values 'string 0))
				     (cons (setq x 'cons) (values 'cons 1))
				     (array (setq x 'array) (values 'array 2))
				     (t (setq x 't) (values 't 9))))
	      '(array 2))
       (eq x 'array)))


(let ((x 'a))
  (equal (ctypecase x
	   (cons "cons")
	   (symbol "symbol")
	   (number "number"))
	 "symbol"))

(let ((x (list 'a)))
  (equal (ctypecase x
	   (cons "cons")
	   (symbol "symbol")
	   (number "number"))
	 "cons"))

(let ((x 0))
  (equal (ctypecase x
	   (cons "cons")
	   (symbol "symbol")
	   (number "number"))
	 "number"))

(HANDLER-CASE
    (LET ((X (MAKE-ARRAY '(3 3))))
      (CTYPECASE X
	(CONS "cons")
	(SYMBOL "symbol")
	(NUMBER "number")))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))


(HANDLER-CASE
    (LET ((A NIL)) (CTYPECASE A))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

;; (let ((x ""))
;;   (equal (ctypecase x (t "anything"))
;; 	    "anything"))

(let ((x ""))
  (and (eql (ctypecase x
	      (string (setq x 'string) 0)
	      (cons (setq x 'cons) 1)
	      (array (setq x 'array) 2))
	    0)
       (eq x 'string)))

(let ((x (list nil)))
  (and (eql (ctypecase x
	      (string (setq x 'string) 0)
	      (cons (setq x 'cons) 1)
	      (array (setq x 'array) 2))
	    1)
       (eq x 'cons)))


(let ((x #*01))
  (and (eql (ctypecase x
	      (string (setq x 'string) 0)
	      (cons (setq x 'cons) 1)
	      (array (setq x 'array) 2))
	    2)
       (eq x 'array)))

;; (let ((x #\a))
;;   (and (eql (ctypecase x
;; 		 (string (setq x 'string) 0)
;; 		 (cons (setq x 'cons) 1)
;; 		 (array (setq x 'array) 2)
;; 		 (t (setq x 't) 9))
;; 	       9)
;; 	  (eq x 't)))

(HANDLER-CASE
    (LET ((X #\a))
      (CTYPECASE X
	(STRING (SETQ X 'STRING) 0)
	(CONS (SETQ X 'CONS) 1)
	(ARRAY (SETQ X 'ARRAY) 2)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(let ((x #*01))
  (and (equal (multiple-value-list (ctypecase x
				     (string (setq x 'string) (values 'string 0))
				     (cons (setq x 'cons) (values 'cons 1))
				     (array (setq x 'array) (values 'array 2))))
	      '(array 2))
       (eq x 'array)))


(let ((x 'a))
  (equal (etypecase x
	   (cons "cons")
	   (symbol "symbol")
	   (number "number"))
	 "symbol"))

(let ((x (list 'a)))
  (equal (etypecase x
	   (cons "cons")
	   (symbol "symbol")
	   (number "number"))
	 "cons"))

(let ((x 0))
  (equal (etypecase x
	   (cons "cons")
	   (symbol "symbol")
	   (number "number"))
	 "number"))

(HANDLER-CASE
    (PROGN
      (LET ((X (MAKE-ARRAY '(3 3))))
	(ETYPECASE X (CONS "cons") (SYMBOL "symbol") (NUMBER "number"))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))


(HANDLER-CASE
    (PROGN
      (LET ((A NIL))
	(ETYPECASE A)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

;; (let ((x ""))
;;   (equal (etypecase x
;; 		       (t "anything"))
;; 	    "anything"))

(let ((x ""))
  (and (eql (etypecase x
	      (string (setq x 'string) 0)
	      (cons (setq x 'cons) 1)
	      (array (setq x 'array) 2))
	    0)
       (eq x 'string)))

(let ((x (list nil)))
  (and (eql (etypecase x
	      (string (setq x 'string) 0)
	      (cons (setq x 'cons) 1)
	      (array (setq x 'array) 2))
	    1)
       (eq x 'cons)))


(let ((x #*01))
  (and (eql (etypecase x
	      (string (setq x 'string) 0)
	      (cons (setq x 'cons) 1)
	      (array (setq x 'array) 2))
	    2)
       (eq x 'array)))

;; (let ((x #\a))
;;   (and (eql (etypecase x
;; 		 (string (setq x 'string) 0)
;; 		 (cons (setq x 'cons) 1)
;; 		 (array (setq x 'array) 2)
;; 		 (t (setq x 't) 9))
;; 	       9)
;; 	  (eq x 't)))

(HANDLER-CASE
    (PROGN
      (LET ((X #\a))
	(ETYPECASE X
	  (STRING (SETQ X 'STRING) 0)
	  (CONS (SETQ X 'CONS) 1)
	  (ARRAY (SETQ X 'ARRAY) 2))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(let ((x #*01))
  (and (equal (multiple-value-list (etypecase x
				     (string (setq x 'string) (values 'string 0))
				     (cons (setq x 'cons) (values 'cons 1))
				     (array (setq x 'array) (values 'array 2))))
	      '(array 2))
       (eq x 'array)))


(macro-function 'multiple-value-bind)
(equal (multiple-value-bind (f r) 
	   (floor 130 11)
	 (list f r))
       '(11 9))

(multiple-value-bind (a b c d)
    (values 0 1 2 3 4 5)
  (and (eql a 0)
       (eql b 1)
       (eql c 2)
       (eql d 3)))

(multiple-value-bind (a b c d)
    (values 0 1)
  (and (eql a 0)
       (eql b 1)
       (eql c nil)
       (eql d nil)))

(equal (multiple-value-list (multiple-value-bind (a b)
				(values 0 1)
			      (values a b 2 3)))
       '(0 1 2 3))

(multiple-value-bind ()
    (values 0 1 2)
  t)

(null (multiple-value-bind () nil))

(eql (multiple-value-bind (a)
	 (floor 130 11)
       (+ a 10))
     21)

(eql (multiple-value-bind (a)
	 (floor 130 11)
       (+ a 10)
       (incf a 100)
       (+ a 10))
     121)



(equal (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))
       '(1 / 2 3 / / 2 0.5))
(eql (+ (floor 5 3) (floor 19 4)) (+ 1 4))
(eql (multiple-value-call #'+ (floor 5 3) (floor 19 4)) (+ 1 2 4 3))

(let ((list nil))
  (and (eql (multiple-value-call (progn (push 'function list) #'+)
				 (progn (push 0 list) 0)
				 (progn (push 1 list) (values 1 2))
				 (progn (push 2 list) (values 3 4 5))
				 (progn (push 3 list) (values 6 7 8 9)))
	    45)
       (equal (reverse list) '(function 0 1 2 3))))

(eql (multiple-value-call #'+ 0 1 2 3 4) 10)
(eql (multiple-value-call #'+) 0)
(equal (multiple-value-list
	(multiple-value-call #'values
			     0 1 (values 2) (values 3 4) (values 5 6 7)))
       '(0 1 2 3 4 5 6 7))
(special-operator-p 'multiple-value-call)



(macro-function 'multiple-value-list)
(equal (multiple-value-list (floor -3 4)) '(-1 1))
(equal (multiple-value-list
	(progn
	  (values 'a 'b)
	  0))
       '(0))
(equal (multiple-value-list
	(prog1
	  (values 'a 'b)
	  0))
       '(a))

(equal (multiple-value-list
	(multiple-value-prog1
	  (values 'a 'b)
	  0))
       '(a b))

(special-operator-p 'multiple-value-prog1)
(eql (multiple-value-prog1 1 2 3) 1)
(eql (multiple-value-prog1 1 2 3) 1)


(let ((temp '(1 2 3)))
  (multiple-value-bind (a b c)
      (multiple-value-prog1
       (values-list temp)
       (setq temp nil)
       (values-list temp))
    (and (eql a 1)
	 (eql b 2)
	 (eql c 3))))


(zerop (multiple-value-prog1 0
			     (values 0 1)
			     (values 0 1 2)))

(equal (multiple-value-list (multiple-value-prog1 (progn 0
							 (values 0 1)
							 (values 0 1 2))))
       '(0 1 2))


(let (quotient remainder)
  (and (eql (multiple-value-setq (quotient remainder) (truncate 3.2 2)) 1)
       (eql quotient 1)
       (eql remainder 1.2)))
(let ((a 7)
      (b 8)
      (c 9))
  (and (eql (multiple-value-setq (a b c) (values 1 2)) 1)
       (eql a 1)
       (eql b 2)
       (eql c NIL)))

(let ((a 0)
      (b 1))
  (and (eql (multiple-value-setq (a b) (values 4 5 6)) 4)
       (eql a 4)
       (eql b 5)))


(null (multiple-value-list (values-list nil)))
(equal (multiple-value-list (values-list '(1))) '(1))
(equal (multiple-value-list (values-list '(1 2))) '(1 2))
(equal (multiple-value-list (values-list '(1 2 3))) '(1 2 3))

(every #'(lambda (list) (equal (multiple-value-list (values-list list)) list))
       '()
       '(a)
       '(a b)
       '(a b c)
       '(a b c d)
       '(a b c d e)
       '(a b c d e f)
       '(a b c d e f g)
       '(a b c d e f g h))


(macro-function 'nth-value)
(eql (nth-value 0 (values 'a 'b)) 'A)
(eql (nth-value 1 (values 'a 'b)) 'B)
(null (nth-value 2 (values 'a 'b)))
(multiple-value-bind (a b eq?)
    (let* ((x 83927472397238947423879243432432432)
	   (y 32423489732)
	   (a (nth-value 1 (floor x y)))
	   (b (mod x y)))
      (values a b (= a b)))
  (and (eql a 3332987528)
       (eql b 3332987528)
       eq?))

(null (nth-value 0 (values)))
(eql (nth-value 0 1) 1)
(null (nth-value 1 1))
(eql (nth-value 0 (values 0 1 2)) 0)
(eql (nth-value 1 (values 0 1 2)) 1)
(eql (nth-value 2 (values 0 1 2)) 2)
(eql (nth-value 3 (values 0 1 2)) nil)
(eql (nth-value 4 (values 0 1 2)) nil)
(eql (nth-value 5 (values 0 1 2)) nil)


(let ((z (list 0 1 2 3)))
  (eql (prog* ((y z)
	      (x (car y)))
	     (return x))
       (car z)))


(macro-function 'prog)
(macro-function 'prog*)
(let ((a 1))
  (eq (prog ((a 2) (b a)) (return (if (= a b) '= '/=))) '/=))

(eq (prog* ((a 2) (b a)) (return (if (= a b) '= '/=))) '=)
(null (prog () 'no-return-value))

(flet ((king-of-confusion (w)
        "Take a cons of two lists and make a list of conses.
Think of this function as being like a zipper."
	(prog (x y z)			;Initialize x, y, z to NIL
	      (setq y (car w) z (cdr w))
	 loop
	      (cond ((null y) (return x))
		    ((null z) (go err)))
	 rejoin
	      (setq x (cons (cons (car y) (car z)) x))
	      (setq y (cdr y) z (cdr z))
	      (go loop)
	 err
	      (cerror "Will self-pair extraneous items"
		      "Mismatch - gleep!  ~S" y)
	      (setq z y)
	      (go rejoin))))
  (and (equal (king-of-confusion '((0 1 2) . (a b c)))
	      '((2 . C) (1 . B) (0 . A)))
       (equal (king-of-confusion '((0 1 2 3 4 5) . (a b c d e f)))
	      '((5 . F) (4 . E) (3 . D) (2 . C) (1 . B) (0 . A)))))


(null (prog () t))
(null (prog ()))
(eql (let ((a 0)
	   (b 0))
       (prog ((a 10)
	      (b 100))
	     (return (+ a b))))
     110)

(prog (a
       (b 1)
       (c 2))
      (return (and (null a) (eql b 1) (eql c 2))))

(prog ((a 0)
       b
       (c 2))
      (return (and (eql a 0) (null b) (eql c 2))))

(prog ((a 0)
       (b 1)
       c)
      (return (and (eql a 0) (eql b 1) (null c))))

(prog (a b c)
      (return (every #'null (list a b c))))

(eql (let ((a 0))
       (declare (special a))
       (flet ((ref-a () a))
	 (prog ((a 10))
	       (declare (special a))
	       (return (ref-a)))))
     10)

(let ((a 0))
  (declare (special a))
  (and (eql (flet ((ref-a () a))
	      (prog ((a 10)
		     b
		     (c 100))
		    (declare (special a))
		    (setq b 1)
		    (return (+ (ref-a) b c))))
	    111)
       (eql a 0)))

(let ((a 0))
  (declare (special a))
  (and (equal (multiple-value-list (flet ((ref-a () a))
				     (prog ((a 10)
					    b
					    (c 100))
					   (declare (special a))
					   (setq b 1)
					   (return (values (ref-a) b c)))))
	      '(10 1 100))
       (eql a 0)))

(let ((a 0))
  (and (eql (prog () (return a)) 0)
       (eql a 0)))


(flet ((rev (list)
	 (prog ((x list)
		(result nil))
	   top
	    (when (null x)
	      (return result))
	    (psetq x (cdr x)
		   result (cons (car x) result))
	    (go top))))
  (and (equal (rev '(0 1 2 3))
	      '(3 2 1 0))
       (equal (rev nil)
	      nil)
       (equal (rev '(0))
	      '(0))))

(eql (prog (val)
	   (setq val 1)
	   (go point-a)
	   (incf val 16)
      point-c
	   (incf val 04)
	   (go point-b)
	   (incf val 32)
      point-a
	   (incf val 02)
	   (go point-c)
	   (incf val 64)
      point-b
	   (incf val 08)
	   (return val))
     15)

(let ((a 0))
  (and (equal (multiple-value-list (prog ((a 100)
					  (b a)
					  (c 1))
					 (return (values a b c))))
	      '(100 0 1))
       (eql a 0)))


(null (prog* () 'no-return-value))
(flet ((king-of-confusion (w)
        "Take a cons of two lists and make a list of conses.
Think of this function as being like a zipper."
	(prog* (x y z)			;Initialize x, y, z to NIL
	      (setq y (car w) z (cdr w))
	 loop
	      (cond ((null y) (return x))
		    ((null z) (go err)))
	 rejoin
	      (setq x (cons (cons (car y) (car z)) x))
	      (setq y (cdr y) z (cdr z))
	      (go loop)
	 err
	      (cerror "Will self-pair extraneous items"
		      "Mismatch - gleep!  ~S" y)
	      (setq z y)
	      (go rejoin))))
  (and (equal (king-of-confusion '((0 1 2) . (a b c)))
	      '((2 . C) (1 . B) (0 . A)))
       (equal (king-of-confusion '((0 1 2 3 4 5) . (a b c d e f)))
	      '((5 . F) (4 . E) (3 . D) (2 . C) (1 . B) (0 . A)))))

(null (prog* () t))
(null (prog* ()))
(eql (let ((a 0)
	   (b 0))
       (prog* ((a 10)
	      (b 100))
	     (return (+ a b))))
     110)

(prog* (a
       (b 1)
       (c 2))
      (return (and (null a) (eql b 1) (eql c 2))))

(prog* ((a 0)
       b
       (c 2))
      (return (and (eql a 0) (null b) (eql c 2))))

(prog* ((a 0)
       (b 1)
       c)
      (return (and (eql a 0) (eql b 1) (null c))))

(prog* (a b c)
      (return (every #'null (list a b c))))

(eql (let ((a 0))
       (declare (special a))
       (flet ((ref-a () a))
	 (prog* ((a 10))
	       (declare (special a))
	       (return (ref-a)))))
     10)

(let ((a 0))
  (declare (special a))
  (and (eql (flet ((ref-a () a))
	      (prog* ((a 10)
		     b
		     (c 100))
		    (declare (special a))
		    (setq b 1)
		    (return (+ (ref-a) b c))))
	    111)
       (eql a 0)))

(let ((a 0))
  (declare (special a))
  (and (equal (multiple-value-list (flet ((ref-a () a))
				     (prog* ((a 10)
					    b
					    (c 100))
					   (declare (special a))
					   (setq b 1)
					   (return (values (ref-a) b c)))))
	      '(10 1 100))
       (eql a 0)))

(let ((a 0))
  (and (eql (prog* () (return a)) 0)
       (eql a 0)))


(flet ((rev (list)
	 (prog* ((x list)
		(result nil))
	   top
	    (when (null x)
	      (return result))
	    (psetq x (cdr x)
		   result (cons (car x) result))
	    (go top))))
  (and (equal (rev '(0 1 2 3))
	      '(3 2 1 0))
       (equal (rev nil)
	      nil)
       (equal (rev '(0))
	      '(0))))

(eql (prog* (val)
	   (setq val 1)
	   (go point-a)
	   (incf val 16)
      point-c
	   (incf val 04)
	   (go point-b)
	   (incf val 32)
      point-a
	   (incf val 02)
	   (go point-c)
	   (incf val 64)
      point-b
	   (incf val 08)
	   (return val))
     15)

(let ((a 0))
  (and (equal (multiple-value-list (prog* ((a 100)
					   (b a)
					   (c 1))
					  (return (values a b c))))
	      '(100 100 1))
       (eql a 0)))



(macro-function 'prog1)
(macro-function 'prog2)

(eql (let ((temp 1))
       (prog1 temp (incf temp) temp))
     1)
(let ((temp t))
  (and (eq (prog1 temp (setq temp nil)) 't)
       (null temp)))

(equal (multiple-value-list (prog1 (values 1 2 3) 4)) '(1))

(let ((temp (list 'a 'b 'c)))
  (and (eq (prog1 (car temp) (setf (car temp) 'alpha)) 'a)
       (equal temp '(ALPHA B C))))

(equal (flet ((swap-symbol-values (x y)
	        (setf (symbol-value x) 
		      (prog1 (symbol-value y)
			(setf (symbol-value y) (symbol-value x))))))
	 (let ((*foo* 1) (*bar* 2))
	   (declare (special *foo* *bar*))
	   (swap-symbol-values '*foo* '*bar*)
	   (list *foo* *bar*)))
       '(2 1))

(let ((temp 1))
  (and (eql (prog2 (incf temp) (incf temp) (incf temp)) 3)
       (eql temp 4)))

(equal (multiple-value-list (prog2 1 (values 2 3 4) 5)) '(2))
(equal (multiple-value-list (prog2 1 (values 2 3 4) 5 (values 6 7))) '(2))

(eql (prog1 1) 1)
(eql (prog1 1 2) 1)
(eql (prog1 1 2 3) 1)

(equal (multiple-value-list (prog1 (values 1 2 3))) '(1))

(equal (multiple-value-list (prog1
				(values 1 2 3)
			      (values 4 5 6)
			      (values 7 8 9)))
       '(1))

(eql (prog2 1 2) 2)
(eql (prog2 1 2 3) 2)
(eql (prog2 1 2 3 4) 2)

(let ((x 0))
  (and (eql (prog2 (incf x)
		(incf x)
	      (incf x)
	      (incf x))
	    2)
       (eql x 4)))



(let ((x (cons 'a 'b))
      (y (list 1 2 3)))
  (and (equal (setf (car x) 'x (cadr y) (car x) (cdr x) y) '(1 X 3))
       (equal x '(X 1 X 3))
       (equal y '(1 X 3))))

(let ((x (cons 'a 'b))
      (y (list 1 2 3)))
  (and (null (psetf (car x) 'x (cadr y) (car x) (cdr x) y))
       (equal x '(X 1 A 3))
       (equal y '(1 A 3))))

(null (setf))
(null (psetf))

(let ((a 0))
  (and (eql (setf a 10) 10)
       (eql a 10)))

(let ((a 0)
      (b 1))
  (and (eql (setf a 10 b 20) 20)
       (eql a 10)
       (eql b 20)))

(let ((a 0)
      (b 1)
      (c 2))
  (and (eql (setf a 10 b (+ a 10) c (+ b 10)) 30)
       (eql a 10)
       (eql b 20)
       (eql c 30)))

(let ((x (list 0 1 2)))
  (and (eq (setf (car x) 'a) 'a)
       (eq (setf (cadr x) 'b) 'b)
       (eq (setf (caddr x) 'c) 'c)
       (equal x '(a b c))))


(let ((a 0))
  (and (null (psetf a 10))
       (eql a 10)))

(let ((a 0)
      (b 1))
  (and (null (psetf a 10 b 20))
       (eql a 10)
       (eql b 20)))

(let ((a 0)
      (b 1)
      (c 2))
  (and (null (psetf a 10 b (+ a 10) c (+ b 10)))
       (eql a 10)
       (eql b 10)
       (eql c 11)))

(let ((x (list 0 1 2)))
  (and (null (psetf (car x) 'a))
       (null (psetf (cadr x) 'b))
       (null (psetf (caddr x) 'c))
       (equal x '(a b c))))


(let ((x (make-array '(2 3) :initial-contents '((a b c) (x y z)))))
  (and (eql (setf (aref x 0 0) 0.0) 0.0)
       (eql (setf (aref x 0 1) 0.1) 0.1)
       (eql (setf (aref x 0 2) 0.2) 0.2)
       (eql (setf (aref x 1 0) 1.0) 1.0)
       (eql (setf (aref x 1 1) 1.1) 1.1)
       (eql (setf (aref x 1 2) 1.2) 1.2)
       (equalp x #2A((0.0 0.1 0.2) (1.0 1.1 1.2)))))

(let ((x (make-array 4 :element-type 'bit :initial-element 0)))
  (and (equalp x #*0000)
       (eql (setf (bit x 0) 1) 1)
       (eql (setf (bit x 2) 1) 1)
       (equal x #*1010)))

(let ((x (copy-seq "dog")))
  (and (eql (setf (char x 0) #\c) #\c)
       (eql (setf (char x 1) #\a) #\a)
       (eql (setf (char x 2) #\t) #\t)
       (equal x "cat")))

(let ((x (copy-seq "dog")))
  (and (eql (setf (schar x 0) #\c) #\c)
       (eql (setf (schar x 1) #\a) #\a)
       (eql (setf (schar x 2) #\t) #\t)
       (equal x "cat")))

(let ((x (copy-seq "dog")))
  (and (eql (setf (elt x 0) #\c) #\c)
       (eql (setf (elt x 1) #\a) #\a)
       (eql (setf (elt x 2) #\t) #\t)
       (equal x "cat")))

(let ((x (list 0 1 2)))
  (and (eql (setf (elt x 0) #\c) #\c)
       (eql (setf (elt x 1) #\a) #\a)
       (eql (setf (elt x 2) #\t) #\t)
       (equal x '(#\c #\a #\t))))

(let ((x #'(lambda (a) (+ a 10)))
      (saved (when (fboundp 'test-fn) (fdefinition 'test-fn))))
  (unwind-protect (and (eq (setf (fdefinition 'test-fn) x) x)
		       (eql (test-fn 10) 20))
    (when saved
      (setf (fdefinition 'test-fn) saved))))

(let ((table (make-hash-table)))
  (and (equal (multiple-value-list (gethash 1 table)) '(NIL NIL))
       (equal (multiple-value-list (gethash 1 table 2)) '(2 NIL))
       (equal (setf (gethash 1 table) "one") "one")
       (equal (setf (gethash 2 table "two") "two") "two")
       (multiple-value-bind (value present-p) (gethash 1 table)
	 (and (equal value "one")
	      present-p))
       (multiple-value-bind (value present-p) (gethash 2 table)
	 (and (equal value "two")
	      present-p))))

(let ((table (make-hash-table)))
  (and (equal (multiple-value-list (gethash nil table)) '(NIL NIL))
       (null (setf (gethash nil table) nil))
       (multiple-value-bind (value present-p) (gethash nil table)
	 (and (equal value NIL)
	      present-p))))

(let ((x (copy-seq #*0101)))
  (and (eql (setf (sbit x 0) 1) 1)
       (eql (setf (sbit x 2) 1) 1)
       (equal x #*1111)))


(let ((a 0)
      (b 1))
  (and (equal (multiple-value-list (setf (values a b) (values 'x 'y 'z)))
	      '(x y))
       (eq a 'x)
       (eq b 'y)))

(let ((x (list 0 1 2))
      (order nil))
  (and
   (equal (multiple-value-list (setf (values (car   (prog1 x (push 0 order)))
					     (cadr  (prog1 x (push 1 order)))
					     (caddr (prog1 x (push 2 order))))
				     (values 'a 'b)))
	  '(a b nil))
   (equal x '(a b nil))
   (equal order '(2 1 0))))


(let ((a 'a)
      (b 'b)
      (c 'c))
  (and (equal (multiple-value-list (setf (values (values a) (values b c))
					 (values 0 1 2 3 4)))
	      '(0 1))
       (eql a 0)
       (eql b 1)
       (null c)))

(let ((a 'a)
      (b 'b)
      (c 'c)
      (d 'd))
  (and (equal (multiple-value-list (setf (values (values a b) (values c d))
					 (values 0 1 2 3 4)))
	      '(0 1))
       (eql a 0)
       (null b)
       (eql c 1)
       (null d)))

(let ((a 'a)
      (b 'b)
      (c 'c)
      (d 'd))
  (and (equal (multiple-value-list (setf (values (values a b) (values c d))
					 (values 0)))
	      '(0 nil))
       (eql a 0)
       (null b)
       (null c)
       (null d)))

(let ((a 'a)
      (b 'b)
      (c 'c))
  (and (equal (multiple-value-list (setf (values a) (values 0 1 2)))
	      '(0))
       (eql a 0)
       (eq b 'b)
       (eq c 'c)))


(let ((x (list 1 2 3))
      (y 'trash))
  (and (eq (shiftf y x (cdr x) '(hi there)) 'TRASH)
       (equal x '(2 3))
       (equal y '(1 HI THERE))))

(let ((x (list 'a 'b 'c)))
  (and (eq (shiftf (cadr x) 'z) 'B)
       (equal x '(A Z C))
       (eq (shiftf (cadr x) (cddr x) 'q) 'Z)
       (equal x '(A (C) . Q))))

(let ((n 0)
      (x (list 'a 'b 'c 'd)))
  (and (eq (shiftf (nth (setq n (+ n 1)) x) 'z) 'B)
       (equal x '(A Z C D))))


(let ((a 0)
      (b 1)
      (c 2)
      (d 3))
  (and (equal (multiple-value-list (shiftf (values a b) (values c d)
					   (values 4 5)))
	      '(0 1))
       (eql a 2)
       (eql b 3)
       (eql c 4)
       (eql d 5)))



(let ((n 0)
      (x (list 'a 'b 'c 'd 'e 'f 'g)))
  (and (null (rotatef (nth (incf n) x)
		      (nth (incf n) x)
		      (nth (incf n) x)))
       (equal x '(A C D B E F G))))

(let ((x (list 'a 'b 'c)))
  (and (null (rotatef (first x) (second x) (third x)))
       (equal x '(b c a))))

(let ((x (list 'a 'b 'c 'd 'e 'f)))
  (and (null (rotatef (second x) (third x) (fourth x) (fifth x)))
       (equal x '(a c d e b f))))

(null (rotatef))
(let ((a 0))
  (and (null (rotatef a))
       (zerop a)))

(let ((x (list 'a 'b 'c))
      (order nil))
  (and (null (rotatef (first  (progn (push 1 order) x))
		      (second (progn (push 2 order) x))
		      (third  (progn (push 3 order) x))))
       (equal x '(b c a))
       (equal order '(3 2 1))))

(let ((x (list 'a 'b 'c))
      (order nil))
  (and (null (psetf (first  (progn (push 1 order) x))
		    (second (progn (push 2 order) x))
		    
		    (second (progn (push 2 order) x))
		    (third  (progn (push 3 order) x))

		    (third  (progn (push 3 order) x))
		    (first  (progn (push 1 order) x))))
       (equal x '(b c a))
       (equal order '(1 3 3 2 2 1))))

(let ((a 0)
      (b 1)
      (c 2)
      (d 3))
  (and (null (rotatef (values a b) (values c d)))
       (eql a 2)
       (eql b 3)
       (eql c 0)
       (eql d 1)))

