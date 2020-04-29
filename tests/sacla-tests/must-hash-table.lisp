;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: must-hash-table.lisp,v 1.8 2004/08/09 02:49:54 yuji Exp $
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

(let ((table (make-hash-table)))
  (and (hash-table-p table)
       (eql (setf (gethash "one" table) 1) 1)
       (equal (multiple-value-list (gethash (copy-seq "one") table))
	      '(NIL NIL))))

(let ((table (make-hash-table :test 'equal)))
  (and (hash-table-p table)
       (eql (setf (gethash "one" table) 1) 1)
       (equal (multiple-value-list (gethash (copy-seq "one") table))
	      '(1 T))))

(make-hash-table :rehash-size 1.5 :rehash-threshold 0.7)

(make-hash-table)
(hash-table-p (make-hash-table))
#-CLISP ; Bruno: unfounded expectations about hash-table-test
(dolist (test '(eq eql equal equalp) t)
  (let ((hash-table (make-hash-table :test test)))
    (unless (and (hash-table-p hash-table)
                 (eq (hash-table-test hash-table) test))
      (return nil))))
#-CLISP ; Bruno: unfounded expectations about hash-table-test
(dolist (test '(eq eql equal equalp) t)
  (let* ((test-function (symbol-function test))
         (hash-table (make-hash-table :test test-function)))
    (unless (and (hash-table-p hash-table)
		 (eq (hash-table-test hash-table) test))
      (return nil))))

(hash-table-p (make-hash-table :size 0))
(hash-table-p (make-hash-table :size 1))
(hash-table-p (make-hash-table :size 2))
(hash-table-p (make-hash-table :size 3))
(hash-table-p (make-hash-table :size 1000))
(hash-table-p (make-hash-table :rehash-size 1))
(hash-table-p (make-hash-table :rehash-size 100))
(hash-table-p (make-hash-table :rehash-size 1.5))
#-clisp (hash-table-p (make-hash-table :rehash-size 1.0))
(hash-table-p (make-hash-table :rehash-threshold 0))
(hash-table-p (make-hash-table :rehash-threshold 0.0))
(hash-table-p (make-hash-table :rehash-threshold 0.1))
(hash-table-p (make-hash-table :rehash-threshold 0.12))
(hash-table-p (make-hash-table :rehash-threshold 0.5))
(hash-table-p (make-hash-table :rehash-threshold 2/3))
(hash-table-p (make-hash-table :rehash-threshold 0.888))
(hash-table-p (make-hash-table :rehash-threshold 0.99))
(hash-table-p (make-hash-table :rehash-threshold 1))
(hash-table-p (make-hash-table :rehash-threshold 1.0))

(let ((table (make-hash-table :size 0 :rehash-size 1.1 :rehash-threshold 0)))
  (and (dotimes (i 10 t)
	 (setf (gethash i table) i))
       (dotimes (i 10 t)
	 (unless (eql (gethash i table) i)
	   (return nil)))
       (hash-table-p table)))

(let ((table (make-hash-table :size 1 :rehash-size 1 :rehash-threshold 1)))
  (and (dotimes (i 100 t)
	 (setf (gethash i table) i))
       (dotimes (i 100 t)
	 (unless (eql (gethash i table) i)
	   (return nil)))
       (hash-table-p table)))



(not (hash-table-p 'hash-table))

(let ((table (make-hash-table)))
  (hash-table-p table))
(not (hash-table-p 37))
(not (hash-table-p '((a . 1) (b . 2))))
(not (hash-table-p (type-of (make-hash-table))))


(let ((table (make-hash-table)))
  (and (zerop (hash-table-count table))
       (equal (setf (gethash 57 table) "fifty-seven") "fifty-seven")
       (eql (hash-table-count table) 1)
       (dotimes (i 100 t) (setf (gethash i table) i))
       (eql (hash-table-count table) 100)))

(zerop (hash-table-count (make-hash-table)))
(let ((table (make-hash-table)))
  (and (eql (setf (gethash 'key table) 9) 9)
       (= (hash-table-count table) 1)))


#-CLISP ;Bruno: unfounded expectations about hash-table-rehash-size
(let ((table (make-hash-table :size 100 :rehash-size 1.4)))
  (= (hash-table-rehash-size table) 1.4))

#-CLISP ;Bruno: unfounded expectations about hash-table-rehash-threshold
(let ((table (make-hash-table :size 100 :rehash-threshold 0.5)))
  (= (hash-table-rehash-threshold table) 0.5))

(<= 0 (hash-table-size (make-hash-table)))

#-CLISP ;Bruno: unfounded expectations about hash-table-test
(eq 'eq (hash-table-test (make-hash-table :test 'eq)))
#-CLISP ;Bruno: unfounded expectations about hash-table-test
(eq 'eq (hash-table-test (make-hash-table :test #'eq)))
#-CLISP ;Bruno: unfounded expectations about hash-table-test
(eq 'eql (hash-table-test (make-hash-table)))
#-CLISP ;Bruno: unfounded expectations about hash-table-test
(eq 'eql (hash-table-test (make-hash-table :test 'eql)))
#-CLISP ;Bruno: unfounded expectations about hash-table-test
(eq 'eql (hash-table-test (make-hash-table :test #'eql)))
#-CLISP ;Bruno: unfounded expectations about hash-table-test
(eq 'equal (hash-table-test (make-hash-table :test 'equal)))
#-CLISP ;Bruno: unfounded expectations about hash-table-test
(eq 'equal (hash-table-test (make-hash-table :test #'equal)))
#-CLISP ;Bruno: unfounded expectations about hash-table-test
(eq 'equalp (hash-table-test (make-hash-table :test 'equalp)))
#-CLISP ;Bruno: unfounded expectations about hash-table-test
(eq 'equalp (hash-table-test (make-hash-table :test #'equalp)))

(let* ((table0 (make-hash-table))
       (table  (make-hash-table
		:size (hash-table-size table0)
		:test (hash-table-test table0)
		:rehash-threshold (hash-table-rehash-threshold table0)
		:rehash-size (hash-table-rehash-size table0))))
  (and (hash-table-p table)
       (zerop (hash-table-count table))
       (eq (type-of table) 'hash-table)))


(let ((table (make-hash-table)))
  (and (equal (multiple-value-list (gethash 1 table)) '(NIL NIL))
       (equal (multiple-value-list (gethash 1 table 2)) '(2 nil))
       (equal (setf (gethash 1 table) "one") "one")
       (equal (setf (gethash 2 table "two") "two") "two")
       (multiple-value-bind (value present-p) (gethash 1 table)
	 (and (equal value "one") present-p))
       (multiple-value-bind (value present-p) (gethash 2 table)
	 (and (equal value "two") present-p))
       (equal (multiple-value-list (gethash nil table)) '(nil nil))
       (null (setf (gethash nil table) nil))
       (multiple-value-bind (value present-p) (gethash nil table)
	 (and (not value) present-p))))

(multiple-value-bind (value present-p)
    (gethash 'key (make-hash-table) 'default)
  (and (eq value 'default) (not present-p)))

(multiple-value-bind (value present-p)
    (gethash 'key (make-hash-table))
  (and (null value) (not present-p)))


(let ((table (make-hash-table)))
  (and (multiple-value-bind (value present-p) (gethash 'key table)
	 (and (null value) (not present-p)))
       (eql (setf (gethash 'key table) 100) 100)
       (multiple-value-bind (value present-p) (gethash 'key table)
	 (and (eql value 100) present-p))))

(let ((table (make-hash-table))
      (list nil))
  (and (eql (setf (gethash (progn (push 0 list) 0)
			   (progn (push 1 list) table)
			   (progn (push 2 list) 'default))
		  (progn (push 3 list) 9))
	    9)
       (equal list '(3 2 1 0))))

(let ((table (make-hash-table)))
  (and (dotimes (i 100 t)
	 (unless (eql (setf (gethash i table) (* i 10)) (* i 10))
	   (return nil)))
       (= (hash-table-count table) 100)
       (dotimes (i 100 t)
	 (unless (multiple-value-bind (value present-p) (gethash i table)
		   (and (eql value (* i 10)) present-p))
	   (return nil)))))


(let ((table (make-hash-table)))
  (and (equal (setf (gethash 100 table) "C") "C")
       (multiple-value-bind (value present-p) (gethash 100 table)
	 (and (equal value "C") present-p))
       (remhash 100 table)
       (multiple-value-bind (value present-p) (gethash 100 table)
	 (and (not value) (not present-p)))
       (not (remhash 100 table))))

(let ((table (make-hash-table)))
  (and (zerop (hash-table-count table))
       (eql (setf (gethash 'a table) 'abc) 'abc)
       (multiple-value-bind (value present-p) (gethash 'a table)
	 (and (eq value 'abc) present-p))
       (eql (hash-table-count table) 1)
       (remhash 'a table)
       (multiple-value-bind (value present-p) (gethash 'a table)
	 (and (not value) (not present-p)))
       (zerop (hash-table-count table))))

(not (remhash 'key (make-hash-table)))


(with-hash-table-iterator (iterator (make-hash-table))
  (macrolet ((test (&environment env)
	       (if (macro-function 'iterator env) t nil)))
    (test)))


(let ((table (make-hash-table))
      (alist nil))
  (dotimes (i 10) (setf (gethash i table) i))
  (with-hash-table-iterator (iterator table)
    (loop
     (multiple-value-bind (more key value) (iterator)
       (unless more
	 (return))
       (push (list key value) alist))))
  (setq alist (sort alist #'< :key #'car))
  (equal alist '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9))))


(let ((table (make-hash-table))
      (eval 0))
  (dotimes (i 10) (setf (gethash i table) i))
  (with-hash-table-iterator (iterator (progn (incf eval) table))
    (loop
     (multiple-value-bind (more key value) (iterator)
       (declare (ignore key value))
       (unless more
	 (return)))))
  (eql eval 1))


(with-hash-table-iterator (iterator (make-hash-table))
  (null (iterator)))


(let ((table (make-hash-table))
      alist0 alist1 alist2)
  (dotimes (i 100) (setf (gethash i table) i))
  (and (with-hash-table-iterator (iterator0 table)
	 (with-hash-table-iterator (iterator1 table)
	   (with-hash-table-iterator (iterator2 table)
	     (loop
              (multiple-value-bind (more0 key0 value0) (iterator0)
		(multiple-value-bind (more1 key1 value1) (iterator1)
		  (multiple-value-bind (more2 key2 value2) (iterator2)
		    (unless (or (every #'null (list more0 more1 more2))
				(every #'identity (list more0 more1 more2)))
		      (return nil))
		    (when (every #'null (list more0 more1 more2))
		      (return t))
		    (push (cons key0 value0) alist0)
		    (push (cons key1 value1) alist1)
		    (push (cons key2 value2) alist2))))))))
       (equal (sort alist0 #'< :key #'car)
	      (setq alist1 (sort alist1 #'< :key #'car)))
       (equal alist1 (sort alist2 #'< :key #'car))))
       


(let ((table (make-hash-table :rehash-size 100))
      (n 0)
      (alist nil))
  (and (dolist (key '(a b c d e f g h i j k) t)
	 (unless (eql (setf (gethash key table) n) n)
	   (return nil))
	 (incf n))
       (remhash 'b table)
       (remhash 'd table)
       (remhash 'f table)
       (remhash 'h table)
       (remhash 'j table)
       (not (remhash 'b table))
       (not (remhash 'd table))
       (not (remhash 'f table))
       (not (remhash 'h table))
       (not (remhash 'j table))
       (with-hash-table-iterator (iterator table)
         (loop
	  (multiple-value-bind (more key value) (iterator)
	    (unless more
	      (return t))
	    (push (cons key value) alist))))
       (equal (sort alist #'< :key #'cdr)
	      '((a . 0) (c . 2) (e . 4) (g . 6) (i . 8) (k . 10)))))


(let ((table (make-hash-table)))
  (and (null (dotimes (i 10) (setf (gethash i table) i)))
       (eql (let ((sum-of-squares 0))
	      (maphash #'(lambda (key val) 
			   (let ((square (* val val)))
			     (incf sum-of-squares square)
			     (setf (gethash key table) square)))
		       table)
	      sum-of-squares)
	    285)
       (eql (hash-table-count table) 10)
       (null (maphash #'(lambda (key val)
			  (when (oddp val) (remhash key table)))
		      table))
       (eql (hash-table-count table) 5)
       (let ((alist nil))
	 (and (null (maphash #'(lambda (key val)
				 (push (list key val) alist))
			     table))
	      (equalp (sort alist #'< :key #'car)
		      '((0 0) (2 4) (4 16) (6 36) (8 64)))))))


(let ((table (make-hash-table))
      (alist nil))
  (and (null (dotimes (i 10) (setf (gethash i table) i)))
       (null (maphash #'(lambda (key val)
			  (if (evenp key)
			      (setf (gethash key table) (* val val))
			    (remhash key table)))
		      table))
       (null (maphash #'(lambda (key val) (push (cons key val) alist)) table))
       (equal (sort alist #'< :key #'car)
	      '((0 . 0) (2 . 4) (4 . 16) (6 . 36) (8 . 64)))))



(flet ((test-hash-table-iterator (hash-table)
         (let ((all-entries '())
	       (generated-entries '())
	       (unique (list nil)))
	   (maphash #'(lambda (key value) (push (list key value) all-entries))
		    hash-table)
	   (with-hash-table-iterator (generator-fn hash-table)
	     (loop     
	      (multiple-value-bind (more? key value) (generator-fn)
		(unless more? (return))
		(unless (eql value (gethash key hash-table unique))
		  (error "Key ~S not found for value ~S" key value))
		(push (list key value) generated-entries))))
	   (unless (= (length all-entries)
		      (length generated-entries)
		      (length (union all-entries generated-entries
				     :key #'car
				     :test (hash-table-test hash-table))))
	     (error "Generated entries and Maphash entries don't correspond"))
	   t)))
  (let ((table (make-hash-table :rehash-size 100))
	(n 0))
    (and (dolist (key '(a b c d e f g h i j k) t)
	   (unless (eql (setf (gethash key table) n) n)
	     (return nil))
	   (incf n))
	 (remhash 'b table)
	 (remhash 'd table)
	 (remhash 'f table)
	 (remhash 'h table)
	 (remhash 'j table)
	 (not (remhash 'b table))
	 (not (remhash 'd table))
	 (not (remhash 'f table))
	 (not (remhash 'h table))
	 (not (remhash 'j table))
	 (test-hash-table-iterator table)
	 (test-hash-table-iterator (make-hash-table)))))




(let ((table (make-hash-table)))
  (and (null (dotimes (i 100) (setf (gethash i table) (format nil "~R" i))))
       (eql (hash-table-count table) 100)
       (multiple-value-bind (value present-p) (gethash 57 table)
	 (and (equal value "fifty-seven") present-p))
       (hash-table-p (clrhash table))
       (zerop (hash-table-count table))
       (multiple-value-bind (value present-p) (gethash 57 table)
	 (and (null value) (not present-p)))))


(let ((code (sxhash 'a)))
  (and (typep code 'fixnum)
       (<= 0 code)))

(dolist (item '(a "" #\a (make-hash-table) (make-array '(2 3 4)) #*0101 "xx") t)
  (let ((code (sxhash item)))
    (unless (and (typep code 'fixnum) (<= 0 code))
      (return nil))))



(let ((table (make-hash-table :rehash-threshold 0.8)))
  (and (eql (setf (gethash 'key table) 'value0) 'value0)
       (eql (hash-table-count table) 1)
       (eql (setf (gethash 'key table) 'value1) 'value1)
       (eql (hash-table-count table) 1)
       (eq (gethash 'key table) 'value1)))

(let ((table (make-hash-table :rehash-threshold 0.8)))
  (and (eql (setf (gethash 'key0 table) 'value0) 'value0)
       (eql (hash-table-count table) 1)
       (eql (setf (gethash 'key1 table) 'value1) 'value1)
       (eql (hash-table-count table) 2)
       (eql (setf (gethash 'key2 table) 'value2) 'value2)
       (eql (hash-table-count table) 3)
       (eql (setf (gethash 'key0 table) 'value00) 'value00)
       (eql (hash-table-count table) 3)
       (eql (setf (gethash 'key2 table) 'value22) 'value22)
       (eql (hash-table-count table) 3)
       (eq (gethash 'key0 table) 'value00)
       (eq (gethash 'key1 table) 'value1)
       (eq (gethash 'key2 table) 'value22)))


(let ((table (make-hash-table :size 0 :test 'eq))
      (key0 (copy-seq "key"))
      (key1 (copy-seq "key")))
  (and (not (eq key0 key1))
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (null value) (not present-p)))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 0 :test 'eql))
      (key0 (copy-seq "key"))
      (key1 (copy-seq "key")))
  (and (not (eql key0 key1))
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (null value) (not present-p)))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 0 :test 'eql))
      (key0 1.0)
      (key1 1.0))
  (and (eql key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 0 :test 'eql))
      (key0 #\a)
      (key1 #\a))
  (and (eql key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 0 :test 'eql))
      (key0 #\a)
      (key1 #\A))
  (and (not (eql key0 key1))
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (null value) (not present-p)))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 16 :test 'equal))
      (key0 'key)
      (key1 'key))
  (and (eq key0 key1)
       (equal key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 0 :test 'equal))
      (key0 1.0)
      (key1 1.0))
  (and (equal key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 0 :test 'equal))
      (key0 #\a)
      (key1 #\a))
  (and (equal key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 0 :test 'equal))
      (key0 #\a)
      (key1 #\A))
  (and (not (equal key0 key1))
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (null value) (not present-p)))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 16 :test 'equal))
      (key0 (copy-seq "key"))
      (key1 (copy-seq "key")))
  (and (not (eq key0 key1))
       (equal key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 16 :test 'equal))
      (key0 (copy-seq "key"))
      (key1 (copy-seq "KEY")))
  (and (not (eq key0 key1))
       (not (equal key0 key1))
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (null value) (not present-p)))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 10 :test 'equal))
      (key0 (copy-seq '(key)))
      (key1 (copy-seq '(key))))
  (and (not (eq key0 key1))
       (equal key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 16 :test 'equal))
      (key0 (copy-seq #*1010))
      (key1 (copy-seq #*1010)))
  (and (not (eq key0 key1))
       (equal key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 16 :test 'equal))
      (key0 (copy-seq #(a b c)))
      (key1 (copy-seq #(a b c))))
  (and (not (eq key0 key1))
       (not (equal key0 key1))
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (null value) (not present-p)))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 10 :test 'equal))
      (key0 (make-pathname))
      (key1 (make-pathname)))
  (and (not (eq key0 key1))
       (equal key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))


(let ((table (make-hash-table :size 0 :test 'equalp))
      (key0 (copy-seq "key"))
      (key1 (copy-seq "key")))
  (and (equalp key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 0 :test 'equalp))
      (key0 1.0)
      (key1 1.0))
  (and (equalp key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 100 :test 'equalp))
      (key0 1)
      (key1 1.0))
  (and (not (eq key0 key1))
       (equalp key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 0 :test 'equalp))
      (key0 #\a)
      (key1 #\a))
  (and (equalp key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 10 :test 'equalp))
      (key0 #\a)
      (key1 #\A))
  (and (not (eq key0 key1))
       (equalp key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 3 :test 'equalp))
      (key0 (copy-seq '(#\a)))
      (key1 (copy-seq '(#\A))))
  (and (not (eq key0 key1))
       (equalp key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :size 3 :test 'equalp))
      (key0 (copy-seq '(#\a (1))))
      (key1 (copy-seq '(#\A (1.0)))))
  (and (not (eq key0 key1))
       (equalp key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table :test 'equalp))
      (key0 (make-hash-table))
      (key1 (make-hash-table)))
  (and (not (eq key0 key1))
       (equalp key0 key1)
       (eq (setf (gethash key0 table) 'value) 'value)
       (multiple-value-bind (value present-p) (gethash key1 table)
	 (and (eq value 'value) present-p))
       (multiple-value-bind (value present-p) (gethash key0 table)
	 (and (eq value 'value) present-p))))

(let ((table (make-hash-table)))
  (and (zerop (hash-table-count table))
       (dolist (pair '((a abc) (a bc) (1 "one") (1.0 "ONE") (#\a a) (#\A b)) t)
	 (unless (eq (setf (gethash (car pair) table) (cadr pair)) (cadr pair))
	   (return nil)))
       (eql (hash-table-count table) 5)
       (eq (gethash 'a table) 'bc)
       (equal (gethash 1 table) "one")
       (equal (gethash 1.0 table) "ONE")
       (eql (gethash #\A table) 'b)
       (eql (gethash #\a table) 'a)))

