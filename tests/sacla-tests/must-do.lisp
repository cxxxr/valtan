;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: must-do.lisp,v 1.8 2004/02/20 07:23:42 yuji Exp $
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

;; dotimes
(null (dotimes (i 10)))
(= (dotimes (temp-one 10 temp-one)) 10)
(let ((temp-two 0))
  (and (eq t (dotimes (temp-one 10 t) (incf temp-two)))
       (eql temp-two 10)))

(progn
  (defun palindromep (string &optional (start 0) (end (length string)))
    (dotimes (k (floor (- end start) 2) t)
      (unless (char-equal (char string (+ start k))
			  (char string (- end k 1)))
	(return nil))))
  (and (palindromep "Able was I ere I saw Elba")
       (not (palindromep "A man, a plan, a canal--Panama!"))
       (equal (remove-if-not #'alpha-char-p          ;Remove punctuation.
			   "A man, a plan, a canal--Panama!")
	      "AmanaplanacanalPanama")
       (palindromep (remove-if-not #'alpha-char-p
				   "A man, a plan, a canal--Panama!"))
       (palindromep
	(remove-if-not #'alpha-char-p
		       "Unremarkable was I ere I saw Elba Kramer, nu?"))))


(let ((count 0))
  (eql (dotimes (i 5 count) (incf count)) 5))

(let ((count 0))
  (eql (dotimes (i 1 count) (incf count)) 1))

(let ((count 0))
  (zerop (dotimes (i 0 count) (incf count))))

(let ((count 0))
  (zerop (dotimes (i -1 count) (incf count))))

(let ((count 0))
  (zerop (dotimes (i -100 count) (incf count))))

(eql (dotimes (i 3 i)) 3)
(eql (dotimes (i 2 i)) 2)
(eql (dotimes (i 1 i)) 1)
(eql (dotimes (i 0 i)) 0)
(eql (dotimes (i -1 i)) 0)
(eql (dotimes (i -2 i)) 0)
(eql (dotimes (i -10 i)) 0)

(let ((list nil))
  (and (eq (dotimes (i 10 t) (push i list)) t)
       (equal list '(9 8 7 6 5 4 3 2 1 0))))

(let ((list nil))
  (equal (dotimes (i 10 (push i list)) (push i list))
	 '(10 9 8 7 6 5 4 3 2 1 0)))

(let ((list nil))
  (equal (dotimes (i '10 (push i list)) (push i list))
	 '(10 9 8 7 6 5 4 3 2 1 0)))

(let ((list nil))
  (equal (dotimes (i (/ 100 10) (push i list)) (push i list))
	 '(10 9 8 7 6 5 4 3 2 1 0)))

(null (dotimes (i 10 t) (return nil)))

(equal (multiple-value-list (dotimes (i 10 t) (return (values 'a 'b 'c))))
       '(a b c))

(let ((val 0))
  (= (dotimes (i 10 val)
       (incf val 1)
       (when (< i 9)
	 (go lp))
       (incf val 2)
       lp
       (incf val 3))
     42))

(= (let ((val 0))
     (dotimes (i 10 val)
       (when (< i 9)
	 (go loop))
       9
       (incf val 100)
       (go last)
       loop
       (when (= i 0)
	 (go 9))
       (incf val)
       last))
   208)

(= 3 (let ((i 3)) (dotimes (i i i) (declare (fixnum i)))))
(= 3 (let ((x 0)) (dotimes (i 3 x) (declare (fixnum i)) (incf x))))
(= 3 (dotimes (i 3 i) (declare (fixnum i))))
(= 3 (let ((x 0)) (dotimes (i 3 x) (declare (fixnum i)) (incf x))))
(equal '((8 6 4 2 0) (9 7 5 3 1))
       (let (even odd)
	 (dotimes (i 10 (list even odd))
	   (cond
	     ((evenp i) (go even))
	     ((oddp i) (go odd))
	     (t (error "logic error")))
	   even (push i even) (go end)
	   odd (push i odd) (go end)
	   end)))


;; dolist
(let ((list (copy-tree '((0) (1) (2) (3)))))
  (and (null (dolist (item list) (incf (car item))))
       (equal list '((1) (2) (3) (4)))))

(eq 'ok (dolist (x '(0 1 2) t) (return 'ok)))
(eq 'ok (dolist (x '(0 1 2) t) (return-from nil 'ok)))
(equal '(ok fine)
       (multiple-value-list (dolist (x '(0 1 2) t) (return (values 'ok 'fine)))))
(equal '(ok fine)
       (multiple-value-list (dolist (x '(0 1 2) t)
			      (return-from nil (values 'ok 'fine)))))

(null (let ((x '(0 1 2))) (dolist (x x x))))
(= 3 (let ((x '(0 1 2)) (i 0)) (dolist (x x i) (incf i))))

(null (dolist (x '())))
(null (dolist (x '(a))))
(eq t (dolist (x nil t)))
(= 6 (let ((sum 0))
       (dolist (x '(0 1 2 3) sum)
	 (declare (fixnum x))
	 (incf sum x))))

(equal '(5 4 3 2 1)
       (let (stack)
	 (flet ((f () (declare (special x)) (1+ x)))
	   (dolist (x '(0 1 2 3 4) stack)
	     (declare (special x))
	     (declare (type fixnum x))
	     (push (f) stack)))))

(equal '((3 1) (4 2 0))
       (let (odd even)
	 (dolist (x '(0 1 2 3 4) (list odd even))
	   (cond
	     ((oddp x) (go odd))
	     ((evenp x) (go even))
	     (t (error "This code mustn't have got executed.")))
	   odd (push x odd) (go loop-end)
	   even (push x even) (go loop-end)
	   loop-end)))


(let ((temp-two '()))
  (equal (dolist (temp-one '(1 2 3 4) temp-two) (push temp-one temp-two))
	 '(4 3 2 1)))

(let ((temp-two 0))
  (and (null (dolist (temp-one '(1 2 3 4)) (incf temp-two)))
       (eql temp-two 4)))


(null (dolist (var nil var)))
(let ((list nil))
  (equal (dolist (var '(0 1 2 3) list)
	   (push var list))
	 '(3 2 1 0)))

(let ((list nil))
  (equal (dolist (var '(0 1 2 3) (push var list))
	   (push var list))
	 '(nil 3 2 1 0)))


(null (dolist (var '(0 1 2 3))))

(let ((list nil))
  (and (null (dolist (var '(0 1 2 3))
	       (push var list)))
       (equal list '(3 2 1 0))))

(let ((list nil))
  (and (eq (dolist (var '() t) (push var list)) t)
       (null list)))

(let ((list '((a) (b) (c)))
      (count 0))
  (dolist (var list t)
    (unless (eq (nth count list) var)
      (return nil))
    (incf count)))

(let ((list nil))
  (and (null (dolist (var '(0 1 2 3) t)
	       (if (= var 2)
		   (return)
		 (push var list))))
       (equal list '(1 0))))

(let ((val 0))
  (= (dolist (var '(a b c) val)
       (incf val 1)
       (unless (eq var 'c)
	 (go lp))
       (incf val 2)
       lp
       (incf val 3))
     14))

(= (let ((val 0))
     (dolist (i '(0 1 2 3 4 5 6 7 8 9) val)
       (when (< i 9)
	 (go loop))
       9
       (incf val 100)
       (go last)
       loop
       (when (= i 0)
	 (go 9))
       (incf val)
       last))
   208)

(let ((val 0))
  (= (dolist (i '(0 1 2 3 4 5 6 7 8 9) val)
       (incf val 1)
       (when (< i 9)
	 (go lp))
       (incf val 2)
       lp
       (incf val 3))
     42))

(eq 'ok (block nil
	  (tagbody
	     (dolist (x '(0 1 2 3) t) (when (oddp x) (go there)))
	   there (return 'ok))))



;; do
(flet ((rev (list)
	    (do ((x list (cdr x))
		 (reverse nil (cons (car x) reverse)))
		((null x) reverse))))
  (and (null (rev nil))
       (equal (rev '(0 1 2 3 4)) '(4 3 2 1 0))))

(flet ((nrev (list)
	 (do ((1st (cdr list) (cdr 1st))
	      (2nd list 1st)
	      (3rd '() 2nd))
	     ((null 2nd) 3rd)
	   (rplacd 2nd 3rd))))
  (and (null (nrev nil))
       (equal (nrev (list 0 1 2 3 4)) '(4 3 2 1 0))))

(flet ((sub (list start end)
	  (do* ((x (nthcdr start list) (cdr x))
		(i start (1+ i))
		(result (list nil))
		(splice result))
	      ((>= i end) (cdr result))
	    (setq splice (cdr (rplacd splice (list (car x))))))))
  (and (eq (sub '() 0 0) '())
       (equal (sub '(0 1 2 3) 1 4) '(1 2 3))
       (equal (sub '(0 1 2 3) 1 1) '())
       (equal (sub '(0 1 2 3) 1 2) '(1))
       (equal (sub '(0 1 2 3) 1 3) '(1 2))))


(eql (do ((temp-one 1 (1+ temp-one))
	  (temp-two 0 (1- temp-two)))
	 ((> (- temp-one temp-two) 5) temp-one))
     4)

(eql (do ((temp-one 1 (1+ temp-one))
	  (temp-two 0 (1+ temp-one)))     
	 ((= 3 temp-two) temp-one))
     3)

(eql (do* ((temp-one 1 (1+ temp-one))
	   (temp-two 0 (1+ temp-one)))
	 ((= 3 temp-two) temp-one))
     2)

(let  ((a-vector (vector 1 nil 3 nil)))
  (and (null (do ((i 0 (+ i 1))
		  (n (array-dimension a-vector 0)))
		 ((= i n))
	       (when (null (aref a-vector i))
		 (setf (aref a-vector i) 0))))
       (equalp a-vector #(1 0 3 0))))

(let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
  (equalp (do ((i 0 (1+ i))
	       n
	       (j 9 (1- j)))
	      ((>= i j) vec)
	    (setq n (aref vec i))
	    (setf (aref vec i) (aref vec j))
	    (setf (aref vec j) n))
	  #(9 8 7 6 5 4 3 2 1 0)))

(let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
  (and (null (do ((i 0 (1+ i))
		  n
		  (j 9 (1- j)))
		 ((>= i j))
	       (setq n (aref vec i))
	       (setf (aref vec i) (aref vec j))
	       (setf (aref vec j) n)))
       (equalp vec #(9 8 7 6 5 4 3 2 1 0))))

(let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
  (and (null (do ((i 0 (1+ i))
		  n
		  (j 9 (1- j)))
		 ((>= i j))
	       (declare (fixnum i j n))
	       (setq n (aref vec i))
	       (setf (aref vec i) (aref vec j))
	       (setf (aref vec j) n)))
       (equalp vec #(9 8 7 6 5 4 3 2 1 0))))

(let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
  (and (null (do ((i 0 (1+ i))
		  n
		  (j 9 (1- j)))
		 ((>= i j))
	       (declare (fixnum i))
	       (declare (fixnum j))
	       (declare (fixnum n))
	       (setq n (aref vec i))
	       (setf (aref vec i) (aref vec j))
	       (setf (aref vec j) n)))
       (equalp vec #(9 8 7 6 5 4 3 2 1 0))))

(let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
  (and (null (do (n
		  (i 0 (1+ i))
		  (j 9 (1- j)))
		 ((>= i j))
	       (declare (fixnum i))
	       (declare (fixnum j))
	       (declare (fixnum n))
	       (setq n (aref vec i))
	       (setf (aref vec i) (aref vec j))
	       (setf (aref vec j) n)))
       (equalp vec #(9 8 7 6 5 4 3 2 1 0))))

(let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
  (and (null (do ((i 0 (1+ i))
		  (j 9 (1- j))
		  n)
		 ((>= i j))
	       (declare (fixnum i))
	       (declare (fixnum j))
	       (declare (fixnum n))
	       (setq n (aref vec i))
	       (setf (aref vec i) (aref vec j))
	       (setf (aref vec j) n)))
       (equalp vec #(9 8 7 6 5 4 3 2 1 0))))

(= (do* ((list (list 0 1 2 3 4 5 6 7 8 9) (cdr list))
	 (elm (car list) (car list))
	 (n 0 (+ n (or elm 0))))
       ((endp list) n))
   45)

(= (do* ((list (list 0 1 2 3 4 5 6 7 8 9) (cdr list))
	 (elm (car list) (car list))
	 (n 0))
       ((endp list) n)
     (incf n elm))
   45)


(let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
  (and (null (do* (n
		   (i 0 (1+ i))
		   (j (- 9 i) (- 9 i)))
		 ((>= i j))
	       (declare (fixnum i))
	       (declare (fixnum j))
	       (declare (fixnum n))
	       (setq n (aref vec i))
	       (setf (aref vec i) (aref vec j))
	       (setf (aref vec j) n)))
       (equalp vec #(9 8 7 6 5 4 3 2 1 0))))

(let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
  (and (null (do* ((i 0 (1+ i))
		   n
		   (j (- 9 i) (- 9 i)))
		 ((>= i j))
	       (declare (fixnum i j n))
	       (setq n (aref vec i))
	       (setf (aref vec i) (aref vec j))
	       (setf (aref vec j) n)))
       (equalp vec #(9 8 7 6 5 4 3 2 1 0))))

(let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
  (and (null (do* ((i 0 (1+ i))
		   (j (- 9 i) (- 9 i))
		   n)
		 ((>= i j))
	       (declare (fixnum i j n))
	       (setq n (aref vec i))
	       (setf (aref vec i) (aref vec j))
	       (setf (aref vec j) n)))
       (equalp vec #(9 8 7 6 5 4 3 2 1 0))))

(let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
  (and (null (do* ((i 0 (1+ i))
		   (j (- 9 i) (- 9 i))
		   n)
		 ((>= i j))
	       (setf n (aref vec i)
		     (aref vec i) (aref vec j)
		     (aref vec j) n)))
       (equalp vec #(9 8 7 6 5 4 3 2 1 0))))

