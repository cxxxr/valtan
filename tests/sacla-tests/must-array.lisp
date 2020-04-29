;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: must-array.lisp,v 1.9 2004/08/09 02:49:54 yuji Exp $
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

(arrayp (make-array nil))
(arrayp (make-array 10))
(vectorp (make-array 10))
(arrayp (make-array '(1 2)))
(arrayp (make-array '(1 2 3)))
(arrayp (make-array '(1 2 3 4)))
(arrayp (make-array '(1 2 3 4 5)))
(arrayp (make-array '(3 3 3)))
(arrayp (make-array '(3 0 3)))
(arrayp (make-array '5 :element-type 'character :displaced-to "array"))
(arrayp "")
(arrayp "array")
(arrayp (make-array '(2 3 4) :adjustable t))
(arrayp (make-array 6))
(arrayp #*1011)
(arrayp "hi")
(not (arrayp 'hi))
(not (arrayp 12))



(let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
  (and (eql (aref array 0 0) 0)
       (eql (aref array 0 1) 1)
       (eql (aref array 0 2) 2)
       (eql (aref array 1 0) 3)
       (eql (aref array 1 1) 4)
       (eql (aref array 1 2) 5)))

(let ((array (make-array '(3 2 1)
			 :initial-contents '(((0) (1)) ((2) (3)) ((4) (5))))))
  (and (eql (aref array 0 0 0) 0)
       (eql (aref array 0 1 0) 1)
       (eql (aref array 1 0 0) 2)
       (eql (aref array 1 1 0) 3)
       (eql (aref array 2 0 0) 4)
       (eql (aref array 2 1 0) 5)))

(let ((array (make-array '(2 2 2 2)
			 :initial-contents
			 '((((0 1) (2 3)) ((4 5) (6 7))) 
			   (((8 9) (10 11)) ((12 13) (14 15)))))))
  (and (eql (aref array 0 0 0 0) 0)
       (eql (aref array 0 0 0 1) 1)
       (eql (aref array 0 0 1 0) 2)
       (eql (aref array 0 0 1 1) 3)
       (eql (aref array 0 1 0 0) 4)
       (eql (aref array 0 1 0 1) 5)
       (eql (aref array 0 1 1 0) 6)
       (eql (aref array 0 1 1 1) 7)
       (eql (aref array 1 0 0 0) 8)
       (eql (aref array 1 0 0 1) 9)
       (eql (aref array 1 0 1 0) 10)
       (eql (aref array 1 0 1 1) 11)
       (eql (aref array 1 1 0 0) 12)
       (eql (aref array 1 1 0 1) 13)
       (eql (aref array 1 1 1 0) 14)
       (eql (aref array 1 1 1 1) 15)))

(let ((array (make-array '(3 3 3 3 3 3) :initial-element nil)))
  (dotimes (i 729)
    (setf (row-major-aref array i) i))
  (dotimes (i 729 t)
    (unless (= (aref array
		     (floor i (* 3 3 3 3 3))
		     (floor (mod i (* 3 3 3 3 3)) (* 3 3 3 3))
		     (floor (mod i (* 3 3 3 3)) (* 3 3 3))
		     (floor (mod i (* 3 3 3)) (* 3 3))
		     (floor (mod i (* 3 3)) (* 3))
		     (mod i 3))
	       i)
      (return nil))))


(zerop (aref (make-array '() :initial-contents 0)))
(let ((array (make-array 10 :initial-contents '(0 1 2 3 4 5 6 7 8 9)))
      (ok t))
  (dotimes (i 10)
    (unless (eql (aref array i) i)
      (setq ok nil)
      (return)))
  ok)

(let ((array (vector 0 1 2 3 4 5 6 7 8 9))
      (ok t))
  (dotimes (i 10)
    (unless (eql (aref array i) i)
      (setq ok nil)
      (return)))
  ok)

(let ((array "0123456789")
      (ok t))
  (dotimes (i 10)
    (unless (char= (aref array i) (char "0123456789" i))
      (setq ok nil)
      (return)))
  ok)


(let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
  (equal (array-dimensions array) '(2 3)))

(equal (array-dimensions (make-array 4)) '(4))
(equal (array-dimensions (make-array '(2 3))) '(2 3))
(equal (array-dimensions (make-array 4 :fill-pointer 2)) '(4))
(equal (array-dimensions (make-array '(2 3 4 5 6))) '(2 3 4 5 6))

(eql (array-dimension (make-array 4) 0) 4)
(eql (array-dimension (make-array '(2 3)) 1) 3)
(eql (array-dimension (make-array '(2 3 4)) 2) 4)

(eq (array-element-type (make-array 4)) t)
(equal (array-element-type (make-array 12 :element-type '(unsigned-byte 8)))
       (upgraded-array-element-type '(unsigned-byte 8)))


(let ((array (make-array '())))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (and (not displaced-to)
	 (zerop displaced-index-offset))))

(let ((array (make-array '10)))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (and (not displaced-to)
	 (zerop displaced-index-offset))))

(let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (and (not displaced-to)
	 (zerop displaced-index-offset))))

(let* ((source (make-array '(2 5)
			   :initial-contents '((1 2 3 4 5) (11 12 13 14 15))))
       (array (make-array 10 :displaced-to source)))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (and (eq displaced-to source)
	 (zerop displaced-index-offset))))

(let* ((source (make-array '10 :initial-element 0))
       (array (make-array '(5 2) :displaced-to source)))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (and (eq displaced-to source)
	 (zerop displaced-index-offset))))

(let* ((e0-0 (list 0 0))
       (e0-1 (list 0 1))
       (e1-0 (list 1 0))
       (e1-1 (list 1 1))
       (source (make-array '(2 2)
			   :initial-contents (list (list e0-0 e0-1)
						   (list e1-0 e1-1))))
       (array (make-array 4 :displaced-to source)))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (and (eq displaced-to source)
	 (zerop displaced-index-offset)
	 (eq (aref array 0) e0-0)
	 (eq (aref array 1) e0-1)
	 (eq (aref array 2) e1-0)
	 (eq (aref array 3) e1-1))))


(let* ((e0-0 (list 0 0))
       (e0-1 (list 0 1))
       (e1-0 (list 1 0))
       (e1-1 (list 1 1))
       (source (make-array '(2 2)
			   :initial-contents (list (list e0-0 e0-1)
						   (list e1-0 e1-1))))
       (array (make-array 2 :displaced-to source :displaced-index-offset 1)))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (and (eq displaced-to source)
	 (eql displaced-index-offset 1)
	 (eq (aref array 0) e0-1)
	 (eq (aref array 1) e1-0))))

(let ((array (make-array 4
			 :element-type 'character
			 :displaced-to "0123456789"
			 :displaced-index-offset 6)))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (and (string= displaced-to "0123456789")
	 (eql displaced-index-offset 6)
	 (eql (aref array 0) #\6)
	 (eql (aref array 1) #\7)
	 (eql (aref array 2) #\8)
	 (eql (aref array 3) #\9))))

(let ((array (make-array '(1 2 5)
			 :element-type 'character
			 :displaced-to "0123456789")))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (and (string= displaced-to "0123456789")
	 (eql displaced-index-offset 0)
	 (eql (aref array 0 0 0) #\0)
	 (eql (aref array 0 0 1) #\1)
	 (eql (aref array 0 0 2) #\2)
	 (eql (aref array 0 0 3) #\3)
	 (eql (aref array 0 0 4) #\4)
	 (eql (aref array 0 1 0) #\5)
	 (eql (aref array 0 1 1) #\6)
	 (eql (aref array 0 1 2) #\7)
	 (eql (aref array 0 1 3) #\8)
	 (eql (aref array 0 1 4) #\9))))

(let* ((source (make-array '(2 5)
			   :initial-contents '("love&" "peace")
			   :element-type 'character))
       (array (make-array 10 :displaced-to source :element-type 'character)))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (and (eq displaced-to source)
	 (eql displaced-index-offset 0)
	 (string= array "love&peace"))))

(array-in-bounds-p (make-array 5) 4)
(not (array-in-bounds-p (make-array 5) -1))
(let ((a (make-array '(7 11) :element-type 'string-char)))
  (and (array-in-bounds-p a 0  0)
       (array-in-bounds-p a 6 10)
       (not (array-in-bounds-p a 0 -1))
       (not (array-in-bounds-p a 0 11))
       (not (array-in-bounds-p a 7  0))))




(let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
  (eql (array-rank array) 2))

(zerop (array-rank (make-array '())))
(eql (array-rank (make-array 10)) 1)
(eql (array-rank (make-array '(2 10))) 2)
(eql (array-rank (make-array '(2 10 1))) 3)
(eql (array-rank (make-array '(2 10 1 3))) 4)
(eql (array-rank "") 1)
(eql (array-rank "a") 1)

(zerop (array-row-major-index (make-array '())))
(zerop (array-row-major-index (make-array '5) 0))
(eql (array-row-major-index (make-array '5) 4) 4)
(eql (array-row-major-index (make-array '10) 3) 3)
(zerop (array-row-major-index (make-array '(3 4)) 0 0))
(eql (array-row-major-index (make-array '(3 4)) 2 3) 11)
(zerop (array-row-major-index (make-array '(3 4 5)) 0 0 0))
(eql (array-row-major-index (make-array '(3 4 5)) 2 3 4) 59)


(let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
  (eql (array-total-size array) 6))

(eql (array-total-size (make-array 4)) 4)
(eql (array-total-size (make-array 4 :fill-pointer 2)) 4)
(eql (array-total-size (make-array 0)) 0)
(eql (array-total-size (make-array '(4 2))) 8)
(eql (array-total-size (make-array '(4 0))) 0)
(eql (array-total-size (make-array '())) 1)
(eql (array-total-size (make-array '(2 3 4 5))) (* 2 3 4 5))


(let ((array (make-array 10
			 :initial-contents '(0 1 2 3 4 5 6 7 8 9)
			 :fill-pointer 0)))
  (dotimes (i 10 t)
    (unless (eql (aref array i) i)
      (return nil))))

(let ((array (make-array '(10 10) :element-type 'number :initial-element 0)))
  (dotimes (i 10)
    (dotimes (j 10)
      (unless (zerop (aref array i j))
	(return nil))
      (setf (aref array i j) (+ (* i 10) j))))
  (dotimes (i 100 t)
    (unless (eql (row-major-aref array i) i)
      (return nil))))

(let ((array (make-array '())))
  (setf (aref array) 100)
  (eql (aref array) 100))

(let ((array (make-array 10 :initial-contents '(a b c d e f g h i j))))
  (setf (aref array 0) #\a)
  (setf (aref array 2) #\c)
  (setf (aref array 4) #\e)
  (setf (aref array 6) #\g)
  (setf (aref array 8) #\i)
  (and (eql (aref array 0) #\a) (eql (aref array 1) 'b)
       (eql (aref array 2) #\c) (eql (aref array 3) 'd)
       (eql (aref array 4) #\e) (eql (aref array 5) 'f)
       (eql (aref array 6) #\g) (eql (aref array 7) 'h)
       (eql (aref array 8) #\i) (eql (aref array 9) 'j)))

(let ((array (vector 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
  (setf (aref array 0) #\a)
  (setf (aref array 2) #\c)
  (setf (aref array 4) #\e)
  (setf (aref array 6) #\g)
  (setf (aref array 8) #\i)
  (and (eql (aref array 0) #\a) (eql (aref array 1) 'b)
       (eql (aref array 2) #\c) (eql (aref array 3) 'd)
       (eql (aref array 4) #\e) (eql (aref array 5) 'f)
       (eql (aref array 6) #\g) (eql (aref array 7) 'h)
       (eql (aref array 8) #\i) (eql (aref array 9) 'j)))

(let ((array (make-array '(3 4 5) :initial-element 0 :element-type 'number)))
  (setf (aref array 0 0 0) 0)
  (setf (aref array 1 1 1) 1)
  (setf (aref array 2 2 2) 2)
  (dotimes (i 3 t)
    (unless (eql (aref array i i i) i)
      (return nil))))

(let* ((array (make-array '(3 4 5 6 7)
			  :initial-element 0 :element-type 'number))
       (array2 (make-array (* 3 4 5 6 7) :displaced-to array)))
  (setf (aref array 2 3 4 5 6) 100)
  (setf (aref array 0 0 0 0 0) 200)
  (eql (reduce #'+ array2) 300))

(adjustable-array-p (make-array 5
				:element-type 'character 
				:adjustable t 
				:fill-pointer 3))

(let ((array (adjust-array (make-array '(2 3)
				       :initial-contents '((0 1 2) (3 4 5))
				       :adjustable t)
			   '(3 2) :initial-element 'undefined)))
  (and (eql (aref array 0 0) 0)
       (eql (aref array 0 1) 1)
       (eql (aref array 1 0) 3)
       (eql (aref array 1 1) 4)
       (eql (aref array 2 0) 'undefined)
       (eql (aref array 2 1) 'undefined)))

(let ((array (adjust-array (make-array '(2 3)
				       :initial-contents '((0 1 2) (3 4 5))
				       :adjustable t)
			   '(3 2) :initial-element 'undefined)))
  (equal (array-dimensions array) '(3 2)))

(let ((array (adjust-array (make-array '(2 3)
                                       :initial-contents '((0 1 2) (3 4 5))
                                       :adjustable t)
                           '(3 2) :initial-element 'undefined)))
  (not (array-has-fill-pointer-p array)))

(let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
  (not (array-has-fill-pointer-p array)))

(array-has-fill-pointer-p (make-array 10 :fill-pointer 0))
(array-has-fill-pointer-p (make-array 8 :fill-pointer 0 :initial-element 8))
(not (array-has-fill-pointer-p (make-array '(2 3 4))))



(let ((array (adjust-array (make-array '(2 3)
				       :initial-contents '((0 1 2) (3 4 5))
				       :adjustable t)
			   '(3 2) :initial-element 'undefined)))
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (and (not displaced-to)
	 (zerop displaced-index-offset))))

(let ((array (adjust-array (make-array '(2 3)
				       :initial-contents '((0 1 2) (3 4 5))
				       :adjustable t)
			   '(3 2) :initial-element 'undefined)))
  (eql (array-rank array) 2))

(let ((array (adjust-array (make-array '(2 3)
				       :initial-contents '((0 1 2) (3 4 5))
				       :adjustable t)
			   '(3 2) :initial-element 'undefined)))
  (eql (array-total-size array) 6))


(eql (fill-pointer (make-array 8 :fill-pointer 4)) 4)
(let ((array (make-array 8 :fill-pointer 4 :initial-element nil)))
  (and (eql (length array) 4)
       (setf (fill-pointer array) 3)
       (eql (fill-pointer array) 3)
       (eql (length array) 3)))

(let ((vector (make-array 10
			  :fill-pointer 0
			  :initial-element #\Space
			  :element-type 'character)))
  (and (eql (vector-push #\a vector) 0)
       (eql (vector-push #\b vector) 1)
       (eql (vector-push #\c vector) 2)
       (string= vector "abc")))

(let ((vector (make-array 3 :fill-pointer t :initial-contents '(a b c))))
  (and (eql (array-dimension vector 0) (fill-pointer vector))
       (equal (concatenate 'list vector) '(a b c))
       (zerop (setf (fill-pointer vector) 0))
       (null (concatenate 'list vector))
       (eql (vector-push 'x vector) 0)
       (equal (concatenate 'list vector) '(x))
       (eq (vector-pop vector) 'x)
       (zerop (length vector))))

(let ((vector (make-array 10 :fill-pointer 0 :initial-element nil)))
  (and (eql (length vector) 0)
       (setf (fill-pointer vector) 10)
       (eql (length vector) 10)
       (setf (fill-pointer vector) 5)
       (eql (length vector) 5)))


(let ((array (make-array '(3 2 1)
			 :initial-contents '(((0) (1)) ((2) (3)) ((4) (5))))))
  (and (eql (aref array 0 0 0) (row-major-aref array 0))
       (eql (aref array 0 1 0) (row-major-aref array 1))
       (eql (aref array 1 0 0) (row-major-aref array 2))
       (eql (aref array 1 1 0) (row-major-aref array 3))
       (eql (aref array 2 0 0) (row-major-aref array 4))
       (eql (aref array 2 1 0) (row-major-aref array 5))))

(let ((array (make-array '(3 2 1)
			 :initial-contents '(((0) (1)) ((2) (3)) ((4) (5))))))
  (and (eql 0 (row-major-aref array 0))
       (eql 1 (row-major-aref array 1))
       (eql 2 (row-major-aref array 2))
       (eql 3 (row-major-aref array 3))
       (eql 4 (row-major-aref array 4))
       (eql 5 (row-major-aref array 5))))


(let* ((array0 (make-array '(3 2 1)
			   :initial-contents '(((0) (1)) ((2) (3)) ((4) (5)))))
       (array1 (make-array 6 :displaced-to array0)))
  (and (eql (aref array1	0) (row-major-aref array0 0))
       (eql (aref array1	1) (row-major-aref array0 1))
       (eql (aref array1	2) (row-major-aref array0 2))
       (eql (aref array1	3) (row-major-aref array0 3))
       (eql (aref array1	4) (row-major-aref array0 4))
       (eql (aref array1	5) (row-major-aref array0 5))))

(let* ((array0 (make-array 6
			   :element-type 'character
			   :initial-contents "abcdef"))
       (array1 (make-array '(3 2 1)
			   :displaced-to array0
			   :element-type 'character)))
  (and (eql (aref array0 0) (row-major-aref array1 0))
       (eql (aref array0 1) (row-major-aref array1 1))
       (eql (aref array0 2) (row-major-aref array1 2))
       (eql (aref array0 3) (row-major-aref array1 3))
       (eql (aref array0 4) (row-major-aref array1 4))
       (eql (aref array0 5) (row-major-aref array1 5))))

(let* ((array0 (make-array 6
			   :element-type 'character
			   :initial-contents "abcdef"))
       (array1 (make-array '(3 2 1)
			   :displaced-to array0
			   :element-type 'character)))
  (and (eql #\a (row-major-aref array1 0))
       (eql #\b (row-major-aref array1 1))
       (eql #\c (row-major-aref array1 2))
       (eql #\d (row-major-aref array1 3))
       (eql #\e (row-major-aref array1 4))
       (eql #\f (row-major-aref array1 5))))

(let ((array (make-array '(3 2 1) :initial-element nil)))
  (setf (row-major-aref array 0) 'a)
  (setf (row-major-aref array 1) 'b)
  (setf (row-major-aref array 2) 'c)
  (setf (row-major-aref array 3) 'd)
  (setf (row-major-aref array 4) 'e)
  (and (eql (aref array 0 0 0) 'a)
       (eql (aref array 0 1 0) 'b)
       (eql (aref array 1 0 0) 'c)
       (eql (aref array 1 1 0) 'd)
       (eql (aref array 2 0 0) 'e)
       (eql (aref array 2 1 0) 'nil)))

(let ((str "abcdefg"))
  (dotimes (i 7 t)
    (unless (eql (char str 0) (row-major-aref str 0))
      (return nil))))

(let ((str (make-array 5 :initial-contents "abcde")))
  (dotimes (i 3)
    (setf (row-major-aref str i) (row-major-aref str (- 4 i))))
  (and (char= (row-major-aref str 0) #\e)
       (char= (row-major-aref str 1) #\d)
       (char= (row-major-aref str 2) #\c)
       (char= (row-major-aref str 3) #\d)
       (char= (row-major-aref str 4) #\e)))


(eq (upgraded-array-element-type t) t)
(and (subtypep (upgraded-array-element-type 'bit) 'bit)
     (subtypep 'bit (upgraded-array-element-type 'bit)))
(and (subtypep (upgraded-array-element-type 'base-char) 'base-char)
     (subtypep 'base-char (upgraded-array-element-type 'base-char)))
(and (subtypep (upgraded-array-element-type 'character) 'character)
     (subtypep 'character (upgraded-array-element-type 'character)))


(simple-vector-p (make-array 6))
(not (simple-vector-p "aaaaaa"))

(let ((sv (make-array 10)))
  (dotimes (i 10)
    (setf (svref sv i) (* i i)))
  (dotimes (i 10 t)
    (unless (eql (svref sv i) (* i i))
      (return nil))))

(let ((sv (vector 'a 'b 'c 'd 'e 'f)))
  (and (eq (svref sv 0) 'a)
       (eq (svref sv 1) 'b)
       (eq (svref sv 2) 'c)
       (eq (svref sv 3) 'd)
       (eq (svref sv 4) 'e)
       (eq (svref sv 5) 'f)))

(let ((sv (make-array 3 :initial-contents '(1 2 last))))
  (and (simple-vector-p sv)
       (eq (svref sv 2) 'last)
       (eql (svref sv 1) 2)
       (eql (svref sv 0) 1)
       (eql (setf (svref sv 1) 'last-but-one) 'last-but-one)
       (eq (svref sv 1) 'last-but-one)))


(let ((vec (vector 1 2 'last)))
  (and (arrayp vec)
       (vectorp vec)
       (simple-vector-p vec)
       (eql (length vec) 3)
       (equal (concatenate 'list vec) '(1 2 last))))

(eq (vector-pop (make-array 3 :initial-contents '(a b c) :fill-pointer t)) 'c)
(eq (vector-pop (make-array 3 :initial-contents '(a b c) :fill-pointer 3)) 'c)
(eq (vector-pop (make-array 3 :initial-contents '(a b c) :fill-pointer 2)) 'b)
(eq (vector-pop (make-array 3 :initial-contents '(a b c) :fill-pointer 1)) 'a)

(let ((vec (make-array 3 :fill-pointer 0)))
  (and (eql (vector-push 'a vec) 0)
       (eql (vector-push 'b vec) 1)
       (eql (vector-push 'c vec) 2)
       (eq (vector-pop vec) 'c)
       (eq (vector-pop vec) 'b)
       (eq (vector-pop vec) 'a)))

(let ((vec (make-array 3 :fill-pointer t :initial-contents '(a b c))))
  (and (setf (fill-pointer vec) 1)
       (eql (vector-push 'y vec) 1)
       (eql (vector-push 'z vec) 2)
       (eq (vector-pop vec) 'z)
       (eq (vector-pop vec) 'y)
       (eq (vector-pop vec) 'a)
       (eql (fill-pointer vec) 0)))

(let ((vec (make-array 3 :fill-pointer t :initial-contents '(a b c))))
  (and (not (vector-push 'x vec))
       (not (vector-push 'y vec))
       (eql (setf (fill-pointer vec) 0) 0)
       (eql (vector-push 'x vec) 0)
       (eql (vector-push 'y vec) 1)
       (eql (vector-push 'z vec) 2)
       (not (vector-push 'l vec))))



(let ((vec (make-array 3
		       :fill-pointer 2
		       :initial-contents '(a b l)
		       :adjustable t)))
  (and (eql (length vec) 2)
       (eql (vector-push-extend 'c vec) 2)
       (eql (length vec) 3)
       (eq (vector-pop vec) 'c)
       (eql (vector-push-extend 'c vec) 2)
       (eql (vector-push-extend 'x vec) 3)
       (eql (vector-push-extend 'y vec) 4)
       (eql (vector-push-extend 'z vec) 5)
       (eql (length vec) 6)))


(let ((vec (make-array 0
		       :fill-pointer t
		       :adjustable t)))
  (dotimes (i 50)
    (vector-push-extend (* i i) vec))
  (dotimes (i 50 t)
    (unless (eql (vector-pop vec) (* (- 49 i) (- 49 i)))
      (return nil))))

(let ((vec (make-array 10
		       :element-type 'character
		       :initial-contents "abcdefghij"
		       :adjustable t
		       :fill-pointer t)))
  (and (eql (vector-push-extend #\x vec) 10)
       (eql (vector-push-extend #\y vec) 11)
       (eql (vector-push-extend #\z vec) 12)
       (string= vec "abcdefghijxyz")))

(vectorp "aaaaaa")
(vectorp (make-array 6 :fill-pointer t))
(not (vectorp (make-array '(2 3 4))))
(vectorp #*11)
(not (vectorp #b11))
(vectorp (make-array 3 :displaced-to "abc" :element-type 'character))

(eql (bit (make-array 8 :element-type 'bit :initial-element 1) 3)
     1)
(eql (sbit (make-array 8 :element-type 'bit :initial-element 1) 3)
     1)

(let ((ba (make-array 8
		      :element-type 'bit
		      :initial-contents '(0 1 0 1 0 1 0 1))))
  (dotimes (i 8 t)
    (unless (or (and (evenp i) (zerop (bit ba i)))
		(and (oddp i) (eql (bit ba i) 1)))
      (return nil))))

(let ((ba (make-array 8
		      :element-type 'bit
		      :initial-contents '(0 1 0 1 0 1 0 1))))
  (dotimes (i 8 t)
    (unless (or (and (evenp i) (zerop (sbit ba i)))
		(and (oddp i) (eql (sbit ba i) 1)))
      (return nil))))

(let ((ba (make-array '(3 3)
		      :element-type 'bit
		      :initial-contents '((0 1 0) (1 0 1) (0 1 0)))))
  (and (zerop (bit ba 0 0))
       (eql (bit ba 0 1) 1)
       (zerop (bit ba 0 2))
       (eql (bit ba 1 0) 1)
       (zerop (bit ba 1 1))
       (eql (bit ba 1 2) 1)
       (zerop (bit ba 2 0))
       (eql (bit ba 2 1) 1)
       (zerop (bit ba 2 2))))

(let ((ba (make-array '(3 3)
                      :element-type 'bit
                      :initial-contents '((0 1 0) (1 0 1) (0 1 0)))))
  (and (zerop (sbit ba 0 0))
       (eql (sbit ba 0 1) 1)
       (zerop (sbit ba 0 2))
       (eql (sbit ba 1 0) 1)
       (zerop (sbit ba 1 1))
       (eql (sbit ba 1 2) 1)
       (zerop (sbit ba 2 0))
       (eql (sbit ba 2 1) 1)
       (zerop (sbit ba 2 2))))

(let ((ba (make-array '(3 3 3) :element-type 'bit)))
  (dotimes (i (* 3 3 3))
    (setf (bit ba
               (floor i 9)
               (floor (mod i 9) 3)
               (mod i 3))
          (if (evenp i) 0 1)))
  (dotimes (i (* 3 3 3) t)
    (unless (eql (row-major-aref ba i) (if (evenp i) 0 1))
      (return nil))))

(let ((ba (make-array '(3 3 3) :element-type 'bit)))
  (dotimes (i (* 3 3 3))
    (setf (sbit ba
                (floor i 9)
                (floor (mod i 9) 3)
                (mod i 3))
          (if (evenp i) 0 1)))
  (dotimes (i (* 3 3 3) t)
    (unless (eql (row-major-aref ba i) (if (evenp i) 0 1))
      (return nil))))

(let ((ba (make-array '(1 2 3 4 5) :element-type 'bit)))
  (dotimes (i (* 1 2 3 4 5))
    (setf (bit ba
               (floor i (* 1 2 3 4 5))
               (floor (mod i (* 2 3 4 5)) (* 3 4 5))
               (floor (mod i (* 3 4 5)) (* 4 5))
               (floor (mod i (* 4 5)) 5)
               (mod i 5))
          (if (evenp i) 0 1)))
  (dotimes (i (* 1 2 3 4 5) t)
    (unless (eql (row-major-aref ba i) (if (evenp i) 0 1))
      (return nil))))

(let ((ba (make-array '(1 2 3 4 5) :element-type 'bit)))
  (dotimes (i (* 1 2 3 4 5))
    (setf (sbit ba
                (floor i (* 1 2 3 4 5))
                (floor (mod i (* 2 3 4 5)) (* 3 4 5))
                (floor (mod i (* 3 4 5)) (* 4 5))
                (floor (mod i (* 4 5)) 5)
                (mod i 5))
          (if (evenp i) 0 1)))
  (dotimes (i (* 1 2 3 4 5) t)
    (unless (eql (row-major-aref ba i) (if (evenp i) 0 1))
      (return nil))))

(let ((ba (make-array 8 :element-type 'bit  :initial-element 1)))
  (and (eql (setf (bit ba 3) 0) 0)
       (eql (bit ba 3) 0)
       (eql (sbit ba 5) 1)
       (eql (setf (sbit ba 5) 0) 0)
       (eql (sbit ba 5) 0)))


(let ((ba (make-array 10 :element-type 'bit :fill-pointer 0)))
  (dotimes (i 10)
    (vector-push (if (oddp i) 0 1) ba))
  (dotimes (i 10 t)
    (unless (and (eql (bit ba i) (if (oddp i) 0 1))
		 (or (not (simple-vector-p ba))
		     (eql (sbit ba i) (if (oddp i) 0 1)))
		 (eql (aref ba i) (if (oddp i) 0 1)))
      (return nil))))

(let ((ba (make-array 10 :element-type 'bit :fill-pointer 0)))
  (dotimes (i 10)
    (vector-push (if (oddp i) 0 1) ba))
  (dotimes (j 10 t)
    (let ((i (- 9 j)))
      (unless (and (eql (bit ba i) (if (oddp i) 0 1))
		   (or (not (simple-vector-p ba))
		       (eql (sbit ba i) (if (oddp i) 0 1)))
		   (eql (aref ba i) (if (oddp i) 0 1))
		   (eql (vector-pop ba) (if (oddp i) 0 1)))
	(return nil)))))


(equal (bit-and #*11101010 #*01101011) #*01101010)
(equal (bit-and #*11101010 #*01101011 nil) #*01101010)
(equal (bit-and (make-array 8 :element-type 'bit :initial-contents #*11101010)
		#*01101011 t)
       #*01101010)
(equal (bit-and #*11101010
		#*01101011
		(make-array 8 :element-type 'bit))
       #*01101010)

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
       (ba  (bit-and ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equal ba #*01101010)))
  
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
       (ba (bit-and ba1 #*10101010 t)))
  (and (eq ba1 ba)
       (equal ba1 #*00000000)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba (bit-and ba1 #*00111110 t)))
  (and (eq ba1 ba)
       (equal ba1 #*00110000)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
       (ba3 (make-array 8 :element-type 'bit))
       (ba4 (bit-and ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equal ba3 #*00110000)
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))

(equalp (bit-and (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101))
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*000 #*000)))

(equalp (bit-and (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101))
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010))
                 nil)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*000 #*000)))


(equalp (bit-and (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*101 #*010))
		 t)
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*000 #*000)))

(equalp (bit-and (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*101 #*010))
		 (make-array '(2 3)
			     :element-type 'bit))
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*000 #*000)))

(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-and ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equalp ba (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*000 #*000)))))


(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba  (bit-and ba1 ba2 t)))
  (and (eq ba1 ba)
       (equalp ba1 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*000 #*000)))))


(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba3 (make-array '(2 3)
			:element-type 'bit))
       (ba4 (bit-and ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equalp ba3 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*000 #*000)))
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))


(equal (bit-andc1 #*11101010 #*01101011) #*00000001)
(equal (bit-andc1 #*11101010 #*01101011 nil) #*00000001)
(equal (bit-andc1 (make-array 8 :element-type 'bit :initial-contents #*11101010)
		  #*01101011 t)
       #*00000001)
(equal (bit-andc1 #*11101010
		  #*01101011
		  (make-array 8 :element-type 'bit))
       #*00000001)

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
       (ba  (bit-andc1 ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equal ba #*00000001)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
       (ba (bit-andc1 ba1 #*10101010 t)))
  (and (eq ba1 ba)
       (equal ba1 #*10101010)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba (bit-andc1 ba1 #*00111110 t)))
  (and (eq ba1 ba)
       (equal ba1 #*00001110)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
       (ba3 (make-array 8 :element-type 'bit))
       (ba4 (bit-andc1 ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equal ba3 #*00001110)
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))

(equalp (bit-andc1 (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*010 #*101))
                   (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*101 #*010)))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*101 #*010)))

(equalp (bit-andc1 (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*010 #*101))
                   (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*101 #*010))
                   nil)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*101 #*010)))

(equalp (bit-andc1 (make-array '(2 3)
			       :element-type 'bit
			       :initial-contents '(#*010 #*101))
		   (make-array '(2 3)
			       :element-type 'bit
			       :initial-contents '(#*101 #*010))
		   t)
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*101 #*010)))

(equalp (bit-andc1 (make-array '(2 3)
			       :element-type 'bit
			       :initial-contents '(#*010 #*101))
		   (make-array '(2 3)
			       :element-type 'bit
			       :initial-contents '(#*101 #*010))
		   (make-array '(2 3)
			       :element-type 'bit))
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*101 #*010)))

(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-andc1 ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equalp ba (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*101 #*010)))))

(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba  (bit-andc1 ba1 ba2 t)))
  (and (eq ba1 ba)
       (equalp ba1 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*101 #*010)))))

(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba3 (make-array '(2 3)
			:element-type 'bit))
       (ba4 (bit-andc1 ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equalp ba3 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*101 #*010)))
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))


(equal (bit-andc2 #*11101010 #*01101011) #*10000000)
(equal (bit-andc2 #*11101010 #*01101011 nil) #*10000000)
(equal (bit-andc2 (make-array 8 :element-type 'bit :initial-contents #*11101010)
		  #*01101011 t)
       #*10000000)
(equal (bit-andc2 #*11101010
		  #*01101011
		  (make-array 8 :element-type 'bit))
       #*10000000)
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
       (ba  (bit-andc2 ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equal ba #*10000000)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
       (ba (bit-andc2 ba1 #*10101010 t)))
  (and (eq ba1 ba)
       (equal ba1 #*01010101)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba (bit-andc2 ba1 #*00111110 t)))
  (and (eq ba1 ba)
       (equal ba1 #*01000001)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
       (ba3 (make-array 8 :element-type 'bit))
       (ba4 (bit-andc2 ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equal ba3 #*01000001)
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))

(equalp (bit-andc2 (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*010 #*101))
                   (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*101 #*010)))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*010 #*101)))

(equalp (bit-andc2 (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*010 #*101))
                   (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*101 #*010))
                   nil)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*010 #*101)))

(equalp (bit-andc2 (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*010 #*101))
                   (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*101 #*010))
                   t)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*010 #*101)))

(equalp (bit-andc2 (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*010 #*101))
                   (make-array '(2 3)
                               :element-type 'bit
                               :initial-contents '(#*101 #*010))
                   (make-array '(2 3)
                               :element-type 'bit))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*010 #*101)))

(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-andc2 ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equalp ba (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*010 #*101)))))

(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-andc2 ba1 ba2 t)))
  (and (eq ba1 ba)
       (equalp ba1 (make-array '(2 3) 
                               :element-type 'bit
                               :initial-contents '(#*010 #*101)))))

(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba3 (make-array '(2 3)
                        :element-type 'bit))
       (ba4 (bit-andc2 ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equalp ba3 (make-array '(2 3) 
                               :element-type 'bit
                               :initial-contents '(#*010 #*101)))
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))


(equal (bit-eqv #*11101010 #*01101011) #*01111110)
(equal (bit-eqv #*11101010 #*01101011 nil) #*01111110)
(equal (bit-eqv (make-array 8 :element-type 'bit :initial-contents #*11101010)
		#*01101011 t)
       #*01111110)
(equal (bit-eqv #*11101010
		#*01101011
		(make-array 8 :element-type 'bit))
       #*01111110)
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
       (ba  (bit-eqv ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equal ba #*01111110)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
       (ba (bit-eqv ba1 #*10101010 t)))
  (and (eq ba1 ba)
       (equal ba1 #*00000000)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba (bit-eqv ba1 #*00111110 t)))
  (and (eq ba1 ba)
       (equal ba1 #*10110000)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
       (ba3 (make-array 8 :element-type 'bit))
       (ba4 (bit-eqv ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equal ba3 #*10110000)
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))

(equalp (bit-eqv (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101))
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*000 #*000)))

(equalp (bit-eqv (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101))
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010))
                 nil)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*000 #*000)))


(equalp (bit-eqv (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*101 #*010))
		 t)
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*000 #*000)))

(equalp (bit-eqv (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*101 #*010))
		 (make-array '(2 3)
			     :element-type 'bit))
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*000 #*000)))

(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-eqv ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equalp ba (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*000 #*000)))))


(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba  (bit-eqv ba1 ba2 t)))
  (and (eq ba1 ba)
       (equalp ba1 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*000 #*000)))))

(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba3 (make-array '(2 3)
			:element-type 'bit))
       (ba4 (bit-eqv ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equalp ba3 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*000 #*000)))
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))




(equal (bit-ior #*11101010 #*01101011) #*11101011)
(equal (bit-ior #*11101010 #*01101011 nil) #*11101011)
(equal (bit-ior (make-array 8 :element-type 'bit :initial-contents #*11101010)
		#*01101011 t)
       #*11101011)
(equal (bit-ior #*11101010
		#*01101011
		(make-array 8 :element-type 'bit))
       #*11101011)
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
       (ba  (bit-ior ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equal ba #*11101011)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
       (ba (bit-ior ba1 #*10101010 t)))
  (and (eq ba1 ba)
       (equal ba1 #*11111111)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba (bit-ior ba1 #*00111110 t)))
  (and (eq ba1 ba)
       (equal ba1 #*01111111)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
       (ba3 (make-array 8 :element-type 'bit))
       (ba4 (bit-ior ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equal ba3 #*01111111)
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))

(equalp (bit-ior (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101))
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*111 #*111)))

(equalp (bit-ior (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101))
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010))
                 nil)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*111 #*111)))

(equalp (bit-ior (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*101 #*010))
		 t)
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*111 #*111)))
(equalp (bit-ior (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*101 #*010))
		 (make-array '(2 3)
			     :element-type 'bit))
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*111 #*111)))
(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-ior ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equalp ba (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*111 #*111)))))


(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba  (bit-ior ba1 ba2 t)))
  (and (eq ba1 ba)
       (equalp ba1 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*111 #*111)))))
(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba3 (make-array '(2 3)
			:element-type 'bit))
       (ba4 (bit-ior ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equalp ba3 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*111 #*111)))
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))




(equal (bit-nand #*11101010 #*01101011) #*10010101)
(equal (bit-nand #*11101010 #*01101011 nil) #*10010101)
(equal (bit-nand (make-array 8 :element-type 'bit :initial-contents #*11101010)
		 #*01101011 t)
       #*10010101)
(equal (bit-nand #*11101010
		 #*01101011
		 (make-array 8 :element-type 'bit))
       #*10010101)
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
       (ba  (bit-nand ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equal ba #*10010101)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
       (ba (bit-nand ba1 #*10101010 t)))
  (and (eq ba1 ba)
       (equal ba1 #*11111111)))

(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba (bit-nand ba1 #*00111110 t)))
  (and (eq ba1 ba)
       (equal ba1 #*11001111)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
       (ba3 (make-array 8 :element-type 'bit))
       (ba4 (bit-nand ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equal ba3 #*11001111)
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))
(equalp (bit-nand (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*010 #*101))
                  (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*101 #*010)))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*111 #*111)))

(equalp (bit-nand (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*010 #*101))
                  (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*101 #*010))
                  nil)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*111 #*111)))

(equalp (bit-nand (make-array '(2 3)
			      :element-type 'bit
			      :initial-contents '(#*010 #*101))
		  (make-array '(2 3)
			      :element-type 'bit
			      :initial-contents '(#*101 #*010))
		  t)
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*111 #*111)))
(equalp (bit-nand (make-array '(2 3)
			      :element-type 'bit
			      :initial-contents '(#*010 #*101))
		  (make-array '(2 3)
			      :element-type 'bit
			      :initial-contents '(#*101 #*010))
		  (make-array '(2 3)
			      :element-type 'bit))
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*111 #*111)))
(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-nand ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equalp ba (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*111 #*111)))))

(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba  (bit-nand ba1 ba2 t)))
  (and (eq ba1 ba)
       (equalp ba1 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*111 #*111)))))
(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba3 (make-array '(2 3)
			:element-type 'bit))
       (ba4 (bit-nand ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equalp ba3 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*111 #*111)))
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))




(equal (bit-nor #*11101010 #*01101011) #*00010100)
(equal (bit-nor #*11101010 #*01101011 nil) #*00010100)
(equal (bit-nor (make-array 8 :element-type 'bit :initial-contents #*11101010)
		#*01101011 t)
       #*00010100)
(equal (bit-nor #*11101010
		#*01101011
		(make-array 8 :element-type 'bit))
       #*00010100)
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
       (ba  (bit-nor ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equal ba #*00010100)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
       (ba (bit-nor ba1 #*10101010 t)))
  (and (eq ba1 ba)
       (equal ba1 #*00000000)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba (bit-nor ba1 #*00111110 t)))
  (and (eq ba1 ba)
       (equal ba1 #*10000000)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
       (ba3 (make-array 8 :element-type 'bit))
       (ba4 (bit-nor ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equal ba3 #*10000000)
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))
(equalp (bit-nor (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101))
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*000 #*000)))

(equalp (bit-nor (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101))
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010))
                 nil)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*000 #*000)))

(equalp (bit-nor (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*101 #*010))
		 t)
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*000 #*000)))
(equalp (bit-nor (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*101 #*010))
		 (make-array '(2 3)
			     :element-type 'bit))
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*000 #*000)))
(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-nor ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equalp ba (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*000 #*000)))))

(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba  (bit-nor ba1 ba2 t)))
  (and (eq ba1 ba)
       (equalp ba1 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*000 #*000)))))
(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba3 (make-array '(2 3)
			:element-type 'bit))
       (ba4 (bit-nor ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equalp ba3 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*000 #*000)))
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))




(equal (bit-orc1 #*11101010 #*01101011) #*01111111)
(equal (bit-orc1 #*11101010 #*01101011 nil) #*01111111)
(equal (bit-orc1 (make-array 8 :element-type 'bit :initial-contents #*11101010)
		#*01101011 t)
       #*01111111)
(equal (bit-orc1 #*11101010
		 #*01101011
		 (make-array 8 :element-type 'bit))
       #*01111111)
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
       (ba  (bit-orc1 ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equal ba #*01111111)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
       (ba (bit-orc1 ba1 #*10101010 t)))
  (and (eq ba1 ba)
       (equal ba1 #*10101010)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba (bit-orc1 ba1 #*00111110 t)))
  (and (eq ba1 ba)
       (equal ba1 #*10111110)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
       (ba3 (make-array 8 :element-type 'bit))
       (ba4 (bit-orc1 ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equal ba3 #*10111110)
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))
(equalp (bit-orc1 (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*010 #*101))
                  (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*101 #*010)))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*101 #*010)))

(equalp (bit-orc1 (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*010 #*101))
                  (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*101 #*010))
                  nil)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*101 #*010)))

(equalp (bit-orc1 (make-array '(2 3)
			      :element-type 'bit
			      :initial-contents '(#*010 #*101))
		  (make-array '(2 3)
			      :element-type 'bit
			      :initial-contents '(#*101 #*010))
		  t)
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*101 #*010)))
(equalp (bit-orc1 (make-array '(2 3)
			      :element-type 'bit
			      :initial-contents '(#*010 #*101))
		  (make-array '(2 3)
			      :element-type 'bit
			      :initial-contents '(#*101 #*010))
		  (make-array '(2 3)
			      :element-type 'bit))
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*101 #*010)))
(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-orc1 ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equalp ba (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*101 #*010)))))

(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba  (bit-orc1 ba1 ba2 t)))
  (and (eq ba1 ba)
       (equalp ba1 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*101 #*010)))))
(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba3 (make-array '(2 3)
			:element-type 'bit))
       (ba4 (bit-orc1 ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equalp ba3 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*101 #*010)))
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))



(equal (bit-orc2 #*11101010 #*01101011) #*11111110)
(equal (bit-orc2 #*11101010 #*01101011 nil) #*11111110)
(equal (bit-orc2 (make-array 8 :element-type 'bit :initial-contents #*11101010)
		#*01101011 t)
       #*11111110)
(equal (bit-orc2 #*11101010
		#*01101011
		(make-array 8 :element-type 'bit))
       #*11111110)
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
       (ba  (bit-orc2 ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equal ba #*11111110)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
       (ba (bit-orc2 ba1 #*10101010 t)))
  (and (eq ba1 ba)
       (equal ba1 #*01010101)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba (bit-orc2 ba1 #*00111110 t)))
  (and (eq ba1 ba)
       (equal ba1 #*11110001)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
       (ba3 (make-array 8 :element-type 'bit))
       (ba4 (bit-orc2 ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equal ba3 #*11110001)
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))
(equalp (bit-orc2 (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*010 #*101))
                  (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*101 #*010)))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*010 #*101)))

(equalp (bit-orc2 (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*010 #*101))
                  (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*101 #*010))
                  nil)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*010 #*101)))

(equalp (bit-orc2 (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*010 #*101))
                  (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*101 #*010))
                  t)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*010 #*101)))

(equalp (bit-orc2 (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*010 #*101))
                  (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*101 #*010))
                  (make-array '(2 3)
                              :element-type 'bit))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*010 #*101)))

(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-orc2 ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equalp ba (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*010 #*101)))))

(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-orc2 ba1 ba2 t)))
  (and (eq ba1 ba)
       (equalp ba1 (make-array '(2 3) 
                               :element-type 'bit
                               :initial-contents '(#*010 #*101)))))

(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba3 (make-array '(2 3)
                        :element-type 'bit))
       (ba4 (bit-orc2 ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equalp ba3 (make-array '(2 3) 
                               :element-type 'bit
                               :initial-contents '(#*010 #*101)))
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))




(equal (bit-xor #*11101010 #*01101011) #*10000001)
(equal (bit-xor #*11101010 #*01101011 nil) #*10000001)
(equal (bit-xor (make-array 8 :element-type 'bit :initial-contents #*11101010)
		#*01101011 t)
       #*10000001)
(equal (bit-xor #*11101010
		#*01101011
		(make-array 8 :element-type 'bit))
       #*10000001)
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
       (ba  (bit-xor ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equal ba #*10000001)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
       (ba (bit-xor ba1 #*10101010 t)))
  (and (eq ba1 ba)
       (equal ba1 #*11111111)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba (bit-xor ba1 #*00111110 t)))
  (and (eq ba1 ba)
       (equal ba1 #*01001111)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
       (ba3 (make-array 8 :element-type 'bit))
       (ba4 (bit-xor ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equal ba3 #*01001111)
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))
(equalp (bit-xor (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101))
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*111 #*111)))

(equalp (bit-xor (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101))
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010))
                 nil)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*111 #*111)))

(equalp (bit-xor (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*101 #*010))
		 t)
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*111 #*111)))
(equalp (bit-xor (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*101 #*010))
		 (make-array '(2 3)
			     :element-type 'bit))
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*111 #*111)))
(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*101 #*010)))
       (ba  (bit-xor ba1 ba2)))
  (and (not (eq ba1 ba))
       (not (eq ba2 ba))
       (not (eq ba1 ba2))
       (equalp ba (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*111 #*111)))))

(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba  (bit-xor ba1 ba2 t)))
  (and (eq ba1 ba)
       (equalp ba1 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*111 #*111)))))
(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba2 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*101 #*010)))
       (ba3 (make-array '(2 3)
			:element-type 'bit))
       (ba4 (bit-xor ba1 ba2 ba3)))
  (and (eq ba3 ba4)
       (equalp ba3 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*111 #*111)))
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))
       (not (eq ba2 ba3))
       (not (eq ba2 ba4))))



(equal (bit-not #*11101010) #*00010101)
(equal (bit-not #*11101010 nil) #*00010101)
(equal (bit-not (make-array 8 :element-type 'bit :initial-contents #*11101010)
		t)
       #*00010101)
(equal (bit-not #*11101010 (make-array 8 :element-type 'bit))
       #*00010101)
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
       (ba  (bit-not ba1)))
  (and (not (eq ba1 ba))
       (equal ba #*00010101)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
       (ba (bit-not ba1 t)))
  (and (eq ba1 ba)
       (equal ba1 #*10101010)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba (bit-not ba1 t)))
  (and (eq ba1 ba)
       (equal ba1 #*10001110)))
(let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
       (ba2 (make-array 8 :element-type 'bit))
       (ba3 (bit-not ba1 ba2)))
  (and (eq ba2 ba3)
       (equal ba2 #*10001110)
       (not (eq ba1 ba2))
       (not (eq ba1 ba3))))

(equalp (bit-not (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101)))
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*101 #*010)))

(equalp (bit-not (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101))
                 nil)
        (make-array '(2 3)
                    :element-type 'bit
                    :initial-contents '(#*101 #*010)))

(equalp (bit-not (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 t)
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*101 #*010)))
(equalp (bit-not (make-array '(2 3)
			     :element-type 'bit
			     :initial-contents '(#*010 #*101))
		 (make-array '(2 3)
			     :element-type 'bit))
	(make-array '(2 3)
		    :element-type 'bit
		    :initial-contents '(#*101 #*010)))
(let* ((ba1 (make-array '(2 3)
                        :element-type 'bit
                        :initial-contents '(#*010 #*101)))
       (ba  (bit-not ba1)))
  (and (not (eq ba1 ba))
       (equalp ba (make-array '(2 3)
                              :element-type 'bit
                              :initial-contents '(#*101 #*010)))))



(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba  (bit-not ba1 t)))
  (and (eq ba1 ba)
       (equalp ba1 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*101 #*010)))))
(let* ((ba1 (make-array '(2 3)
			:element-type 'bit
			:initial-contents '(#*010 #*101)))
       (ba3 (make-array '(2 3)
			:element-type 'bit))
       (ba4 (bit-not ba1 ba3)))
  (and (eq ba3 ba4)
       (equalp ba3 (make-array '(2 3) 
			       :element-type 'bit
			       :initial-contents '(#*101 #*010)))
       (not (eq ba1 ba3))
       (not (eq ba1 ba4))))



(bit-vector-p (make-array 6 :element-type 'bit  :fill-pointer t))
(bit-vector-p #*)
(not (bit-vector-p (make-array 6)))

(not (simple-bit-vector-p (make-array 6)))
(simple-bit-vector-p #*)
(simple-bit-vector-p #*0101)
(simple-bit-vector-p #*0)
(simple-bit-vector-p #*1)
(simple-bit-vector-p (make-array 6 :element-type 'bit))

(equal (concatenate 'list
                    (adjust-array (make-array 5 :initial-contents '(0 1 2 3 4))
                                  10
                                  :initial-element -1))
       '(0 1 2 3 4 -1 -1 -1 -1 -1))


(let* ((array0 (make-array '(3 2)
                           :initial-contents
                           '((e0-0 e0-1) (e1-0 e1-1) (e2-0 e2-1))))
       (array (adjust-array array0
                            '(4 3)
                            :initial-element 0)))
  (and (eq (aref array 0 0) 'e0-0)
       (eq (aref array 0 1) 'e0-1)
       (eql (aref array 0 2) '0)
       (eq (aref array 1 0) 'e1-0)
       (eq (aref array 1 1) 'e1-1)
       (eql (aref array 1 2) 0)
       (eq (aref array 2 0) 'e2-0)
       (eq (aref array 2 1) 'e2-1)
       (eql (aref array 2 2) 0)))


(let* ((array0 (make-array '(3 2)
                           :initial-contents
                           '((e0-0 e0-1) (e1-0 e1-1) (e2-0 e2-1))))
       (array (adjust-array array0
                            '(1 1)
                            :initial-element 0)))
  (eq (aref array 0 0) 'e0-0))


(let* ((array0 (make-array '(3 2) :initial-element 0))
       (array1 (make-array 6 :initial-element 1))
       (array (adjust-array array1 3 :displaced-to array0)))
  (and (equal (array-dimensions array) '(3))
       (every #'zerop array)))



(let* ((array0 (make-array '(3 2) :initial-contents '((0 1) (2 3) (4 5))))
       (array1 (make-array 6 :initial-element 1))
       (array (adjust-array array1 3
                            :displaced-to array0
                            :displaced-index-offset 3)))
  (and (equal (array-dimensions array) '(3))
       (eql (aref array 0) 3)
       (eql (aref array 1) 4)
       (eql (aref array 2) 5)))


(let* ((array0 (make-array '(3 2) :initial-contents '((0 1) (2 3) (4 5))))
       (array1 (make-array 6 :displaced-to array0))
       (array (adjust-array array1 9 :initial-element '-1)))
  (and (equal (array-dimensions array) '(9))
       (multiple-value-bind (displaced-to displaced-index-offset)
           (array-displacement array)
         (and (null displaced-to) (zerop displaced-index-offset)))
       (eql (aref array 0) 0)
       (eql (aref array 1) 1)
       (eql (aref array 2) 2)
       (eql (aref array 3) 3)
       (eql (aref array 4) 4)
       (eql (aref array 5) 5)
       (eql (aref array 6) -1)
       (eql (aref array 7) -1)
       (eql (aref array 8) -1)))


(let* ((array0 (make-array '(4 4)
			   :adjustable t
			   :initial-contents
			   '(( alpha   beta    gamma     delta )
			     ( epsilon zeta    eta       theta )
			     ( iota    kappa   lambda    mu    )
			     ( nu      xi      omicron   pi    ))))
       (array (adjust-array array0 '(3 5) :initial-element 'baz)))
  (equalp array
	  #2A(( alpha     beta      gamma     delta     baz )
	      ( epsilon   zeta      eta       theta     baz )
	      ( iota      kappa     lambda    mu        baz ))))


(let* ((array0 (make-array 3 :initial-element 0))
       (array1 (make-array 3 :adjustable t :displaced-to array0))
       (array2 (make-array 3 :displaced-to array1)))
  (and (adjustable-array-p array1)
       (eq array1 (adjust-array array1 6 :initial-contents '(a b c d e f)))
       (multiple-value-bind (displaced-to displaced-index-offset)
	   (array-displacement array1)
	 (and (null displaced-to) (zerop displaced-index-offset)))
       (eq (aref array1 0) 'a)
       (eq (aref array1 1) 'b)
       (eq (aref array1 2) 'c)
       (eq (aref array1 3) 'd)
       (eq (aref array1 4) 'e)
       (eq (aref array1 5) 'f)
       (eq (aref array2 0) 'a)
       (eq (aref array2 1) 'b)
       (eq (aref array2 2) 'c)))

(let* ((str0 (make-array 10
                         :element-type 'character
                         :initial-contents "abcdefghij"))
       (str1 (make-array 7
                         :adjustable t
                         :element-type 'character
                         :displaced-to str0
                         :displaced-index-offset 3))
       (str2 (make-array 3
                         :element-type 'character
                         :displaced-to str1
                         :displaced-index-offset 4)))
  (and (string= str0 "abcdefghij")
       (string= str1 "defghij")
       (string= str2 "hij")
       (adjustable-array-p str1)
       (eq str1 (adjust-array str1 10 :initial-contents "QRSTUVWXYZ"))
       (string= str2 "UVW")))


(let* ((bv (make-array 10
		       :element-type 'bit
		       :initial-contents #*1010101010
		       :fill-pointer t)))
  (and (dotimes (i 10 t)
	 (unless (eql (vector-pop bv) (if (evenp i) 0 1))
	   (return nil)))
       (zerop (length bv))))

(let* ((bv (make-array 10
		       :adjustable t
		       :element-type 'bit
		       :fill-pointer 0)))
  (dotimes (i 100)
    (vector-push-extend (if (oddp i) 0 1) bv))
  (dotimes (i 100 t)
    (unless (eql (vector-pop bv) (if (oddp i) 1 0))
      (return nil))))



(let* ((str (make-array 10
		       :element-type 'character
		       :initial-contents "abcdefghjk"
		       :fill-pointer t)))
  (and (dotimes (i 10 t)
	 (unless (char= (vector-pop str) (aref "kjhgfedcba" i))
	   (return nil)))
       (zerop (length str))))

(let* ((str (make-array 10
			:adjustable t
			:element-type 'character
			:fill-pointer 0)))
  (dotimes (i 100)
    (vector-push-extend (if (oddp i) #\a #\z) str))
  (dotimes (i 100 t)
    (unless (char= (vector-pop str) (if (oddp i) #\z #\a))
      (return nil))))


