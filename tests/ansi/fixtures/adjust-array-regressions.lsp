(in-package :cl-test)

(deftest adjust-array-keeps-identity-and-common-subscripts
  (let ((array (make-array '(2 3) :adjustable t :initial-contents '((1 2 3) (4 5 6)))))
    (values (eq array (adjust-array array '(3 2) :initial-element :new))
            (array-dimensions array)
            (loop for i below (array-total-size array) collect (row-major-aref array i))))
  t (3 2) (1 2 4 5 :new :new))

(deftest adjust-array-zero-rank-and-replacement-contents
  (let ((array (make-array nil :initial-element :old :adjustable t)))
    (values (aref (adjust-array array nil :initial-element :unused))
            (aref (adjust-array array nil :initial-contents :new))))
  :old :new)

(deftest adjust-array-displaced-views-follow-new-storage
  (let* ((base (make-array 3 :initial-contents '(1 2 3) :adjustable t))
         (view (make-array 2 :displaced-to base :displaced-index-offset 1)))
    (adjust-array base 4 :initial-contents '(10 20 30 40))
    (values (aref view 0) (aref view 1)
            (progn (setf (aref view 1) 99) (aref base 2))))
  20 30 99)

(deftest adjust-array-retargets-and-detaches-displacement
  (let* ((first (vector 1 2 3 4)) (second (vector 10 20 30 40))
         (view (make-array 2 :adjustable t :displaced-to first :displaced-index-offset 1)))
    (adjust-array view 2 :displaced-to second)
    (values (aref view 0) (aref view 1) first second
            (progn (adjust-array view 3 :initial-element :new)
                   (setf (aref second 0) 77)
                   (list (aref view 0) (aref view 1) (aref view 2)))
            (multiple-value-list (array-displacement view))))
  10 20 #(1 2 3 4) #(77 20 30 40) (10 20 :new) (nil 0))

(deftest adjust-array-preserves-fill-pointer-and-overrides-contents
  (let ((array (make-array 3 :initial-contents '(1 2 3) :fill-pointer 2 :adjustable t)))
    (adjust-array array 4 :initial-contents '(4 5 6 7) :fill-pointer nil)
    (values (fill-pointer array) (coerce array 'list) (aref array 3)
            (progn (adjust-array array 4 :fill-pointer t) (fill-pointer array))))
  2 (4 5) 7 4)

(deftest adjust-array-string-content-and-vector-extension
  (let ((string (make-array 2 :element-type 'character :initial-contents "ab"
                              :fill-pointer 2 :adjustable t)))
    (values (vector-push-extend #\c string 3)
            (>= (array-total-size string) 5)
            (copy-seq string)
            (vector-pop string)
            (copy-seq string)))
  2 t "abc" #\c "ab")

(deftest vector-fill-pointer-validation-and-full-push
  (let ((vector (make-array 1 :initial-element :old :fill-pointer t)))
    (values (vector-push :new vector) vector (fill-pointer vector)
            (handler-case (setf (fill-pointer vector) 2) (type-error () :rejected))
            (handler-case (vector-push :new #(1)) (error () :no-fill-pointer))
            (handler-case (adjust-array #(1) 2 :fill-pointer 1) (error () :no-fill-pointer))))
  nil #(:old) 1 :rejected :no-fill-pointer :no-fill-pointer)

(deftest displaced-bit-operations-and-vector-pop
  (let* ((base (make-array 5 :element-type 'bit :initial-contents '(1 0 1 1 0)))
         (view (make-array 3 :element-type 'bit :displaced-to base :displaced-index-offset 1
                            :fill-pointer 3)))
    (values (bit view 0) (bit view 1)
            (bit-and view #*110)
            (bit-not view)
            (vector-pop view)
            (progn (setf (bit view 0) 1) (bit base 1))))
  0 1 #*010 #*100 1 1)