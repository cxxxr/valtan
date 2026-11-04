(in-package :cl-test)

(deftest equalp-displaced-vectors-use-active-elements
  (let* ((base (vector :prefix 1 2 99 :suffix))
         (view (make-array 3 :displaced-to base :displaced-index-offset 1 :fill-pointer 2)))
    (values (equalp view #(1 2)) (equalp #(1 2) view)
            (equalp view #(1 3)) (equalp view #(1 2 99))))
  t t nil nil)

(deftest stable-sort-displaced-vector-preserves-outside-storage
  (let* ((base (vector :prefix 3 1 2 :inactive :suffix))
         (view (make-array 4 :displaced-to base :displaced-index-offset 1 :fill-pointer 3)))
    (values (eq (stable-sort view #'<) view)
            (equalp view #(1 2 3)) base))
  t t #(:prefix 1 2 3 :inactive :suffix))

(deftest empty-uninterned-symbol
  (let ((symbol (read-from-string "#:||")))
    (values (symbol-name symbol) (symbol-package symbol)))
  "" nil)

(deftest displaced-array-logical-index
  (let* ((base (vector 0 1 2 3 4 5))
         (view (make-array '(2 2) :displaced-to base :displaced-index-offset 1)))
    (values (array-row-major-index view 1 0) (aref view 1 0)
            (row-major-aref view 3)))
  2 3 4)

(deftest nested-displaced-array-mutation
  (let* ((base (vector 0 1 2 3 4 5))
         (first (make-array 4 :displaced-to base :displaced-index-offset 1))
         (second (make-array 2 :displaced-to first :displaced-index-offset 1)))
    (setf (aref second 1) 99)
    (values (aref base 3) (aref first 2) (aref second 1)))
  99 99 99)

(deftest displaced-string-shares-updates
  (let* ((base (copy-seq "xxSPACEyy"))
         (first (make-array 7 :element-type 'character :displaced-to base
                              :displaced-index-offset 1))
         (second (make-array 5 :element-type 'character :displaced-to first
                               :displaced-index-offset 1 :fill-pointer 4)))
    (setf (char second 0) #\s)
    (values (char base 2) (string-downcase second)
            (symbol-name (intern second))))
  #\s "spac" "sPAC")

(deftest multidimensional-character-assignment
  (let ((array (make-array '(2 2) :element-type 'character :initial-element #\a)))
    (setf (aref array 1 0) #\b)
    (row-major-aref array 2))
  #\b)
