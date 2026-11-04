(in-package :cl-test)

(deftest character-name-designators
  (values (name-char 'space) (name-char "sPaCe") (name-char nil))
  #\Space #\Space nil)

(deftest character-name-displaced
  (name-char (make-array 5 :element-type 'character
                         :displaced-to "  sPaCe  " :displaced-index-offset 2))
  #\Space)

(deftest hash-iterator-value-count
  (let ((table (make-hash-table)))
    (setf (gethash :key table) :value)
    (with-hash-table-iterator (next table)
      (values (multiple-value-list (next)) (multiple-value-list (next)))))
  (t :key :value) (nil))

(deftest hash-iterator-cleared-table
  (let ((table (make-hash-table)))
    (setf (gethash :a table) 1 (gethash :b table) 2)
    (with-hash-table-iterator (next table)
      (next)
      (clrhash table)
      (multiple-value-list (next))))
  (nil))

(deftest hash-iterator-macro-binding
  (macrolet ((macro-visible-p (name &environment env)
               (not (null (macro-function name env)))))
    (with-hash-table-iterator (next (make-hash-table))
      (macro-visible-p next)))
  t)
