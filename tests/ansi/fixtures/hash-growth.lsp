(in-package :cl-test)
(deftest hash-growth-preserves-entries
  (let ((table (make-hash-table :test 'equal)))
    (dotimes (i 5000) (setf (gethash (code-char i) table) i))
    (setf (gethash (code-char 2000) table) :updated)
    (values (hash-table-count table)
            (gethash (code-char 2000) table)
            (loop for i below 5000
                  always (or (= i 2000) (eql (gethash (code-char i) table) i)))
            (remhash (code-char 1234) table)
            (gethash (code-char 1234) table :missing)
            (hash-table-count table)))
  5000 :updated t t :missing 4999)
