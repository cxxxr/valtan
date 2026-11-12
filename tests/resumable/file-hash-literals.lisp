(in-package :cl-user)

(eval-when (:compile-toplevel)
  (defparameter *file-hash-key* (list :key 7))
  (defparameter *file-hash-token* (gensym "HASH-TOKEN"))
  (defparameter *file-hash-tables*
    (loop for test in '(eq eql equal equalp)
          collect (let ((table (make-hash-table :test test
                                               :rehash-size 2.0
                                               :rehash-threshold 0.75)))
                    (setf (gethash :self table) table
                          (gethash *file-hash-key* table) *file-hash-token*
                          (gethash *file-hash-token* table) *file-hash-key*
                          (gethash :nil table) nil
                          (gethash "Case" table) 42)
                    table)))
  ;; An EQ key may be the table itself. Two tables can also refer to each other.
  (setf (gethash (first *file-hash-tables*) (first *file-hash-tables*))
        (second *file-hash-tables*)
        (gethash :peer (second *file-hash-tables*)) (first *file-hash-tables*)))

(defun file-hash-tables () '#.*file-hash-tables*)
(defun file-hash-key () '#.*file-hash-key*)
(defun file-hash-token () '#.*file-hash-token*)

(defun check-file-hash-literals ()
  (let ((tables (file-hash-tables))
        (key (file-hash-key)) (token (file-hash-token)))
    (loop for table in tables
          for test in '(eq eql equal equalp)
          for count in '(6 6 5 5)
          do (assert (eq test (hash-table-test table)))
             (assert (= count (hash-table-count table)))
             (assert (= 2.0 (hash-table-rehash-size table)))
             (assert (= 0.75 (hash-table-rehash-threshold table)))
             (assert (eq table (gethash :self table)))
             (assert (eq token (gethash key table)))
             (assert (eq key (gethash token table)))
             (assert (eq (not (null (gethash (copy-list key) table)))
                         (not (null (member test '(equal equalp))))))
             (multiple-value-bind (value present) (gethash :nil table)
               (assert (null value)) (assert present))
             (multiple-value-bind (value present) (gethash :missing table)
               (assert (null value)) (assert (null present)))
             (assert (eql (gethash "cASE" table) (if (eq test 'equalp) 42 nil))))
    (assert (eq (gethash (first tables) (first tables)) (second tables)))
    (assert (eq (gethash :peer (second tables)) (first tables)))
    t))
