(deftest check-type.datum-and-retry
  (let ((value :bad) (data nil))
    (handler-bind ((type-error
                    (lambda (condition)
                      (push (type-error-datum condition) data)
                      (store-value (if (= (length data) 1) :still-bad 7) condition))))
      (values (check-type value integer) value (reverse data))))
  nil 7 (:bad :still-bad))

(deftest check-type.place-store-order
  (let ((cell (list :bad)) (reads 0) (seen nil))
    (handler-bind ((type-error
                    (lambda (condition)
                      (setf seen reads)
                      (store-value 9 condition))))
      (check-type (car (progn (incf reads) cell)) integer))
    (values (car cell) reads seen))
  9 3 2)

(deftest check-type.place-no-store-on-exit
  (let ((cell (list :bad)) (reads 0))
    (block done
      (handler-bind ((type-error (lambda (condition)
                                  (declare (ignore condition))
                                  (return-from done))))
        (check-type (car (progn (incf reads) cell)) integer)))
    (values (car cell) reads))
  :bad 2)

(deftest check-type.description
  (let ((value 1) (calls 0) (message nil))
    (check-type value integer (progn (incf calls) "an integer"))
    (setf value :bad)
    (handler-bind ((type-error (lambda (condition)
                                (setf message (princ-to-string condition))
                                (store-value 3 condition))))
      (check-type value integer (progn (incf calls) "an integer")))
    (values value calls (not (null (search "an integer" message)))))
  3 1 t)
