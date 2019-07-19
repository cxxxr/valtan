(in-package :common-lisp)

(defun open (filename &key (direction :input) element-type if-exists if-does-not-exist)
  (declare (ignore element-type if-exists if-does-not-exist))
  (ecase direction
    (:input
     (make-file-input-stream
      :string-stream
      (make-string-input-stream
       (system::js-string-to-array
        ((ffi:ref "fs" "readFileSync")
         (system::array-to-js-string filename)
         (system::array-to-js-string "utf-8"))))))
    ;(:output)
    ))

(defmacro with-open-file ((var filename &rest options
                                        &key (direction :input)
                                             element-type
                                             if-exists
                                             if-does-not-exist)
                          &body body)
  (declare (ignore direction element-type if-exists if-does-not-exist))
  `(with-open-stream (open ,filename ,@options)
     ,@body))
