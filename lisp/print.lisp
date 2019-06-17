(in-package :common-lisp)

#|
(defun write (object &key array base case circle escape gensym length level lines miser-width
                          pprint-dispatch pretty radix readably right-margin stream)
  (declare (ignore array base case circle escape gensym length level lines miser-width
                   pprint-dispatch pretty radix readably right-margin stream))
  )
|#

(defun princ (object &optional (stream *standard-output*))
  (cond ((symbolp object)
         (write-string (symbol-name object) stream))
        (t
         (error "princ error"))))

(defun princ-to-string (object)
  (with-output-to-string (stream)
    (princ object stream)))

(defun format (destination control-string &rest format-arguments)
  )

