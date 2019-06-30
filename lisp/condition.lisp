(in-package :common-lisp)

(defun error (datum &rest arguments)
  (system::error (system::array-to-js-string (apply #'format nil datum arguments))))

(defmacro assert (test-form &optional place datum-form argument-form)
  (declare (ignore place datum-form argument-form))
  `(unless ,test-form
     (error ,(format nil "assertion error: ~S" test-form))))

(defun type-error (value type-name)
  (error "The value ~S is not of the expected type ~A" value type-name))

(defun eof-error ()
  (error "End of file"))
