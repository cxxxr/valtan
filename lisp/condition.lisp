(in-package :common-lisp)

(defun error (datum &rest arguments)
  ;(system:%error (format nil datum arguments))
  (system::error (system::array-to-js-string datum)))

(defmacro assert (test-form &optional place datum-form argument-form)
  (declare (ignore place datum-form argument-form))
  `(unless ,test-form
     (error ,(format nil "assertion error: ~S" test-form))))
