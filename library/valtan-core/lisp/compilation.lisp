(in-package :common-lisp)

(defun eval (x)
  (ffi:js-eval (compiler:compile-toplevel x)))

(defun macroexpand-1 (form &optional environment)
  (compiler::!macroexpand-1 form environment))

(defun macroexpand (form &optional environment)
  (declare (ignore environment))
  (multiple-value-bind (form expanded-p)
      (macroexpand-1 form)
    (if expanded-p
        (macroexpand-1 form)
        form)))

(defun macro-function (symbol &optional environment)
  (declare (ignore environment))
  (compiler::get-macro symbol))

(defun special-operator-p (symbol)
  (not (null (member symbol
                     '(block catch eval-when flet function go if labels let let* load-time-value
                       locally macrolet multiple-value-call multiple-value-prog1 progn progv quote
                       return-from setq symbol-macrolet tagbody the throw unwind-protect)))))

(defmacro define-compiler-macro (name lambda-list &body body)
  (declare (ignore name lambda-list body))
  `',name)

(defun load (file)
  (with-open-file (in file)
    (loop :with eof := '#:eof
          :for form := (read in nil eof)
          :until (eq form eof)
          :do (eval form))))
