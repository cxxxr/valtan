(in-package :compiler)

(defun make-ir (op &rest args)
  (ecase (length args)
    (1 (vector op (first args)))
    (2 (vector op (first args) (second args)))
    (3 (vector op (first args) (second args) (third args)))))

(defun ir-op (ir) (aref ir 0))
(defun ir-arg1 (ir) (aref ir 1))
(defun ir-arg2 (ir) (aref ir 2))
(defun ir-arg3 (ir) (aref ir 3))

(defstruct parsed-lambda-list
  vars
  rest-var
  optionals
  keys
  min
  max)

(defun collect-variables (parsed-lambda-list)
  (append (parsed-lambda-list-vars parsed-lambda-list)
          (if (parsed-lambda-list-rest-var parsed-lambda-list)
              (list (parsed-lambda-list-rest-var parsed-lambda-list)))
          (mapcar #'first (parsed-lambda-list-optionals parsed-lambda-list))
          (remove nil (mapcar #'third (parsed-lambda-list-optionals parsed-lambda-list)))
          (mapcar #'first (parsed-lambda-list-keys parsed-lambda-list))
          (remove nil (mapcar #'third (parsed-lambda-list-keys parsed-lambda-list)))))

(defstruct binding
  name
  type
  value)
