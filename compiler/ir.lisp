(in-package :compiler)

(defstruct (ir (:constructor %make-ir))
  op
  return-value-p
  multiple-values-p
  args)

(defun ir-arg1 (ir) (first (ir-args ir)))
(defun ir-arg2 (ir) (second (ir-args ir)))
(defun ir-arg3 (ir) (third (ir-args ir)))

(defmethod print-object ((ir ir) stream)
  (cond ((ir-arg3 ir)
         (format stream "(~S ~S ~S ~S)" (ir-op ir) (ir-arg1 ir) (ir-arg2 ir) (ir-arg3 ir)))
        ((ir-arg2 ir)
         (format stream "(~S ~S ~S)" (ir-op ir) (ir-arg1 ir) (ir-arg2 ir)))
        ((ir-arg1 ir)
         (format stream "(~S ~S)" (ir-op ir) (ir-arg1 ir)))))

(defun make-ir (op return-value-p multiple-values-p &rest args)
  (%make-ir :op op
            :return-value-p return-value-p
            :multiple-values-p multiple-values-p
            :args args))

(defstruct parsed-lambda-list
  vars
  rest-var
  optionals
  keys
  min
  max
  allow-other-keys)

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

(defstruct tagbody-value
  index
  level)
