(in-package :compiler)

(defstruct compiland
  vars
  functions
  body)

(defstruct lir-fn
  name
  lambda-list
  body)

(defun make-lir (op &rest args)
  (cons op args))

(defun lir-op (lir)
  (first lir))

(defun lir-arg1 (lir)
  (second lir))

(defun lir-arg2 (lir)
  (third lir))

(defun lir-jump-label (lir)
  (ecase (lir-op lir)
    (jump
     (lir-arg1 lir))
    (fjump
     (lir-arg2 lir))))

(defun lir-jump-p (lir)
  (case (lir-op lir)
    ((jump fjump) t)
    (otherwise nil)))

(defstruct basic-block
  id
  code
  succ
  pred)
