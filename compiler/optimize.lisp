(in-package :compiler)

(defmacro define-optimize (name (ir) &body body)
  (let ((g-name (make-symbol (string name))))
    `(progn
       (setf (get ',name 'optimize) g-name)
       (defun ,g-name (,ir)
         ,@body))))

(defun get-optimizer (ir)
  (get (ir-op ir) 'optimize))

(defun optimize (ir)
  (funcall (get-optimizer ir) ir))

(define-optimize const (ir)
  ir)

(define-optimize lref (ir)
  ir)

(define-optimize gref (ir)
  ir)

(define-optimize lset (ir)
  ir)

(define-optimize gset (ir)
  ir)

(define-optimize if (ir)
  ir)

(define-optimize progn (ir)
  ir)

(define-optimize lambda (ir)
  ir)

(define-optimize let (ir)
  ir)

(define-optimize lcall (ir)
  ir)

(define-optimize call (ir)
  ir)

(define-optimize unwind-protect (ir)
  ir)

(define-optimize block (ir)
  ir)

(define-optimize return-from (ir)
  ir)

(define-optimize tagbody (ir)
  ir)

(define-optimize go (ir)
  ir)

(define-optimize catch (ir)
  ir)

(define-optimize throw (ir)
  ir)

(define-optimize *:%defun (ir)
  ir)

(define-optimize *:%defpackage (ir)
  ir)

(define-optimize *:%in-package (ir)
  ir)

(define-optimize ffi:ref (ir)
  ir)

(define-optimize ffi:set (ir)
  ir)

(define-optimize ffi:var (ir)
  ir)

(define-optimize ffi:typeof (ir)
  ir)

(define-optimize ffi:new (ir)
  ir)

(define-optimize ffi:aget (ir)
  ir)

(define-optimize js-call (ir)
  ir)

(define-optimize module (ir)
  ir)
