(cl:in-package :valtan-core)

(cl:defmacro system:multiple-value-call (function cl:&rest args)
  `(cl:multiple-value-call ,function ,@args))

(cl:defun system:structure-p (structure)
  (cl:declare (cl:ignore structure))
  cl:nil)

(cl:defun system:%structure-name (structure)
  (cl:declare (cl:ignore structure))
  cl:nil)

(cl:defun system:%structure-slot-count (structure)
  (cl:declare (cl:ignore structure))
  cl:nil)

(cl:defun system:%structure-ref (structure index)
  (cl:declare (cl:ignore structure index))
  cl:nil)

(cl:defmacro system:defmacro* (name lambda-list cl:&body body)
  `(cl:defmacro ,name ,lambda-list ,@body))

(cl:defun system:%car (x)
  (cl:car x))

(cl:defun system:%cdr (x)
  (cl:cdr x))

(cl:defun system:%rplaca (cons x)
  (cl:rplaca cons x))

(cl:defun system:%rplacd (cons x)
  (cl:rplacd cons x))

(cl:defun system:%put (symbol key value)
  (cl:setf (cl:get symbol key) value))

(cl:defun system:error (value)
  (cl:error value))

(cl:defun js::-object ()
  )

(cl:defun js::eval (x)
  (cl:declare (cl:ignore x)))

(cl:defun ffi:set (var value)
  (cl:declare (cl:ignore var value)))

(cl:defun ffi:aget (array index)
  (cl:declare (cl:ignore array index)))

(cl:defun ffi:ref (cl:&rest args)
  (cl:declare (cl:ignore args)))

(cl:defun ffi:typeof (x)
  (cl:declare (cl:ignore x)))

(cl:defun ffi:instanceof (value instance)
  (cl:declare (cl:ignore value instance)))

(cl:defun *:array-to-js-string (array)
  (cl:declare (cl:ignore array)))

(cl:defun *:js-string-to-array (js-string)
  (cl:declare (cl:ignore js-string)))

(cl:defun *:js-array-to-array (js-array)
  (cl:declare (cl:ignore js-array)))

(cl:defun array-contents (x)
  (cl:declare (cl:ignore x)))
