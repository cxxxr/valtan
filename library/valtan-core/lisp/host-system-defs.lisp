(cl:in-package :valtan-core)

(cl:defun system:make-symbol (name)
  (cl:make-symbol name))

(cl:defun system:%symbol-name (symbol)
  (cl:symbol-name symbol))

(cl:defun system:symbol-value (symbol)
  (cl:symbol-value symbol))

(cl:defun system:symbol-function (symbol)
  (cl:symbol-function symbol))

(cl:defun system:symbol-plist (symbol)
  (cl:symbol-plist symbol))

(cl:defun system:fset (symbol function)
  (cl:setf (cl:symbol-function symbol) function))

(cl:defun system:symbol-package-name (symbol)
  (cl:package-name (cl:symbol-package symbol)))

(cl:defun system:put-symbol-plist (symbol plist)
  (cl:setf (cl:symbol-plist symbol) plist))

(cl:defun system:string-append (&rest strings)
  (cl:apply #'cl:concatenate 'cl:string strings))

(cl:defun system:%add (x y)
  (cl:+ x y))

(cl:defun system:%sub (x y)
  (cl:- x y))

(cl:defun system:%mul (x y)
  (cl:* x y))

(cl:defun system:%negate (x)
  (cl:- x))

(cl:defun system:%floor (x y)
  (values (cl:floor x y)))

(cl:defun system:%rem (x y)
  (values (cl:rem x y)))

(cl:defun system:%logand (x y)
  (cl:logand x y))

(cl:defun system:%= (x y)
  (cl:= x y))

(cl:defun system:%/= (x y)
  (cl:/= x y))

(cl:defun system:%> (x y)
  (cl:> x y))

(cl:defun system:%< (x y)
  (cl:< x y))

(cl:defun system:%>= (x y)
  (cl:>= x y))

(cl:defun system:%<= (x y)
  (cl:<= x y))

(cl:defmacro system:multiple-value-call (function cl:&rest args)
  `(cl:multiple-value-call ,function ,@args))

(cl:defun system:structure-p (structure)
  (cl:declare (cl:ignore structure))
  cl:nil)

(cl:defun system:make-structure (name &rest args)
  (cl:declare (cl:ignore name args))
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

(cl:defun system:%copy-structure (structure)
  (cl:declare (cl:ignore structure))
  cl:nil)

(cl:defun system:%structure-set (structure index value)
  (cl:declare (cl:ignore structure index value))
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

(cl:defun system:make-raw-string ()
  )

(cl:defun system:array-to-js-string (array)
  (cl:declare (cl:ignore array)))

(cl:defun system:js-string-to-array (js-string)
  (cl:declare (cl:ignore js-string)))

(cl:defun system:js-array-to-array (js-array)
  (cl:declare (cl:ignore js-array)))

(cl:defun array-contents (x)
  (cl:declare (cl:ignore x)))

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
