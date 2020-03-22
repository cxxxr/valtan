(cl:in-package :valtan-core)


;;; lisp.jsに対応
(cl:defun system:make-symbol (name)
  (cl:make-symbol name))

(cl:defun system:symbol-plist (symbol)
  (cl:symbol-plist symbol))

(cl:defun system:put-symbol-plist (symbol plist)
  (cl:setf (cl:symbol-plist symbol) plist))

(cl:defun system:symbol-value (symbol)
  (cl:symbol-value symbol))

(cl:defun system:symbol-function (symbol)
  (cl:symbol-function symbol))

(cl:defun system:symbol-name (symbol)
  (cl:symbol-name symbol))

(cl:defun system:symbol-package-name (symbol)
  (cl:package-name (cl:symbol-package symbol)))

(cl:defun system:fset (symbol function)
  (cl:setf (cl:symbol-function symbol) function))

(cl:defun system:map-package-symbols (package function)
  (cl:declare (cl:ignore package function))
  (cl:error "unimplemented"))

(cl:defun system:put (symbol key value)
  (cl:setf (cl:get symbol key) value))

(cl:defun system:package-name (package)
  (cl:package-name package))

(cl:defun system:package-nicknames (package)
  (cl:package-nicknames package))

(cl:defun system:intern (name package)
  (cl:declare (cl:ignore name package))
  (cl:error "unimplemented"))

(cl:defun system:find-symbol (name package)
  (cl:declare (cl:ignore name package))
  (cl:error "unimplemented"))

(cl:defun system:make-package (name nicknames use-packager-names)
  (cl:declare (cl:ignore name nicknames use-packager-names))
  (cl:error "unimplemented"))

(cl:defun system:%add (x y)
  (cl:+ x y))

(cl:defun system:%sub (x y)
  (cl:- x y))

(cl:defun system:%negate (x)
  (cl:- x))

(cl:defun system:%mul (x y)
  (cl:* x y))

(cl:defun system:%rem (x y)
  (values (cl:rem x y)))

(cl:defun system:%floor (x y)
  (values (cl:floor x y)))

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

(cl:defun system:apply (function args)
  (cl:apply function args))

(cl:defun system:%car (x)
  (cl:car x))

(cl:defun system:%cdr (x)
  (cl:cdr x))

(cl:defun system:%rplaca (cons x)
  (cl:rplaca cons x))

(cl:defun system:%rplacd (cons x)
  (cl:rplacd cons x))

(cl:defun system:js-array-to-list (js-array)
  (cl:declare (cl:ignore js-array))
  (cl:error "unimplemented"))

(cl:defun system:list-to-js-array (list)
  list)

(cl:defmacro system:multiple-value-call (function cl:&rest args)
  `(cl:multiple-value-call ,function ,@args))

(cl:defclass structure ()
  ((name :initarg :name :reader structure-name)
   (values :initarg :values :reader structure-values)))

(cl:defmethod cl:print-object ((object structure) stream)
  (if (cl:and (cl:eq (structure-name object) 'array)
              (cl:stringp (cl:first (structure-values object))))
      (cl:prin1 (cl:first (structure-values object)) stream)
      (cl:print-unreadable-object (object stream)
        (cl:princ (structure-name object) stream))))

(cl:defun system:structure-p (structure)
  (cl:typep structure 'structure))

(cl:defun system:make-structure (name &rest args)
  (cl:make-instance 'structure :name name :values args))

(cl:defun system:%structure-name (structure)
  (structure-name structure))

(cl:defun system:%structure-slot-count (structure)
  (cl:length (structure-values structure)))

(cl:defun system:%structure-ref (structure index)
  (cl:elt (structure-values structure) index))

(cl:defun system:%copy-structure (structure)
  (cl:make-instance 'structure
                    :name (structure-name structure)
                    :values (structure-values structure)))

(cl:defun system:%structure-set (structure index value)
  (cl:setf (cl:elt (structure-values structure) index)
           value))

(cl:defun system:error (value)
  (cl:error value))

(cl:defun system:%code-char (code)
  (cl:code-char code))

(cl:defun system:%char-code (char)
  (cl:char-code char))


(cl:defmacro system:defmacro* (name lambda-list cl:&body body)
  `(cl:defmacro ,name ,lambda-list ,@body))

(cl:defun system:make-raw-string ()
  )

(cl:defun system:expand-raw-string (raw-string n)
  (cl:declare (cl:ignore raw-string n))
  (cl:error "unimplemented"))

(cl:defun system:code-to-raw-string (code)
  (cl:declare (cl:ignore code))
  (cl:error "unimplemented"))

(cl:defun system:sub-raw-string/2 (raw-string start)
  (cl:declare (cl:ignore raw-string start))
  (cl:error "unimplemented"))

(cl:defun system:sub-raw-string/3 (raw-string start end)
  (cl:declare (cl:ignore raw-string start end))
  (cl:error "unimplemented"))

(cl:defun system:concat-raw-string/2 (raw-string-1 raw-string-2)
  (cl:declare (cl:ignore raw-string-1 raw-string-2))
  (cl:error "unimplemented"))

(cl:defun system:concat-raw-string/3 (raw-string-1 raw-string-2 raw-string-3)
  (cl:declare (cl:ignore raw-string-1 raw-string-2 raw-string-3))
  (cl:error "unimplemented"))

(cl:defun system:raw-string-upcase (raw-string)
  (cl:declare (cl:ignore raw-string))
  (cl:error "unimplemented"))

(cl:defun system:raw-string-downcase (raw-string)
  (cl:declare (cl:ignore raw-string))
  (cl:error "unimplemented"))

(cl:defun system:number-to-raw-string (number)
  (cl:princ-to-string number))

(cl:defun system:make-raw-array (size)
  (cl:make-array size))

(cl:defun system:raw-array-length (raw-array)
  (cl:length raw-array))

(cl:defun system:raw-array-ref (raw-array index)
  (cl:aref raw-array index))

(cl:defun system:raw-array-set (raw-array index value)
  (cl:setf (cl:aref raw-array index) value))

(cl:defun system:fill-raw-array (raw-array element)
  (cl:fill raw-array element))

(cl:defun system:make-map ()
  (cl:make-hash-table :test 'cl:equal))

(cl:defun system:map-get (map key)
  (cl:gethash key map))

(cl:defun system:map-set (map key value)
  (cl:setf (cl:gethash key map) value))

(cl:defun system:map-length (map)
  (cl:hash-table-count map))

(cl:defun system:map-clear (map)
  (cl:clrhash map))

(cl:defun system:function-name (function)
  (cl:declare (cl:ignore function))
  (cl:error "unimplemented"))

(cl:defun system:unknown-object-to-string (object)
  (cl:declare (cl:ignore object))
  (cl:error "unimplemented"))

(cl:defun system:array-to-js-string (array)
  (cl:declare (cl:ignore array))
  (cl:error "unimplemented"))

(cl:defun system:js-string-to-array (js-string)
  (cl:declare (cl:ignore js-string))
  (cl:error "unimplemented"))

(cl:defun system:js-array-to-array (js-array)
  (cl:declare (cl:ignore js-array))
  (cl:error "unimplemented"))


(cl:defun js::-object ()
  )

(cl:defun js::eval (x)
  (cl:declare (cl:ignore x))
  (cl:error "unimplemented"))

(cl:defun js::console.log (raw-string)
  (cl:declare (cl:ignore raw-string))
  (cl:error "unimplemented"))


(cl:defun ffi:set (var value)
  (cl:declare (cl:ignore var value))
  (cl:error "unimplemented"))

(cl:defun ffi:aget (array index)
  (cl:aref array index))

(cl:defun ffi:ref (cl:&rest args)
  (cl:declare (cl:ignore args))
  (cl:error "unimplemented"))

(cl:defun ffi:typeof (x)
  (cl:declare (cl:ignore x))
  (cl:error "unimplemented"))

(cl:defun ffi:instanceof (value instance)
  (cl:declare (cl:ignore value instance))
  (cl:error "unimplemented"))
