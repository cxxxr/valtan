(cl:in-package :valtan-core)

(cl:defparameter system:+null+ '#:null)

(cl:defclass structure ()
  ((name :initarg :name :reader structure-name)
   (values :initarg :values :reader structure-values)))

(cl:defmethod cl:print-object ((object structure) stream)
  (cl:case (structure-name object)
    ;; ((string array)
    ;;  (cl:prin1 (cl:first (structure-values object)) stream))
    (cl:otherwise
     (cl:print-unreadable-object (object stream)
       (cl:format stream "STRUCTUER ~A ~S"
                  (structure-name object)
                  (structure-values object))))))

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

(cl:defun system::make-structure-array! (contents &optional (element-type t element-type-p))
  (system:make-structure 'array
                         contents
                         (cl:list (cl:length contents))
                         (cl:length contents)
                         nil
                         nil
                         nil
                         1
                         (cl:cond (element-type-p element-type)
                                  ((cl:stringp contents) 'character)
                                  (t 't))))


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
  (cl:or (cl:symbol-name symbol)
         system:+null+))

(cl:defun system:symbol-package-name (symbol)
  (cl:let ((package (cl:symbol-package symbol)))
    (if package
        (cl:package-name package)
        system:+null+)))

(cl:defun system:fset (symbol function)
  (cl:setf (cl:symbol-function symbol) function))

(cl:defun system:map-package-symbols (package function)
  (cl:do-symbols (s package nil)
    (cl:funcall function s)))

(cl:defun system:put (symbol key value)
  (cl:setf (cl:get symbol key) value))

(cl:defun system:package-name (package)
  (cl:package-name package))

(cl:defun system:package-nicknames (package)
  (cl:package-nicknames package))

(cl:defun system:intern (name package)
  (cl:intern name package))

(cl:defun system:find-symbol (name package)
  (cl:find-symbol name package))

(cl:defun system:make-package (name nicknames use-package-names)
  (cl:make-package name :nicknames nicknames :use use-package-names))

(cl:defun system:%defpackage (package &key export use nicknames)
  (flet ((ref-string (structure)
           (system:%structure-ref structure 0)))
    (setq package (ref-string package)
          export (mapcar #'ref-string export)
          use (mapcar #'ref-string use)
          nicknames (mapcar #'ref-string nicknames))
    (let ((package (or (cl:find-package package)
                       (cl:make-package package :use use :nicknames nicknames))))
      (cl:export export package)
      package)))

(cl:defun system:export (symbols package)
  (cl:export symbols package))

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

(cl:defun system:raw-array-to-list (raw-array)
  raw-array)

(cl:defun system:list-to-raw-array (list)
  list)

(cl:defmacro system:multiple-value-call (function cl:&rest args)
  `(cl:multiple-value-call ,function ,@args))

(cl:defun system:error (value)
  (cl:error value))

(cl:defun system:%code-char (code)
  (cl:code-char code))

(cl:defun system:%char-code (char)
  (cl:char-code char))


(cl:defmacro system:defmacro* (name lambda-list cl:&body body)
  `(cl:defmacro ,name ,lambda-list ,@body))

(cl:defun system:make-raw-string ()
  (cl:make-string 0))

(cl:defun system:expand-raw-string (raw-string n)
  (let ((new-string (cl:make-string n :initial-element #.(cl:code-char 0))))
    (cl:replace new-string raw-string)
    new-string))

(cl:defun system:code-to-raw-string (code)
  (cl:string (cl:code-char code)))

(cl:defun system:sub-raw-string/2 (raw-string start)
  (cl:subseq raw-string start))

(cl:defun system:sub-raw-string/3 (raw-string start end)
  (cl:subseq raw-string start end))

(flet ((concat (&rest raw-strings)
         (cl:apply #'cl:concatenate
                   'cl:string
                   (cl:mapcar (cl:lambda (raw-string)
                                (if (cl:characterp raw-string)
                                    (cl:string raw-string)
                                    raw-string))
                              raw-strings))))
  (cl:defun system:concat-raw-string/2 (raw-string-1 raw-string-2)
    (concat raw-string-1 raw-string-2))

  (cl:defun system:concat-raw-string/3 (raw-string-1 raw-string-2 raw-string-3)
    (concat raw-string-1 raw-string-2 raw-string-3)))

(cl:defun system:raw-string-upcase (raw-string)
  (cl:string-upcase raw-string))

(cl:defun system:raw-string-downcase (raw-string)
  (cl:string-downcase raw-string))

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

(cl:defun system:array-to-raw-string (array)
  (cl:declare (cl:ignore array))
  (cl:error "unimplemented"))

(cl:defun system:raw-string-to-array (raw-string)
  (cl:declare (cl:ignore raw-string))
  (cl:error "unimplemented"))

(cl:defun system:raw-array-to-array (raw-array)
  (cl:declare (cl:ignore raw-array))
  (cl:error "unimplemented"))

(cl:defun system:read-whole-file (filename)
  (cl:with-open-file (in filename)
    (cl:with-output-to-string (out)
      (cl:let* ((buffer-size 4096)
                (buffer (cl:make-array buffer-size :element-type 'cl:character)))
        (cl:loop
          :for bytes-read := (cl:read-sequence buffer in)
          :do (cl:write-sequence buffer out :start 0 :end bytes-read)
          :while (cl:= bytes-read buffer-size))))))

(cl:defun system:write-raw-string-to-stdout (raw-string)
  (cl:write-string raw-string))


(cl:defun js::-object ()
  (cl:error "unimplemented"))

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
