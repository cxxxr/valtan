#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-deftype-lambda-list (lambda-list)
    (let ((state nil))
      (mapcar (lambda (x)
                (cond ((eq x '&optional)
                       (setq state :optional)
                       x)
                      ((eq x '&key)
                       (setq state :key)
                       x)
                      ((member x lambda-list-keywords)
                       (setq state nil)
                       x)
                      ((member state '(:optional :key))
                       (list (if (symbolp x) (car x) x)
                             ''*))
                      (t
                       x)))
              lambda-list))))

(defmacro deftype (name lambda-list &body body)
  (setq lambda-list (parse-deftype-lambda-list lambda-list))
  `(progn
     (setf (deftype-expander ',name)
           (lambda ,lambda-list ,@body))
     ',name))

(defun deftype-expander (symbol)
  (get symbol 'deftype-expander))

(defun (cl:setf deftype-expander) (expander symbol)
  (cl:setf (cl:get symbol 'deftype-expander) expander))

(defun expand-deftype (type)
  (do ()
      (nil)
    (let ((name (if (consp type) (car type) type))
          (args (if (consp type) (cdr type) nil)))
      (let ((expander (deftype-expander name)))
        (if expander
            (setq type (apply expander args))
            (return type))))))

(defun canonicalize-type (type)
  (setq type (expand-deftype type))
  (let ((name (if (consp type) (car type) type))
        (args (if (consp type) (cdr type) nil)))
    (case name
      ((array simple-array vector simple-vector bit-vector simple-bit-vector)
       (list 'array
             (if (member (car args) '(* nil))
                 t
                 (car args))))
      ((string simple-string base-string simple-base-string)
       '(array character))
      (otherwise
       type))))

(defun typep (object type &optional environment)
  (declare (ignore environment))
  (case type
    (null (null object))
    (list (listp object))
    (cons (consp object))
    (symbol (symbolp object))
    (string (cl:stringp object))
    ;; (hash-table (hash-table-p object))
    (vector (vectorp object))
    ;; (array (arrayp object))
    (integer (integerp object))
    (numberp (numberp object))
    (otherwise
     (cond ((and (*:structure-p object)
                 (eq (*:%structure-name object)
                     'standard-instance)
                 (find-class type nil))
            (subclassp (class-of object) (find-class type nil)))
           ((*:structure-p object)
            (eq (*:%structure-name object)
                type))))))

(defun subtypep (type1 type2 &optional environment)
  (declare (ignore environment))
  (cond ((eq type1 type2)
         (values t t))
        ((and (member type1 '(character base-char standard-char extended-char))
              (eq type2 'character))
         (values t t))
        (t
         (values nil nil))))

(defmacro typecase (keyform &body cases)
  (let ((gvalue (cl:gensym)))
    `(let ((,gvalue ,keyform))
       (cond ,@(cl:mapcar (lambda (c)
                            (cl:destructuring-bind (type . body) c
                              (if (eq type 'otherwise)
                                  `(t ,@body)
                                  `((typep ,gvalue ',type) ,@body))))
                          cases)))))

(defmacro etypecase (keyform &body cases)
  (let ((gvalue (cl:gensym))
        (expected-type
          `(or ,@(mapcar #'first cases))))
    `(let ((,gvalue ,keyform))
       (cond ,@(mapcar (lambda (c)
                         (destructuring-bind (type . body) c
                           `((typep ,gvalue ',type) ,@body)))
                       cases)
             (t (error "~S fell through ETYPECASE expression. Wanted one of ~S."
                       ,gvalue
                       ',expected-type))))))

(defun type-of (object)
  (typecase object
    (integer
     'integer)
    (character
     'character)
    (symbol
     (cond ((eq object t) 'boolean)
           ((eq object nil) 'null)
           ((keywordp object) 'keyword)
           (t 'symbol)))
    (array
     `(array (array-element-type object)))
    (otherwise
     (cond ((*:structure-p object)
            (*:%structure-name object))
           (t
            (error "unexpected object: ~S" object))))))
