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
                       (if (symbolp x)
                           (list x ''*)
                           x))
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
    (atom (atom object))
    (symbol (symbolp object))
    (keyword (keywordp object))
    (string (cl:stringp object))
    (simple-string (simple-string-p object))
    (base-string (cl:stringp object))
    (simple-base-string (simple-string-p object))
    (character (characterp object))
    (base-char (characterp object))
    (standard-char (and (characterp object)
                        (standard-char-p object)))
    (hash-table (and (*:structure-p object)
                     (eq (*:%structure-name object) 'hash-table)))
    ;; Use direct structure checks to avoid circular dependency with defstruct predicates
    (vector (and (*:structure-p object)
                 (eq (*:%structure-name object) 'array)
                 (= 1 (array-rank object))))
    (simple-vector (and (*:structure-p object)
                        (eq (*:%structure-name object) 'array)
                        (= 1 (array-rank object))
                        (eq (array-element-type object) t)
                        (not (array-fill-pointer object))
                        (not (array-displaced-to object))))
    (array (and (*:structure-p object)
                (eq (*:%structure-name object) 'array)))
    (simple-array (and (*:structure-p object)
                       (eq (*:%structure-name object) 'array)
                       (not (array-fill-pointer object))
                       (not (array-displaced-to object))))
    (bit-vector (and (*:structure-p object)
                     (eq (*:%structure-name object) 'array)
                     (= 1 (array-rank object))
                     (eq (array-element-type object) 'bit)))
    (simple-bit-vector (and (*:structure-p object)
                            (eq (*:%structure-name object) 'array)
                            (= 1 (array-rank object))
                            (eq (array-element-type object) 'bit)
                            (not (array-fill-pointer object))
                            (not (array-displaced-to object))))
    (integer (integerp object))
    (fixnum (integerp object))
    (bignum nil)  ; JavaScript doesn't have bignums in the traditional sense
    (number (numberp object))
    (real (realp object))
    (rational (rationalp object))
    (float (floatp object))
    (function (functionp object))
    (compiled-function (functionp object))
    (sequence (or (listp object) (vectorp object)))
    ((t) t)  ; type T means any object matches
    (otherwise
     (cond ((consp type)
            (case (car type)
              (or (some (lambda (ty) (typep object ty)) (cdr type)))
              (and (every (lambda (ty) (typep object ty)) (cdr type)))
              (not (not (typep object (cadr type))))
              (member (member object (cdr type)))
              (eql (eql object (cadr type)))
              (satisfies (funcall (cadr type) object))
              ((array simple-array vector simple-vector)
               (and (arrayp object)
                    (or (null (cdr type))
                        (eq (cadr type) '*)
                        (eq (array-element-type object) (cadr type)))))
              (otherwise nil)))
           ((and (*:structure-p object)
                 (eq (*:%structure-name object)
                     'standard-instance)
                 (find-class type nil))
            (subclassp (class-of object) (find-class type nil)))
           ((*:structure-p object)
            (eq (*:%structure-name object)
                type))
           (t nil)))))

(defun subtypep (type1 type2 &optional environment)
  (declare (ignore environment))
  (cond ;; Same type
        ((eq type1 type2)
         (values t t))
        ;; Everything is a subtype of T
        ((eq type2 t)
         (values t t))
        ;; NIL is a subtype of everything
        ((eq type1 'nil)
         (values t t))
        ;; Character types
        ((and (member type1 '(character base-char standard-char extended-char))
              (eq type2 'character))
         (values t t))
        ;; Bit type
        ((and (eq type1 'bit)
              (member type2 '(bit integer fixnum number real rational t)))
         (values t t))
        ;; Integer types
        ((and (eq type1 'fixnum)
              (member type2 '(integer number real rational t)))
         (values t t))
        ((and (eq type1 'integer)
              (member type2 '(number real rational t)))
         (values t t))
        ;; Number types
        ((and (eq type1 'float)
              (member type2 '(number real t)))
         (values t t))
        ((and (eq type1 'real)
              (member type2 '(number t)))
         (values t t))
        ((and (eq type1 'rational)
              (member type2 '(real number t)))
         (values t t))
        ;; String types
        ((and (member type1 '(string simple-string base-string simple-base-string))
              (member type2 '(string vector array sequence t)))
         (values t t))
        ;; Vector/array types
        ((and (member type1 '(vector simple-vector bit-vector simple-bit-vector))
              (member type2 '(array sequence t)))
         (values t t))
        ((and (eq type1 'array)
              (eq type2 't))
         (values t t))
        ;; List types
        ((and (member type1 '(null cons list))
              (eq type2 'list))
         (values t t))
        ((and (member type1 '(null cons list))
              (eq type2 'sequence))
         (values t t))
        ((and (eq type1 'null)
              (member type2 '(list symbol atom t)))
         (values t t))
        ;; Symbol types
        ((and (eq type1 'keyword)
              (member type2 '(symbol atom t)))
         (values t t))
        ((and (eq type1 'symbol)
              (member type2 '(atom t)))
         (values t t))
        ;; Atom
        ((and (member type1 '(symbol number character array function hash-table))
              (eq type2 'atom))
         (values t t))
        ;; Function types
        ((and (eq type1 'compiled-function)
              (member type2 '(function t)))
         (values t t))
        ;; Unknown relationship
        (t
         (values nil nil))))

(defmacro typecase (keyform &body cases)
  (let ((gvalue (cl:gensym)))
    `(let ((,gvalue ,keyform))
       (cond ,@(cl:mapcar (lambda (c)
                            (cl:destructuring-bind (type . body) c
                              (if (cl:member type '(otherwise t))
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
             (t (error 'type-error
                       :datum ,gvalue
                       :expected-type ',expected-type))))))

;; Simplified ctypecase without restart-case (signals error on no match)
(defmacro ctypecase (keyplace &body cases)
  (let ((gvalue (cl:gensym))
        (expected-type
          `(or ,@(mapcar #'first cases))))
    `(let ((,gvalue ,keyplace))
       (cond ,@(mapcar (lambda (c)
                         (destructuring-bind (type . body) c
                           `((typep ,gvalue ',type) ,@body)))
                       cases)
             (t (error 'type-error
                       :datum ,gvalue
                       :expected-type ',expected-type))))))

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
