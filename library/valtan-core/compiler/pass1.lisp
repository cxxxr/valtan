(in-package :compiler)

(defparameter *pass1-form-table* (make-hash-table))

(defvar *require-modules* '())
(defvar *export-modules* '())
(defvar *lexenv* nil)
(defvar *compile-level* -1)
(defvar *macro-definitions* '())

(defun toplevel-p (&optional (level *compile-level*))
  (>= 0 level))

(defun make-variable-binding (symbol &key (special-p (special-p symbol)) init-value)
  (make-binding :kind (if special-p :special :variable)
                :name symbol
                :init-value init-value
                :id (genvar symbol)))

(defun make-function-binding (symbol &key init-value)
  (make-binding :kind :function
                :name symbol
                :init-value init-value
                :id (genvar symbol)))

(defun make-macro-binding (name value)
  (make-binding :kind :macro
                :name name
                :id value))

(defun make-symbol-macro-binding (name value)
  (make-binding :kind :symbol-macro
                :name name
                :id value))

(defun make-block-binding (name)
  (make-binding :kind :block
                :name name
                :id (genvar (string name) :symbol)))

(defun make-tag-binding (name value)
  (make-binding :kind :tag
                :name name
                :id value))

(defun count-if-used (binding &optional set-p)
  (when binding
    (if set-p
        (incf (binding-set-count binding))
        (incf (binding-used-count binding)))))

(defmacro def-pass1-form (name (lambda-list return-value-p multiple-values-p) &body body)
  (let ((fn-name (intern (format nil "PASS1-~A" name))))
    (multiple-value-bind (whole lambda-list)
        (if (eq '&whole (car lambda-list))
            (values (cadr lambda-list) (cddr lambda-list))
            (values '#:whole lambda-list))
      `(progn
         (defun ,fn-name (,return-value-p ,multiple-values-p ,whole ,@lambda-list)
           (declare (ignorable ,return-value-p ,multiple-values-p ,whole))
           ,@body)
         (setf (gethash ',name *pass1-form-table*) ',fn-name)))))

(defun get-macro (symbol)
  (get symbol 'macro))

(defun (setf get-macro) (form symbol)
  (setf (get symbol 'macro) form))

(defun get-symbol-macro (symbol)
  (get symbol 'symbol-macro))

(defun (setf get-symbol-macro) (value symbol)
  (setf (get symbol 'symbol-macro) value))

(defun special-p (symbol)
  (get symbol 'special))

(defun (setf special-p) (value symbol)
  (setf (get symbol 'special) value))

(defun constant-p (symbol)
  (get symbol 'constant))

(defun (setf constant-p) (value symbol)
  (setf (get symbol 'constant) value))

(defun lookup (symbol types &optional (bindings *lexenv*))
  (dolist (binding bindings)
    (when (and (if (consp types)
                   (member (binding-kind binding) types)
                   (eq types (binding-kind binding)))
               (eq symbol (binding-name binding)))
      (return binding))))

(defun extend-lexenv (bindings lexenv)
  (append bindings lexenv))

(defun variable-symbol-p (x)
  (and (symbolp x)
       (not (null x))
       (not (keywordp x))
       (not (js-symbol-p x))))

(defun proper-list-p (x)
  (and (listp x)
       (null (cdr (last x)))))

(defun duplicate-var-p (vars)
  (do ((var* vars (rest var*)))
      ((null var*))
    (when (member (first var*) (rest var*))
      (return (first var*)))))

(defun parse-lambda-list (lambda-list)
  (labels ((lambda-list-error ()
             (compile-error "Bad lambda list: ~S" lambda-list))
           (check-variable (x)
             (when (or (not (variable-symbol-p x))
                       (member x '(&rest &optional &key)))
               (lambda-list-error)))
           (add-optional-vars (kind arg add-fn)
             (cond ((symbolp arg)
                    (check-variable arg)
                    (ecase kind
                      (:optional
                       (funcall add-fn (list arg nil nil)))
                      (:key
                       (funcall add-fn (list arg nil nil (make-keyword arg))))))
                   ((consp arg)
                    (unless (proper-list-p arg)
                      (lambda-list-error))
                    (let ((len (length arg)))
                      (case len
                        ((1 2 3)
                         (when (= len 3)
                           (check-variable (third arg))
                           (when (eq (first arg) (third arg))
                             (lambda-list-error)))
                         (ecase kind
                           (:optional
                            (check-variable (first arg))
                            (funcall add-fn (list (first arg)
                                                  (second arg)
                                                  (third arg))))
                           (:key
                            (multiple-value-bind (var value supplied-var keyword)
                                (cond ((consp (first arg))
                                       (let ((arg1 (first arg)))
                                         (unless (proper-list-p arg1)
                                           (lambda-list-error))
                                         (unless (= 2 (length arg1))
                                           (lambda-list-error))
                                         (unless (symbolp (first arg1))
                                           (lambda-list-error))
                                         (check-variable (second arg1))
                                         (values (second arg1)
                                                 (second arg)
                                                 (third arg)
                                                 (first arg1))))
                                      (t
                                       (check-variable (first arg))
                                       (values (first arg)
                                               (second arg)
                                               (third arg)
                                               (make-keyword (first arg)))))
                              (funcall add-fn (list var value supplied-var keyword))))))
                        (t
                         (lambda-list-error)))))
                   (t
                    (lambda-list-error)))))
    (let ((vars '())
          (rest-var nil)
          (state nil)
          (optionals '())
          (keys '())
          (optional-p nil)
          (key-p nil)
          (allow-other-keys nil))
      (do ((arg* lambda-list (cdr arg*)))
          ((null arg*))
        (when (atom arg*)
          (lambda-list-error))
        (let ((arg (car arg*)))
          (cond ((eq arg '&allow-other-keys)
                 (unless (eq state :key)
                   (lambda-list-error))
                 (unless (null (cdr arg*))
                   (lambda-list-error))
                 (setf allow-other-keys t))
                ((eq arg '&key)
                 (when key-p
                   (lambda-list-error))
                 (setf key-p t)
                 (setf state :key))
                ((eq state :key)
                 (add-optional-vars :key arg (lambda (x) (push x keys))))
                (rest-var
                 (lambda-list-error))
                ((eq arg '&rest)
                 (unless (consp (cdr arg*))
                   (lambda-list-error))
                 (check-variable (second arg*))
                 (setq rest-var (second arg*))
                 (setq arg* (cdr arg*)))
                ((eq arg '&optional)
                 (when optional-p
                   (lambda-list-error))
                 (setf optional-p t)
                 (when (eq state :optional)
                   (lambda-list-error))
                 (setq state :optional))
                ((eq state :optional)
                 (add-optional-vars :optional arg (lambda (x) (push x optionals))))
                (t
                 (check-variable arg)
                 (push arg vars)))))
      (setq vars (nreverse vars)
            optionals (nreverse optionals)
            keys (nreverse keys))
      (let* ((min (length vars))
             (parsed-lambda-list
               (make-parsed-lambda-list
                :vars vars
                :optionals optionals
                :rest-var rest-var
                :keys keys
                :min (length vars)
                :max (cond (rest-var nil)
                           (t (+ min
                                 (length optionals)
                                 (* 2 (length keys)))))
                :allow-other-keys allow-other-keys)))
        (let ((all-vars (collect-variables parsed-lambda-list)))
          (when (duplicate-var-p all-vars)
            (lambda-list-error)))
        parsed-lambda-list))))

(defun get-transform (symbol)
  (get symbol 'transform))

(defun transform-symbol-p (symbol)
  (get-transform symbol))

(defmacro def-transform (name lambda-list &body body)
  `(setf (get ',name 'transform)
         (lambda ,lambda-list ,@body)))

(def-transform defun (name lambda-list &rest body)
  (cond ((and (consp name)
              (eq 'setf (first name))
              (= 2 (length name))
              (proper-list-p name)
              (variable-symbol-p (second name)))
         (let ((gensyms (gensym "GENSYMS"))
               (g-args (gensym "ARGS"))
               (g-store (gensym "STORE"))
               (g-setter (gensym "SETTER"))
               (g-getter (gensym "GETTER"))
               (setf-fn (intern (format nil "(SETF ~A)" (second name))
                                (symbol-package (second name)))))
           `(progn
              (defun ,setf-fn ,lambda-list ,@body)
              (define-setf-expander ,(second name) (&rest ,g-args)
                (let ((,gensyms (mapcar (lambda (x) (declare (ignore x)) (gensym))
                                        ,g-args))
                      (,g-store (gensym "G-STORE"))
                      (,g-setter ',setf-fn)
                      (,g-getter ',(second name)))
                  (values ,gensyms
                          ,g-args
                          (list ,g-store)
                          (list* ,g-setter ,g-store ,gensyms)
                          (list* ,g-getter ,gensyms))))
              (*:put ',(second name) '*:fdefinition-setf #',setf-fn)
              ',name)))
        ((variable-symbol-p name)
         (if (toplevel-p)
             `(*:%defun ,name ,lambda-list ,@body)
             `(*:fset ',name
                      (lambda ,lambda-list
                        (block ,name ,@(parse-body body t))))))
        (t
         (compile-error "The NAME argument to DEFUN, ~S, is not a function name." name))))

(def-transform defmacro (name lambda-list &rest body)
  (multiple-value-bind (body declares docstring)
      (parse-body body t)
    (declare (ignore declares docstring))
    (let* ((args (gensym))
           (fn `(lambda (&rest ,args) (destructuring-bind ,lambda-list ,args ,@body))))
      (setf (get-macro name) fn)
      (setf *macro-definitions* (nconc *macro-definitions* (list name)))
      `',name)))

(def-transform *:defmacro* (name lambda-list &rest body)
  (multiple-value-bind (body declares docstring)
      (parse-body body t)
    (declare (ignore declares docstring))
    (let ((fn `(lambda ,lambda-list ,@body)))
      (setf (get-macro name) fn)
      (setf *macro-definitions* (nconc *macro-definitions* (list name)))
      `',name)))

(def-transform define-symbol-macro (name expansion)
  (setf (get-symbol-macro name) expansion)
  (setf *macro-definitions* (nconc *macro-definitions* (list name)))
  `',name)

(def-transform lambda (args &rest body)
  `(function (lambda ,args ,@body)))

(def-transform *:named-lambda (name args &rest body)
  `(function (*:named-lambda ,name ,args ,@body)))

(def-transform defvar (var &optional (value nil value-p) (doc nil docp))
  `(progn
     (declaim (special ,var))
     ,(when value-p
        `(if (boundp ',var) nil (*:%set ',var ,value)))
     ,(when docp
        `(setf (documentation ',var 'variable) ,doc))
     (*:put ',var 'special t)
     ',var))

(def-transform defparameter (var value &optional (doc nil docp))
  `(progn
     (declaim (special ,var))
     (*:%set ',var ,value)
     ,(when docp
        `(setf (documentation ',var 'variable) ,doc))
     (*:put ',var 'special t)
     ',var))

(def-transform defconstant (name value &optional (doc nil docp))
  (setf (constant-p name) (make-variable-binding name :init-value (eval value)))
  `(progn
     (*:%set ',name ,value)
     ,(when docp
        `(setf (documentation ',name 'variable) ,doc))
     (*:put ',name 'constant t)
     ',name))

(def-transform *:backquote (x)
  (expand-backquote x))

(defun pass1-const (x return-value-p)
  (make-hir 'const return-value-p nil x))

(defun js-symbol-to-ref-form (symbol return-value-p)
  (let ((names (parse-js-name symbol)))
    (make-hir 'ffi:ref return-value-p nil names)))

(defun pass1-refvar (symbol return-value-p)
  (let (binding)
    (cond ((js-symbol-p symbol)
           (js-symbol-to-ref-form symbol return-value-p))
          ((setq binding (constant-p symbol))
           (pass1-const (binding-init-value binding) return-value-p))
          (t
           (let ((binding (lookup symbol '(:variable :special))))
             (count-if-used binding)
             (if (and binding (eq (binding-kind binding) :variable))
                 (make-hir 'lref return-value-p nil binding)
                 (make-hir 'gref return-value-p nil symbol)))))))

(defun pass1-forms (forms return-value-p multiple-values-p)
  (if (null forms)
      (list (pass1-const nil return-value-p))
      (let ((new-forms '()))
        (do ((form* forms (rest form*)))
            ((null form*))
          (push (pass1 (first form*)
                       (if (null (rest form*))
                           return-value-p
                           nil)
                       (if (null (rest form*))
                           multiple-values-p
                           nil))
                new-forms))
        (nreverse new-forms))))

(defun pre-process-declaration-specifier (specs)
  (labels ((invalid-declaration-specifier (spec)
             (compile-error "Invalid declaration specifier: ~S" spec)))
    (dolist (spec specs)
      (unless (consp spec)
        (invalid-declaration-specifier spec))
      (case (first spec)
        ((special)
         (unless (proper-list-p spec)
           (invalid-declaration-specifier spec))
         (let ((vars (rest spec)))
           (do ((var* vars (rest var*)))
               ((null var*))
             (unless (variable-symbol-p (first var*))
               (invalid-declaration-specifier spec))
             (when (member (first var*) (rest var*))
               (invalid-declaration-specifier spec)))))))))

(defun pass1-declares (specs inner-lexenv *lexenv*)
  (pre-process-declaration-specifier specs)
  (dolist (spec specs)
    (case (first spec)
      ((special)
       (dolist (symbol (rest spec))
         (let ((binding (lookup symbol '(:variable :special) inner-lexenv)))
           (if binding
               (setf (binding-kind binding) :special)
               (push (make-variable-binding symbol :special-p t)
                     *lexenv*)))))
      ((optimize))
      ((inline))
      ((dynamic-extent))
      (otherwise
       (multiple-value-bind (type vars)
           (if (eq 'type (first spec))
               (values (second spec) (cddr spec))
               (values (first spec) (cdr spec)))
         (dolist (var vars)
           (let ((binding (lookup var '(:variable :special) *lexenv*)))
             (if binding
                 (setf (binding-var-type binding) type)
                 (compile-error "invalid declaration specifier: ~S" spec))))))))
  *lexenv*)

(defun pass1-lambda-list (parsed-lambda-list)
  (let ((vars (parsed-lambda-list-vars parsed-lambda-list))
        (rest-var (parsed-lambda-list-rest-var parsed-lambda-list))
        (inner-lexenv '()))
    (setf (parsed-lambda-list-vars parsed-lambda-list)
          (mapcar (lambda (var)
                    (let ((binding (make-variable-binding var)))
                      (push binding inner-lexenv)
                      binding))
                  vars))
    (flet ((f (opt)
             (let ((binding (make-variable-binding (first opt))))
               (setf (first opt) binding)
               (setf (second opt)
                     (let ((*lexenv* (extend-lexenv inner-lexenv *lexenv*)))
                       (pass1 (second opt) t nil)))
               (push binding inner-lexenv))
             (when (third opt)
               (let ((binding (make-variable-binding (third opt))))
                 (setf (third opt) binding)
                 (push binding inner-lexenv)))))
      (mapc #'f (parsed-lambda-list-optionals parsed-lambda-list))
      (mapc #'f (parsed-lambda-list-keys parsed-lambda-list)))
    (when rest-var
      (let ((binding (make-variable-binding rest-var)))
        (push binding inner-lexenv)
        (setf (parsed-lambda-list-rest-var parsed-lambda-list)
              binding)))
    inner-lexenv))

(defun pass1-lambda (form return-value-p)
  (assert (member (first form) '(lambda *:named-lambda)))
  (multiple-value-bind (name args)
      (if (eq (first form) 'lambda)
          (values nil (rest form))
          (values (second form) (rest (rest form))))
    (when (atom args)
      (compile-error "~S is not a valid lambda expression" form))
    (destructuring-bind (lambda-list &body body) args
      (let ((parsed-lambda-list (parse-lambda-list lambda-list)))
        (multiple-value-bind (body declares docstring)
            (parse-body body t)
          (declare (ignore docstring))
          (let* ((inner-lexenv (pass1-lambda-list parsed-lambda-list))
                 (*lexenv* (extend-lexenv inner-lexenv *lexenv*))
                 (*lexenv* (pass1-declares declares inner-lexenv *lexenv*)))
            (set-hir-position
             form
             (make-hir 'lambda
                       return-value-p
                       nil
                       name
                       parsed-lambda-list
                       (set-hir-position body
                                         (make-hir 'progn t t (pass1-forms body t t)))))))))))

(defun %macro-function (symbol)
  (let ((binding (lookup symbol :macro)))
    ;; (count-if-used binding)
    (cond (binding
           (binding-id binding))
          (t
           (if *in-host-runtime*
               (let ((fn (get-macro symbol)))
                 (if fn
                     (eval fn)
                     nil))
               (let ((fn (get-macro symbol)))
                 (cond ((functionp fn) fn)
                       (fn
                        (setf (get-macro symbol) (eval fn)))
                       (t
                        nil))))))))

(defun %macroexpand-1 (form)
  (cond ((symbolp form)
         (let ((binding (lookup form :symbol-macro)))
           (count-if-used binding)
           (if binding
               (values (binding-id binding) t)
               (let ((expansion (get-symbol-macro form)))
                 (if expansion
                     (values expansion t)
                     (values form nil))))))
        ((and (consp form) (symbolp (first form)))
         (let ((position (get-form-position form))
               (function (%macro-function (first form))))
           (if function
               (let ((expanded-form (apply function (rest form))))
                 (add-source-info* expanded-form position)
                 (values expanded-form t))
               (values form nil))))
        (t
         (values form nil))))

(defun pass1-call (form return-value-p multiple-values-p)
  (let ((fn (first form))
        (args (rest form)))
    (cond ((variable-symbol-p fn)
           (let ((binding (lookup fn :function)))
             (count-if-used binding)
             (cond (binding
                    (set-hir-position
                     form
                     (make-hir 'lcall
                               return-value-p
                               multiple-values-p
                               binding
                               (mapcar (lambda (arg)
                                         (pass1 arg t nil))
                                       args))))
                   ((transform-symbol-p fn)
                    (pass1 (apply (get-transform fn) args)
                           return-value-p
                           multiple-values-p))
                   (t
                    (set-hir-position
                     form
                     (make-hir 'call
                               return-value-p
                               multiple-values-p
                               fn
                               (mapcar (lambda (arg)
                                         (pass1 arg t nil))
                                       args)))))))
          ((and (consp fn)
                (eq 'lambda (first fn)))
           (pass1 (list* 'funcall fn args)
                  return-value-p
                  multiple-values-p))
          ((js-symbol-p fn)
           (let ((names (parse-js-name fn)))
             (set-hir-position
              form
              (make-hir 'js-call
                        return-value-p
                        multiple-values-p
                        (pass1-ref-names (first names) (rest names))
                        (mapcar (lambda (arg)
                                  (pass1 arg t nil))
                                args)))))
          ((and (consp fn)
                (eq 'ffi:ref (first fn)))
           (set-hir-position
            form
            (make-hir 'js-call
                      return-value-p
                      multiple-values-p
                      (pass1-ref-names (cadr fn) (cddr fn))
                      (mapcar (lambda (arg)
                                (pass1 arg t nil))
                              args))))
          (t
           (compile-error "Illegal function call: ~S" form)))))

(defun pass1 (form return-value-p multiple-values-p)
  (multiple-value-bind (form expanded-p)
      (%macroexpand-1 form)
    (cond (expanded-p
           (pass1 form return-value-p multiple-values-p))
          ((member form '(t nil))
           (pass1-const form return-value-p))
          ((keywordp form)
           (pass1-const form return-value-p))
          ((symbolp form)
           (pass1-refvar form return-value-p))
          ((atom form)
           (pass1-const form return-value-p))
          (t
           (let ((fn (gethash (first form) *pass1-form-table*))
                 (*compile-level*
                   (if (member (first form)
                               '(progn
                                 ;macrolet ;; macroletの本体をトップレベル扱いにするとなぜかエラーになる
                                 symbol-macrolet
                                 locally
                                 eval-when))
                       *compile-level*
                       (1+ *compile-level*))))
             (if fn
                 (apply fn return-value-p multiple-values-p form (rest form))
                 (pass1-call form return-value-p multiple-values-p)))))))

(def-pass1-form quote ((x) return-value-p multiple-values-p)
  (pass1-const x return-value-p))

(def-pass1-form setq ((&whole setq-form &rest args) return-value-p multiple-values-p)
  (if (not (evenp (length args)))
      (compile-error "Setq with odd number of args")
      (let ((forms '()))
        (do ((args args (rest (rest args))))
            ((null args))
          (when (constant-p (first args))
            (compile-error "~S is a constant" (first args)))
          (unless (variable-symbol-p (first args))
            (compile-error "~S is not a variable" (first args)))
          (let* ((symbol (first args))
                 (binding (lookup symbol :variable))
                 (value (pass1 (second args) t nil)))
            (count-if-used binding)
            (count-if-used binding t)
            (push (set-hir-position
                   args
                   (make-hir (if binding 'lset 'gset)
                             (if (null (cddr args))
                                 return-value-p
                                 nil)
                             nil
                             (or binding symbol)
                             value))
                  forms)))
        (cond ((null forms)
               (pass1-const nil return-value-p))
              ((length=1 forms)
               (first forms))
              (t
               (set-hir-position
                setq-form
                (make-hir 'progn
                          return-value-p
                          nil
                          (nreverse forms))))))))

(def-pass1-form if ((&whole if-form test then &optional else) return-value-p multiple-values-p)
  (set-hir-position
   if-form
   (make-hir 'if
             return-value-p
             multiple-values-p
             (pass1 test t nil)
             (pass1 then return-value-p multiple-values-p)
             (pass1 else return-value-p multiple-values-p))))

(def-pass1-form progn ((&whole progn-form &rest forms) return-value-p multiple-values-p)
  (set-hir-position
   progn-form
   (make-hir 'progn
             return-value-p
             multiple-values-p
             (pass1-forms forms return-value-p multiple-values-p))))

(def-pass1-form function ((&whole function-form thing) return-value-p multiple-values-p)
  (cond
    ((symbolp thing)
     (let ((binding (lookup thing :function)))
       (count-if-used binding)
       (if binding
           (set-hir-position
            function-form
            (make-hir 'lref return-value-p nil binding))
           (pass1 `(symbol-function ',thing)
                  return-value-p
                  nil))))
    ((and (consp thing) (member (first thing) '(lambda *:named-lambda)))
     (pass1-lambda thing return-value-p))
    (t
     (compile-error "~S is not a legal function name" thing))))

(defun check-let-form (bindings)
  (unless (listp bindings)
    (compile-error "Malformed LET bindings: ~S" bindings))
  (mapcar (lambda (b)
            (let ((b (if (listp b) b (list b))))
              (unless (or (and (consp b)
                               (<= 1 (length b) 2))
                          (symbolp b))
                (compile-error "Malformed LET binding: ~S" b))
              (let ((var (first b)))
                (when (constant-p var)
                  (compile-error "~S is a constant" var))
                (unless (variable-symbol-p var)
                  (compile-error "~S is not a variable" var)))
              b))
          bindings))

(defun pass1-let-body (let-form bindings body return-value-p multiple-values-p)
  (multiple-value-bind (body declares)
      (parse-body body nil)
    (let* ((inner-lexenv bindings)
           (*lexenv* (extend-lexenv inner-lexenv *lexenv*))
           (*lexenv* (pass1-declares declares inner-lexenv *lexenv*)))
      (set-hir-position
       let-form
       (make-hir 'let
                 return-value-p
                 multiple-values-p
                 bindings
                 (pass1-forms body return-value-p multiple-values-p))))))

(def-pass1-form let ((&whole let-form bindings &rest body) return-value-p multiple-values-p)
  (setf bindings (check-let-form bindings))
  (let ((bindings (mapcar (lambda (b)
                            (make-variable-binding
                             (first b)
                             :init-value (pass1 (second b) t nil)))
                          bindings)))
    (pass1-let-body let-form bindings body return-value-p multiple-values-p)))

(def-pass1-form let* ((&whole let-form bindings &rest body) return-value-p multiple-values-p)
  (setf bindings (check-let-form bindings))
  (let ((*lexenv* *lexenv*))
    (let ((bindings (mapcar (lambda (b)
                              (let* ((b (make-variable-binding
                                         (first b)
                                         :init-value (pass1 (second b) t nil))))
                                (push b *lexenv*)
                                b))
                            bindings)))
      (pass1-let-body let-form bindings body return-value-p multiple-values-p))))

(defun check-flet-definitions (definitions)
  (unless (listp definitions)
    (compile-error "Malformed definitions: ~S" definitions))
  (dolist (definition definitions)
    (unless (consp definition)
      (compile-error "Invalid definition form: ~S" definition))
    (unless (variable-symbol-p (first definition))
      (compile-error "Illegal function name: ~S" (first definition)))))

(defun parse-flet-definitions (definitions compile-lambda-p)
  (mapcar (lambda (definition)
            (make-function-binding
             (first definition)
             :init-value (let ((fn `(lambda ,@(rest definition))))
                           (if compile-lambda-p
                               (pass1-lambda fn t)
                               fn))))
          definitions))

(def-pass1-form flet ((&whole flet-form definitions &rest body) return-value-p multiple-values-p)
  (check-flet-definitions definitions)
  (let ((bindings (parse-flet-definitions definitions t)))
    (multiple-value-bind (body declares)
        (parse-body body nil)
      (let* ((inner-lexenv bindings)
             (*lexenv* (extend-lexenv inner-lexenv *lexenv*))
             (*lexenv* (pass1-declares declares inner-lexenv *lexenv*)))
        (set-hir-position
         flet-form
         (make-hir 'let
                   return-value-p
                   multiple-values-p
                   bindings
                   (pass1-forms body return-value-p multiple-values-p)))))))

(def-pass1-form labels ((&whole labels-form definitions &rest body) return-value-p multiple-values-p)
  (check-flet-definitions definitions)
  (let ((bindings (parse-flet-definitions definitions nil)))
    (multiple-value-bind (body declares)
        (parse-body body nil)
      (let* ((inner-lexenv bindings)
             (*lexenv* (extend-lexenv inner-lexenv *lexenv*))
             (*lexenv* (pass1-declares declares inner-lexenv *lexenv*)))
        (dolist (b bindings)
          (setf (binding-init-value b)
                (pass1 (binding-init-value b) t nil)))
        (set-hir-position
         labels-form
         (make-hir 'let
                   return-value-p
                   multiple-values-p
                   bindings
                   (pass1-forms body return-value-p multiple-values-p)))))))

(def-pass1-form macrolet ((definitions &rest body) return-value-p multiple-values-p)
  (check-flet-definitions definitions)
  (let ((*lexenv*
          (extend-lexenv (mapcar (lambda (definition)
                                   (make-macro-binding (first definition)
                                                       (eval `(lambda ,@(rest definition)))))
                                 definitions)
                         *lexenv*)))
    (apply #'pass1-progn return-value-p multiple-values-p nil body)))

(def-pass1-form symbol-macrolet ((definitions &rest body) return-value-p multiple-values-p)
  (check-flet-definitions definitions)
  (let ((*lexenv*
          (extend-lexenv (mapcar (lambda (definition)
                                   (make-symbol-macro-binding (first definition)
                                                              (second definition)))
                                 definitions)
                         *lexenv*)))
    (apply #'pass1-progn return-value-p multiple-values-p nil body)))

(def-pass1-form unwind-protect ((&whole unwind-protected-form protected &rest cleanup) return-value-p multiple-values-p)
  (set-hir-position
   unwind-protected-form
   (make-hir 'unwind-protect
             return-value-p
             multiple-values-p
             (pass1 protected return-value-p multiple-values-p)
             (apply #'pass1-progn nil nil nil cleanup))))

(def-pass1-form block ((&whole block-form name &rest forms) return-value-p multiple-values-p)
  (unless (symbolp name)
    (compile-error "The block name ~S is not a symbol." name))
  (let* ((binding (make-block-binding name))
         (*lexenv* (cons binding *lexenv*)))
    (let ((body (pass1-forms forms return-value-p multiple-values-p)))
      (set-hir-position
       block-form
       (make-hir 'block
                 return-value-p
                 multiple-values-p
                 binding
                 body)))))

(def-pass1-form return-from ((&whole return-from-form name &optional value) return-value-p multiple-values-p)
  (unless (symbolp name)
    (compile-error "~S is not a symbol" name))
  (let ((binding (lookup name :block)))
    (count-if-used binding)
    (if binding
        (set-hir-position
         return-from-form
         (make-hir 'return-from return-value-p nil binding (pass1 value t t)))
        (compile-error "return for unknown block: ~S" name))))

(defvar *tagbody-id* 0)

(defun make-tagbody-id ()
  (format nil
          "~A_~A"
          (if *in-host-runtime* :host :target)
          *tagbody-id*))

(defun tag-literal-p (x)
  (or (symbolp x) (integerp x)))

(def-pass1-form tagbody ((&whole tagbody-form &rest statements) return-value-p multiple-values-p)
  (let ((*tagbody-id* (gensym)))
    (let* ((tags (remove-if-not #'tag-literal-p statements))
           (*lexenv*
             (extend-lexenv (mapcar (lambda (tag)
                                      (make-tag-binding tag
                                                        (make-tagbody-value :index (gensym)
                                                                            :id (make-tagbody-id))))
                                    tags)
                            *lexenv*))
           (entry-tagbody-value
             (make-tag-binding nil (make-tagbody-value :index (gensym) :id (make-tagbody-id))))
           (part-statements '())
           (tag-statements-pairs '())
           (none '#:none)
           (last-tag none)
           (last-tag-form (get-form-position statements)))
      (flet ((add-statements ()
               (unless part-statements
                 (setf part-statements (list (pass1-const nil nil))))
               (if (eq last-tag none)
                   (push (cons entry-tagbody-value
                               (set-hir-position
                                last-tag-form
                                (make-hir 'progn nil nil (nreverse part-statements))))
                         tag-statements-pairs)
                   (push (let ((binding (lookup last-tag :tag)))
                           (assert binding)
                           (cons binding
                                 (set-hir-position
                                  last-tag-form
                                  (make-hir 'progn nil nil (nreverse part-statements)))))
                         tag-statements-pairs))
               (setf part-statements nil)))
        (do ((statements* statements (rest statements*)))
            ((null statements*)
             (add-statements))
          (cond ((tag-literal-p (first statements*))
                 (add-statements)
                 (setf last-tag (first statements*)
                       last-tag-form statements*))
                (t
                 (push (pass1 (first statements*) nil nil) part-statements))))
        (set-hir-position
         tagbody-form
         (make-hir 'tagbody
                   return-value-p
                   nil
                   (make-tagbody-id)
                   (nreverse tag-statements-pairs)
                   nil))))))

(def-pass1-form go ((&whole go-form tag) return-value-p multiple-values-p)
  (unless (tag-literal-p tag)
    (compile-error "~S is not a symbol" tag))
  (let ((binding (lookup tag :tag)))
    (unless binding
      (compile-error "attempt to GO to nonexistent tag: ~A" tag))
    (count-if-used binding)
    (set-hir-position
     go-form
     (make-hir 'go nil nil binding))))

(def-pass1-form locally ((&rest body) return-value-p multiple-values-p)
  (multiple-value-bind (body declares)
      (parse-body body nil)
    (let ((*lexenv* (pass1-declares declares nil *lexenv*)))
      (apply #'pass1-progn return-value-p multiple-values-p nil body))))

(def-pass1-form declaim ((&rest specs) return-value-p multiple-values-p)
  (pre-process-declaration-specifier specs)
  (dolist (spec specs)
    (case (first spec)
      ((special)
       (dolist (symbol (rest spec))
         (setf (special-p symbol) t)))))
  (pass1-const nil return-value-p))

(def-pass1-form eval-when ((situations &rest body) return-value-p multiple-values-p)
  (cond ((toplevel-p)
         (when (or (member :compile-toplevel situations)
                   (member 'compile situations))
           (eval `(progn ,@body)))
         (if (or (member :load-toplevel situations)
                   (member 'load situations))
             (pass1-toplevel `(progn ,@body))
             (pass1-const nil return-value-p)))
        ((or (member :execute situations)
             (member 'eval situations))
         (pass1 `(progn ,@body) return-value-p multiple-values-p))
        (t
         (pass1-const nil return-value-p))))

(def-pass1-form *:%defun ((&whole defun-form name lambda-list &rest body) return-value-p multiple-values-p)
  (multiple-value-bind (body declares docstring)
      (parse-body body t)
    (declare (ignore docstring))
    (let* ((body `(block ,name ,@body))
           (fn (pass1 (if (null declares)
                          `(*:named-lambda ,name ,lambda-list ,body)
                          `(*:named-lambda ,name ,lambda-list (declare ,@declares) ,body))
                      t nil)))
      (pushnew (cons name fn) *known-toplevel-functions*)
      (set-hir-position
       defun-form
       (make-hir '*:%defun
                 return-value-p
                 nil
                 name
                 fn)))))

(def-pass1-form *:multiple-value-call ((&whole form function &rest args) return-value-p multiple-values-p)
  (set-hir-position
   form
   (make-hir 'call
             return-value-p
             multiple-values-p
             '*:multiple-value-call
             (cons (pass1 function t nil)
                   (mapcar (lambda (arg)
                             (pass1 arg t t))
                           args)))))

(def-pass1-form *:%defpackage ((&whole form
                                       name &key export use nicknames)
                               return-value-p multiple-values-p)
  (let ((name (string name))
        (export-names (mapcar #'string export))
        (use-package-names (mapcar #'string use)))
    (set-hir-position
     form
     (make-hir '*:%defpackage
               return-value-p
               nil
               name
               (list export-names
                     use-package-names
                     nicknames)))))

(def-pass1-form in-package ((&whole form name) return-value-p multiple-values-p)
  (setq *package* (find-package name))
  (setq valtan-core::*package* (find-package name)) ;TODO
  (let ((name (string name)))
    (set-hir-position
     form
     (make-hir '*:%in-package
               return-value-p
               nil
               name))))

(defun ref-form-p (x)
  (and (consp x)
       (eq 'ffi:ref (car x))))

(defun ident-place-p (x)
  (or (stringp x)
      (symbolp x)
      (ref-form-p x)))

(defun pass1-ref-names (object keys)
  (let ((arguments '()))
    (push (if (stringp object) object (pass1 object t nil)) arguments)
    (dolist (key keys)
      (setq arguments
            (nconc arguments
                   (cond ((stringp key)
                          (list key))
                         ((symbolp key)
                          (parse-js-name key))
                         (t
                          (compile-error "~S is not a string or symbol" key))))))
    arguments))

(def-pass1-form ffi:ref ((&whole form object &rest keys) return-value-p multiple-values-p)
  (let ((arguments (pass1-ref-names object keys)))
    (set-hir-position
     form
     (make-hir 'ffi:ref return-value-p nil arguments))))

(def-pass1-form ffi:set ((&whole form lhs rhs) return-value-p multiple-values-p)
  (set-hir-position
   form
   (make-hir 'ffi:set
             return-value-p
             nil
             (pass1 lhs t nil)
             (pass1 rhs t nil))))

(defun convert-var (var)
  (cond ((ref-form-p var)
         (pass1 var nil nil))
        ((js-symbol-p var)
         (js-symbol-to-ref-form var nil))
        (t
         (string var))))

(def-pass1-form ffi:var ((&whole form &rest vars) return-value-p multiple-values-p)
  (dolist (var vars)
    (unless (ident-place-p var)
      (compile-error "~S is not a variable identifier" var)))
  (set-hir-position
   form
   (make-hir 'ffi:var
             return-value-p
             multiple-values-p
             (mapcar #'convert-var vars))))

(def-pass1-form ffi:require ((&rest args) return-value-p multiple-values-p)
  (destructuring-bind (var module-name)
      (if (length=1 args)
          (list nil (first args))
          args)
    (when var
      (unless (ident-place-p var)
        (compile-error "~S is not a variable identifer" var)))
    (unless (stringp module-name)
      (compile-error "~S is not a string" module-name))
    (push (cons (if var (convert-var var)) module-name)
          *require-modules*)
    (pass1-const nil return-value-p)))

(def-pass1-form ffi:export ((name &key as) return-value-p pmultiple-values-p)
  (unless (ident-place-p name)
    (compile-error "~S is not a variable identifer" name))
  (push (cons (convert-var name) (and as (convert-var as)))
        *export-modules*)
  (pass1-const nil return-value-p))

(def-pass1-form ffi:typeof ((&whole form x) return-value-p multiple-values-p)
  (set-hir-position
   form
   (make-hir 'ffi:typeof
             return-value-p
             nil
             (pass1 x t nil))))

(def-pass1-form ffi:new ((&whole form constructor &rest args) return-value-p multiple-values-p)
  (set-hir-position
   form
   (make-hir 'ffi:new
             return-value-p
             nil
             (pass1 constructor t nil)
             (mapcar (lambda (arg)
                       (pass1 arg t nil))
                     args))))

(def-pass1-form ffi:aget ((&whole form array index &rest other-indexes) return-value-p multiple-value-p)
  (set-hir-position
   form
   (make-hir 'ffi:aget
             return-value-p
             nil
             (pass1 array t nil)
             (cons (pass1 index t nil)
                   (mapcar (lambda (x)
                             (pass1 x t nil))
                           other-indexes)))))

(defun pass1-toplevel (form &optional return-value-p multiple-values-p)
  (let ((*lexenv* '())
        (*compile-level* -1))
    (pass1 form return-value-p multiple-values-p)))

(defun pass1-module (file hir-forms export-modules)
  (make-hir 'module nil nil file hir-forms export-modules))

(defun pass1-dump-macros (&optional (macro-definitions *macro-definitions*))
  (mapcar (lambda (name)
            (pass1-toplevel
             (if (get-symbol-macro name)
                 `(*:put ',name 'symbol-macro ,(get-symbol-macro name))
                 `(*:put ',name 'macro ,(get-macro name)))))
          macro-definitions))
