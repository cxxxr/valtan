(in-package :compiler)

(defparameter *pass1-form-table* (make-hash-table))

(defvar *require-modules* '())
(defvar *defined-function-names* '())
(defvar *called-function-names* '())
(defvar *lexenv*)

(defun make-variable-binding (symbol &optional (special-p (special-p symbol)))
  (make-binding :type (if special-p :special :variable)
                :name symbol
                :value symbol))

(defun make-function-binding (symbol)
  (make-binding :type :function
                :name symbol
                :value symbol))

(defun make-macro-binding (name value)
  (make-binding :type :macro
                :name name
                :value value))

(defun make-symbol-macro-binding (name value)
  (make-binding :type :symbol-macro
                :name name
                :value value))

(defun make-block-binding (name)
  (make-binding :type :block
                :name name
                :value name))

(defun make-tag-binding (name value)
  (make-binding :type :tag
                :name name
                :value value))

(defun count-if-used (binding)
  (when binding
    (incf (binding-used-count binding))))

(defmacro def-pass1-form (name (lambda-list return-value-p multiple-values-p) &body body)
  (let ((fn-name (intern (format nil "PASS1-~A" name))))
    `(progn
       (defun ,fn-name (,return-value-p ,multiple-values-p ,@lambda-list)
         (declare (ignorable ,return-value-p ,multiple-values-p))
         ,@body)
       (setf (gethash ',name *pass1-form-table*) ',fn-name))))

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

(defun lookup (symbol types &optional (bindings *lexenv*))
  (dolist (binding bindings)
    (when (and (if (consp types)
                   (member (binding-type binding) types)
                   (eq types (binding-type binding)))
               (eq symbol (binding-name binding)))
      (return binding))))

(defun extend-lexenv (bindings lexenv)
  (append bindings lexenv))

(defun variable-symbol-p (x)
  (and (symbolp x)
       (not (null x))
       (not (keywordp x))))

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
                                         (unless (keywordp (first arg1))
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
  (pushnew name *defined-function-names*)
  (cond ((and (consp name)
              (eq 'setf (first name))
              (= 2 (length name))
              (proper-list-p name)
              (variable-symbol-p (second name)))
         ;; TODO
         )
        ((variable-symbol-p name)
         `(system::fset ',name (lambda ,lambda-list ,@body)))
        (t
         (compile-error "The NAME argument to DEFUN, ~S, is not a function name." name))))

(def-transform defmacro (name lambda-list &rest body)
  (let* ((args (gensym))
         (fn `(lambda (,args) (destructuring-bind ,lambda-list ,args ,@body))))
    (setf (get-macro name) (eval fn))
    `',name))

(def-transform define-symbol-macro (name expansion)
  (setf (get-symbol-macro name) expansion)
  `',name)

(def-transform lambda (args &rest body)
  `(function (lambda ,args ,@body)))

(def-transform defvar (x &optional (value nil value-p) doc)
  (declare (ignore doc))
  `(progn
     (declaim (special ,x))
     ,(when value-p
        `(if (boundp ',x) nil (setq ,x ,value)))
     ',x))

(def-transform in-package (package)
  (let ((package (find-package package)))
    (setq *package* package)
    nil))

(defun expand-quasiquote (x)
  (cond ((atom x)
         (list 'quote x))
        ((eq 'system::unquote (first x))
         (assert (= 2 (length x)))
         (second x))
        ((and (consp (first x))
              (eq (first (first x)) 'system::unquote-splicing))
         (assert (= 2 (length (first x))))
         (list 'append
               (second (first x))
               (expand-quasiquote (rest x))))
        (t
         (list 'cons
               (expand-quasiquote (first x))
               (expand-quasiquote (rest x))))))

(def-transform system::quasiquote (x)
  (expand-quasiquote x))

(defmacro system::quasiquote (x)
  (expand-quasiquote x))

(defun pass1-const (x return-value-p)
  (make-ir 'const return-value-p nil x))

(defun pass1-refvar (symbol return-value-p)
  (let ((binding (lookup symbol '(:variable :special))))
    (count-if-used binding)
    (if (and binding (eq (binding-type binding) :variable))
        (make-ir 'lref return-value-p nil binding)
        (make-ir 'gref return-value-p nil symbol))))

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

(defun parse-body (body look-docstring-p)
  (let ((declares '())
        (docstring nil))
    (do ((forms body (rest forms)))
        ((null forms)
         (values nil declares docstring))
      (let ((form (first forms)))
        (cond ((and (consp form)
                    (eq 'declare (first form)))
               (setf declares (append (rest form) declares)))
              ((and (stringp form) look-docstring-p (rest forms))
               (setq docstring form))
              (t
               (return (values forms declares docstring))))))))

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
               (setf (binding-type binding) :special)
               (push (make-variable-binding symbol t)
                     *lexenv*)))))))
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
  (assert (eq 'lambda (first form)))
  (when (atom (rest form))
    (compile-error "~S is not a valid lambda expression" form))
  (let ((parsed-lambda-list (parse-lambda-list (second form)))
        (body (cddr form)))
    (multiple-value-bind (body declares docstring)
        (parse-body body t)
      (declare (ignore docstring))
      (let* ((inner-lexenv (pass1-lambda-list parsed-lambda-list))
             (*lexenv* (extend-lexenv inner-lexenv *lexenv*))
             (*lexenv* (pass1-declares declares inner-lexenv *lexenv*)))
        (make-ir 'lambda
                 return-value-p
                 nil
                 parsed-lambda-list
                 (pass1-forms body t t))))))

(defun %macroexpand-1 (form)
  (cond ((symbolp form)
         (let ((binding (lookup form :symbol-macro)))
           (count-if-used binding)
           (if binding
               (values (binding-value binding) t)
               (let ((expansion (get-symbol-macro form)))
                 (if expansion
                     (values expansion t)
                     (values form nil))))))
        ((and (consp form) (symbolp (first form)))
         (let ((binding (lookup (first form) :macro)))
           (count-if-used binding)
           (if binding
               (values (apply (binding-value binding) (rest form)) t)
               (let ((fn (get-macro (first form))))
                 (if fn
                     (values (funcall fn (rest form)) t)
                     (values form nil))))))
        (t
         (values form nil))))

(defun pass1-call (form return-value-p multiple-values-p)
  (let ((fn (first form))
        (args (rest form)))
    (cond ((and (variable-symbol-p fn))
           (let ((binding (lookup fn :function)))
             (count-if-used binding)
             (cond (binding
                    (make-ir 'lcall
                             return-value-p
                             multiple-values-p
                             binding
                             (mapcar (lambda (arg)
                                       (pass1 arg t nil))
                                     args)))
                   ((transform-symbol-p fn)
                    (pass1 (apply (get-transform fn) args)
                           return-value-p
                           multiple-values-p))
                   (t
                    (pushnew fn *called-function-names*)
                    (make-ir 'call
                             return-value-p
                             multiple-values-p
                             fn
                             (mapcar (lambda (arg)
                                       (pass1 arg t nil))
                                     args))))))
          ((and (consp fn)
                (eq 'lambda (first fn)))
           (pass1 (list* 'funcall fn args)
                  return-value-p
                  multiple-values-p))
          ((and (consp fn)
                (eq 'ffi:ref (first fn)))
           (make-ir 'js-call
                    return-value-p
                    multiple-values-p
                    (ir-arg1 (pass1 fn return-value-p nil))
                    (mapcar (lambda (arg)
                              (pass1 arg t nil))
                            args)))
          (t
           (compile-error "Illegal function call: ~S" form)))))

(defun pass1 (form return-value-p multiple-values-p)
  (multiple-value-bind (form expanded-p)
      (%macroexpand-1 form)
    (cond (expanded-p
           (pass1 form return-value-p multiple-values-p))
          ((null form)
           (pass1-const nil return-value-p))
          ((keywordp form)
           (pass1-const form return-value-p))
          ((symbolp form)
           (pass1-refvar form return-value-p))
          ((atom form)
           (pass1-const form return-value-p))
          (t
           (let ((fn (gethash (first form) *pass1-form-table*)))
             (if fn
                 (apply fn return-value-p multiple-values-p (rest form))
                 (pass1-call form return-value-p multiple-values-p)))))))

(def-pass1-form quote ((x) return-value-p multiple-values-p)
  (pass1-const x return-value-p))

(def-pass1-form setq ((&rest args) return-value-p multiple-values-p)
  (if (not (evenp (length args)))
      (compile-error "Setq with odd number of args")
      (let ((forms '()))
        (do ((args args (rest (rest args))))
            ((null args))
          (unless (variable-symbol-p (first args))
            (compile-error "~S is not a variable" (first args)))
          (let* ((symbol (first args))
                 (binding (lookup symbol :variable))
                 (value (pass1 (second args) t nil)))
            (count-if-used binding)
            (push (make-ir (if binding 'lset 'gset)
                           (if (null (rest args))
                               return-value-p
                               nil)
                           nil
                           (or binding symbol)
                           value)
                  forms)))
        (if (null forms)
            (pass1-const nil return-value-p)
            (make-ir 'progn
                     return-value-p
                     nil
                     (nreverse forms))))))

(def-pass1-form if ((test then &optional else) return-value-p multiple-values-p)
  (make-ir 'if
           return-value-p
           multiple-values-p
           (pass1 test t nil)
           (pass1 then return-value-p multiple-values-p)
           (pass1 else return-value-p multiple-values-p)))

(def-pass1-form progn ((&rest forms) return-value-p multiple-values-p)
  (make-ir 'progn
           return-value-p
           multiple-values-p
           (pass1-forms forms return-value-p multiple-values-p)))

(def-pass1-form function ((thing) return-value-p multiple-values-p)
  (cond
    ((symbolp thing)
     (let ((binding (lookup thing :function)))
       (count-if-used binding)
       (if binding
           (make-ir 'lref return-value-p nil binding)
           (pass1 `(symbol-function ',thing)
                  return-value-p
                  nil))))
    ((and (consp thing) (eq (first thing) 'lambda))
     (pass1-lambda thing return-value-p))
    (t
     (compile-error "~S is not a legal function name" thing))))

(def-pass1-form let ((bindings &rest body) return-value-p multiple-values-p)
  (unless (listp bindings)
    (compile-error "Malformed LET bindings: ~S" bindings))
  (let ((bindings (mapcar (lambda (b)
                            (unless (and (consp b)
                                         (<= 1 (length b) 2))
                              (compile-error "Malformed LET binding: ~S" b))
                            (let ((var (first b)))
                              (unless (variable-symbol-p var)
                                (compile-error "~S is not a variable" var))
                              (list (make-variable-binding var)
                                    (pass1 (second b) t nil))))
                          bindings)))
    (multiple-value-bind (body declares)
        (parse-body body nil)
      (let* ((inner-lexenv (mapcar #'first bindings))
             (*lexenv* (extend-lexenv inner-lexenv *lexenv*))
             (*lexenv* (pass1-declares declares inner-lexenv *lexenv*)))
        (make-ir 'let
                 return-value-p
                 multiple-values-p
                 bindings
                 (pass1-forms body return-value-p multiple-values-p))))))

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
            (list (make-function-binding (first definition))
                  (let ((fn `(lambda ,@(rest definition))))
                    (if compile-lambda-p
                        (pass1-lambda fn t)
                        fn))))
          definitions))

(def-pass1-form flet ((definitions &rest body) return-value-p multiple-values-p)
  (check-flet-definitions definitions)
  (let ((bindings (parse-flet-definitions definitions t)))
    (multiple-value-bind (body declares)
        (parse-body body nil)
      (let* ((inner-lexenv (mapcar #'first bindings))
             (*lexenv* (extend-lexenv inner-lexenv *lexenv*))
             (*lexenv* (pass1-declares declares inner-lexenv *lexenv*)))
        (make-ir 'let
                 return-value-p
                 multiple-values-p
                 bindings
                 (pass1-forms body return-value-p multiple-values-p))))))

(def-pass1-form labels ((definitions &rest body) return-value-p multiple-values-p)
  (check-flet-definitions definitions)
  (let ((bindings (parse-flet-definitions definitions nil)))
    (multiple-value-bind (body declares)
        (parse-body body nil)
      (let* ((inner-lexenv (mapcar #'first bindings))
             (*lexenv* (extend-lexenv inner-lexenv *lexenv*))
             (*lexenv* (pass1-declares declares inner-lexenv *lexenv*)))
        (make-ir 'let
                 return-value-p
                 multiple-values-p
                 (mapcar (lambda (b)
                           (list (first b) (pass1 (second b) t nil)))
                         bindings)
                 (pass1-forms body return-value-p multiple-values-p))))))

(def-pass1-form macrolet ((definitions &rest body) return-value-p multiple-values-p)
  (check-flet-definitions definitions)
  (let ((*lexenv*
          (extend-lexenv (mapcar (lambda (definition)
                                   (make-macro-binding (first definition)
                                                       (eval `(lambda ,@(rest definition)))))
                                 definitions)
                         *lexenv*)))
    (apply #'pass1-progn return-value-p multiple-values-p body)))

(def-pass1-form symbol-macrolet ((definitions &rest body) return-value-p multiple-values-p)
  (check-flet-definitions definitions)
  (let ((*lexenv*
          (extend-lexenv (mapcar (lambda (definition)
                                   (make-symbol-macro-binding (first definition)
                                                              (second definition)))
                                 definitions)
                         *lexenv*)))
    (apply #'pass1-progn return-value-p multiple-values-p body)))

(def-pass1-form unwind-protect ((protected &rest cleanup) return-value-p multiple-values-p)
  (make-ir 'unwind-protect
           return-value-p
           multiple-values-p
           (pass1 protected return-value-p multiple-values-p)
           (apply #'pass1-progn nil nil cleanup)))

(def-pass1-form block ((name &rest forms) return-value-p multiple-values-p)
  (unless (symbolp name)
    (compile-error "The block name ~S is not a symbol." name))
  (let* ((binding (make-block-binding name))
         (*lexenv* (cons binding *lexenv*)))
    (make-ir 'block
             return-value-p
             multiple-values-p
             binding
             (pass1-forms forms return-value-p multiple-values-p))))

(def-pass1-form return-from ((name &optional value) return-value-p multiple-values-p)
  (unless (symbolp name)
    (compile-error "~S is not a symbol" name))
  (let ((binding (lookup name :block)))
    (count-if-used binding)
    (if binding
        (make-ir 'return-from nil nil binding (pass1 value t t))
        (compile-error "return for unknown block: ~S" name))))

(defvar *tagbody-level* 0)

(def-pass1-form tagbody ((&rest statements) return-value-p multiple-values-p)
  (let* ((tags (remove-if-not #'symbolp statements))
         (index 0)
         (*lexenv*
           (extend-lexenv (mapcar (lambda (tag)
                                    (incf index)
                                    (make-tag-binding tag
                                                      (make-tagbody-value :index index
                                                                          :level *tagbody-level*)))
                                  tags)
                          *lexenv*)))
    (let* ((part-statements '())
           (tag-statements-pairs '())
           (none '#:none)
           (last-tag none))
      (flet ((add-statements ()
               (unless part-statements
                 (setf part-statements (list (pass1-const nil nil))))
               (push (if (eq last-tag none)
                         (cons (make-tagbody-value :index 0 :level *tagbody-level*)
                               (make-ir 'progn nil nil (nreverse part-statements)))
                         (let ((binding (lookup last-tag :tag)))
                           (assert binding)
                           (count-if-used binding)
                           (cons (binding-value binding)
                                 (make-ir 'progn nil nil (nreverse part-statements)))))
                     tag-statements-pairs)
               (setf part-statements nil)))
        (let ((*tagbody-level* (1+ *tagbody-level*)))
          (do ((statements* statements (rest statements*)))
              ((null statements*)
               (add-statements))
            (cond ((symbolp (first statements*))
                   (add-statements)
                   (setf last-tag (first statements*)))
                  (t
                   (push (pass1 (first statements*) nil nil) part-statements)))))
        (make-ir 'tagbody
                 return-value-p
                 nil
                 *tagbody-level*
                 (nreverse tag-statements-pairs))))))

(def-pass1-form go ((tag) return-value-p multiple-values-p)
  (unless (symbolp tag)
    (compile-error "~S is not a symbol" tag))
  (let ((binding (lookup tag :tag)))
    (unless binding
      (compile-error "attempt to GO to nonexistent tag: ~A" tag))
    (count-if-used binding)
    (make-ir 'go nil nil *tagbody-level* (binding-value binding))))

(def-pass1-form catch ((tag &rest body) return-value-p multiple-values-p)
  (make-ir 'catch
           return-value-p
           multiple-values-p
           (pass1 tag t nil)
           (apply #'pass1-progn return-value-p multiple-values-p body)))

(def-pass1-form throw ((tag result) return-value-p multiple-values-p)
  (make-ir 'throw
           t
           t
           (pass1 tag t nil)
           (pass1 result t t)))

(def-pass1-form locally ((&rest body) return-value-p multiple-values-p)
  (multiple-value-bind (body declares)
      (parse-body body nil)
    (let ((*lexenv* (pass1-declares declares nil *lexenv*)))
      (apply #'pass1-progn return-value-p multiple-values-p body))))

(def-pass1-form declaim ((&rest specs) return-value-p multiple-values-p)
  (pre-process-declaration-specifier specs)
  (dolist (spec specs)
    (case (first spec)
      ((special)
       (dolist (symbol (rest spec))
         (setf (special-p symbol) t)))))
  (pass1-const nil return-value-p))

(def-pass1-form ffi:require ((name module-name) return-value-p multiple-values-p)
  (unless (or (symbolp name) (stringp name))
    (compile-error "~S is not a string designator" name))
  (unless (stringp module-name)
    (compile-error "~S is not a string" module-name))
  (push (cons name module-name) *require-modules*)
  (pass1-const nil return-value-p))

(defun pass1-ref-names (names)
  (let ((arguments '()))
    (dolist (name names)
      (unless (or (symbolp name) (stringp name))
        (compile-error "~S is not a string designator" name))
      (push (string name) arguments))
    (nreverse arguments)))

(def-pass1-form ffi:ref ((name &rest names) return-value-p multiple-values-p)
  (let ((arguments (pass1-ref-names (cons name names))))
    (make-ir 'ffi:ref return-value-p nil arguments)))

(def-pass1-form ffi:set ((&rest args) return-value-p multiple-values-p)
  (unless (<= 2 (length args))
    (compile-error "invalid number of arguments: ffi:set"))
  (let ((names (butlast args))
        (value (first (last args))))
    (let ((arguments (pass1-ref-names names)))
      (make-ir 'ffi:set return-value-p nil arguments (pass1 value t nil)))))

(def-pass1-form ffi:var ((&rest vars) return-value-p multiple-values-p)
  (make-ir 'ffi:var
           return-value-p
           multiple-values-p
           (mapcar (lambda (var)
                     (unless (or (symbolp var) (stringp var))
                       (compile-error "~S is not a string designator" var))
                     (string var))
                   vars)))

(def-pass1-form ffi:typeof ((x) return-value-p multiple-values-p)
  (make-ir 'ffi:typeof
           return-value-p
           nil
           (pass1 x t nil)))

(defun pass1-toplevel (form)
  (let ((*lexenv* '()))
    (pass1 form nil nil)))
