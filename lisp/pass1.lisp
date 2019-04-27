(in-package :compiler)

(defparameter *pass1-form-table* (make-hash-table))

(defvar *lexenv*)

(defun make-variable-binding (symbol &optional (special-p (special-p symbol)))
  (make-binding :name symbol
                :type (if special-p :special :variable)
                :value symbol))

(defun make-function-binding (symbol)
  (make-binding :name symbol
                :type :function
                :value symbol))

(defmacro def-pass1-form (name lambda-list &body body)
  (let ((fn-name (intern (format nil "PASS1-~A" name))))
    `(progn
       (defun ,fn-name ,lambda-list ,@body)
       (setf (gethash ',name *pass1-form-table*) ',fn-name))))

(defun get-macro (symbol)
  (get symbol 'macro))

(defun (setf get-macro) (form symbol)
  (setf (get symbol 'macro) form))

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
                                         (values (second arg1) (second arg) (third arg) (first arg1))))
                                      (t
                                       (check-variable (first arg))
                                       (values (first arg) (second arg) (third arg) (make-keyword (first arg)))))
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
  `(system::fset ',name (lambda ,lambda-list ,@body)))

(def-transform defmacro (name lambda-list &rest body)
  (setf (get-macro name) (eval `(lambda ,lambda-list ,@body)))
  `(system::add-global-macro ',name (lambda ,lambda-list ,@body)))

(def-transform lambda (args &rest body)
  `(function (lambda ,args ,@body)))

(def-transform defvar (x &optional (value nil value-p) doc)
  (declare (ignore doc))
  `(progn
     (declaim (special ,x))
     ,(when value-p
        `(if (boundp ',x) nil (setq ,x ,value)))
     ',x))

(defun expand-quasiquote (x)
  (cond ((atom x)
         (list 'quote x))
        ((eq 'system::unquote (first x))
         (assert (= 2 (length x)))
         (second x))
        ((and (consp (first x))
              (eq (first (first x)) 'system::unquote-splicing))
         (assert (= 2 (length x)))
         (list 'append (second (first x)) (rest x)))
        (t
         (list 'cons
               (expand-quasiquote (first x))
               (expand-quasiquote (rest x))))))

(def-transform system::quasiquote (x)
  (expand-quasiquote x))

(defun pass1-const (x)
  (make-ir 'const x))

(defun pass1-refvar (symbol)
  (let ((binding (lookup symbol :variable)))
    (if binding
        (make-ir 'lref binding)
        (make-ir 'gref symbol))))

(defun pass1-forms (forms)
  (if (null forms)
      (list (pass1-const nil))
      (mapcar #'pass1 forms)))

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
              ((and (stringp form) look-docstring-p)
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
               (push (make-binding :name symbol :value symbol :type :special)
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
                       (pass1 (second opt))))
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

(defun pass1-lambda (form)
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
                 parsed-lambda-list
                 (pass1-forms body))))))

(defun %macroexpand-1 (form)
  (cond ((symbolp form)
         (values form nil))
        ((and (consp form) (symbolp (first form)))
         (let ((binding (lookup (first form) :macro)))
           (if binding
               (values (apply (binding-value binding) (rest form)) t)
               (let ((fn (get-macro (first form))))
                 (if fn
                     (values (apply fn (rest form)) t)
                     (values form nil))))))
        (t
         (values form nil))))

(defun pass1-call (form)
  (let ((fn (first form))
        (args (rest form)))
    (cond ((and (variable-symbol-p fn))
           (let ((binding (lookup fn :function)))
             (cond (binding
                    (make-ir 'lcall binding (mapcar #'pass1 args)))
                   ((transform-symbol-p fn)
                    (pass1 (apply (get-transform fn) args)))
                   (t
                    (make-ir 'call fn (mapcar #'pass1 args))))))
          ((consp fn)
           (unless (eq 'lambda (first fn))
             (compile-error "Illegal function call: ~S" form))
           (pass1 (list* 'funcall fn args)))
          (t
           (compile-error "Illegal function call: ~S" form)))))

(defun pass1 (form)
  (multiple-value-bind (form expanded-p)
      (%macroexpand-1 form)
    (cond (expanded-p
           (pass1 form))
          ((null form)
           (pass1-const nil))
          ((keywordp form)
           (pass1-const form))
          ((symbolp form)
           (pass1-refvar form))
          ((atom form)
           (pass1-const form))
          (t
           (let ((fn (gethash (first form) *pass1-form-table*)))
             (if fn
                 (apply fn (rest form))
                 (pass1-call form)))))))

(def-pass1-form quote (x)
  (pass1-const x))

(def-pass1-form setq (&rest args)
  (if (not (evenp (length args)))
      (compile-error "Setq with odd number of args")
      (let ((forms '()))
        (do ((args args (rest (rest args))))
            ((null args))
          (unless (variable-symbol-p (first args))
            (compile-error "~S is not a variable" (first args)))
          (let* ((symbol (first args))
                 (binding (lookup symbol :variable))
                 (value (pass1 (second args))))
            (push (if binding
                      (make-ir 'lset binding value)
                      (make-ir 'gset symbol value))
                  forms)))
        (make-ir 'progn
                 (if (null forms)
                     (list (pass1-const nil))
                     (nreverse forms))))))

(def-pass1-form if (test then &optional else)
  (make-ir 'if
           (pass1 test)
           (pass1 then)
           (pass1 else)))

(def-pass1-form progn (&rest forms)
  (make-ir 'progn (pass1-forms forms)))

(def-pass1-form function (thing)
  (cond
    ((symbolp thing)
     (let ((binding (lookup thing :function)))
       (if binding
           (make-ir 'lref binding)
           (pass1 `(symbol-function ',thing)))))
    ((and (consp thing) (eq (car thing) 'lambda))
     (pass1-lambda thing))
    (t
     (compile-error "~S is not a legal function name" thing))))

(def-pass1-form let (bindings &rest body)
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
                                    (pass1 (second b)))))
                          bindings)))
    (multiple-value-bind (body declares)
        (parse-body body nil)
      (let* ((inner-lexenv (mapcar #'first bindings))
             (*lexenv* (extend-lexenv inner-lexenv *lexenv*))
             (*lexenv* (pass1-declares declares inner-lexenv *lexenv*)))
        (make-ir 'let
                 bindings
                 (pass1-forms body))))))

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
                        (pass1-lambda fn)
                        fn))))
          definitions))

(def-pass1-form flet (definitions &rest body)
  (check-flet-definitions definitions)
  (let ((bindings (parse-flet-definitions definitions t)))
    (multiple-value-bind (body declares)
        (parse-body body nil)
      (let* ((inner-lexenv (mapcar #'first bindings))
             (*lexenv* (extend-lexenv inner-lexenv *lexenv*))
             (*lexenv* (pass1-declares declares inner-lexenv *lexenv*)))
        (make-ir 'let
                 bindings
                 (pass1-forms body))))))

(def-pass1-form labels (definitions &rest body)
  (check-flet-definitions definitions)
  (let ((bindings (parse-flet-definitions definitions nil)))
    (multiple-value-bind (body declares)
        (parse-body body nil)
      (let* ((inner-lexenv (mapcar #'first bindings))
             (*lexenv* (extend-lexenv inner-lexenv *lexenv*))
             (*lexenv* (pass1-declares declares inner-lexenv *lexenv*)))
        (make-ir 'let
                 (mapcar (lambda (b)
                           (list (first b) (pass1 (second b))))
                         bindings)
                 (pass1-forms body))))))

(def-pass1-form macrolet (definitions &rest body)
  (check-flet-definitions definitions)
  (let ((*lexenv*
          (extend-lexenv (mapcar (lambda (definition)
                                   (make-binding :name (first definition)
                                                 :type :macro
                                                 :value (eval `(lambda ,@(rest definition)))))
                                 definitions)
                         *lexenv*)))
    (apply #'pass1-progn body)))

(def-pass1-form block (name &rest forms)
  (unless (symbolp name)
    (compile-error "The block name ~S is not a symbol." name))
  (let* ((binding (make-binding :name name :type :block :value name))
         (*lexenv* (cons binding *lexenv*)))
    (make-ir 'block binding (pass1-forms forms))))

(def-pass1-form return-from (name &optional value)
  (unless (symbolp name)
    (compile-error "~S is not a symbol" name))
  (let ((binding (lookup name :block)))
    (if binding
        (make-ir 'return-from binding (pass1 value))
        (compile-error "return for unknown block: ~S" name))))

(defvar *tagbody-level* 0)

(def-pass1-form tagbody (&rest statements)
  (let* ((tags (remove-if-not #'symbolp statements))
         (index 0)
         (*lexenv*
           (extend-lexenv (mapcar (lambda (tag)
                                    (incf index)
                                    (make-binding :name tag
                                                  :type :tag
                                                  :value (make-tagbody-value :index index :level *tagbody-level*)))
                                  tags)
                          *lexenv*)))
    (let* ((part-statements '())
           (tag-statements-pairs '())
           (none '#:none)
           (last-tag none))
      (flet ((add-statements ()
               (unless part-statements
                 (setf part-statements (list (pass1-const nil))))
               (push (if (eq last-tag none)
                         (cons (make-tagbody-value :index 0 :level *tagbody-level*)
                               (make-ir 'progn (nreverse part-statements)))
                         (let ((binding (lookup last-tag :tag)))
                           (assert binding)
                           (cons (binding-value binding)
                                 (make-ir 'progn (nreverse part-statements)))))
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
                   (push (pass1 (first statements*)) part-statements)))))
        (make-ir 'tagbody
                 *tagbody-level*
                 (nreverse tag-statements-pairs))))))

(def-pass1-form go (tag)
  (unless (symbolp tag)
    (compile-error "~S is not a symbol" tag))
  (let ((binding (lookup tag :tag)))
    (unless binding
      (compile-error "attempt to GO to nonexistent tag: ~A" tag))
    (make-ir 'go *tagbody-level* (binding-value binding))))

(def-pass1-form declaim (&rest specs)
  (pre-process-declaration-specifier specs)
  (dolist (spec specs)
    (case (first spec)
      ((special)
       (dolist (symbol (rest spec))
         (setf (special-p symbol) t)))))
  (pass1-const nil))

(defun pass1-toplevel (form)
  (let ((*lexenv* '()))
    (pass1 form)))
