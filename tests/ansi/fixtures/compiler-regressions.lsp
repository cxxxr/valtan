(in-package :cl-test)

(deftest compiler-unwind-preserves-nonlocal-block-values
  (multiple-value-list
    (block done
      (unwind-protect (return-from done (values 1 2)) (values 9 10))
      :unreachable))
  (1 2))

(deftest compiler-unwind-preserves-thrown-zero-and-multiple-values
  (values (multiple-value-list
            (catch 'done (unwind-protect (throw 'done (values 1 2)) (values 9 10))))
          (multiple-value-list
            (catch 'done (unwind-protect (throw 'done (values)) (values 9 10))))
          (multiple-value-list (values :after)))
  (1 2) nil (:after))

(deftest compiler-ftype-declares-local-functions
  (flet ((increment (x) (1+ x)))
    (declare (ftype (function (integer) integer) increment))
    (labels ((twice (x) (increment (increment x))))
      (declare (ftype (function (integer) integer) twice))
      (twice 3)))
  5)

(defvar *progv-regression-value* :outside)
(deftest compiler-progv-preserves-values-and-restores-on-exit
  (values (multiple-value-list
            (progv '(*progv-regression-value*) '(:inside)
              (values *progv-regression-value* :second)))
          *progv-regression-value*
          (catch 'done
            (progv '(*progv-regression-value*) '(:unwinding)
              (throw 'done *progv-regression-value*)))
          *progv-regression-value*)
  (:inside :second) :outside :unwinding :outside)

(deftest compiler-progv-missing-values-and-original-unbound-state
  (let ((fresh (gensym)))
    (values (progv '(*progv-regression-value*) nil (boundp '*progv-regression-value*))
            *progv-regression-value*
            (progv (list fresh) '(42) (symbol-value fresh))
            (boundp fresh)
            (multiple-value-list (progv nil '(1 2) (values)))))
  nil :outside 42 nil nil)

(deftest compiler-progv-nesting-and-lexical-independence
  (let ((progv-lexical :lexical))
    (values (progv '(progv-lexical) '(:dynamic)
              (list progv-lexical (symbol-value 'progv-lexical)))
            (progv '(*progv-regression-value*) '(:outer)
              (list (progv '(*progv-regression-value*) '(:inner) *progv-regression-value*)
                    *progv-regression-value*))
            *progv-regression-value*))
  (:lexical :dynamic) (:inner :outer) :outside)

(deftest compiler-progv-evaluation-order-and-error-unwind
  (let ((order nil))
    (values (progv (progn (push :symbols order) '(*progv-regression-value*))
                   (progn (push :values order) '(:inside))
              (push :body order) *progv-regression-value*)
            (reverse order)
            (handler-case
                (progv '(*progv-regression-value*) '(:error) (error "Unwind PROGV"))
              (error () *progv-regression-value*))))
  :inside (:symbols :values :body) :outside)

(defmacro whole-form (&whole whole &rest arguments)
  (declare (ignore arguments))
  (list 'quote whole))
(deftest macro-whole-form-identity
  (let ((form (list 'whole-form 1 2)))
    (values (eq (second (macroexpand-1 form)) form)
            (eq (second (funcall (macro-function 'whole-form) form nil)) form)))
  t t)

(deftest local-macro-whole-form
  (macrolet ((local (&whole whole x) (declare (ignore x)) (list 'quote whole)))
    (local 10))
  (local 10))

(defun (setf named-place) (value cell)
  (return-from named-place (setf (car cell) value)))
(deftest setf-function-implicit-block
  (let ((cell (list 1)))
    (values (funcall #'(setf named-place) 8 cell) (car cell)))
  8 8)

(define-setf-expander first-alias (cell &environment env)
  (return-from first-alias (get-setf-expansion (list 'car cell) env)))
(deftest setf-expander-implicit-block
  (let ((cell (list 1)))
    (values (setf (first-alias cell) 9) (car cell)))
  9 9)
(deftest local-macro-destructuring-and-block
  (macrolet ((sum ((a b) &optional (c 3))
               (return-from sum `(+ ,a ,b ,c))))
    (sum (1 2) 4))
  7)
(deftest constant-forms
  (values (constantp 17) (constantp '(quote anything)) (constantp :anything)
          (constantp 'most-positive-fixnum) (constantp 'unbound-nonconstant))
  t t t t nil)
(deftest compiled-function-predicate
  (values (compiled-function-p (compile nil '(lambda () 3)))
          (compiled-function-p 3))
  t nil)
(deftest keyword-permission
  (make-list 3 :unknown 9 :allow-other-keys t :initial-element :x)
  (:x :x :x))
(deftest keyword-leftmost-permission
  (funcall (compile nil '(lambda (&key x) x)) :x 7 :bad 1
           :allow-other-keys t :allow-other-keys nil)
  7)
(deftest keyword-leftmost-rejection
  (handler-case
      (funcall (compile nil '(lambda (&key x) x)) :x 7 :bad 1
               :allow-other-keys nil :allow-other-keys t)
    (program-error () :rejected))
  :rejected)
(deftest empty-keyword-list
  (values (funcall (compile nil '(lambda (&key) :ok)) :allow-other-keys t :bad 1)
          (handler-case (funcall (compile nil '(lambda (&key) :ok)) :bad 1)
            (program-error () :rejected)))
  :ok :rejected)
(deftest leftmost-keyword-value
  (funcall (compile nil '(lambda (&key (x 4)) x)) :x 9 :x 8)
  9)
(deftest odd-keyword-list
  (handler-case (funcall (compile nil '(lambda (&key &allow-other-keys) :ok)) :bad)
    (program-error () :rejected))
  :rejected)
(deftest package-literal (eq (eval (list 'quote (find-package :cl))) (find-package :cl)) t)
(deftest iterative-make-list (length (make-list 30000 :initial-element :x)) 30000)

(deftest macro-definition-enclosing-symbol-macro
  (symbol-macrolet ((table '(:answer 42)))
    (macrolet ((answer () (getf table :answer)))
      (answer)))
  42)
(deftest macro-definition-enclosing-macro
  (macrolet ((outer (x) `(list 'quote (+ ,x 2))))
    (macrolet ((inner (x) (outer x)))
      (inner 40)))
  42)
(deftest macro-definition-sibling-scope
  (macrolet ((value () 10))
    (macrolet ((value () 20) (other () (value)))
      (list (value) (other))))
  (20 10))
(deftest lexical-variable-shadows-symbol-macro
  (let ((cell (list 1)))
    (symbol-macrolet ((x (car cell)))
      (values (let ((x 7)) (setq x 9) x) x (car cell))))
  9 1 1)
(deftest macro-parameter-shadows-symbol-macro
  (symbol-macrolet ((x 99))
    (macrolet ((identity-form (x) x))
      (identity-form 7)))
  7)
(deftest local-function-shadows-local-macro
  (macrolet ((value () 1))
    (flet ((value () 2)) (value)))
  2)
(define-symbol-macro global-empty-expansion nil)
(deftest global-nil-symbol-macro
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1 'global-empty-expansion)
    (values global-empty-expansion expansion expanded-p))
  nil nil t)
(deftest lexical-variable-shadows-global-symbol-macro
  (let ((global-empty-expansion 8))
    (setq global-empty-expansion 9)
    global-empty-expansion)
  9)
(deftest eval-does-not-capture-macro-definition-environment
  (symbol-macrolet ((runtime-only-name :captured))
    (macrolet ((check () (list 'quote (eval '(boundp 'runtime-only-name)))))
      (check)))
  nil)

(deftest call-arguments-capture-values-before-later-effects
  (let ((x 0))
    (values (setq x 1) x (setq x 2) x
            (list x (setq x 3) x)))
  1 1 2 2 (2 3 3))
(deftest funcall-function-form-precedes-arguments
  (let ((order nil))
    (values (funcall (progn (push :function order) #'list)
                     (progn (push :first order) 1)
                     (progn (push :second order) 2))
            (reverse order)))
  (1 2) (:function :first :second))
(deftest local-macro-declarations
  (values (macrolet ((nil () 7)) (declare (optimize)) (nil))
          (symbol-macrolet () (declare (optimize))))
  7 nil)
(deftest symbol-macro-cannot-be-special-or-constant
  (values (handler-case (eval '(symbol-macrolet ((x 1)) (declare (special x)) x))
            (program-error () :rejected))
          (handler-case (eval '(symbol-macrolet ((nil 1)) nil))
            (program-error () :rejected)))
  :rejected :rejected)

(defvar *return-binding* :outside)
(deftest let-returns-value-before-restoring-special-binding
  (values (let ((*return-binding* :inside)) *return-binding*) *return-binding*)
  :inside :outside)
(deftest unwind-protect-captures-value-before-cleanup
  (let ((x 1))
    (values (unwind-protect x (setq x 2)) x))
  1 2)
(deftest unwind-protect-preserves-all-values
  (let ((x 1))
    (values (multiple-value-list (unwind-protect (values x 3) (setq x 2) (values 9)))
            x))
  (1 3) 2)
(deftest let-preserves-zero-and-multiple-values
  (values (multiple-value-list (let ((*return-binding* :inside)) (values)))
          (multiple-value-list (let ((*return-binding* :inside)) (values *return-binding* 4)))
          *return-binding*)
  nil (:inside 4) :outside)

(deftest optional-default-is-lazy
  (let ((count 0))
    (let ((f (lambda (&optional (x (incf count) supplied)) (list x supplied))))
      (values (funcall f 10) count (funcall f) count)))
  (10 t) 0 (1 nil) 1)
(deftest keyword-default-is-lazy
  (let ((count 0))
    (let ((f (lambda (&key (x (incf count) supplied)) (list x supplied))))
      (values (funcall f :x nil) count (funcall f) count)))
  (nil t) 0 (1 nil) 1)
(deftest keyword-default-sees-rest-and-earlier-keywords
  (funcall (lambda (&rest rest &key (x (length rest)) (y (+ x 1)) &allow-other-keys)
             (values x y rest))
           :ignored 9)
  2 3 (:ignored 9))
(deftest special-parameter-restored-after-default-error
  (let ((*return-binding* :outside))
    (handler-case
        (funcall (lambda (*return-binding* &optional (x (error "default failed")))
                   (declare (ignore x)))
                 :inside)
      (error () nil))
    *return-binding*)
  :outside)
(deftest special-parameter-restored-after-keyword-error
  (let ((*return-binding* :outside))
    (handler-case
        (funcall (lambda (*return-binding* &key x) (declare (ignore x))) :inside :invalid 9)
      (program-error () nil))
    *return-binding*)
  :outside)
(deftest default-changes-outer-special-before-binding
  (let ((*return-binding* :outside))
    (values (funcall (lambda (&optional (*return-binding*
                                         (progn (setq *return-binding* :changed) :inside)))
                       *return-binding*))
            *return-binding*))
  :inside :changed)
(defvar *other-binding* :other)
(deftest partial-parameter-binding-unwinds-only-bound-parameters
  (let ((*return-binding* :outside) (*other-binding* :other))
    (handler-case
        (funcall (lambda (*return-binding* &optional (*other-binding* (error "unbound parameter"))))
                 :inside)
      (error () nil))
    (values *return-binding* *other-binding*))
  :outside :other)

(deftest locally-special-parameter-visible-in-later-default
  (let ((x :outside))
    (declare (special x))
    (values (funcall (lambda (x &optional (y x)) (declare (special x)) y) :inside)
            x))
  :inside :outside)

(deftest auxiliary-parameters-initialize-in-order
  (let ((offset 10))
    (funcall (lambda (x &optional (y (+ x 1)) &key (z (+ y 1)) &allow-other-keys
                       &aux (a (+ z offset)) (b (+ a 1)) uninitialized)
               (values a b uninitialized))
             1 2 :ignored 99))
  13 14 nil)
(deftest auxiliary-bindings-shadow-parameters-and-earlier-auxiliaries
  (funcall (lambda (x &aux (original (lambda () x)) (x (+ x 1)) (x (* x 2)))
             (values x (funcall original))) 3)
  8 3)
(deftest special-auxiliary-shadowing-restores-outer-binding
  (let ((x :outside))
    (declare (special x))
    (values (funcall (lambda (x &aux (x (1+ x))) (declare (special x)) x) 3) x))
  4 :outside)
(deftest special-auxiliary-shadowing-unwinds-on-error
  (let ((x :outside))
    (declare (special x))
    (handler-case
        (funcall (lambda (x &aux (x (1+ x)) (fail (error "auxiliary failed")))
                   (declare (special x) (ignore fail)) x) 3)
      (error () nil))
    x)
  :outside)
(deftest repeated-required-parameters-remain-invalid
  (handler-case (progn (eval '(function (lambda (x x) x))) :accepted)
    (error () :rejected))
  :rejected)
(deftest special-auxiliary-parameter-unwinds-on-initialization-error
  (let ((*return-binding* :outside))
    (handler-case
        (funcall (lambda (&aux (*return-binding* :inside) (fail (error "auxiliary failed")))
                   (declare (ignore fail))))
      (error () nil))
    *return-binding*)
  :outside)
(deftest locally-special-auxiliary-parameter-is-visible-to-next-initializer
  (let ((x :outside))
    (declare (special x))
    (values (funcall (lambda (&aux (x :inside) (y x)) (declare (special x)) y)) x))
  :inside :outside)

(deftest local-functions-have-implicit-named-blocks
  (values (flet ((local (x) "Local documentation."
                   (declare (integer x))
                   (when (plusp x) (return-from local :positive))
                   :nonpositive))
            (local 2))
          (labels ((countdown (x)
                     (when (zerop x) (return-from countdown :done))
                     (countdown (1- x))))
            (countdown 3)))
  :positive :done)

(deftest local-setf-function-has-lexical-scope
  (let ((cell (list 1)))
    (flet (((setf local-slot) (value object) (setf (car object) value)))
      (setf (local-slot cell) 8)
      (values (car cell) (funcall #'(setf local-slot) 9 cell) (car cell))))
  8 9 9)

(deftest local-setf-function-implicit-block
  (labels (((setf local-slot) (value object)
             (declare (ignore object))
             (return-from local-slot (1+ value))))
    (funcall #'(setf local-slot) 4 nil))
  5)

(deftest named-loop-return-and-user-return-have-distinct-blocks
  (values (loop named chosen return :chosen)
          (block nil (loop named chosen do (return :outer)) :wrong)
          (loop named chosen for x below 3 always (< x 4))
          (loop named chosen for x below 5 thereis (and (= x 2) x)))
  :chosen :outer t 2)

(deftest loop-into-accumulation-with-predicate
  (values (loop for x in '(1 2 3) collect x into items always (plusp x))
          (loop for x in '(1 2 3) always (plusp x) sum x into total))
  t t)

(deftest funcall-invalid-designator-signals-type-error
  (handler-case (funcall 17) (type-error () :caught))
  :caught)

(deftest throw-can-cross-an-active-condition-handler
  (handler-bind ((error (lambda (condition) (declare (ignore condition))
                          (throw 'handler-result (values :handled 2)))))
    (catch 'handler-result (error "Exercise the outer handler")))
  :handled 2)

(deftest throw-unwinds-and-selects-innermost-matching-catch
  (let ((cleanups nil))
    (values (catch 'outer
              (unwind-protect
                   (catch 'inner
                     (unwind-protect (throw 'outer :done)
                       (push :inner cleanups)))
                (push :outer cleanups)))
            cleanups
            (catch 'same (list (catch 'same (throw 'same 8))))))
  :done (:outer :inner) (8))

(defvar *compiler-plist-special* 7)
(defconstant compiler-plist-constant 11)
(deftest compiler-flags-survive-user-plist-replacement
  (let ((old-special (symbol-plist '*compiler-plist-special*))
        (old-constant (symbol-plist 'compiler-plist-constant)))
    (unwind-protect
         (progn
           (setf (symbol-plist '*compiler-plist-special*) nil
                 (symbol-plist 'compiler-plist-constant) nil)
           (values (let ((*compiler-plist-special* 19)) (eval '*compiler-plist-special*))
                   (constantp 'compiler-plist-constant)
                   (get '*compiler-plist-special* 'special)))
      (setf (symbol-plist '*compiler-plist-special*) old-special
            (symbol-plist 'compiler-plist-constant) old-constant)))
  19 t nil)

(deftest assigned-macro-functions-participate-in-function-binding
  (let ((name (gensym)))
    (setf (macro-function name)
          (lambda (form environment) (declare (ignore environment))
            (list 'quote (second form))))
    (setf (symbol-plist name) nil)
    (let ((bound (not (null (fboundp name))))
          (expansion (macroexpand (list name :expanded))))
      (fmakunbound name)
      (values bound expansion (macro-function name) (fboundp name))))
  t (quote :expanded) nil nil)

(deftest handler-bind-accepts-compound-condition-types
  (block done
    (handler-bind (((or warning error) (lambda (c) (declare (ignore c))
                                         (return-from done :caught))))
      (signal 'simple-error))
    :missed)
  :caught)

(deftest local-functions-allow-nil-and-keyword-names
  (values (flet ((nil () 7) (:answer () 9)) (+ (nil) (:answer)))
          (labels ((nil (n) (if (zerop n) :done (nil (1- n))))) (nil 3)))
  16 :done)

(deftest nested-defun-preserves-declarations-and-returns-name
  (let ((x 1))
    (declare (special x))
    (let ((x 2))
      (let ((name (defun nested-definition (&optional (y x))
                    (declare (special x)) (values y x))))
        (values name (multiple-value-list (funcall name))))))
  nested-definition (2 1))

(deftest sequential-bindings-shadow-earlier-names
  (values (let* ((x 1) (x (1+ x))) x)
          (let ((x 1))
            (declare (special x))
            (let* ((x 2) (y x)) (declare (special x)) (list x y)))
          (let ((x 1))
            (let ((x 2)) (declare (special x)) (setq x 3))
            x))
  2 (2 2) 1)

(deftest compiler-macros-have-public-expansion-functions
  (values (not (null (macro-function 'defun))) (not (null (fboundp 'defun)))
          (handler-case (funcall (macro-function 'defun) '(defun unused ()))
            (program-error () :wrong-arity))
          (length (multiple-value-list (function-lambda-expression #'cons)))
          (progn (fdefinition 'cond) (fdefinition 'setq) :present))
  t t :wrong-arity 3 :present)

(deftest destructuring-binds-whole-rest-and-key-patterns
  (values (destructuring-bind (&whole (a . b) c . d) '(1 . 2) (list a b c d))
          (destructuring-bind (a &rest (b c)) '(1 2 3) (list a b c))
          (destructuring-bind (&optional ((a b) '(1 2)) &key ((:pair (c d)) '(3 4)))
              nil (list a b c d))
          (destructuring-bind (&key) '(:allow-other-keys t :extra 1) :allowed))
  (1 2 1 2) (1 2 3) (1 2 3 4) :allowed)

(deftest macro-whole-pattern-contains-the-macro-name
  (macrolet ((capture (&whole (name a b) x y) `(quote (,name ,a ,b ,x ,y))))
    (capture 1 2))
  (capture 1 2 1 2))

(deftest matching-empty-case-clauses-return-nil
  (values (case :key (:key)) (ecase :key (:key))
          (let ((x :key)) (ccase x (:key)))
          (typecase 1 (integer)) (etypecase 1 (integer))
          (let ((x 1)) (ctypecase x (integer))))
  nil nil nil nil nil nil)

(deftest nonlocal-exits-distinguish-lexical-blocks-with-the-same-name
  (block done
    (flet ((escape () (return-from done :outer)))
      (block done (escape)))
    :wrong)
  :outer)

(deftest labels-body-declarations-do-not-affect-local-definitions
  (let ((x :dynamic))
    (declare (special x))
    (let ((x :lexical))
      (labels ((local () x)) (declare (special x)) (local))))
  :lexical)

(deftest funcall-rejects-special-operators-and-macro-designators
  (values (handler-case (funcall 'quote 1) (undefined-function () :rejected))
          (handler-case (funcall 'defconstant '(defconstant unused 1))
            (undefined-function () :rejected)))
  :rejected :rejected)

(deftest locally-defined-macro-retains-lexical-environment
  (let ((name (gensym)) (value 4))
    (eval `(defmacro ,name () :placeholder))
    (let ((expander (lambda (form environment)
                      (declare (ignore form environment)) value)))
      (setf (macro-function name) expander)
      (setq value 9)
      (eval (list name))))
  9)

(deftest nested-defmacro-captures-default-initializers
  (let ((default :captured))
    (defmacro local-default-fixture (&optional (arg default)) arg)
    (values (macroexpand-1 '(local-default-fixture))))
  :captured)

(deftest closures-capture-fresh-let-bindings-in-loops
  (let ((functions
          (loop for item in '(1 2 3)
                collect (let ((saved item)) (lambda () saved)))))
    (mapcar #'funcall functions))
  (1 2 3))

(deftest closures-share-one-binding-but-not-other-iterations
  (let ((pairs (loop for item in '(1 2)
                    collect (let ((value item))
                              (list (lambda () (incf value)) (lambda () value))))))
    (funcall (caar pairs))
    (mapcar (lambda (pair) (funcall (second pair))) pairs))
  (2 2))

(defvar *load-time-value-counter* 0)

(deftest load-time-value-initializes-once-before-calling-compiled-function
  (let ((*load-time-value-counter* 0))
    (let ((fn (compile nil '(lambda ()
                             (load-time-value (list (incf *load-time-value-counter*)))))))
      (values *load-time-value-counter*
              (eq (funcall fn) (funcall fn))
              (funcall fn)
              *load-time-value-counter*)))
  1 t (1) 1)

(deftest load-time-value-nested-and-single-valued
  (multiple-value-list
   (funcall (compile nil '(lambda ()
                          (load-time-value (values (load-time-value (+ 20 22) t) :extra))))))
  (42))

(deftest load-time-value-does-not-capture-lexical-environment
  (progn
    (setf (symbol-value 'load-time-value-global) :global)
    (funcall (compile nil '(lambda ()
                           (let ((load-time-value-global :local))
                             (load-time-value load-time-value-global))))))
  :global)

(deftest eval-top-level-definitions-in-order
  (eval '(progn
           (defparameter *eval-constant-source* 41)
           (defconstant +eval-constant-result+ (1+ *eval-constant-source*))
           (values +eval-constant-result+ *eval-constant-source*)))
  42 41)

(deftest eval-when-execute-only-in-eval
  (let ((name (gensym)))
    (eval `(progn
             (eval-when (:compile-toplevel :load-toplevel) (set ',name :wrong))
             (eval-when (:execute) (set ',name :right))))
    (symbol-value name))
  :right)

(defvar *expansion-order-value* :initial)
(defmacro expansion-order-value () `',*expansion-order-value*)
(deftest eval-macrolet-processes-expansions-and-definitions-in-order
  (let ((*expansion-order-value* :initial))
    (eval '(macrolet ((define-value (value)
                       `(progn (setq *expansion-order-value* ,value)
                               (expansion-order-value))))
             (define-value 10)
             (define-value 20))))
  20)

(deftest eval-nested-macrolet-expander-retains-outer-macro-environment
  (eval '(macrolet ((outer () :expanded))
           (macrolet ((inner () (outer)))
             (values (inner) (outer)))))
  :expanded :expanded)

(deftest eval-symbol-macrolet-preserves-order-and-multiple-values
  (let ((*expansion-order-value* :initial))
    (eval '(symbol-macrolet ((cell *expansion-order-value*))
             (setq cell :changed)
             (values cell (expansion-order-value)))))
  :changed :changed)

(deftest eval-locally-preserves-order-and-execute-only-semantics
  (let ((*expansion-order-value* :initial))
    (eval '(locally (declare (special *expansion-order-value*))
             (eval-when (:compile-toplevel) (setq *expansion-order-value* :wrong))
             (eval-when (:execute) (setq *expansion-order-value* :executed))
             (expansion-order-value))))
  :executed)

(deftest eval-called-inside-local-macro-environment-starts-with-null-environment
  (handler-case
      (eval '(macrolet ((fixture-local-only () :wrong))
               (eval '(fixture-local-only))))
    (undefined-function () :null-environment))
  :null-environment)

(deftest multiple-value-list-of-closure
  (let ((captured 42))
    (funcall (first (multiple-value-list (lambda () captured)))))
  42)

(deftest psetf-preserves-new-and-previous-functions
  (let ((a (lambda () 21)) (b nil))
    (psetf a (lambda () 42) b a)
    (values (funcall a) (funcall b)))
  42 21)

(deftest multiple-value-call-mixes-zero-closure-and-many-values
  (let ((args (multiple-value-call #'list (values) (lambda () :closure) (values :a :b))))
    (values (length args) (funcall (first args)) (rest args)))
  3 :closure (:a :b))

(deftest multiple-value-call-assignment-is-single-valued
  (let ((x nil))
    (values (multiple-value-list (setq x (values 10 20))) x))
  (10) 10)

(deftest multiple-value-call-preserves-the-values
  (multiple-value-list (the (values integer integer) (values 10 20)))
  (10 20))
(define-condition compiler-fixture-style-warning (style-warning simple-condition) ())
(define-condition compiler-fixture-expansion-error (error) ())
(defmacro compiler-fixture-warn (style-p)
  (warn (if style-p 'compiler-fixture-style-warning 'simple-warning)
        :format-control "Compiler fixture warning" :format-arguments nil)
  :compiled)
(defmacro compiler-fixture-fail ()
  (error 'compiler-fixture-expansion-error))

(deftest compile-warning-result-flags
  (handler-bind ((warning #'muffle-warning))
    (loop for style-p in '(t nil)
          collect (multiple-value-bind (function warnings failure)
                      (compile nil `(lambda () (compiler-fixture-warn ,style-p)))
                    (list (funcall function) (not (null warnings)) (not (null failure))))))
  ((:compiled t nil) (:compiled t t)))

(deftest compile-error-recovery-retains-diagnostic
  (let ((diagnostics (make-string-output-stream)))
    (multiple-value-bind (function warnings failure)
        (let ((*error-output* diagnostics))
          (compile nil '(lambda () (compiler-fixture-fail))))
      (values (functionp function) warnings failure
              (not (null (search "Compilation failed" (get-output-stream-string diagnostics))))
              (handler-case (funcall function)
                (compiler-fixture-expansion-error () :original-condition)))))
  t t t t :original-condition)

(deftest compile-named-error-recovery-installs-failing-function
  (let ((name (gensym)) (*error-output* (make-broadcast-stream)))
    (multiple-value-bind (result warnings failure)
        (compile name '(lambda (x) (compiler-fixture-fail)))
      (values (eq result name) warnings failure
              (handler-case (funcall name 42)
                (compiler-fixture-expansion-error () :failed)))))
  t t t :failed)

(deftest compile-runtime-error-is-not-compilation-failure
  (multiple-value-bind (function warnings failure)
      (compile nil '(lambda () (error 'compiler-fixture-expansion-error)))
    (values warnings failure
            (handler-case (funcall function)
              (compiler-fixture-expansion-error () :runtime))))
  nil nil :runtime)

(deftest compile-invalid-arguments-still-signal
  (values (handler-case (compile 42 '(lambda () 1)) (type-error () :name))
          (handler-case (compile nil 42) (type-error () :definition)))
  :name :definition)

(defvar *partial-binding-first* :outside-first)
(defvar *partial-binding-second* :outside-second)
(deftest letstar-partial-special-bindings-unwind-after-initializer-exit
  (values
   (loop repeat 3 collect
     (catch 'stop
       (let* ((*partial-binding-first* :inside-first)
              (*partial-binding-second*
                (throw 'stop *partial-binding-first*)))
         :unreachable)))
   *partial-binding-first* *partial-binding-second*)
  (:inside-first :inside-first :inside-first) :outside-first :outside-second)

(deftest letstar-partial-special-bindings-restore-repeated-name
  (values
   (catch 'stop
     (let* ((*partial-binding-first* :first)
            (*partial-binding-first* :second)
            (value (throw 'stop *partial-binding-first*)))
       value))
   *partial-binding-first*)
  :second :outside-first)
