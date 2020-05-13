(cl:defpackage :valtan-core/loop
  (:use :cl))
(cl:in-package :valtan-core/loop)

(defvar *loop-exps*)
(defvar *loop-end-tag*)

(defvar *named*)
(defvar *loop-variables*)
(defvar *initially-forms*)
(defvar *finally-forms*)
(defvar *loop-test-forms*)
(defvar *before-update-forms*)
(defvar *after-update-forms*)
(defvar *result-forms*)

(defvar *accumulators*)
(defvar *hash-table-iterators*)

(defvar *loop-body*)

(defstruct list-collector
  head
  tail)

(defstruct sum-counter
  var)

(defstruct maxmin-collector
  var)

(defmacro with-list-collector ((head tail var) &body body)
  `(let* ((,head (list nil))
          (,tail ,head)
          ,@(when var `(,var)))
     ,@body
     ,@(unless var `((cdr ,head)))))

(defmacro collecting (head tail value var kind)
  `(progn
     (setf (cdr ,tail)
           ,(ecase kind
              (:collect `(list ,value))
              (:append `(copy-list ,value))
              (:nconc value)))
     ,(ecase kind
        ((:collect)
         `(setf ,tail (cdr ,tail)))
        ((:append :nconc)
         `(do ()
              ((null (cdr ,tail)))
            (setq ,tail (cdr ,tail)))))
     ,@(when var
         `((setf ,var (cdr ,head))))))

(defmacro with-sum-counter ((counter-var var) &body body)
  `(let ((,counter-var 0))
     ,@body
     ,@(unless var `(,counter-var))))

(defmacro sum-count (counter-var value kind)
  (ecase kind
    (:count
     `(when ,value
        (setq ,counter-var (+ ,counter-var 1))))
    (:sum
     `(setq ,counter-var (+ ,counter-var ,value)))))

(defmacro with-maxmin-collector ((maxmin-var var) &body body)
  `(let ((,maxmin-var nil))
     ,@body
     ,@(unless var `(,maxmin-var))))

(defmacro collect-maxmin (maxmin-var value kind)
  (let ((g-value (gensym #"VALUE")))
    `(let ((,g-value ,value))
       (when (or (null ,maxmin-var)
                 (,(ecase kind
                     (:max '<)
                     (:min '>))
                  ,maxmin-var
                  ,g-value))
         (setq ,maxmin-var ,g-value)))))

(defun loop-error (msg &rest args)
  (apply #'error msg args))

(defun ensure-keyword (x)
  (cond ((keywordp x)
         x)
        ((symbolp x)
         (intern (symbol-name x) :keyword))
        (t
         x)))

(defun to-keyword (x)
  (cond ((keywordp x)
         x)
        ((symbolp x)
         (intern (symbol-name x) :keyword))
        (t
         (loop-error "unexpected expression: ~S" x))))

(defun next-exp ()
  (pop *loop-exps*))

(defun lookahead ()
  (car *loop-exps*))

(defun end-of-loop-p ()
  (null *loop-exps*))

(defun check-simple-var (x)
  (unless (and (symbolp x)
               (not (keywordp x))
               (not (null x)))
    (loop-error "~S is not variable" x)))

(defun add-loop-variable (var form)
  (push (list var form) *loop-variables*))

(defun add-loop-test-form (form)
  (push form *loop-test-forms*))

(defun add-before-update-form (form)
  (push form *before-update-forms*))

(defun add-after-update-form (form)
  (push form *after-update-forms*))

(defun add-initially-form (form)
  (push form *initially-forms*))

(defun add-finally-form (form)
  (push form *finally-forms*))

(defun once-only-value (form &optional name)
  (if (consp form)
      (let ((var (if name (gensym (string name)) (gensym))))
        (add-loop-variable var form)
        var)
      form))

(defun type-spec ()
  (let ((x (lookahead)))
    (cond ((member x '(cl:fixnum cl:float cl:t cl:nil))
           (next-exp))
          ((eq (ensure-keyword x) :of-type)
           (next-exp)
           (next-exp)))))

(defun it ()
  (gensym #"IT"))

(defun parse-compound-forms ()
  (do ((forms nil))
      ((or (end-of-loop-p)
           (symbolp (lookahead)))
       (nreverse forms))
    (push (next-exp) forms)))

(defun gen-d-bind (d-var form)
  (labels ((f (d-var value)
             (cond ((null d-var))
                   ((consp d-var)
                    (let ((car-var (gensym #"CAR"))
                          (cdr-var (gensym #"CDR")))
                      (add-loop-variable car-var `(car ,value))
                      (add-loop-variable cdr-var `(cdr ,value))
                      (f (car d-var) car-var)
                      (f (cdr d-var) cdr-var)))
                   (t
                    (check-simple-var d-var)
                    (add-loop-variable d-var value)))))
    (let ((var (gensym #"DESTRUCTURING-VAR")))
      (add-loop-variable var form)
      (f d-var var))))

(defun parse-with-clause ()
  (do ()
      (nil)
    (let ((var (next-exp)))
      (type-spec)
      (gen-d-bind var
                  (if (eq (to-keyword (lookahead)) :=)
                      (progn
                        (next-exp)
                        (next-exp))
                      nil))
      (if (eq (to-keyword (lookahead)) :and)
          (next-exp)
          (return)))))

(defun parse-for-as-arithmetic (var)
  (check-simple-var var)
  (let ((init-keyword nil)
        (end-keyword nil)
        (init-value 0)
        (end-value nil)
        (dir nil)
        (dir-keyword nil)
        (then-or-equal nil)
        (by-value nil))
    (labels ((combination-error (key1 key2)
               (loop-error "The combination of ~S and ~S is invalid" key1 key2))
             (check-init-form (keyword)
               (when init-keyword
                 (combination-error init-keyword keyword)))
             (check-end-form (keyword)
               (when end-keyword
                 (combination-error end-keyword keyword)))
             (check-by-form ()
               (when by-value
                 (combination-error :by :by)))
             (check-dir (dir1 keyword)
               (when (and dir1 dir (not (eq dir1 dir)))
                 (combination-error keyword dir-keyword))))
      (do ()
          (nil)
        (let ((keyword (ensure-keyword (lookahead))))
          (case keyword
            ((:from :upfrom :downfrom)
             (next-exp)
             (check-init-form keyword)
             (let ((dir1 (case keyword
                           (:from nil)
                           (:upfrom :up)
                           (:downfrom :down))))
               (check-dir dir1 keyword)
               (setf init-keyword keyword
                     init-value (once-only-value (next-exp) keyword)
                     dir (or dir1 dir)
                     dir-keyword keyword)))
            ((:to :upto :downto :below :above)
             (next-exp)
             (check-end-form keyword)
             (let ((dir1 (case keyword
                           (:to nil)
                           ((:upto :below) :up)
                           ((:downto :above) :down))))
               (check-dir dir1 keyword)
               (setf end-keyword keyword
                     end-value (once-only-value (next-exp) keyword)
                     then-or-equal (case keyword
                                     ((:to :upto :downto) t)
                                     (otherwise nil))
                     dir (or dir1 dir)
                     dir-keyword keyword)))
            (:by
             (next-exp)
             (check-by-form)
             (setf by-value (once-only-value (next-exp) keyword)))
            (otherwise
             (return)))))
      (add-loop-variable var init-value)
      (when end-value
        (add-loop-test-form `(,(if (eq dir :down)
                                   (if then-or-equal '>= '>)
                                   (if then-or-equal '<= '<))
                              ,var
                              ,end-value)))
      (add-after-update-form `(setq ,var
                                    (,(if (eq dir :down) '- '+)
                                     ,var
                                     ,(or by-value 1)))))))

(defmacro d-setq (d-var form)
  (let ((binds '()))
    (labels ((f (d-var value)
               (cond ((null d-var) nil)
                     ((consp d-var)
                      (let ((car-var (gensym #"CAR"))
                            (cdr-var (gensym #"CDR")))
                        (push (list car-var `(car ,value)) binds)
                        (push (list cdr-var `(cdr ,value)) binds)
                        (append (f (car d-var) car-var)
                                (f (cdr d-var) cdr-var))))
                     (t
                      (check-simple-var d-var)
                      `((setq ,d-var ,value))))))
      (let* ((var (gensym #"DESTRUCTURING-VAR"))
             (body (f d-var var)))
        `(let* ((,var ,form) . ,(reverse binds))
           (declare (ignorable ,@(mapcar #'car binds)))
           ,@body)))))

(defun parse-for-as-equals-then (d-var)
  (let* ((init-form (next-exp))
         (update-form
           (if (and (symbolp (lookahead))
                    (string= (lookahead) :then))
               (progn
                 (next-exp)
                 (next-exp))
               init-form)))
    (gen-d-bind d-var nil)
    (add-initially-form `(d-setq ,d-var ,init-form))
    (add-after-update-form `(d-setq ,d-var ,update-form))))

(defun parse-for-as-by-clause ()
  (when (eq (ensure-keyword (lookahead)) :by)
    (next-exp)
    (next-exp)))

(defun function-form-p (form)
  (when (and (consp form)
             (eq 'function (car form)))
    (cadr form)))

(defun by-form (by-form var)
  (if by-form
      (let ((function-name (function-form-p by-form)))
        (if function-name
            `(,function-name ,var)
            `(funcall ,(once-only-value by-form)
                      ,var)))
      `(cdr ,var)))

(defun parse-for-as-in-list (d-var)
  (let ((list-form (next-exp))
        (by-form (parse-for-as-by-clause))
        (temporary-var (gensym)))
    (add-loop-variable temporary-var list-form)
    (add-after-update-form `(setq ,temporary-var ,(by-form by-form temporary-var)))
    (add-loop-test-form temporary-var)
    (gen-d-bind d-var nil)
    (add-before-update-form `(d-setq ,d-var (car ,temporary-var)))))

(defun parse-for-as-on-list (d-var)
  (let ((list-form (next-exp))
        (by-form (parse-for-as-by-clause))
        (temporary-var (gensym)))
    (add-loop-variable temporary-var list-form)
    (add-after-update-form `(setq ,temporary-var ,(by-form by-form temporary-var)))
    (add-loop-test-form `(consp ,temporary-var))
    (gen-d-bind d-var nil)
    (add-before-update-form `(d-setq ,d-var ,temporary-var))))

(defun parse-for-as-across (var)
  (check-simple-var var)
  (let ((vector-form (next-exp))
        (vector-var (gensym #"VECTOR"))
        (index-var (gensym #"INDEX"))
        (length-var (gensym #"LENGTH")))
    (add-loop-variable vector-var vector-form)
    (add-loop-variable length-var `(length ,vector-var))
    (add-loop-variable var nil)
    (add-before-update-form `(setq ,var (aref ,vector-var ,index-var)))
    (add-loop-variable index-var 0)
    (add-loop-test-form `(< ,index-var ,length-var))
    (add-before-update-form `(setq ,index-var (+ ,index-var 1)))))

(defun parse-for-as-hash (hash-first-var hash-key-p)
  (next-exp)
  (ecase (ensure-keyword (next-exp))
    ((:in :of)))
  (let* ((null '#:null)
         (hash-table (next-exp))
         (hash-second-var null))
    (when (eq :using (ensure-keyword (lookahead)))
      (next-exp)
      (let ((form (next-exp)))
        (assert (and (consp form)
                     (= 2 (length form))
                     (eq (if hash-key-p
                             :hash-value
                             :hash-key)
                         (ensure-keyword (car form)))))
        (setq hash-second-var (cadr form))))
    (let* ((hash-second-var (if (eq hash-second-var null)
                                (gensym (if hash-key-p #"HASH-VALUE" #"HASH-KEY"))
                                hash-second-var))
           (hash-table-var (gensym #"HASH-TABLE"))
           (hash-more-var (gensym #"HASH-MORE"))
           (hash-table-next (gensym #"HASH-NEXT"))
           (hash-first-temp (if (listp hash-first-var)
                                (gensym (if hash-key-p #"HASH-KEY-TEMP" #"HASH-VALUE-TEMP"))
                                nil))
           (hash-second-temp (if (listp hash-second-var)
                                 (gensym (if hash-key-p #"HASH-VALUE-TEMP" #"HASH-KEY-TEMP"))
                                 nil))
           (hash-key-var (if hash-key-p
                             (or hash-first-temp hash-first-var)
                             (or hash-second-temp hash-second-var)))
           (hash-value-var (if hash-key-p
                               (or hash-second-temp hash-second-var)
                               (or hash-first-temp hash-first-var))))
      (cond (hash-first-temp
             (gen-d-bind hash-first-var nil)
             (add-loop-variable hash-first-temp nil))
            (t
             (add-loop-variable hash-first-var nil)))
      (cond (hash-second-temp
             (gen-d-bind hash-second-var nil)
             (add-loop-variable hash-second-temp nil))
            (t
             (add-loop-variable hash-second-var nil)))
      (add-loop-variable hash-table-var hash-table)
      (add-loop-variable hash-more-var nil)
      (push (cons hash-table-next hash-table-var) *hash-table-iterators*)
      (push `(unless (multiple-value-setq
                         (,hash-more-var ,hash-key-var ,hash-value-var)
                       (,hash-table-next))
               (go ,*loop-end-tag*))
            *loop-body*)
      (when hash-first-temp
        (push `(d-setq ,hash-first-var ,hash-first-temp) *loop-body*))
      (when hash-second-temp
        (push `(d-setq ,hash-second-var ,hash-second-temp) *loop-body*)))))

(defun parse-for-as-hash-or-package (var)
  (ecase (ensure-keyword (lookahead))
    ((:each :the)
     (next-exp)))
  (ecase (ensure-keyword (lookahead))
    ((:hash-key :hash-keys)
     (parse-for-as-hash var t))
    ((:hash-values :hash-value)
     (parse-for-as-hash var nil))
    #+(or)
    ((:symbol :symbols :present-symbol :present-symbols :external-symbol :external-symbols)
     )))

(defun parse-for-as-clause ()
  (let ((var (next-exp)))
    (type-spec)
    (case (to-keyword (lookahead))
      ((:=)
       (next-exp)
       (parse-for-as-equals-then var))
      ((:in)
       (next-exp)
       (parse-for-as-in-list var))
      ((:on)
       (next-exp)
       (parse-for-as-on-list var))
      ((:across)
       (next-exp)
       (parse-for-as-across var))
      ((:being)
       (next-exp)
       (parse-for-as-hash-or-package var))
      (otherwise
       (parse-for-as-arithmetic var))))
  (when (eq (ensure-keyword (lookahead)) :and)
    (next-exp)
    (parse-for-as-clause)))

(defun parse-initial-final-clause (exp)
  (case exp
    ((:initially)
     (next-exp)
     (add-initially-form (parse-compound-forms))
     t)
    ((:finally)
     (next-exp)
     (add-finally-form (parse-compound-forms))
     t)))

(defun parse-variable-clause ()
  (let ((exp (ensure-keyword (lookahead))))
    (case exp
      ((:with)
       (next-exp)
       (parse-with-clause)
       t)
      ((:for :as)
       (next-exp)
       (parse-for-as-clause)
       t)
      (otherwise
       (parse-initial-final-clause exp)))))

(defun parse-doing-clause ()
  (let ((forms (parse-compound-forms)))
    `(progn ,@forms)))

(defun parse-form-or-it ()
  (let ((exp (next-exp)))
    (if (eq :it (ensure-keyword exp))
        (it)
        exp)))

(defun parse-return-clause ()
  `(return ,(parse-form-or-it)))

(defun parse-unconditional-clause (exp)
  (case exp
    ((:do :doing)
     (next-exp)
     (parse-doing-clause))
    ((:return)
     (next-exp)
     (parse-return-clause))
    (otherwise nil)))

(defun parse-into-clause ()
  (when (eq (ensure-keyword (lookahead)) :into)
    (next-exp)
    (let ((var (next-exp)))
      (check-simple-var var)
      var)))

(defmacro select-accumulator ((kind into) &body body)
  (let ((g-kind/accumulator (gensym #"KIND/ACCUMULATOR"))
        (g-kind (gensym #"KIND"))
        (g-into (gensym #"INTO")))
    `(let ((,g-kind/accumulator (gethash into *accumulators*))
           (,g-kind ,kind)
           (,g-into ,into))
       (cond (,g-kind/accumulator
              ;; TODO: loop-error
              (assert (eq (car ,g-kind/accumulator) ,g-kind))
              (cdr ,g-kind/accumulator))
             (t
              (let ((accumulator (progn ,@body)))
                (setf (gethash ,g-into *accumulators*)
                      (cons ,g-kind accumulator))
                accumulator))))))

(defun parse-collect-clause (kind)
  (let ((form-or-it (parse-form-or-it))
        (into (parse-into-clause)))
    (let ((list-collector
            (select-accumulator (kind into)
              (make-list-collector :head (gensym #"LIST-HEAD")
                                   :tail (gensym #"LIST-TAIL")))))
      `(collecting ,(list-collector-head list-collector)
                   ,(list-collector-tail list-collector)
                   ,form-or-it
                   ,into
                   ,kind))))

(defun parse-count-clause (kind)
  (let ((form-or-it (parse-form-or-it))
        (into (parse-into-clause)))
    (let ((sum-counter
            (select-accumulator (kind into)
              (make-sum-counter :var (or into (gensym #"SUM-COUNTER"))))))
      `(sum-count ,(sum-counter-var sum-counter)
                  ,form-or-it
                  ,kind))))

(defun parse-maxmin-clause (kind)
  (let ((form-or-it (parse-form-or-it))
        (into (parse-into-clause)))
    (let ((maxmin-collector
            (select-accumulator (kind into)
              (make-maxmin-collector :var (or into (gensym #"MAXMIN-COLLECTOR"))))))
      `(collect-maxmin ,(maxmin-collector-var maxmin-collector)
                       ,form-or-it
                       ,kind))))

(defun parse-accumulation-clause (exp)
  (case exp
    ((:collect :collecting)
     (next-exp)
     (parse-collect-clause :collect))
    ((:append :appending)
     (next-exp)
     (parse-collect-clause :append))
    ((:nconc :nconcing)
     (next-exp)
     (parse-collect-clause :nconc))
    ((:count :counting)
     (next-exp)
     (parse-count-clause :count))
    ((:sum :summing)
     (next-exp)
     (parse-count-clause :sum))
    ((:maximize :maximizing)
     (next-exp)
     (parse-maxmin-clause :max))
    ((:minimize :minimizing)
     (next-exp)
     (parse-maxmin-clause :min))))

(defun parse-selectable-clause ()
  (let ((exp (ensure-keyword (lookahead))))
    (or (parse-unconditional-clause exp)
        (parse-accumulation-clause exp)
        (parse-conditional-clause exp)
        (loop-error "unexpected form: ~S" exp))))

(defun parse-conditional-then-else-clause ()
  (let ((forms (list (parse-selectable-clause))))
    (do ()
        ((not (eq (ensure-keyword (lookahead)) :and)))
      (push (parse-selectable-clause) forms))
    `(progn ,@(nreverse forms))))

(defun parse-conditional-clause-1 (test-name)
  (let ((condition (next-exp))
        (then (parse-conditional-then-else-clause))
        (else
          (when (eq (ensure-keyword (lookahead)) :else)
            (parse-conditional-then-else-clause))))
    (when (eq (ensure-keyword (lookahead)) :end)
      (next-exp))
    (ecase test-name
      ((:if :when)
       `(if ,condition
            ,then
            ,else))
      ((:unless)
       `(if (not ,condition)
            ,then
            ,else)))))

(defun parse-conditional-clause (exp)
  (case exp
    ((:if :when :unless)
     (next-exp)
     (parse-conditional-clause-1 exp))))

(defun parse-termination-test-clause (exp)
  (case exp
    ((:while)
     (next-exp)
     `(unless ,(next-exp)
        (go ,*loop-end-tag*)))
    ((:until)
     (next-exp)
     `(when ,(next-exp)
        (go ,*loop-end-tag*)))
    #+(or)((:repeat)) ;TODO
    ((:always)
     (next-exp)
     (push `(return t) *result-forms*)
     `(unless ,(next-exp)
        (return nil)))
    ((:never)
     (next-exp)
     (push `(return t) *result-forms*)
     `(when ,(next-exp)
        (return nil)))
    ((:thereis)
     (next-exp)
     (let ((it (gensym #"THERE")))
       `(let ((,it ,(next-exp)))
          (when ,it (return ,it)))))))

(defun parse-main-clause ()
  (let ((exp (ensure-keyword (lookahead))))
    (let ((form
            (or (parse-unconditional-clause exp)
                (parse-accumulation-clause exp)
                (parse-conditional-clause exp)
                (parse-termination-test-clause exp))))
      (if form
          (push form *loop-body*)
          (or (parse-initial-final-clause exp)
              (loop-error "unexpected form: ~S" exp))))))

(defun parse-name-clause ()
  (when (eq (ensure-keyword (lookahead)) :named)
    (next-exp)
    (let ((name (next-exp)))
      (unless name
        (loop-error "Name clause must be a symbol (actual value: ~S)" name))
      (setq *named* name))))

(macrolet ((def (parse-name)
             `(do ()
                  ((end-of-loop-p))
                (let ((x (lookahead)))
                  (unless (symbolp x)
                    (loop-error "unexpected token: ~S" x))
                  (unless (,parse-name)
                    (return))))))
  (defun parse-variable-clauses ()
    (def parse-variable-clause))
  (defun parse-main-clauses ()
    (def parse-main-clause)))

(defun parse-loop (forms)
  (let ((*loop-exps* forms))
    (parse-name-clause)
    (parse-variable-clauses)
    (parse-main-clauses)))

(defun expand-complex-loop (forms)
  (let ((*named* nil)
        (*initially-forms* '())
        (*finally-forms* '())
        (*loop-variables* '())
        (*accumulators* (make-hash-table))
        (*hash-table-iterators* '())
        (*result-forms* '())
        (*loop-test-forms* '())
        (*before-update-forms* '())
        (*after-update-forms* '())
        (*loop-body* '())
        (*loop-end-tag* (gensym #"LOOP-END"))
        (loop-start (gensym #"LOOP-START")))
    (parse-loop forms)
    (let ((tagbody-loop-form
            `(tagbody
               ,@(nreverse *initially-forms*)
               ,loop-start
               ,@(mapcar (lambda (form)
                           `(unless ,form
                              (go ,*loop-end-tag*)))
                   (nreverse *loop-test-forms*))
               ,@(nreverse *before-update-forms*)
               ,@(nreverse *loop-body*)
               ,@(nreverse *after-update-forms*)
               (go ,loop-start)
               ,*loop-end-tag*
               ,@(nreverse *finally-forms*)
               ,@(nreverse *result-forms*))))
      (maphash (lambda (name kind/accumulator)
                 (let ((accumulator (cdr kind/accumulator)))
                   (typecase accumulator
                     (list-collector
                      (setq tagbody-loop-form
                            `(with-list-collector (,(list-collector-head accumulator)
                                                   ,(list-collector-tail accumulator)
                                                   ,name)
                               ,tagbody-loop-form)))
                     (sum-counter
                      (setq tagbody-loop-form
                            `(with-sum-counter (,(sum-counter-var accumulator)
                                                ,name)
                               ,tagbody-loop-form)))
                     (maxmin-collector
                      (setq tagbody-loop-form
                            `(with-maxmin-collector (,(maxmin-collector-var accumulator)
                                                     ,name)
                               ,tagbody-loop-form))))))
               *accumulators*)
      (dolist (iterator *hash-table-iterators*)
        (setq tagbody-loop-form
              `(with-hash-table-iterator (,(car iterator) ,(cdr iterator))
                 ,tagbody-loop-form)))
      (let ((loop-variables (nreverse *loop-variables*)))
        `(block ,*named*
           (let* (,@loop-variables)
             (declare (ignorable ,@(mapcar #'car loop-variables)))
             ,tagbody-loop-form))))))

(defun expand-loop (forms)
  (if (and forms (symbolp (car forms)))
      (expand-complex-loop forms)
      (let ((tag (gensym #"LOOP-START")))
        `(block nil
           (tagbody
             ,tag
             ,@forms
             (go ,tag))))))

#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defmacro loop (&body forms)
  (valtan-core/loop::expand-loop forms))
