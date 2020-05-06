cl::(declaim (optimize (speed 0) (safety 3) (debug 3)))

(cl:defpackage :valtan-core/loop
  (:use :cl)
  (:shadowing-import-from :valtan-core :gensym))
(cl:in-package :valtan-core/loop)

(defvar *loop-exps*)
(defvar *loop-end-tag*)

(defvar *named*)
(defvar *temporary-variables*)
(defvar *initially-forms*)
(defvar *finally-forms*)
(defvar *with-clauses*)
(defvar *for-clauses*)

(defvar *accumulators*)

(defvar *loop-body*)

(defstruct for-clause
  var
  init-form
  before-update-form
  after-update-form
  endp-form)

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
     (setf ,tail (cdr ,tail))
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
  (let ((g-value (gensym "VALUE")))
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

(defun check-variable (var)
  (unless (and (symbolp var)
               (not (keywordp var))
               (not (null var)))
    (loop-error "~S is not variable" var)))

(defun type-spec ()
  (case (lookahead)
    ((cl:fixnum cl:float cl:t cl:nil)
     (next-exp))
    ((of-type)
     (next-exp)
     (next-exp))))

(defun it ()
  (gensym "IT"))

(defun parse-compound-forms ()
  (do ((forms nil))
      ((or (end-of-loop-p)
           (symbolp (lookahead)))
       (nreverse forms))
    (push (next-exp) forms)))

(defun parse-with-clause ()
  (do ()
      (nil)
    (let ((var (next-exp)))
      (check-variable var)
      (type-spec)
      (let ((initial-form
              (if (eq (to-keyword (lookahead)) :=)
                  (progn
                    (next-exp)
                    (next-exp))
                  nil)))
        (setf *with-clauses*
              (nconc *with-clauses*
                     (list (list var initial-form)))))
      (if (eq (to-keyword (lookahead)) :and)
          (next-exp)
          (return)))))

(defun parse-for-as-arithmetic (var first-op)
  (let* ((form1 (next-exp))
         (second-op (to-keyword (next-exp)))
         (form2 (next-exp))
         (by-form (when (eq (ensure-keyword (lookahead)) :by)
                    (next-exp)
                    (next-exp)))
         (form2-gsym (when (consp form2) (gensym)))
         (by-form-gsym (when (consp by-form) (gensym)))
         step-op
         while-op)
    (when form2-gsym
      (push (list form2-gsym form2) *temporary-variables*))
    (when by-form-gsym
      (push (list by-form-gsym by-form) *temporary-variables*))
    (cond ((and (member first-op '(:from :upfrom))
                (member second-op '(:to :upto :below)))
           (setf step-op '+
                 while-op (if (eq second-op :below) '>= '>)))
          ((and (member first-op '(:from :downfrom))
                (member second-op '(:to :downto :above)))
           (setf step-op '-
                 while-op (if (eq second-op :above) '<= '<)))
          (t
           (loop-error "The combination of ~S and ~S is invalid" first-op second-op)))
    (setf *for-clauses*
          (nconc *for-clauses*
                 (list (make-for-clause :var var
                                        :init-form form1
                                        :after-update-form `(,step-op ,var
                                                                      ,(or by-form-gsym
                                                                           by-form
                                                                           1))
                                        :endp-form `(,while-op ,var
                                                                ,(or form2-gsym
                                                                     form2))))))))

(defun parse-for-as-equals-then (var)
  (let* ((init-form (next-exp))
         (update-form
           (if (and (symbolp (lookahead))
                    (string= (lookahead) :then))
               (progn
                 (next-exp)
                 (next-exp))
               init-form)))
    (setf *for-clauses*
          (nconc *for-clauses*
                 (list (make-for-clause :var var
                                        :init-form init-form
                                        :after-update-form update-form))))))

(defun parse-for-as-in-list (var)
  (let ((list-form (next-exp))
        (temporary-var (gensym)))
    (setf *for-clauses*
          (nconc *for-clauses*
                 (list (make-for-clause :var temporary-var
                                        :init-form list-form
                                        :after-update-form `(cdr ,temporary-var)
                                        :endp-form `(null ,temporary-var)))
                 (list (make-for-clause :var var
                                        :init-form nil
                                        :before-update-form `(car ,temporary-var)))))))

(defun parse-for-as-on-list (var)
  (let ((list-form (next-exp)))
    (setf *for-clauses*
          (nconc *for-clauses*
                 (list (make-for-clause :var var
                                        :init-form list-form
                                        :after-update-form `(cdr ,var)
                                        :endp-form `(null ,var)))))))

(defun parse-for-as-across (var)
  (let ((vector-form (next-exp))
        (vector-var (gensym "VECTOR"))
        (index-var (gensym "INDEX"))
        (length-var (gensym "LENGTH")))
    (push (list vector-var vector-form) *temporary-variables*)
    (push (list length-var `(length ,vector-var)) *temporary-variables*)
    (setf *for-clauses*
          (nconc *for-clauses*
                 (list (make-for-clause :endp-form `(>= ,index-var ,length-var)))))
    (setf *for-clauses*
          (nconc *for-clauses*
                 (list
                  (make-for-clause :var var
                                   :init-form nil
                                   :before-update-form `(aref ,vector-var ,index-var))
                  (make-for-clause :var index-var
                                   :init-form 0
                                   :before-update-form `(+ ,index-var 1)))))))

(defun parse-for-as-hash-or-package (var)
  (declare (ignore var))
  (error "unimplemnted"))

(defun parse-for-as-clause ()
  (let ((var (next-exp)))
    (check-variable var)
    (type-spec)
    (let ((name (to-keyword (next-exp))))
      (ecase name
        ((:from :upfrom :downfrom)
         (parse-for-as-arithmetic var name))
        ((:=)
         (parse-for-as-equals-then var))
        ((:in)
         (parse-for-as-in-list var))
        ((:on)
         (parse-for-as-on-list var))
        ((:across)
         (parse-for-as-across var))
        ((:being)
         (parse-for-as-hash-or-package var))))))

(defun parse-initial-final-clause (exp)
  (case exp
    ((:initially)
     (next-exp)
     (setf *initially-forms*
           (nconc *initially-forms* (parse-compound-forms))))
    ((:finally)
     (next-exp)
     (setf *finally-forms*
           (nconc *finally-forms* (parse-compound-forms))))))

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
      (check-variable var)
      var)))

(defmacro select-accumulator ((kind into) &body body)
  (let ((g-kind/accumulator (gensym "KIND/ACCUMULATOR"))
        (g-kind (gensym "KIND"))
        (g-into (gensym "INTO")))
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
              (make-list-collector :head (gensym "LIST-HEAD")
                                   :tail (gensym "LIST-TAIL")))))
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
              (make-sum-counter :var (or into (gensym "SUM-COUNTER"))))))
      `(sum-count ,(sum-counter-var sum-counter)
                  ,form-or-it
                  ,kind))))

(defun parse-maxmin-clause (kind)
  (let ((form-or-it (parse-form-or-it))
        (into (parse-into-clause)))
    (let ((maxmin-collector
            (select-accumulator (kind into)
              (make-maxmin-collector :var (or into (gensym "MAXMIN-COLLECTOR"))))))
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

(defun parse-conditional-clause (exp)
  (declare (ignore exp))
  )

(defun parse-termination-test-clause (exp)
  (declare (ignore exp))
  )

(defun parse-main-clause ()
  (let ((exp (ensure-keyword (lookahead))))
    (let ((form
            (or (parse-unconditional-clause exp)
                (parse-accumulation-clause exp)
                (parse-conditional-clause exp)
                (parse-termination-test-clause exp)
                (parse-initial-final-clause exp)
                (loop-error "unexpected form: ~S" exp))))
      (push form *loop-body*))))

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
        (*with-clauses* '())
        (*temporary-variables* '())
        (*for-clauses* '())
        (*accumulators* (make-hash-table))
        (*loop-body* '())
        (*loop-end-tag* (gensym "LOOP-END"))
        (loop-start (gensym "LOOP-START")))
    (parse-loop forms)
    (let ((tagbody-loop-form
            `(tagbody
               ,@*initially-forms*
               ,loop-start
               ,@(mapcan (lambda (for-clause)
                           (let ((endp-form (for-clause-endp-form for-clause)))
                             (when endp-form
                               (list `(when ,endp-form
                                        (go ,*loop-end-tag*))))))
                  *for-clauses*)
               ,@(mapcan (lambda (for-clause)
                           (let ((var (for-clause-var for-clause))
                                 (update (for-clause-before-update-form for-clause)))
                             (when update
                               (list `(setq ,var ,update)))))
                  *for-clauses*)
               ,@(nreverse *loop-body*)
               ,@(mapcan (lambda (for-clause)
                           (let ((var (for-clause-var for-clause))
                                 (update (for-clause-after-update-form for-clause)))
                             (when update
                               (list `(setq ,var ,update)))))
                  *for-clauses*)
               (go ,loop-start)
               ,*loop-end-tag*
               ,@*finally-forms*)))
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
      `(block ,*named*
         (let* (,@*with-clauses*
                ,@(mapcar (lambda (for-clause)
                            (let ((var (for-clause-var for-clause))
                                  (init (for-clause-init-form for-clause)))
                              `(,var ,init)))
                          *for-clauses*)
                ,@(nreverse *temporary-variables*))
           ,tagbody-loop-form)))))

(defun expand-loop (forms)
  (if (and forms (symbolp (car forms)))
      (expand-complex-loop forms)
      (let ((tag (gensym "LOOP-START")))
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
