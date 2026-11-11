(in-package :cl-user)

;; Run outside the timed region, with values that cannot be constant folded.
(defun perf-check-numeric (x y)
  (list (+ x y) (- x y) (* x y) (/ x y)
        (= x y) (/= x y) (< x y) (> x y) (<= x y) (>= x y)
        (1+ x) (1- x) (zerop x)))
(assert (equal (perf-check-numeric 6 2) '(8 4 12 3 nil t nil t nil t 7 5 nil)))
(assert (equal (perf-check-numeric 0 2) '(2 -2 0 0 nil t t nil t nil 1 -1 t)))
(assert (equal (subseq (perf-check-numeric 1 2) 0 4) '(3 -1 2 1/2)))
(assert (equal (subseq (perf-check-numeric 9007199254740991 2) 0 3)
               '(9007199254740993 9007199254740989 18014398509481982)))
(assert (floatp (+ (identity 1.0) 2)))
(assert (= (+ (identity (complex 1 2)) 3) (complex 4 2)))

(let ((x 1)) (assert (= (+ x (setq x 4)) 5)))
(let ((x 1)) (assert (= (- x (setq x 4)) -3)))
(let ((x 1)) (assert (< x (setq x 4))))
(let ((seen nil))
  (handler-case (+ (identity 'bad) (progn (setq seen t) 2))
    (type-error () nil))
  (assert seen))
(assert (handler-case (progn (+ (identity 'bad) 2) nil) (type-error () t)))
(assert (handler-case (progn (< (identity (complex 1 2)) 2) nil) (type-error () t)))
(assert (handler-case (progn (/ (identity 1) 0) nil) (division-by-zero () t)))
(assert (handler-case (progn (1+ (identity 1) 2) nil) (program-error () t)))
(assert (handler-case (progn (<) nil) (program-error () t)))
(assert (= (+) 0))
(assert (= (*) 1))
(assert (= (- (identity 3)) -3))
(assert (= (+ (identity 1) 2 3) 6))
(flet ((+ (a b) (list a b))) (assert (equal (+ 1 2) '(1 2))))
(assert (null (car nil)))
(assert (null (cdr nil)))
(assert (handler-case (progn (car (identity 1)) nil) (type-error () t)))
(assert (handler-case (progn (cdr (identity 1)) nil) (type-error () t)))
(assert (char= (identity #\a) #\a))
(assert (not (char= (identity #\a) #\b)))
(assert (handler-case (progn (char= (identity 1) #\a) nil) (type-error () t)))
(assert (handler-case (progn (char= #\a (identity 1)) nil) (type-error () t)))
(assert (null (not (identity 0))))
(assert (atom (identity nil)))
(assert (listp (identity nil)))
(assert (not (listp (identity 1))))
(assert (handler-case (progn (setf (elt (copy-seq "012") 'index) #\a) nil)
          (type-error () t)))
(assert (handler-case (progn (setf (elt (copy-seq "012") "xyz") #\a) nil)
          (type-error () t)))

;; Values saved across cleanup must not alias the single-value register.
(assert (equal (multiple-value-list
                (unwind-protect (values 1 2 3) (+ (identity 8) 9))) '(1 2 3)))
(assert (equal (multiple-value-list
                (unwind-protect (+ (identity 1) 2) (values 8 9))) '(3)))
(assert (equal (multiple-value-list
                (unwind-protect (values) (+ (identity 8) 9))) nil))
(assert (equal (multiple-value-call #'list
                (+ (identity 1) 2) (values 4 5) (values) (1+ (identity 5))) '(3 4 5 6)))
(assert (equal (multiple-value-list
                (multiple-value-prog1 (+ (identity 1) 2) (+ (identity 5) 6))) '(3)))

;; VALUES-LIST must keep zero values, retain object identity, reject improper
;; and circular input, and detach its result from later source-list mutation.
(dolist (function (list #'values-list (compile nil '(lambda (items) (values-list items)))))
  (assert (null (multiple-value-list (funcall function nil))))
  (let* ((object (list :object)) (items (list nil object 3)))
    (multiple-value-bind (first second third) (funcall function items)
      (assert (null first)) (assert (eq second object)) (assert (= third 3))))
  (let ((items (list 1 2 3)))
    (assert (equal (multiple-value-list
                    (unwind-protect (funcall function items)
                      (setf (car items) 9 (cdr items) nil))) '(1 2 3))))
  (let* ((cycle (list 1 2 3)) (prefix (list* 4 5 cycle)))
    (setf (cdddr cycle) cycle)
    (dolist (invalid (list 17 (cons 1 2) cycle prefix))
      (assert (handler-case (progn (funcall function invalid) nil)
                (type-error (condition) (eq (type-error-datum condition) invalid)))))))
(assert (equal (multiple-value-call #'list (values-list '(1 2))
                                 (values-list nil) (values-list '(3))) '(1 2 3)))

;; The runtime compiler must use the same lowering and preserve evaluation order.
(assert (= (funcall (compile nil '(lambda (x) (+ x (setq x 4)))) 1) 5))
(assert (equal (multiple-value-list
                (funcall (compile nil '(lambda (x) (unwind-protect (values x 2) (+ x 9)))) 1))
               '(1 2)))

;; Binding contexts share global cells while keeping dynamic LET/parameter and
;; PROGV bindings independent. This is runtime groundwork, not a thread API.
(defvar *binding-context-value* :global)
(defun call-in-new-binding-context (thunk)
  ((ffi:ref "lisp" "withBindingContext")
   ((ffi:ref "lisp" "createBindingContext")) thunk))
(defun check-binding-contexts ()
  (setf *binding-context-value* :global)
  (let ((*binding-context-value* :local))
    (assert (eq (call-in-new-binding-context
                  (lambda ()
                    (assert (eq *binding-context-value* :global))
                    (setf *binding-context-value* :changed))) :changed))
    (assert (eq *binding-context-value* :local)))
  (assert (eq *binding-context-value* :changed))
  (let ((symbol (make-symbol "PROGV-CONTEXT")))
    (progv (list symbol symbol) '(1 2)
      (assert (= (symbol-value symbol) 2))
      (call-in-new-binding-context
       (lambda ()
         (assert (not (boundp symbol)))
         (set symbol 3)))
      (assert (= (symbol-value symbol) 2)))
    (assert (= (symbol-value symbol) 3))
    (progv (list symbol) nil
      (assert (not (boundp symbol)))
      (call-in-new-binding-context (lambda () (set symbol 4)))
      (assert (not (boundp symbol))))
    (assert (= (symbol-value symbol) 4)))
  (flet ((parameter (*binding-context-value* &optional
                      (x (call-in-new-binding-context
                          (lambda ()
                            (assert (eq *binding-context-value* :changed))
                            (setf *binding-context-value* :parameter-global)))))
           (list *binding-context-value* x)))
    (assert (equal (parameter :parameter-local) '(:parameter-local :parameter-global))))
  (assert (eq *binding-context-value* :parameter-global))
  (let ((*binding-context-value* :outer))
    (assert (eq (catch 'done
                  (let ((*binding-context-value* :inner))
                    (call-in-new-binding-context
                     (lambda () (setf *binding-context-value* :exit-global)))
                    (throw 'done *binding-context-value*))) :inner))
    (assert (eq *binding-context-value* :outer)))
  (assert (eq *binding-context-value* :exit-global)))
(check-binding-contexts)
(funcall (compile nil '(lambda () (check-binding-contexts))))
;; Compile the binding body itself in the runtime compiler as well.
(funcall (compile nil
                  '(lambda ()
                     (setf *binding-context-value* :global)
                     (let ((*binding-context-value* :runtime-local))
                       (call-in-new-binding-context
                        (lambda ()
                          (assert (eq *binding-context-value* :global))
                          (setf *binding-context-value* :runtime-global)))
                       (assert (eq *binding-context-value* :runtime-local)))
                     (assert (eq *binding-context-value* :runtime-global)))))

;; The bootstrap MAP-FOREACH iterator keeps ordinary callbacks and nonlocal
;; exits working, for both strong and weak native maps.
(dolist (map (list (system:make-map) (system:weak-op #j"map" #j"key" #j"eq")))
  (let ((key (list :key)) (seen nil))
    (assert (null (system:map-foreach map (lambda (&rest args) (declare (ignore args)) (error "Empty map callback")))))
    (system:map-set map key 42)
    (assert (null (system:map-foreach map (lambda (value actual-key) (setf seen (list value actual-key))))))
    (assert (= (first seen) 42))
    (assert (eq (second seen) key))
    (assert (eq (catch 'map-exit
                  (system:map-foreach map (lambda (value actual-key)
                                           (declare (ignore value))
                                           (throw 'map-exit actual-key)))) key))))
