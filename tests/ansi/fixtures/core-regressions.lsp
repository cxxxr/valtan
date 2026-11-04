(in-package :cl-test)

;; Exercise runtime compilation: these definitions are read after startup.
(defmacro runtime-list (&body body) `(list ,@body))
(deftest runtime-macro (runtime-list 1 2 3) (1 2 3))
(defstruct runtime-record value)
(deftest runtime-structure
  (let ((r (make-runtime-record :value 12)))
    (setf (runtime-record-value r) 27)
    (runtime-record-value r))
  27)
(deftest compiled-constant (+ most-positive-fixnum 1) 9007199254740992)
(deftest bignum-exact
  (- (+ 9007199254740992 1) 9007199254740992)
  1)
(deftest bignum-literal
  (= 123456789012345678901234567890
     (parse-integer "123456789012345678901234567890"))
  t)
(deftest bignum-shift (ash (ash 1 80) -80) 1)
(deftest negative-shift (ash -7 -1) -4)
(deftest bignum-length (integer-length (ash 1 80)) 81)
(deftest integer-division
  (multiple-value-list (floor -100000000000000000001 10))
  (-10000000000000000001 9))
(deftest runtime-compile
  (funcall (compile nil '(lambda (a b) (values (+ a b) (- a b)))) 7 3)
  10 4)
(deftest coerce-sequence (coerce '(#\a #\b) 'string) "ab")
(deftest skipped-package
  (read-from-string "#+valtan-no-such-feature NO-SUCH-PACKAGE:X 42")
  42 45)
(deftest skipped-evaluation
  (let ((*read-eval* nil))
    (car (list (read-from-string "#+valtan-no-such-feature #.(error 1) 17"))))
  17)
(deftest read-eval-disabled
  (handler-case (let ((*read-eval* nil)) (read-from-string "#.(+ 1 2)"))
    (reader-error () :rejected))
  :rejected)
(deftest two-way-stream-routing
  (let* ((input (make-string-input-stream "xy"))
         (output (make-string-output-stream))
         (stream (make-two-way-stream input output)))
    (write-string "ab" stream)
    (values (read-char stream) (read-char stream)
            (get-output-stream-string output)
            (eq input (two-way-stream-input-stream stream))
            (eq output (two-way-stream-output-stream stream))))
  #\x #\y "ab" t t)
(deftest random-state-copy
  (let* ((first (make-random-state t))
         (second (make-random-state first)))
    (and (not (eq first second))
         (loop repeat 30
               always (let ((x (random 123456789012345678901234567890 first))
                            (y (random 123456789012345678901234567890 second)))
                        (and (= x y) (<= 0 x) (< x 123456789012345678901234567890))))))
  t)
(defgeneric runtime-generic (x)
  (:method ((x integer)) (+ x 1)))
(deftest generic-function-method
  (values (runtime-generic 9)
          (not (null (find-method #'runtime-generic nil (list (find-class 'integer))))))
  10 t)
(deftest pathname-literal
  (let ((path #p"/tmp/example.lisp"))
    (values (pathnamep path) (pathname-directory path)
            (pathname-name path) (pathname-type path) (namestring path)))
  t (:absolute "tmp") "example" "lisp" "/tmp/example.lisp")
(defmacro runtime-expand (form &environment env) (macroexpand form env))
(deftest macro-environment
  (macrolet ((local-sum (x y) (list '+ x y)))
    (runtime-expand (local-sum 2 3)))
  5)
(define-method-combination runtime-combination () ((methods (:part)))
  `(list ,@(mapcar (lambda (method) `(call-method ,method)) methods)))
(defgeneric runtime-combined (x) (:method-combination runtime-combination))
(defmethod runtime-combined :part ((x t)) :general)
(defmethod runtime-combined :part ((x integer)) (+ x 1))
(deftest long-method-combination (runtime-combined 9) (10 :general))
