(in-package :cl-test)

(defun (setf environment-traced-place) (value cell) (setf (car cell) value))
(deftest environment-trace-setf-function
  (let ((cell (list nil)) (original (fdefinition '(setf environment-traced-place))))
    (unwind-protect
         (values (not (zerop (length (with-output-to-string (*trace-output*)
                                        (trace (setf environment-traced-place))
                                        (setf (environment-traced-place cell) :updated)
                                        (untrace (setf environment-traced-place))))))
                 cell (eq original (fdefinition '(setf environment-traced-place))))
      (untrace (setf environment-traced-place))))
  t (:updated) t)

(deftest environment-setf-function-binding-lifecycle
  (let* ((name (list 'setf (gensym))) (cell (list :old)))
    (values (not (fboundp name))
            (progn (eval `(defun ,name (value cell) (setf (car cell) value)))
                   (not (null (fboundp name))))
            (funcall (fdefinition name) :new cell)
            cell
            (equal (fmakunbound name) name)
            (not (fboundp name))))
  t t :new (:new) t t)

(deftest environment-setf-redefinition-affects-place-calls
  (let ((original (fdefinition '(setf environment-traced-place))) (cell (list nil)))
    (unwind-protect
         (progn
           (setf (fdefinition '(setf environment-traced-place))
                 (lambda (value cell) (setf (car cell) (list value)) value))
           (setf (environment-traced-place cell) :new)
           cell)
      (setf (fdefinition '(setf environment-traced-place)) original)))
  ((:new)))

(deftest environment-documentation-returns-one-value
  (let ((function (lambda () nil)) (name (list 'setf (gensym))))
    (values (multiple-value-list (documentation function t))
            (multiple-value-list (documentation name 'function))
            (progn (setf (documentation function t) "Stored")
                   (multiple-value-list (documentation function 'function)))))
  (nil) (nil) ("Stored"))

(deftest environment-universal-time-epoch
  (values (multiple-value-list (decode-universal-time 0 0))
          (encode-universal-time 0 0 0 1 1 1900 0)
          (encode-universal-time 0 0 1 4 7 1976 5))
  (0 0 0 1 1 1900 0 nil 0) 0 2414296800)

(deftest environment-gregorian-century-rules
  (values (subseq (multiple-value-list (decode-universal-time
                                        (+ 86400 (encode-universal-time 0 0 0 28 2 1900 0)) 0)) 3 7)
          (subseq (multiple-value-list (decode-universal-time
                                        (+ 86400 (encode-universal-time 0 0 0 28 2 2000 0)) 0)) 3 7))
  (1 3 1900 3) (29 2 2000 1))

(deftest environment-fractional-zone-and-second-precision
  (values (multiple-value-list (decode-universal-time
                                (encode-universal-time 59 58 23 29 2 2000 -11/2) -11/2))
          (multiple-value-list (decode-universal-time 0 1/3600)))
  (59 58 23 29 2 2000 1 nil -11/2) (59 59 23 31 12 1899 6 nil 1/3600))

(deftest environment-calendar-beyond-javascript-date-range
  (let* ((year 12345678901234567890)
         (time (encode-universal-time 12 34 5 6 7 year 0))
         (parts (multiple-value-list (decode-universal-time time 0))))
    (values (subseq parts 0 6) (nth 7 parts) (nth 8 parts)))
  (12 34 5 6 7 12345678901234567890) nil 0)

(deftest environment-current-time-and-local-roundtrip
  (let* ((time (get-universal-time)) (parts (multiple-value-list (decode-universal-time time))))
    (values (typep time '(integer 0 *)) (length parts)
            (= time (apply #'encode-universal-time (subseq parts 0 6)))
            (length (multiple-value-list (get-decoded-time)))))
  t 9 t 9)

(deftest environment-clock-and-sleep
  (let ((real (get-internal-real-time)) (run (get-internal-run-time)))
    (values (sleep 1/50) (>= (- (get-internal-real-time) real) 20)
            (>= (get-internal-run-time) run)
            (constantp 'internal-time-units-per-second)))
  nil t t t)

(deftest environment-time-preserves-multiple-values
  (let ((result nil))
    (values (not (zerop (length (with-output-to-string (*trace-output*)
                                  (setq result (multiple-value-list (time (values 1 2 3))))))))
            result))
  t (1 2 3))

(deftest environment-invalid-calendar-inputs
  (values (handler-case (encode-universal-time 0 0 0 29 2 1900 0) (error () :rejected))
          (handler-case (decode-universal-time -1 0) (type-error () :rejected))
          (handler-case (decode-universal-time 0 25) (type-error () :rejected))
          (handler-case (sleep -1) (type-error () :rejected)))
  :rejected :rejected :rejected :rejected)

(deftest environment-apropos-symbols-and-output
  (let ((pkg (make-package "ENVIRONMENT-REGRESSION" :use nil)))
    (unwind-protect
         (let ((symbol (intern "AN-EXAMPLE" pkg)) (result :unset))
           (intern "Z-EXAMPLE" pkg)
           (values (equal (apropos-list "example" pkg) (list symbol (find-symbol "Z-EXAMPLE" pkg)))
                   (not (null (search "AN-EXAMPLE"
                                      (with-output-to-string (*standard-output*)
                                        (setq result (multiple-value-list (apropos "EXAMPLE" pkg)))))))
                   result))
      (delete-package pkg)))
  t t nil)

(defclass environment-described () ())
(defmethod describe-object ((object environment-described) stream)
  (declare (ignore object)) (write-string "Custom description" stream) (values 1 2))
(deftest environment-describe-protocol
  (let ((result :unset))
    (values (with-output-to-string (out)
              (setq result (multiple-value-list (describe (make-instance 'environment-described) out))))
            result))
  "Custom description" nil)

(deftest environment-inspection-output
  (values (not (zerop (length (with-output-to-string (*standard-output*) (room)))))
          (not (zerop (length (with-output-to-string (*standard-output*) (disassemble #'car)))))
          (string= (lisp-implementation-type) "Valtan")
          (stringp (lisp-implementation-version)))
  t t t t)

(deftest environment-home-is-a-directory
  (let ((home (user-homedir-pathname)))
    (values (pathnamep home) (pathname-name home) (pathname-type home) (pathname-version home)
            (length (multiple-value-list (user-homedir-pathname)))))
  t nil nil nil 1)

(defun environment-traced-values (x) (if x (values x (1+ x)) (values)))
(declaim (notinline environment-traced-values))
(deftest environment-trace-preserves-values-and-restores-function
  (let ((original #'environment-traced-values) (result nil) (zero-result :unset) (names nil))
    (unwind-protect
         (values (not (zerop (length (with-output-to-string (*trace-output*)
                                        (trace environment-traced-values)
                                        (setq names (trace))
                                        (setq result (multiple-value-list (environment-traced-values 4)))
                                        (setq zero-result (multiple-value-list (environment-traced-values nil)))
                                        (untrace environment-traced-values)))))
                 result zero-result names (eq #'environment-traced-values original))
      (untrace environment-traced-values)))
  t (4 5) nil (environment-traced-values) t)

(defgeneric environment-traced-generic (x))
(defmethod environment-traced-generic ((x t)) :general)
(declaim (notinline environment-traced-generic))
(deftest environment-trace-survives-generic-method-changes
  (unwind-protect
       (let ((value nil))
         (values (not (zerop (length (with-output-to-string (*trace-output*)
                                        (trace environment-traced-generic)
                                        (eval '(defmethod environment-traced-generic ((x integer)) :integer))
                                        (setq value (environment-traced-generic 1))))))
                 value (typep #'environment-traced-generic 'generic-function)))
    (untrace environment-traced-generic))
  t :integer t)

(deftest environment-documentation-function-aliases
  (let ((function (lambda () nil)))
    (values (setf (documentation function t) "Function documentation")
            (documentation function 'function)
            (setf (documentation function 'function) "Updated documentation")
            (documentation function t)))
  "Function documentation" "Function documentation" "Updated documentation" "Updated documentation")

(deftest environment-documentation-class-and-method
  (let ((class (find-class 'environment-described))
        (method (find-method #'environment-traced-generic nil (list (find-class 't)))))
    (values (setf (documentation class t) "Class documentation")
            (documentation 'environment-described 'type)
            (setf (documentation method t) "Method documentation")
            (documentation method t)))
  "Class documentation" "Class documentation" "Method documentation" "Method documentation")
