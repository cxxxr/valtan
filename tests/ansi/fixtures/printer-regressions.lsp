(in-package :cl-test)

(deftest write-keyword-controls
  (write-to-string '(15 16) :base 16 :radix t)
  "(#xF #x10)")

(deftest print-level-and-length
  (values (write-to-string '(1 (2 3) 4) :level 1)
          (write-to-string '(1 2 3) :length 2)
          (write-to-string '(1 2 . 3) :length 2)
          (write-to-string #(1 2 3) :length 1))
  "(1 # 4)" "(1 2 ...)" "(1 2 . 3)" "#(1 ...)")

(deftest print-readably-ignores-limits
  (values (write-to-string '(1 (2 3)) :readably t :level 0 :length 0)
          (write-to-string "abc" :level 0 :length 0)
          (write-to-string #*101 :level 0 :length 0))
  "(1 (2 3))" "\"abc\"" "#*101")

(deftest write-returns-object
  (let ((object (list :a :b)) (stream (make-string-output-stream)))
    (eq (write object :stream stream) object))
  t)

(deftest write-circle-keyword
  (let ((object (list 1)))
    (setf (cdr object) object)
    (let ((*print-circle* nil))
      (let ((read-back (read-from-string (write-to-string object :circle t :base 16 :radix t))))
        (values (car read-back) (eq read-back (cdr read-back))))))
  1 t)

(deftest princ-disables-readably
  (let ((*print-readably* t)) (princ-to-string "abc"))
  "abc")

(deftest dispatch-infix-uses-decimal-under-every-input-base
  (let ((*readtable* (copy-readtable nil)))
    (make-dispatch-macro-character #\!)
    (set-dispatch-macro-character #\! #\A
      (lambda (stream sub-character argument)
        (declare (ignore stream sub-character))
        argument))
    (loop for base in '(2 8 16 36)
          collect (let ((*read-base* base))
                    (list (read-from-string "#16r10") (read-from-string "!12a")))))
  ((16 12) (16 12) (16 12) (16 12)))

(deftest rational-radix-printing-roundtrips-in-every-base
  (loop for base from 2 to 36
        nconc (loop for value in '(0 1 -17 49683528 9007199254740993 2/3)
                    for text = (write-to-string value :base base :radix t :readably t)
                    for actual = (let ((*read-base* base)) (read-from-string text))
                    unless (eql value actual)
                      collect (list base value text actual)))
  nil)

(deftest readable-symbols-ignore-readtable-case-and-escape-radix-digits
  (let ((*readtable* (copy-readtable nil)) (*print-base* 36) (*read-base* 36))
    (setf (readtable-case *readtable*) :invert)
    (let* ((object (intern "mixedCase" :cl-test))
           (text (write-to-string (list object nil 'car) :readably t)))
      (let ((*readtable* (copy-readtable nil)))
        (equal (read-from-string text) (list object nil 'car)))))
  t)

(deftest named-control-characters-round-trip
  (let ((character (code-char 0)))
    (values (eql character (name-char (char-name character)))
            (eql character (read-from-string (write-to-string character)))
            (eql character (read-from-string (format nil "~@C" character)))))
  t t t)
