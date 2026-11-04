(in-package :cl-test)

(deftest pretty-fill-uses-following-section
  (let ((*print-pretty* t) (*print-right-margin* 7) (*print-miser-width* nil))
    (write-to-string '(aa bb cc)))
  "(AA BB
 CC)")

(deftest pretty-linear-breaks-whole-block
  (let ((*print-pretty* t) (*print-right-margin* 7) (*print-miser-width* nil))
    (with-output-to-string (s) (pprint-linear s '(aa bb cc))))
  "(AA
 BB
 CC)")

(deftest pretty-per-line-prefix-and-raw-newline
  (let ((*print-pretty* t))
    (with-output-to-string (s)
      (pprint-logical-block (s nil :per-line-prefix "//" :suffix "!")
        (write-string "A " s) (terpri s) (write-char #\B s))))
  "//A 
//B!")

(deftest pretty-current-indentation
  (let ((*print-pretty* t) (*print-miser-width* nil))
    (with-output-to-string (s)
      (pprint-logical-block (s nil :prefix "(" :suffix ")")
        (write-char #\A s) (pprint-indent :current 2 s)
        (pprint-newline :mandatory s) (write-char #\B s))))
  "(A
    B)")

(deftest pretty-tabs-measure-section-origin
  (let ((*print-pretty* t))
    (with-output-to-string (s)
      (pprint-logical-block (s nil :prefix "[")
        (write-char #\A s) (pprint-tab :section-relative 1 4 s)
        (write-char #\B s))))
  "[A   B")

(deftest pretty-dispatch-copies-and-priorities
  (let* ((*print-pretty* t) (*print-pprint-dispatch* (copy-pprint-dispatch nil))
         (copy (copy-pprint-dispatch)))
    (set-pprint-dispatch '(eql :custom) (lambda (s x) (declare (ignore x)) (write-string "one" s)))
    (set-pprint-dispatch '(member :custom :other) (lambda (s x) (declare (ignore x)) (write-string "two" s)) 1)
    (values (write-to-string :custom)
            (let ((*print-pprint-dispatch* copy)) (write-to-string :custom))))
  "two" ":CUSTOM")

(deftest pretty-dotted-tail-and-length
  (let ((*print-pretty* t))
    (values (write-to-string '(1 2 . 3))
            (write-to-string '(1 2 3) :length 2)))
  "(1 2 . 3)" "(1 2 ...)")

(deftest pretty-shared-object-labels
  (let* ((*print-pretty* t) (*print-circle* t) (x (list 8)))
    (with-output-to-string (s)
      (pprint-logical-block (s (list x x) :prefix "(" :suffix ")")
        (pprint-logical-block (s x :prefix "(" :suffix ")") (write 8 :stream s))
        (write-char #\Space s)
        (pprint-logical-block (s x :prefix "(" :suffix ")") (write 8 :stream s)))))
  "(#1=(8) #1#)")

(deftest pretty-level-skips-nested-body
  (let ((*print-pretty* t) (*print-level* 1) (*print-readably* nil))
    (with-output-to-string (s)
      (pprint-logical-block (s '(1) :prefix "(" :suffix ")")
        (pprint-logical-block (s '(2) :prefix "[" :suffix "]") (error "Must not run")))))
  "(#)")

(deftest pretty-disabled-keeps-length-and-dotted-tail
  (let ((*print-pretty* nil) (*print-length* 1) (*print-readably* nil))
    (with-output-to-string (s) (pprint-fill s '(a b c))))
  "(A ...)")

(deftest format-logical-block-fill-and-prefix
  (let ((*print-pretty* t) (*print-right-margin* 4) (*print-miser-width* nil))
    (format nil "~@<//~@;~@{~A~^ ~}~:@>" 1 2 3))
  "//1
//2
//3")

(deftest format-logical-block-circular-list
  (let* ((*print-pretty* t) (*print-circle* t) (x (list 0)))
    (setf (cdr x) x)
    (format nil "~:<~@{~A~^ ~}~:>" x))
  "#1=(0 . #1#)")

(deftest format-logical-block-length
  (let ((*print-pretty* t) (*print-length* 2) (*print-readably* nil))
    (format nil "~:<~@{~A~^ ~}~:>" '(1 2 3 4)))
  "(1 2 ...)")

(deftest format-logical-block-indent
  (let ((*print-pretty* t) (*print-miser-width* nil))
    (format nil "~:<M~1:I~:@_M~:>" '(m m)))
  "(M
   M)")

(deftest format-logical-block-escape-preserves-suffix
  (let ((*print-pretty* t)) (format nil "~:<~A~^xxx~:>" '(1)))
  "(1)")

(deftest nil-element-string-remains-a-string
  (let ((s (make-array 0 :element-type nil)))
    (values (stringp s) (typep s 'simple-string) (array-element-type s)
            (string-upcase s) (write-to-string s)))
  t t nil "" "\"\"")

(deftest pretty-nil-element-prefix
  (let ((*print-pretty* t))
    (with-output-to-string (s)
      (pprint-logical-block (s nil :prefix (make-array 0 :element-type nil))
        (write-char #\A s))))
  "A")

(deftest printed-string-escapes-reader-metacharacters
  (let ((s (coerce '(#\" #\\ #\A) 'string)))
    (equal (read-from-string (write-to-string s)) s))
  t)

(deftest symbol-case-retains-unconverted-letters
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (values (write-to-string '|XYZ| :escape nil :case :downcase)
            (write-to-string '|xYZ| :escape nil :case :capitalize)))
  "XYZ" "XYZ")

(deftest decimal-float-roundtrip-rounds-once
  (let ((x 8.606458298906593d-101))
    (= x (read-from-string (write-to-string x))))
  t)

(deftest nil-array-and-string-subtyping
  (values (subtypep '(array nil (*)) 'string)
          (subtypep 'simple-string '(simple-array * 1))
          (typep (make-array 0 :element-type nil) 'base-string))
  t t nil)

(deftest array-printer-limits-each-nesting-level
  (values (write-to-string #2A((1 2) (3 4)) :length 1 :readably nil :pretty nil)
          (write-to-string #2A((#2A((9)))) :level 3 :readably nil :pretty nil))
  "#2A((1 ...) ...)" "#2A((#2A(#)))")

(deftest unreadable-printer-type-separator
  (let* ((*print-readably* nil)
         (s (with-output-to-string (s) (print-unreadable-object (17 s :type t)))))
    (values (subseq s 0 2) (char s (- (length s) 2)) (char s (1- (length s)))))
  "#<" #\Space #\>)

(deftest unreadable-printer-signals-readable-request
  (let ((*print-readably* t))
    (handler-case
        (with-output-to-string (s) (print-unreadable-object (17 s)))
      (print-not-readable (c) (print-not-readable-object c))))
  17)
