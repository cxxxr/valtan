(in-package :cl-test)

(deftest escaped-colon-in-symbol
  (let ((symbol (read-from-string "prefix\\:suffix")))
    (values (symbol-name symbol) (eq (symbol-package symbol) *package*)))
  "PREFIX:SUFFIX" t)

(deftest escaped-keyword-colon
  (let ((symbol (read-from-string ":a\\:b")))
    (values (symbol-name symbol) (keywordp symbol)))
  "A:B" t)

(deftest multiple-escape-with-suffix
  (symbol-name (read-from-string "|foo|bar"))
  "fooBAR")

(deftest escaped-numeric-symbol
  (let ((symbol (read-from-string "#:\\1")))
    (values (symbol-name symbol) (symbol-package symbol)))
  "1" nil)

(deftest escaped-dot-in-list
  (mapcar #'symbol-name (read-from-string "(\\. |.|)"))
  ("." "."))

(deftest invert-preserves-escaped-case
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :invert)
    (values (symbol-name (read-from-string "FOO\\b"))
            (symbol-name (read-from-string "foo\\B"))
            (symbol-name (read-from-string "FoO\\B"))))
  "foob" "FOOB" "FoOB")

(deftest qualified-escaped-name
  (eq (read-from-string "CL:|CAR|") 'car)
  t)

(deftest nested-block-comments
  (values (read-from-string "#||#1")
          (read-from-string "#|#||#|#2")
          (read-from-string "#|a #|b|# c|#3")
          (read-from-string "#|#|x|##|y|#|#4"))
  1 2 3 4)
