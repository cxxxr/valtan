(in-package :cl-test)

(deftest syntax-copy-does-not-alias-dispatch-tables
  (let* ((*readtable* (copy-readtable nil))
         (source (copy-readtable nil)))
    (set-syntax-from-char #\! #\# *readtable* source)
    (set-dispatch-macro-character #\! #\Q (lambda (s c n) (declare (ignore s c n)) :copied))
    (values (read-from-string "!q")
            (get-dispatch-macro-character #\# #\Q source)))
  :copied nil)

(deftest syntax-copy-roundtrip-whitespace-and-escape
  (let ((*readtable* (copy-readtable nil)))
    (set-syntax-from-char #\! #\Space)
    (set-syntax-from-char #\? #\\)
    (let ((copy (copy-readtable)))
      (set-syntax-from-char #\! #\X)
      (let ((*readtable* copy))
        (values (symbol-name (read-from-string "!!?aBC"))
                (read-from-string "12!34")))))
  "aBC" 12)

(deftest preserving-whitespace-recursive-read-inherits-control
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\! (lambda (s c) (declare (ignore c)) (read s t nil t)))
    (with-input-from-string (s "!12 x")
      (values (read-preserving-whitespace s) (read-char s))))
  12 #\Space)

(deftest preserving-whitespace-nonrecursive-read-resets-control
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\! (lambda (s c) (declare (ignore c)) (read s)))
    (with-input-from-string (s "!12 x")
      (values (read-preserving-whitespace s) (read-char s))))
  12 #\x)

(deftest reader-error-retains-stream-information
  (with-input-from-string (s ".."); invalid token
    (handler-case (read s)
      (reader-error (c) (streamp (stream-error-stream c)))))
  t)

(deftest read-delimited-list-skips-comments-at-delimiter
  (with-input-from-string (s "1 #|comment|# ] 9")
    (values (read-delimited-list #\] s) (read s)))
  (1) 9)

(deftest string-single-escape-removes-itself
  (read-from-string (coerce '(#\" #\\ #\a #\\ #\# #\") 'string))
  "a#" 6)

(deftest read-suppressed-structure-does-not-resolve-name
  (let ((*read-suppress* t))
    (read-from-string "#S(no-such-structure :a invalid)"))
  nil 32)

(deftest readable-symbol-names-escape-reader-syntax
  (let ((*package* (find-package :cl-test)))
    (mapcar (lambda (name)
              (let ((symbol (make-symbol name)))
                (symbol-name (read-from-string (write-to-string symbol :readably t)))))
            '(":" "1" "." "a|b" "a\\b")))
  (":" "1" "." "a|b" "a\\b"))

(deftest parse-integer-whitespace-independent-of-readtable
  (let ((*readtable* (copy-readtable nil)))
    (set-syntax-from-char #\Space #\X)
    (set-syntax-from-char #\! #\Space)
    (values (parse-integer " 12 ")
            (handler-case (parse-integer "!12") (parse-error () :invalid))))
  12 :invalid)
