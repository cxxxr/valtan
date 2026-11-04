(in-package :cl-test)

(deftest format-flushes-output-line
  (format t "FORMAT-FLUSH-REGRESSION~%")
  nil)

(deftest format-iteration-type-error
  (handler-case (format nil "~{~A~}" 17)
    (type-error (condition) (type-error-datum condition)))
  17)

(deftest format-negative-padding
  (format nil "~3,,-1A ~vD" "ABCD" -123456789012345678901234567890 1234)
  "ABCD 1234")

(deftest format-function-iteration
  (format nil "~{~}" (formatter "~A") '(1 2 3))
  "123")

(deftest print-named-character
  (format nil "~S" #\Newline)
  "#\\Newline")

(deftest format-printer-controls
  (let ((*print-base* 16) (*print-radix* t) (*print-readably* t))
    (format nil "~A ~S ~D" 255 255 255))
  "#xFF #xFF 255")

(deftest print-numeric-types
  (values (write-to-string 3.0) (write-to-string #c(3 4))
          (let ((*print-base* 2) (*print-radix* t)) (write-to-string -10)))
  "3.0" "#C(3 4)" "#b-1010")

(deftest format-parameters
  (format nil "~v,'0D ~:@D ~~ ~A" 5 42 1234567 :done)
  "00042 +1,234,567 ~ DONE")

(deftest format-exact-radix
  (format nil "~X ~B" 18446744073709551616 10)
  "10000000000000000 1010")

(deftest formatter-unused-tail
  (let ((stream (make-string-output-stream)))
    (values (funcall (formatter "~A: ~D") stream 'items 3 :unused)
            (get-output-stream-string stream)))
  (:unused) "ITEMS: 3")

(deftest format-function-control
  (format nil (formatter "~{~D~^, ~}") '(1 2 3))
  "1, 2, 3")

(deftest format-conditionals
  (format nil "~[zero~;one~:;many~] ~:[no~;yes~] ~@[~A~]" 8 t "present")
  "many yes present")

(deftest format-recursion-and-argument-position
  (format nil "~A ~:*~A ~? ~A" :x "~A~A" '(1 2) :last)
  "X X 12 LAST")

(deftest format-sublist-iteration
  (format nil "~:{(~A ~A)~}" '((1 2) (3 4)))
  "(1 2)(3 4)")

(deftest format-remaining-arguments
  (format nil "~@{~D~^/~}" 1 2 3)
  "1/2/3")

(deftest format-case-and-plural
  (format nil "~:(~A~): ~D item~:P, ~D categor~:@P" "hello WORLD" 1 2)
  "Hello World: 1 item, 2 categories")

(deftest format-stream-return-and-column
  (let ((stream (make-string-output-stream)))
    (write-string "a" stream)
    (values (format stream "~&b~4Tc~%~&") (get-output-stream-string stream)))
  nil "a
b   c
")

(deftest format-justification
  (format nil "~10<~A~;~A~>" "left" "right")
  "left right")

(deftest format-character-modifiers
  (format nil "~:C ~@C ~C" #\Space #\a #\b)
  "Space #\\a b")

(deftest format-english-and-roman
  (format nil "~R; ~:R; ~@R; ~:@R" 42 23 1994 9)
  "forty-two; twenty-third; MCMXCIV; VIIII")

(deftest format-fixed-and-money
  (format nil "~8,2F ~,2F ~10,2,2F ~8,2,0,'*F ~2,3,9,'0$" 3.5 3.5 3.5 123456789.0 3.5)
  "    3.50 3.50     350.00 ******** 000003.50")

(deftest format-string-destination
  (let ((destination (make-array 3 :element-type 'character :initial-contents "abc"
                                  :fill-pointer 1 :adjustable t)))
    (values (format destination "~A~D" "xy" 3) (coerce destination 'list)))
  nil (#\a #\x #\y #\3))
