(in-package :common-lisp)

(defun stringp (x)
  (and (arrayp x)
       (eq 'character (array-element-type x))))

(defun string (x)
  (cond ((stringp x)
         x)
        ((characterp x)
         (system::js-string-to-array ((ffi:ref "String" "fromCharCode") (char-code x))))
        ((symbolp x)
         (symbol-name x))
        (t
         (error "type error"))))

(defun string= (x y)
  (eql (system::array-to-js-string (string x))
       (system::array-to-js-string (string y))))

(defun string-append (x y)
  (unless (stringp x)
    (error "type error"))
  (unless (stringp y)
    (error "type error"))
  (system::js-string-to-array
   ((ffi:ref (system::array-to-js-string x) "concat")
    (system::array-to-js-string y))))
