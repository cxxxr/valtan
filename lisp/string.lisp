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
  (eql (array-contents (string x))
       (array-contents (string y))))

(defun string-append (x y)
  (unless (stringp x)
    (error "type error"))
  (unless (stringp y)
    (error "type error"))
  (system::js-string-to-array
   ((ffi:ref (array-contents x) "concat")
    (array-contents y))))
