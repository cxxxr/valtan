(in-package :cl-user)

#|
(ffi:console.log (char= #\a #\newline))

(write-line "..........")
(write-char #\a)
(write-char #\b)
(terpri)
|#

#|
(write-line "---------------------")
(print (find-package "COMMON-LISP"))
(write-line "---------------------")
(print (symbol-package t))

(let ((char #\/))
  ((ffi:ref "console.log") char)
  (ffi:console.log
   (or (char<= #\a char #\z)
       (char<= #\A char #\Z))))

(print (digit-char-p #\/))
|#
