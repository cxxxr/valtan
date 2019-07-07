(in-package :common-lisp)

(defun char-downcase (char)
  (if (char<= #\A char #\Z)
      (code-char (+ (- (char-code char) (char-code #\A))
                    (char-code #\a)))
      char))

(defun char-upcase (char)
  (if (char<= #\a char #\z)
      (code-char (+ (- (char-code char) (char-code #\a))
                    (char-code #\A)))
      char))
