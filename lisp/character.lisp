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

(defun %alpha-to-digit (char)
  (+ 10 (- (char-code (char-downcase char))
           (char-code #\a))))

(defun %digit-char-to-digit (char)
  (when (char<= #\0 char #\9)
    (- (char-code char)
       (char-code #\0))))

(defun digit-char-p (char &optional (radix 10))
  (assert (<= 2 radix 36))
  (let ((digit (if (or (char<= #\a char #\z)
                       (char<= #\A char #\Z))
                   (%alpha-to-digit char)
                   (%digit-char-to-digit char))))
    (when (and digit (< -1 digit radix))
      digit)))

(defun char-cmp-ignore-case (cmp character characters)
  (dolist (c characters t)
    (unless (funcall cmp
                     (char-code (char-upcase character))
                     (char-code (char-upcase c)))
      (return nil))
    (setq character c)))

(defun char-cmp (cmp character characters)
  (dolist (c characters t)
    (unless (funcall cmp (char-code character) (char-code c))
      (return nil))
    (setq character c)))

(defun char= (character &rest characters)
  (char-cmp #'= character characters))

(defun char< (character &rest characters)
  (char-cmp #'< character characters))

(defun char> (character &rest characters)
  (char-cmp #'> character characters))

(defun char<= (character &rest characters)
  (char-cmp #'<= character characters))

(defun char>= (character &rest characters)
  (char-cmp #'>= character characters))

(defun char-equal (character &rest characters)
  (char-cmp-ignore-case #'= character characters))

(defun char-lessp (character &rest characters)
  (char-cmp-ignore-case #'< character characters))

(defun char-greaterp (character &rest characters)
  (char-cmp-ignore-case #'> character characters))

(defun char-not-lessp (character &rest characters)
  (char-cmp-ignore-case #'>= character characters))

(defun char-not-greaterp (character &rest characters)
  (char-cmp-ignore-case #'<= character characters))

(defun char/= (character &rest characters)
  (do ((c character (pop characters)))
      ((null characters) t)
    (when (member (char-code c) characters :test #'= :key #'char-code)
      (return nil))))

(defun char-not-equal (character &rest characters)
  (do ((c character (pop characters)))
      ((null characters) t)
    (when (member (char-code (char-upcase c))
                  characters
                  :test #'/=
                  :key (lambda (char) (char-code (char-upcase char))))
      (return nil))))
