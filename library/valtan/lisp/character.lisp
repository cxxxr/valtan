(in-package :common-lisp)

(defparameter char-code-limit 1114112)

(defparameter *char-name-table*
  '((#\newline . "Newline")
    (#\space . "Space")
    (#\rubout . "Rubout")
    (#\page . "Page")
    (#\tab . "Tab")
    (#\backspace . "Backspace")
    (#\return . "Return")
    (#\linefeed . "Linefeed")))

(defun code-char (code)
  (unless (and (integerp code)
               (<= 0 code))
    (type-error code '(integer 0 *)))
  (*:%code-char code))

(defun char-code (char)
  (unless (characterp char)
    (type-error char 'character))
  (*:%char-code char))

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
                  :test #'=
                  :key (lambda (char) (char-code (char-upcase char))))
      (return nil))))

(defun character (x)
  (flet ((type-error ()
           (type-error x '(or character (array character (1))))))
    (cond ((characterp x) x)
          ((stringp x)
           (if (= 1 (length x))
               (char x 0)
               (type-error)))
          ((symbolp x)
           (if (= 1 (length (symbol-name x)))
               (char (symbol-name x) 0)
               (type-error)))
          (t
           (type-error)))))

(defun alpha-char-p (c)
  (char<= #\a (char-downcase c) #\z))

(defun digit-char (weight &optional (radix 10))
  (and (>= weight 0)
       (< weight radix)
       (< weight 36)
       (code-char (if (< weight 10)
                      (+ 48 weight)
                      (+ 55 weight)))))

(defun char-int (char) (char-code char))

(defun alphanumericp (c)
  (or (alpha-char-p c)
      (char<= #\0 c #\9)))

(defun graphic-char-p (c)
  (setq c (char-code c))
  (or (<= 32 c 126)
      (<= 160 c)))

(defun standard-char-p (c)
  (setq c (char-code c))
  (or (<= 32 c 126)
      (= c 10)))

(defun upper-case-p (c)
  (char<= #\A c #\Z))

(defun lower-case-p (c)
  (char<= #\a c #\z))

(defun both-case-p (c)
  (or (upper-case-p c)
      (lower-case-p c)))

(defun char-name (c)
  (unless (characterp c) (type-error c 'character))
  (cdr (assoc c *char-name-table*)))

(defun name-char (name)
  (unless (stringp name) (type-error name 'string))
  (car (rassoc name *char-name-table* :test #'equal)))
