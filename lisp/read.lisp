(in-package :common-lisp)

(defvar *inner-list-p* nil)
(defvar *dot-marker* (gensym "DOT"))

(defparameter *whitespaces* '(#\space #\tab #\newline #\linefeed #\page #\return))

(defun whitespacep (c)
  (find c *whitespaces*))

(defun peek-char (&optional peek-type (stream *standard-input*) (eof-error-p t) eof-value
                            recursive-p)
  (cond ((null peek-type)
         (let ((c (stream-peek-char stream)))
           (if (eq c :eof)
               (if eof-error-p
                   (eof-error)
                   eof-value)
               c)))
        (t
         (do ()
             (nil)
           (let ((c (stream-peek-char stream)))
             (cond ((eq c :eof)
                    (if eof-error-p
                        (eof-error)
                        (return eof-value)))
                   ((if (eq peek-type t)
                        (not (whitespacep c))
                        (eql c peek-type))
                    (return c))
                   (t
                    (stream-read-char stream))))))))

(defstruct (readtable (:predicate readtablep)
                      (:copier %copy-readtable))
  (case :upcase)
  (table (make-hash-table))
  (dispatch-macro-table (make-hash-table)))

(defvar *readtable* (make-readtable))

(defun init-readtable (readtable)
  (set-macro-character #\( 'read-list nil readtable)
  (set-macro-character #\) 'read-right-paren nil readtable)
  (set-macro-character #\' 'read-quote nil readtable)
  (set-macro-character #\` 'read-quasiquote nil readtable)
  (set-macro-character #\, 'read-unquote nil readtable)
  (set-macro-character #\; 'read-line-comment nil readtable)
  (set-macro-character #\" 'read-string nil readtable)
  (set-macro-character #\# 'read-dispatch-macro-character t readtable)
  readtable)

(defun set-readtable (to-readtable from-readtable)
  (setf (readtable-case to-readtable) (readtable-case from-readtable))
  (setf (readtable-table to-readtable) (readtable-table from-readtable))
  (setf (readtable-dispatch-macro-table to-readtable)
        (readtable-dispatch-macro-table from-readtable))
  from-readtable)

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  (cond ((and from-readtable to-readtable)
         (set-readtable to-readtable from-readtable))
        (from-readtable
         (%copy-readtable from-readtable))
        (to-readtable
         (init-readtable (set-readtable to-readtable (make-readtable))))
        (t
         (init-readtable (make-readtable)))))

(defun get-macro-character (char &optional (readtable *readtable*))
  (let ((value (gethash char (readtable-table readtable))))
    (if value
        (values (car value) (cdr value))
        (values nil nil))))

(defun set-macro-character (char function &optional non-terminating-p (readtable *readtable*))
  (setf (gethash char (readtable-table readtable))
        (cons function non-terminating-p))
  t)

(defun terminate-macro-character-p (c)
  (multiple-value-bind (function non-terminating-p)
      (get-macro-character c)
    (and function non-terminating-p)))

(defun number-string-p (token)
  (let ((pos (case (aref token 0)
               ((#\+ #\-) 1)
               (otherwise 0)))
        (found-number-p))
    (when (and (= pos 1)
               (= 1 (length token)))
      (return-from number-string-p nil))
    (do ((i pos (1+ i)))
        ((>= i (length token)))
      (let ((c (aref token i)))
        (cond ((char<= #\0 c #\9)
               (setq found-number-p t))
              ((char= #\. c)
               (setq pos (1+ i))
               (return))
              (t
               (return-from number-string-p nil)))))
    (do ((i pos (1+ i)))
        ((>= i (length token)))
      (let ((c (aref token i)))
        (cond ((char<= #\0 c #\9)
               (setq found-number-p t))
              (t
               (return-from number-string-p nil)))))
    found-number-p))

(defun check-dot (token)
  (cond ((string= token ".")
         (unless *inner-list-p*
           (error "dot error")))
        ((not (dotimes (i (length token))
                (unless (char= #\. (aref token i))
                  (return t))))
         (error "dot error"))))

(defun parse-symbol (token)
  (flet ((f (package-name symbol-name external-p)
           (let ((package (find-package package-name)))
             (cond
               ((null package)
                (error "Package ~A does not exist." package-name))
               ((find #\: symbol-name)
                (error "too many colons after ~S name" package-name))
               (t
                ;; TODO: コロンが一つの場合にそのシンボルがexternalか確認する
                (intern symbol-name package-name))))))
    (let ((pos (position #\: token)))
      (cond ((null pos)
             (intern token))
            ((char= #\: (aref token (1+ pos)))
             (f (subseq token 0 pos)
                (subseq token (+ pos 2))
                nil))
            (t
             (f (subseq token 0 pos)
                (subseq token (1+ pos))
                t))))))

(defun parse-token (token)
  (cond ((number-string-p token)
         (ffi::parse-float token))
        (t
         (check-dot token)
         (if (string= token ".")
             *dot-marker*
             (parse-symbol token)))))

(defun read-multiple-escape-1 (stream)
  (with-output-to-string (out)
    (do ()
        (nil)
      (let ((c (read-char stream)))
        (case c
          (#\| (return))
          (#\\)
          (otherwise
           (write-char c out)))))))

(defun read-multiple-escape (stream)
  (parse-token (read-multiple-escape-1 stream)))

(defun read-token-1 (stream c)
  (with-output-to-string (out)
    (do ((c c (read-char stream nil nil)))
        (nil)
      (cond ((null c)
             (return))
            ((whitespacep c)
             (return))
            ((get-macro-character c)
             (unread-char c stream)
             (return))
            ((char= c #\\)
             (write-char (read-char stream) out))
            ((char= c #\|)
             (write-string (read-multiple-escape-1 stream) out))
            (t
             (write-char (case (readtable-case *readtable*)
                           (:upcase (char-upcase c))
                           (:downcase (char-downcase c))
                           (otherwise c))
                         out))))))

(defun read-token (stream c)
  (parse-token (read-token-1 stream c)))

(defun read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (do () (nil)
    (let* ((inner-eof-value '#:eof)
           (c (peek-char t stream eof-error-p inner-eof-value recursive-p)))
      (cond ((eq c inner-eof-value)
             (return eof-value))
            (t
             (read-char stream)
             (multiple-value-bind (function non-terminating-p)
                 (get-macro-character c)
               (cond
                 (function
                  (let ((values (multiple-value-list (funcall function stream c))))
                    (when values
                      (return (first values)))))
                 ((char= c #\|)
                  (return (read-multiple-escape stream)))
                 (t
                  (return (read-token stream c))))))))))

(defun read-list (stream c)
  (declare (ignore c))
  (let ((head nil)
        (tail nil)
        (*inner-list-p* t))
    (do () (nil)
      (let ((c (peek-char t stream t nil t)))
        (cond ((char= c #\))
               (read-char stream t nil t)
               (return))
              (t
               (let ((x (read stream)))
                 (cond ((eq x *dot-marker*)
                        (unless head (error "dot error"))
                        (setf (cdr tail) (read stream t nil t))
                        (unless (char= (peek-char t stream t nil t) #\))
                          (error "dot error"))
                        (read-char stream t nil t)
                        (return))
                       ((null tail)
                        (setf head (setf tail (list x))))
                       (t
                        (setf (cdr tail) (list x))
                        (setf tail (cdr tail)))))))))
    head))

(defun read-right-paren (stream c)
  (declare (ignore stream c))
  (error "unmatched close parenthesis"))

(defun read-quote (stream c)
  (declare (ignore c))
  (list 'quote (read stream t nil t)))

(defun read-quasiquote (stream c)
  (declare (ignore c))
  (list 'system::quasiquote (read stream t nil t)))

(defun read-unquote (stream c)
  (declare (ignore c))
  (cond ((char= #\@ (peek-char nil stream t nil t))
         (read-char stream t nil t)
         (list 'system::unquote-splicing (read stream t nil t)))
        (t
         (list 'system::unquote (read stream t nil t)))))

(defun read-line-comment (stream c)
  (declare (ignore c))
  (read-line stream t nil t)
  (values))

(defun read-string (stream c)
  (declare (ignore c))
  (with-output-to-string (out)
    (do ()
        (nil)
      (let ((c (read-char stream t nil t)))
        (case c
          (#\"
           (return))
          (#\\
           (write-char (read-char stream t nil t) out))
          (otherwise
           (write-char c out)))))))

(defun read-dispatch-macro-character (stream disp-char)
  (let* ((sub-char (char-upcase (read-char stream t nil t)))
         (digit (digit-char-p sub-char)))
    (let ((fn (get-dispatch-macro-character disp-char sub-char)))
      (unless fn
        (error "no dispatch function defined for #\~A" sub-char)))))

(defun read-from-string (string &optional eof-error-p eof-value)
  (with-input-from-string (in string)
    (read in eof-error-p eof-value)))

(defun read-char (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((c (stream-read-char stream)))
    (if (eq c :eof)
        (if eof-error-p
            (eof-error)
            eof-value)
        c)))

(defun unread-char (character &optional (stream *standard-input*))
  (stream-unread-char stream))

(defun read-line (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (multiple-value-bind (string next-line-p)
      (stream-read-line stream)
    (if (and (string= string "") (not next-line-p))
        (if eof-error-p
            (eof-error)
            (values eof-value t))
        (values string (not next-line-p)))))

(copy-readtable nil *readtable*)
