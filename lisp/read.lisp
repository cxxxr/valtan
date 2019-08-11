(in-package :common-lisp)

(defvar *inner-list-p* nil)
(defvar *dot-marker* (gensym "DOT"))
(defvar *read-label-table*)
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
  (make-dispatch-macro-character #\# t readtable)
  (set-dispatch-macro-character #\# #\\ 'read-sharp-backslash)
  (set-dispatch-macro-character #\# #\' 'read-sharp-quote)
  (set-dispatch-macro-character #\# #\( 'read-sharp-left-paren)
  (set-dispatch-macro-character #\# #\: 'read-sharp-colon)
  (set-dispatch-macro-character #\# #\+ 'read-sharp-plus-minus)
  (set-dispatch-macro-character #\# #\- 'read-sharp-plus-minus)
  (set-dispatch-macro-character #\# #\= 'read-sharp-equal)
  (set-dispatch-macro-character #\# #\# 'read-sharp-sharp)
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

(defun make-dispatch-macro-character (char &optional non-terminating-p (readtable *readtable*))
  (setf (gethash char (readtable-dispatch-macro-table readtable))
        (make-hash-table))
  (set-macro-character char 'read-dispatch-macro-character non-terminating-p readtable)
  t)

(defun get-dispatch-table (readtable disp-char)
  (let ((dispatch-table (gethash disp-char (readtable-dispatch-macro-table readtable))))
    (unless dispatch-table
      (error "~S is not a dispatching macro character." disp-char))
    dispatch-table))

(defun set-dispatch-macro-character (disp-char sub-char function &optional (readtable *readtable*))
  (let ((dispatch-table (get-dispatch-table readtable disp-char)))
    (when (digit-char-p sub-char)
      (error "SUB-CHAR must not be a decimal digit: ~S" sub-char))
    (setf (gethash (char-upcase sub-char) dispatch-table)
          function)
    t))

(defun get-dispatch-macro-character (disp-char sub-char &optional (readtable *readtable*))
  (let ((dispatch-table (get-dispatch-table readtable disp-char)))
    (values (gethash (char-upcase sub-char) dispatch-table))))

(defun non-terminate-macro-character-p (c)
  (multiple-value-bind (function non-terminating-p)
      (get-macro-character c)
    (and function (not non-terminating-p))))

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
               ((or (not external-p)
                    (string= package-name "JS"))
                (intern symbol-name package-name))
               (t
                (multiple-value-bind (symbol status)
                    (find-symbol symbol-name package-name)
                  (if (eq status :external)
                      (intern symbol-name package-name)
                      (error "Symbol ~S not found in the ~A package."
                             symbol-name package-name))))))))
    (let ((pos (position #\: token)))
      (cond ((null pos)
             (intern token))
            ((= pos 0)
             (f :keyword (subseq token 1) nil))
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
         (js:parse-float token))
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

(defun delimiter-p (c)
  (or (null c)
      (whitespacep c)
      (non-terminate-macro-character-p c)
      (char= c #\\)
      (char= c #\|)))

(defun maybe-invert (token)
  (when (eq :invert (readtable-case *readtable*))
    (let ((state nil))
      (dotimes (i (length token)
                  (setq token
                        (ecase state
                          ((nil))
                          (:upcase
                           (string-downcase token))
                          (:downcase
                           (string-upcase token)))))
        (let ((c (aref token i)))
          (cond ((char<= #\a c #\z)
                 (when (eq state :upcase)
                   (return))
                 (setq state :downcase))
                ((char<= #\A c #\Z)
                 (when (eq state :downcase)
                   (return))
                 (setq state :upcase)))))))
  token)

(defun read-token-1 (stream c readtable-case)
  (maybe-invert
   (with-output-to-string (out)
     (do ((c c (read-char stream nil nil)))
         (nil)
       (cond ((null c)
              (return))
             ((whitespacep c)
              (return))
             ((non-terminate-macro-character-p c)
              (unread-char c stream)
              (return))
             ((char= c #\\)
              (write-char (read-char stream) out))
             ((char= c #\|)
              (write-string (read-multiple-escape-1 stream) out))
             (t
              (write-char (case readtable-case
                            (:upcase (char-upcase c))
                            (:downcase (char-downcase c))
                            (otherwise c))
                          out)))))))

(defun read-token (stream c)
  (let ((token (read-token-1 stream c (readtable-case *readtable*))))
    (parse-token token)))

(defun read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (let ((*read-label-table* (if recursive-p *read-label-table* (make-hash-table))))
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
                    (return (read-token stream c)))))))))))

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
               (let ((x (read stream t nil t)))
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
  (peek-char #\newline stream t nil t)
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
         (arg (when (digit-char-p sub-char)
                (js:parse-float
                 (with-output-to-string (out)
                   (write-char sub-char out)
                   (do ((c (peek-char nil stream t nil t) (read-char stream t nil t)))
                       (nil)
                     (if (digit-char-p c)
                         (write-char c out)
                         (return))))))))
    (when arg
      (setq sub-char (read-char stream t nil t)))
    (let ((fn (get-dispatch-macro-character disp-char sub-char)))
      (unless fn
        (error "no dispatch function defined for ~S" sub-char))
      (funcall fn stream sub-char arg))))

(defun read-sharp-backslash (stream sub-char arg)
  (declare (ignore sub-char arg))
  (let ((char (read-char stream t nil t))
        (next-char (peek-char nil stream nil nil t)))
    (if (delimiter-p next-char)
        char
        (let ((name (read-token-1 stream char nil)))
          (cond ((string-equal name "newline")
                 #\newline)
                ((string-equal name "space")
                 #\space)
                ((string-equal name "rubout")
                 #\rubout)
                ((string-equal name "page")
                 #\page)
                ((string-equal name "tab")
                 #\tab)
                ((string-equal name "backspace")
                 #\backspace)
                ((string-equal name "return")
                 #\return)
                ((string-equal name "linefeed")
                 #\linefeed)
                (t
                 (error "unrecognized character name: ~S" name)))))))

(defun read-sharp-quote (stream sub-char arg)
  (declare (ignore sub-char arg))
  (list 'function (read stream t nil t)))

(defun read-sharp-left-paren (stream sub-char arg)
  (declare (ignore sub-char arg))
  (apply #'vector (read-delimited-list #\) stream t)))

(defun read-sharp-colon (stream sub-char arg)
  (declare (ignore sub-char arg))
  (let ((token (read-token-1 stream
                             (read-char stream t nil t)
                             (readtable-case *readtable*))))
    (when (number-string-p token)
      (error "The symbol following #: has numeric syntax: ~S" token))
    (make-symbol token)))

(defun featurep (test)
  (cond ((consp test)
         (case (first test)
           ((:not not)
            (cond
              ((cddr test)
               (error "too many subexpressions in feature expression: ~S" test))
              ((null (cdr test))
               (error "too few subexpressions in feature expression: ~S" test))
              (t (not (featurep (cadr test))))))
           ((:and and)
            (every #'featurep (rest test)))
           ((:or or)
            (some #'featurep (rest test)))
           (otherwise
            (error "unknown operator in feature expression: ~S." test))))
        ((symbolp test)
         (not (null (member test *features* :test #'string=))))
        (t
         (error "invalid feature expression: ~S" test))))

(defun read-sharp-plus-minus (stream sub-char arg)
  (declare (ignore arg))
  ;; TODO: testがnilの場合は*read-suppress*をnilにしてreadする必要がある
  (let ((test (let ((*keyword* (find-package :keyword)))
                (read stream t nil t)))
        (form (read stream t nil t)))
    (if (char= sub-char (if (featurep test) #\+ #\-))
        form
        (values))))

(defun read-sharp-equal (stream sub-char arg)
  (unless arg
    (error "Reader dispatch macro character #\= requires an argument."))
  (let ((form (read stream t nil t)))
    (multiple-value-bind (form defined)
        (gethash arg *read-label-table*)
      (when defined
        (error "Multiply defined label: #~D=" arg)))
    (setf (gethash arg *read-label-table*) form)))

(defun read-sharp-sharp (stream sub-char arg)
  (unless arg
    (error "Reader dispatch macro character #\# requires an argument."))
  (multiple-value-bind (form defined)
      (gethash arg *read-label-table*)
    (unless defined
      (error "Reference to undefined label #~D#" arg))
    form))

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

(defun read-delimited-list (char &optional (stream *standard-input*) recursive-p)
  (let ((head nil)
        (tail nil))
    (do () (nil)
      (let ((c (peek-char t stream t nil recursive-p)))
        (cond ((char= c #\))
               (read-char stream t nil recursive-p)
               (return))
              (t
               (let ((x (read stream t nil recursive-p)))
                 (cond ((null tail)
                        (setf head (setf tail (list x))))
                       (t
                        (setf (cdr tail) (list x))
                        (setf tail (cdr tail)))))))))
    head))

(copy-readtable nil *readtable*)
