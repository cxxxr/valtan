(in-package :common-lisp)

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
             ()
           (let ((c (stream-peek-char stream)))
             (cond ((eq c :eof)
                    (if eof-error-p
                        (eof-error)
                        (return eof-value)))
                   ((if (eq peek-type t)
                        (not (find c '(#\space #\tab #\newline)))
                        (eql c peek-type))
                    (return c))
                   (t
                    (stream-read-char stream))))))))

(defstruct (readtable (:predicate readtablep))
  (case :upcase)
  (table (make-hash-table)))

(defvar *readtable* (make-readtable))

(defun get-macro-character (char &optional (readtable *readtable*))
  (let ((value (gethash char (readtable-table readtable))))
    (if value
        (values (car value) (cdr value))
        (values nil nil))))

(defun set-macro-character (char function &optional non-terminating-p (readtable *readtable*))
  (setf (gethash char (readtable-table readtable))
        (cons function non-terminating-p))
  t)

(defun read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (let* ((inner-eof-value '#:eof)
         (c (peek-char t stream eof-error-p inner-eof-value recursive-p)))
    (cond ((eq c inner-eof-value)
           eof-value)
          (t
           (multiple-value-bind (function non-terminating-p)
               (get-macro-character c)
             (funcall function stream c))))))

(defun read-char (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((c (stream-read-char stream)))
    (if (eq c :eof)
        (if eof-error-p
            (eof-error)
            eof-value)
        c)))

(defun read-line (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (multiple-value-bind (string next-line-p)
      (stream-read-line stream)
    (if (and (string= string "") (not next-line-p))
        (if eof-error-p
            (eof-error)
            (values eof-value t))
        (values string (not next-line-p)))))
