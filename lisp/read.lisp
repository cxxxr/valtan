(in-package :common-lisp)

#+(or)
(defun peek-char (&optional peek-type (stream *standard-input*) (eof-error-p t) eof-value
                            recursive-p)
  )

#+(or)
(defun read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (let ((c (peek-char t stream eof-error-p eof-value recursive-p)))
    ))

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
