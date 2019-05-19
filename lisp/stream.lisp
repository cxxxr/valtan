(in-package :common-lisp)

(defstruct (string-output-stream (:copier nil)
                                 (:predicate nil)
                                 (:constructor %make-string-output-stream))
  )

(defun make-string-output-stream (&key element-type)
  (declare (ignore element-type))
  (%make-string-output-stream))

(defun get-output-stream-string (stream)
  )

(defmacro with-output-to-string ((var &optional string &key element-type) &body body)
  )

(defun write-char (char &optional (stream *standard-output*))
  )

(defun write-string (string &optional (stream *standard-output*) &key start end)
  )

(defun write-line (string &optional (stream *standard-output*) &key start end)
  )
