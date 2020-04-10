(defpackage :valtan-host.emitter-stream
  (:use :cl :trivial-gray-streams)
  (:export :emitter-stream
           :emitter-stream-line
           :emitter-stream-column))
(in-package :valtan-host.emitter-stream)

(defclass emitter-stream (fundamental-output-stream)
  ((stream
    :initarg :stream
    :reader emitter-stream-stream)
   (line
    :initform 1
    :reader emitter-stream-line)
   (column
    :initform 0
    :reader emitter-stream-column)))

(defun next (stream character)
  (with-slots (line column) stream
    (cond ((char= character #\newline)
           (incf line)
           (setf column 0))
          (t
           (incf column)))))

(defmethod stream-write-char ((stream emitter-stream) character)
  (write-char character (emitter-stream-stream stream))
  (next stream character)
  character)

(defmethod stream-line-column ((stream emitter-stream))
  (emitter-stream-column stream))

(defmethod stream-write-string ((stream emitter-stream) string &optional start end)
  (write-string string (emitter-stream-stream stream) :start start :end end)
  (loop :for c :across string
        :do (next stream c))
  string)

(defmethod stream-terpri ((stream emitter-stream))
  (stream-write-char stream #\newline))
