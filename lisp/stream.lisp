(in-package :common-lisp)

(defstruct (string-output-stream (:copier nil)
                                 (:constructor %make-string-output-stream))
  buffer)

(defun make-string-output-stream (&key element-type)
  (declare (ignore element-type))
  (%make-string-output-stream :buffer ""))

(defun get-output-stream-string (stream)
  (let ((string (string-output-stream-buffer stream)))
    (setf (string-output-stream-buffer stream) "")
    string))

(defun stream-write-char (stream char)
  (unless (characterp char)
    (error "type error"))
  (cond ((string-output-stream-p stream)
         (setf (string-output-stream-buffer stream)
               ((ffi:ref (string-output-stream-buffer stream) "concat")
                (string char)))
         char)
        (t
         (error "stream-write-char trap"))))

(defun stream-write-string (stream string)
  (unless (stringp string)
    (error "type error"))
  (cond ((string-output-stream-p stream)
         (setf (string-output-stream-buffer stream)
               ((ffi:ref (string-output-stream-buffer stream) "concat")
                string))
         string)
        (t
         (error "stream-write-string trap"))))

(defmacro with-output-to-string ((var &optional string &key element-type) &body body)
  (declare (ignore string element-type))
  `(let ((,var (make-string-output-stream)))
     ,@body
     (get-output-stream-string ,var)))

(defun write-char (char &optional (stream *standard-output*))
  (stream-write-char stream char))

(defun write-string (string &optional (stream *standard-output*) &key start end)
  (stream-write-string stream string))

(defun write-line (string &optional (stream *standard-output*) &key start end)
  (stream-write-string stream string)
  (stream-write-char stream #\newline))
