(in-package :common-lisp)

(defstruct (string-output-stream (:copier nil)
                                 (:constructor %make-string-output-stream))
  (buffer ""))

(defun make-string-output-stream (&key element-type)
  (declare (ignore element-type))
  (%make-string-output-stream :buffer ""))

(defun get-output-stream-string (stream)
  (let ((string (string-output-stream-buffer stream)))
    (setf (string-output-stream-buffer stream) "")
    string))

(defstruct (standard-output-stream (:copier nil))
  (buffer ""))

(defvar *standard-output* (make-standard-output-stream))

(defun flush (stream)
  (let ((x (standard-output-stream-buffer stream)))
    (ffi:console.log (array-contents x))
    (setf (standard-output-stream-buffer stream) "")))

(defun stream-write-char (stream char)
  (unless (characterp char)
    (error "type error"))
  (cond ((string-output-stream-p stream)
         (setf (string-output-stream-buffer stream)
               (string-append (string-output-stream-buffer stream)
                              (string char)))
         char)
        ((standard-output-stream-p stream)
         (if (char= char #\newline)
             (flush stream) ; console.logが改行もしてしまうのでchar自体は出力しない
             (setf (standard-output-stream-buffer stream)
                   (string-append (standard-output-stream-buffer stream)
                                  (string char)))))
        (t
         (error "stream-write-char trap")))
  char)

(defun stream-write-string (stream string)
  (unless (stringp string)
    (error "type error"))
  (cond ((string-output-stream-p stream)
         (setf (string-output-stream-buffer stream)
               (string-append (string-output-stream-buffer stream)
                              string))
         string)
        ((standard-output-stream-p stream)
         (setf (standard-output-stream-buffer stream)
               (string-append (standard-output-stream-buffer stream)
                              string)))
        (t
         (error "stream-write-string trap")))
  string)

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

(defun finish-output (&optional (stream *standard-output*))
  (flush stream))
