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
(defvar *error-output* (make-standard-output-stream))

(defun flush (stream)
  (let ((x (standard-output-stream-buffer stream)))
    (when (< 0 (length x))
      (ffi:console.log (system::array-to-js-string x))
      (setf (standard-output-stream-buffer stream) ""))))

(defun stream-write-char (stream char)
  (unless (characterp char)
    (type-error char 'character))
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
         (type-error stream 'output-stream)))
  char)

(defun stream-write-string (stream string)
  (unless (stringp string)
    (type-error string 'string))
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
         (type-error stream 'output-stream)))
  string)

(defmacro with-output-to-string ((var &optional string #|&key element-type|#) &body body)
  (declare (ignore string #|element-type|#))
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

(defun terpri (&optional (stream *standard-output*))
  (write-char #\newline stream))

(defstruct (string-input-stream (:copier nil)
                                (:constructor %make-string-input-stream))
  string
  position
  end)

(defun make-string-input-stream (string &optional (start 0) end)
  (%make-string-input-stream :string string
                             :position start
                             :end (or end (length string))))

(defun string-input-stream-eof-p (stream)
  (>= (string-input-stream-position stream)
      (string-input-stream-end stream)))

(defstruct (standard-input-stream (:copier nil)))

(defvar *standard-input* (make-standard-input-stream))

(defun stream-read-line (stream)
  (cond ((string-input-stream-p stream)
         (if (string-input-stream-eof-p stream)
             (values "" nil)
             (let ((start (string-input-stream-position stream)))
               (do ((i (string-input-stream-position stream) (1+ i)))
                   ((>= i (string-input-stream-end stream))
                    (setf (string-input-stream-position stream) i)
                    (values (subseq (string-input-stream-string stream) start) nil))
                 (when (char= #\newline
                              (aref (string-input-stream-string stream)
                                    i))
                   (setf (string-input-stream-position stream) (1+ i))
                   (return (values (subseq (string-input-stream-string stream)
                                           start i)
                                   t)))))))
        ((standard-input-stream-p stream)
         (system::get-line stream))
        (t
         (type-error stream 'input-stream))))

(defun stream-read-char (stream)
  (cond ((string-input-stream-p stream)
         (if (string-input-stream-eof-p stream)
             :eof
             (prog1 (aref (string-input-stream-string stream)
                          (string-input-stream-position stream))
               (incf (string-input-stream-position stream)))))
        ((standard-input-stream-p stream)
         )
        (t
         (type-error stream 'input-stream))))
