#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defvar *:*get-stdin-line-function*)

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

(defun compute-text-column (text)
  (let ((pos (position #\newline text :from-end t)))
    (if pos
        (- (length text) pos 1)
        (length text))))

(defun stream-column (stream)
  (cond ((string-output-stream-p stream)
         (compute-text-column (string-output-stream-buffer stream)))
        ((standard-output-stream-p stream)
         (compute-text-column (standard-output-stream-buffer stream)))
        (t
         (type-error stream 'output-stream))))

(defun flush (stream)
  (when (standard-output-stream-p stream)
    (let ((x (standard-output-stream-buffer stream)))
      (when (< 0 (length x))
        (system:write-raw-string-to-stdout (*:array-to-raw-string x))
        (setf (standard-output-stream-buffer stream) "")))))

(defun stream-write-char (stream char)
  (unless (characterp char)
    (type-error char 'character))
  (cond ((string-output-stream-p stream)
         (setf (string-output-stream-buffer stream)
               (*:string-append (string-output-stream-buffer stream)
                                      (string char)))
         char)
        ((standard-output-stream-p stream)
         (setf (standard-output-stream-buffer stream)
               (*:string-append (standard-output-stream-buffer stream)
                                (string char)))

         (when (char= char #\newline)
           (flush stream)))
        (t
         (type-error stream 'output-stream)))
  char)

(defun stream-write-string (stream string)
  (unless (stringp string)
    (type-error string 'string))
  (cond ((string-output-stream-p stream)
         (setf (string-output-stream-buffer stream)
               (*:string-append (string-output-stream-buffer stream)
                                      string))
         string)
        ((standard-output-stream-p stream)
         (setf (standard-output-stream-buffer stream)
               (*:string-append (standard-output-stream-buffer stream)
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
  (declare (ignore start end)
           #+(and (not valtan) sbcl)(sb-ext:muffle-conditions cl:warning))
  (stream-write-string stream string))

(defun write-line (string &optional (stream *standard-output*) &key start end)
  (declare (ignore start end)
           #+(and (not valtan) sbcl)(sb-ext:muffle-conditions cl:warning))
  (stream-write-string stream string)
  (stream-write-char stream #\newline)
  string)

(defun finish-output (&optional (stream *standard-output*))
  (flush stream)
  nil)

(defun force-output (&optional (stream *standard-output*))
  (flush stream)
  nil)

(defun terpri (&optional (stream *standard-output*))
  (write-char #\newline stream))

(defun fresh-line (&optional (stream *standard-output*))
  (when (< 0 (stream-column stream))
    (terpri stream)))

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

(defstruct file-input-stream
  string-stream
  (line 1)
  (column 0))

(defstruct (standard-input-stream (:copier nil))
  (string "")
  (position 0))

(defun empty-buffer-p (stream)
  (<= (length (standard-input-stream-string stream))
      (standard-input-stream-position stream)))

(defun fetch-stdin-line (stream)
  (setf (standard-input-stream-string stream)
        (*:string-append (funcall *:*get-stdin-line-function*) (string #\newline))
        (standard-input-stream-position stream)
        0))

(defun fetch-stdin-line-if-required (stream)
  (do ()
      ((not (empty-buffer-p stream)))
    (fetch-stdin-line stream)))

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
         (fetch-stdin-line-if-required stream)
         (prog1 (values (if (= 0 (standard-input-stream-position stream))
                            (subseq (standard-input-stream-string stream)
                                    0
                                    (1- (length (standard-input-stream-string stream))))
                            (subseq (standard-input-stream-string stream)
                                    (standard-input-stream-position stream)
                                    (1- (length (standard-input-stream-string stream)))))
                        t)
           (setf (standard-input-stream-position stream)
                 (length (standard-input-stream-string stream)))))
        ((file-input-stream-p stream)
         (multiple-value-bind (string missing-newline-p)
             (stream-read-line (file-input-stream-string-stream stream))
           (cond (missing-newline-p
                  (incf (file-input-stream-line stream))
                  (setf (file-input-stream-column stream) 0))
                 (t
                  (setf (file-input-stream-column stream) (length string))))
           (values string missing-newline-p)))
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
         (fetch-stdin-line-if-required stream)
         (prog1 (aref (standard-input-stream-string stream)
                      (standard-input-stream-position stream))
           (incf (standard-input-stream-position stream))))
        ((file-input-stream-p stream)
         (let ((char (stream-read-char (file-input-stream-string-stream stream))))
           (cond ((eq char :eof))
                 ((char= char #\newline)
                  (incf (file-input-stream-line stream))
                  (setf (file-input-stream-column stream) 0))
                 (t
                  (incf (file-input-stream-column stream))))
           char))
        (t
         (type-error stream 'input-stream))))

(defun stream-unread-char (stream)
  (cond ((string-input-stream-p stream)
         (decf (string-input-stream-position stream))
         nil)
        ((standard-input-stream-p stream)
         (decf (standard-input-stream-position stream))
         nil)
        ((file-input-stream-p stream)
         (stream-unread-char (file-input-stream-string-stream stream)))
        (t
         (type-error stream 'input-stream))))

(defun stream-peek-char (stream)
  (cond ((string-input-stream-p stream)
         (if (string-input-stream-eof-p stream)
             :eof
             (aref (string-input-stream-string stream)
                   (string-input-stream-position stream))))
        ((standard-input-stream-p stream)
         (fetch-stdin-line-if-required stream)
         (aref (standard-input-stream-string stream)
               (standard-input-stream-position stream)))
        ((file-input-stream-p stream)
         (stream-peek-char (file-input-stream-string-stream stream)))
        (t
         (type-error stream 'input-stream))))

(defmacro with-input-from-string ((stream string) &body body)
  `(let ((,stream (make-string-input-stream ,string)))
     ,@body))

(defmacro with-open-stream ((var stream) &body body)
  `(let ((,var ,stream))
     (unwind-protect (progn ,@body)
       (close ,var))))

(defun close (stream)
  (declare (ignore stream))
  t)

(defun streamp (x)
  (or (string-output-stream-p x)
      (standard-output-stream-p x)
      (string-input-stream-p x)
      (file-input-stream-p x)
      (standard-input-stream-p x)))

(defun file-position (stream)
  (cond ((string-input-stream-p stream)
         (string-input-stream-position stream))
        ((file-input-stream-p stream)
         (string-input-stream-position (file-input-stream-string-stream stream)))
        ((standard-input-stream-p stream)
         (standard-input-stream-position stream))
        ((string-output-stream-p stream)
         nil)
        ((standard-output-stream-p stream)
         nil)
        (t
         (type-error stream 'stream))))
