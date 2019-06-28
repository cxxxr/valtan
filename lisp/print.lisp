(in-package :common-lisp)

(defvar *print-escape* t)

(defmacro print-unreadable-object ((object stream &key type identity) &body body)
  (declare (ignore object type identity))
  (let ((g-stream (gensym)))
    `(let ((,g-stream ,stream))
       (write-string "#<" ,g-stream)
       ,@body
       (write-string ">" ,g-stream)
       nil)))

(defun print-symbol (symbol stream)
  (if *print-escape*
      (if (eq (symbol-package symbol) *package*)
          (write-string (symbol-name symbol) stream)
          (progn
            (cond ((null (symbol-package symbol))
                   (write-string "#:" stream)
                   (write-string (symbol-name symbol) stream))
                  ((keywordp symbol)
                   (write-string ":" stream)
                   (write-string (symbol-name symbol) stream))
                  (t
                   (write-string (package-name (symbol-package symbol)) stream)
                   (multiple-value-bind (_symbol status)
                       (find-symbol (symbol-name symbol) (symbol-package symbol))
                     (if (eq :external status)
                         (write-string ":" stream)
                         (write-string "::" stream)))
                   (write-string (symbol-name symbol) stream)))))
      (write-string (symbol-name symbol) stream)))

(defun print-string (string stream)
  (if *print-escape*
      nil
      (write-string string stream)))

(defun print-number (number stream)
  (write-string (system::js-string-to-array ((ffi:ref "String") number)) stream))

(defun print-character (char stream)
  (if *print-escape*
      nil
      (write-char char stream)))

(defun print-cons (cons stream)
  (labels ((f (x)
             (when (consp x)
               (write (car x) :stream stream)
               (unless (listp (cdr x))
                 (write-string " . " stream)
                 (write (cdr x) :stream stream))
               (when (consp (cdr x))
                 (write-char #\space stream)
                 (f (cdr x))))))
    (write-char #\( stream)
    (f cons)
    (write-char #\) stream)))

(defun print-vector (vector stream)
  (write-string "#(" stream)
  (let ((len (length vector)))
    (do ((i 0 (1+ i)))
        ((= i len))
      (write (aref vector i) :stream stream)
      (when (< i (1- len))
        (write-char #\space stream))))
  (write-string ")" stream))

(defun print-function (function stream)
  (print-unreadable-object (function stream)
    (write-string (system::js-string-to-array ((ffi:ref "String") function))
                  stream)))

(defun write (object &key array
                          base
                          case
                          circle
                          ((:escape *print-escape*) *print-escape*)
                          gensym
                          length
                          level
                          lines
                          miser-width
                          pprint-dispatch
                          pretty
                          radix
                          readably
                          right-margin
                          (stream *standard-output*))
  (cond ((symbolp object)
         (print-symbol object stream))
        ((stringp object)
         (print-string object stream))
        ((numberp object)
         (print-number object stream))
        ((characterp object)
         (print-character object stream))
        ((consp object)
         (print-cons object stream))
        ((vectorp object)
         (print-vector object stream))
        ((functionp object)
         (print-function object stream))
        (t
         (error "write: unexpected object"))))

(defun princ (object &optional (stream *standard-output*))
  (write object :escape nil :stream stream))

(defun prin1 (object &optional (stream *standard-output*))
  (write object :escape t :stream stream))

(defun princ-to-string (object)
  (with-output-to-string (stream)
    (princ object stream)))

(defun print (object &optional (stream *standard-output*))
  (terpri stream)
  (prin1 object stream)
  (write-char #\space stream))

(defun format (destination control-string &rest format-arguments)
  (flet ((take ()
           (unless format-arguments
             (error "No more arguments"))
           (pop format-arguments)))
    (let ((string
            (with-output-to-string (buffer)
              (do ((i 0 (1+ i)))
                  ((>= i (length control-string)))
                (let ((c (aref control-string i)))
                  (cond ((char= c #\~)
                         (incf i)
                         (when (>= i (length control-string))
                           (error "format error"))
                         (case (aref control-string i)
                           ((#\a #\A)
                            (let ((arg (take)))
                              (princ arg buffer)))
                           ((#\d #\D)
                            (let ((arg (take)))
                              (princ arg buffer)))
                           ((#\c #\C)
                            (let ((arg (take)))
                              (princ arg buffer)))
                           ((#\%)
                            (write-char #\newline buffer))
                           (otherwise
                            (error "unexpected format directive"))))
                        (t
                         (write-char c buffer))))))))
      (cond ((eq destination t)
             (write-string string *standard-output*))
            ((eq destination nil)
             string)
            (t
             (write-string string destination))))))
