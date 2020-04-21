#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

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
      (cond ((null (symbol-package symbol))
             (write-string "#:" stream)
             (write-string (symbol-name symbol) stream))
            ((keywordp symbol)
             (write-string ":" stream)
             (write-string (symbol-name symbol) stream))
            ((or (eq (symbol-package symbol) *package*)
                 (eq symbol (find-symbol (symbol-name symbol))))
             (write-string (symbol-name symbol) stream))
            (t
             (write-string (package-name (symbol-package symbol)) stream)
             (multiple-value-bind (_symbol status)
                 (find-symbol (symbol-name symbol) (symbol-package symbol))
               (declare (ignore _symbol))
               (if (eq :external status)
                   (write-string ":" stream)
                   (write-string "::" stream)))
             (write-string (symbol-name symbol) stream)))
      (write-string (symbol-name symbol) stream)))

(defun print-string (string stream)
  (cond (*print-escape*
         (write-char #\" stream)
         (write-string string stream)
         (write-char #\" stream))
        (t
         (write-string string stream))))

(defun print-number (number stream)
  (write-string (system:raw-string-to-array (system:number-to-raw-string number)) stream))

(defun print-character (char stream)
  (cond (*print-escape*
         (write-string "#\\" stream)
         (write-char char stream))
        (t
         (write-char char stream))))

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
  (cl:print-unreadable-object (function stream)
    (write-string "Function" stream)
    (let ((name (system:function-name function)))
      (when name
        (write-char #\space stream)
        (write-string name stream)))))

(defun print-package (package stream)
  (cl:print-unreadable-object (package stream)
    (write-string "PACKAGE " stream)
    (write-char #\" stream)
    (write-string (package-name package) stream)
    (write-char #\" stream)))

(defun print-structure (structure stream)
  (funcall (structure-printer structure) structure stream))

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
  (declare (ignore array base case circle gensym length level lines miser-width pprint-dispatch
                   pretty radix readably right-margin))
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
        ((packagep object)
         (print-package object stream))
        ((*:structure-p object)
         (print-structure object stream))
        (t
         (cl:print-unreadable-object (nil stream)
           (write-string (system:unknown-object-to-string object)
                         stream)))))

(defun write-to-string (object &rest args
                               &key array base case circle escape gensym length level
                                    lines miser-width pprint-dispatch pretty radix readably right-margin)
  (declare (ignore array base case circle escape gensym length level
                   lines miser-width pprint-dispatch pretty radix readably right-margin))
  (with-output-to-string (stream)
    (apply #'write object :stream stream args)))

(defun princ (object &optional (stream *standard-output*))
  (write object :escape nil :stream stream)
  object)

(defun princ-to-string (object)
  (with-output-to-string (stream)
    (princ object stream)))

(defun prin1 (object &optional (stream *standard-output*))
  (write object :escape t :stream stream)
  object)

(defun prin1-to-string (obejct)
  (with-output-to-string (stream)
    (prin1 object stream)))

(defun print (object &optional (stream *standard-output*))
  (terpri stream)
  (prin1 object stream)
  (write-char #\space stream)
  object)

(defun princ-to-string (object)
  (with-output-to-string (stream)
    (princ object stream)))

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
                            (princ (take) buffer))
                           ((#\d #\D)
                            (princ (take) buffer))
                           ((#\c #\C)
                            (princ (take) buffer))
                           ((#\s #\S)
                            (prin1 (take) buffer))
                           ((#\%)
                            (write-char #\newline buffer))
                           ((#\&)
                            (fresh-line buffer))
                           (otherwise
                            (error "unexpected format directive"))))
                        (t
                         (write-char c buffer))))))))
      (cond ((eq destination t)
             (write-string string *standard-output*)
             (finish-output *standard-output*))
            ((eq destination nil)
             string)
            (t
             (write-string string destination)
             (finish-output destination))))))

(defun %format (string &rest args)
  (ffi:cl->js (apply #'format nil (ffi:js->cl string) args)))

#+valtan
((ffi:ref "lisp" "setLispFormatFunction") #'%format)
