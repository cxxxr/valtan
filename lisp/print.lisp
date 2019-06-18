(in-package :common-lisp)

#|
(defun write (object &key array base case circle escape gensym length level lines miser-width
                          pprint-dispatch pretty radix readably right-margin stream)
  (declare (ignore array base case circle escape gensym length level lines miser-width
                   pprint-dispatch pretty radix readably right-margin stream))
  )
|#

(defun print-cons (cons stream print-fn)
  (labels ((f (x)
             (when (consp x)
               (funcall print-fn (car x) stream)
               (unless (listp (cdr x))
                 (write-string " . " stream)
                 (funcall print-fn (cdr x) stream))
               (when (consp (cdr x))
                 (write-char #\space stream)
                 (f (cdr x))))))
    (write-char #\( stream)
    (f cons)
    (write-char #\) stream)))

(defun print-vector (vector stream print-fn)
  (write-string "#(" stream)
  (let ((len (length vector)))
    (do ((i 0 (1+ i)))
        ((= i len))
      (funcall print-fn (aref vector i) stream)
      (when (< i (1- len))
        (write-char #\space stream))))
  (write-string ")" stream))

(defun princ (object &optional (stream *standard-output*))
  (cond ((symbolp object)
         (write-string (symbol-name object) stream))
        ((stringp object)
         (write-string object stream))
        ((numberp object)
         (write-string (system::js-string-to-array ((ffi:ref "String") object)) stream))
        ((characterp object)
         (write-char object stream))
        ((consp object)
         (print-cons object stream #'princ))
        ((vectorp object)
         (print-vector object stream #'princ))
        (t
         (error "princ error"))))

(defun princ-to-string (object)
  (with-output-to-string (stream)
    (princ object stream)))

(defun format (destination control-string &rest format-arguments)
  )

