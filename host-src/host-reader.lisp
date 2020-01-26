(defpackage :valtan-host.reader
  (:use :cl)
  (:export :read-in-valtan))
(in-package :valtan-host.reader)

(defparameter *whitespaces* '(#\space #\tab #\newline #\linefeed #\page #\return))

(defvar *js-readtable*
  (let ((*readtable* (copy-readtable)))
    (set-macro-character
     #\`
     (lambda (s c)
       (declare (ignore c))
       (list '*:quasiquote
             (read s t nil t))))
    (set-macro-character
     #\,
     (lambda (s c)
       (declare (ignore c))
       (cond ((eql #\@ (peek-char nil s t nil t))
              (read-char s t nil t)
              (list '*:unquote-splicing (read s t nil t)))
             (t
              (list '*:unquote
                    (read s t nil t))))))
    (set-dispatch-macro-character
     #\# #\j
     (lambda (s c n)
       (declare (ignore c n))
       (case (peek-char nil s)
         (#\:
          (read-char s)
          (let ((tokens (read-js-ident s)))
            `(ffi:ref ,@tokens)))
         (#\[
          (read-js-array s))
         (#\{
          (read-js-object s))
         (otherwise
          (let ((form (read s t nil t)))
            `(ffi:cl->js ,form))))))
    *readtable*))

(defmacro *:quasiquote (x)
  (compiler::expand-quasiquote x))

(defun non-terminate-macro-character-p (c)
  (multiple-value-bind (function non-terminating-p)
      (get-macro-character c)
    (and function (not non-terminating-p))))

(defun whitespacep (c)
  (find c *whitespaces*))

(defun delimiter-p (c)
  (or (null c)
      (whitespacep c)
      (non-terminate-macro-character-p c)
      (char= c #\\)
      (char= c #\|)
      (char= c #\:)))

(defun read-js-ident-1 (in)
  (with-output-to-string (out)
    (loop :for c := (peek-char nil in nil nil)
          :while c
          :do (cond ((or (alphanumericp c) (char= c #\_))
                     (write-char c out)
                     (read-char in))
                    ((delimiter-p c)
                     (return))
                    (t
                     (error "invalid character: ~S" c))))))

(defun read-js-ident (in)
  (loop :for token := (read-js-ident-1 in)
        :if (string/= token "") :collect token :into tokens
        :do (case (peek-char nil in nil nil)
              (#\: (read-char in))
              (otherwise (return tokens)))))

(defun read-js-array (in)
  (flet ((read-js-array (s c)
           (declare (ignore c))
           `(ffi::array ,@(read-delimited-list #\] s t))))
    (let ((*readtable* (copy-readtable *js-readtable*)))
      (set-macro-character #\[ #'read-js-array)
      (set-macro-character #\] (get-macro-character #\)) nil)
      (read in t nil t))))

(defun read-js-object (in)
  (labels ((read-js-object-1 (s c)
             (declare (ignore c))
             (let ((plist '()))
               (loop
                 (when (eq #\} (peek-char t s t nil t))
                   (read-char s t nil t)
                   (return))
                 (let ((key (read s t nil t))
                       (value (read s t nil t)))
                   (push key plist)
                   (push value plist)))
               `(ffi:object ,@(nreverse plist)))))
    (let ((*readtable* (copy-readtable *js-readtable*)))
      (set-macro-character #\{ #'read-js-object-1)
      (set-macro-character #\} (get-macro-character #\)) nil)
      (read in t nil t))))

(defun read-in-valtan (&optional (stream *standard-input*) (eof-error-p t) eof-value)
  (let ((*readtable* *js-readtable*))
    (handler-bind ((sb-int:simple-reader-package-error
                     (lambda (condition)
                       (let ((name (first (simple-condition-format-arguments condition)))
                             (package (slot-value condition 'package)))
                         (export (intern name package) package)
                         (continue condition)))))
      (read stream eof-error-p eof-value))))
