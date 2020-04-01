(defpackage :valtan-host.reader
  (:use :cl)
  (:export :read-in-valtan
           :map-file-forms))
(in-package :valtan-host.reader)

#+(or)
(defparameter *whitespaces* '(#\space #\tab #\newline #\linefeed #\page #\return))

#+(or)
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
    (let ((fn (get-macro-character #\" *readtable*)))
      (set-dispatch-macro-character
       #\# #\"
       (lambda (s c n)
         (declare (ignore n))
         (funcall fn s c))))
    *readtable*))

(defmacro *:quasiquote (x)
  (compiler::expand-quasiquote x))

#+(or)
(defun non-terminate-macro-character-p (c)
  (multiple-value-bind (function non-terminating-p)
      (get-macro-character c)
    (and function (not non-terminating-p))))

#+(or)
(defun whitespacep (c)
  (find c *whitespaces*))

#+(or)
(defun delimiter-p (c)
  (or (null c)
      (whitespacep c)
      (non-terminate-macro-character-p c)
      (char= c #\\)
      (char= c #\|)
      (char= c #\:)))

#+(or)
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

#+(or)
(defun read-js-ident (in)
  (loop :for token := (read-js-ident-1 in)
        :if (string/= token "") :collect token :into tokens
        :do (case (peek-char nil in nil nil)
              (#\: (read-char in))
              (otherwise (return tokens)))))

#+(or)
(defun read-js-array (in)
  (flet ((read-js-array (s c)
           (declare (ignore c))
           `(ffi::array ,@(read-delimited-list #\] s t))))
    (let ((*readtable* (copy-readtable *js-readtable*)))
      (set-macro-character #\[ #'read-js-array)
      (set-macro-character #\] (get-macro-character #\)) nil)
      (read in t nil t))))

#+(or)
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

#+(or)
(defun read-in-valtan (&optional (stream *standard-input*) (eof-error-p t) eof-value
                                 (*readtable* *js-readtable*))
  (handler-bind ((sb-int:simple-reader-package-error
                   (lambda (condition)
                     (let ((name (first (simple-condition-format-arguments condition)))
                           (package (slot-value condition 'package)))
                       (export (intern name package) package)
                       (continue condition)))))
    (read stream eof-error-p eof-value)))

(defun call-with-valtan-reader (package function)
  (let ((string-reader (fdefinition 'valtan-core.reader:string-reader))
        (array-reader (fdefinition 'valtan-core.reader:array-reader)))
    (setf (fdefinition 'valtan-core.reader:string-reader)
          (lambda (&rest args)
            (let ((valtan-string (apply string-reader args)))
              (first (valtan-core::structure-values valtan-string)))))
    (setf (fdefinition 'valtan-core.reader:array-reader)
          (lambda (&rest args)
            (let ((valtan-array (apply array-reader args)))
              (first (valtan-core::structure-values valtan-array)))))
    (unwind-protect
         (let ((valtan-core::*package* package)
               (*package* package))
           (funcall function))
      (setf (fdefinition 'valtan-core.reader:string-reader) string-reader)
      (setf (fdefinition 'valtan-core.reader:array-reader) array-reader))))

(defun read-in-valtan ()
  (call-with-valtan-reader
   *package*
   (lambda ()
     (valtan-core::read))))

(defun map-file-forms (function file)
  (call-with-valtan-reader
   (find-package :valtan-user)
   (lambda ()
     (valtan-core::with-open-file (stream file)
       (loop :with eof-value := '#:eof
             :for form := (valtan-core::read stream nil eof-value)
             :until (eq form eof-value)
             :do (funcall function form))))))
