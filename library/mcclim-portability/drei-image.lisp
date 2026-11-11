(in-package #:drei-lisp-syntax)

(defclass valtan-local-image () ())

;; Keep explicitly selected image objects and upstream Swank configurations.
#+clim-without-swank
(defun default-image () (make-instance 'valtan-local-image))

(defmethod arglist ((image valtan-local-image) (symbol symbol))
  (declare (ignore image))
  (valtan-image-support:arglist symbol))

(defmethod simple-completions ((image valtan-local-image) string default-package)
  (declare (ignore image))
  (valtan-image-support:completions string default-package))

(defmethod get-class-keyword-parameters ((image valtan-local-image) class)
  (declare (ignore image))
  (valtan-image-support:class-keywords class))

(defun valtan-compile-with-notes (function)
  (let ((start (get-internal-real-time)) (notes nil) (success nil))
    (flet ((note (condition class)
             (push (make-instance class :message (princ-to-string condition)
                                        :short-message (princ-to-string condition)) notes)))
      (handler-case
          (handler-bind ((warning (lambda (condition)
                                    (note condition (if (typep condition 'style-warning)
                                                        'style-warning-compiler-note 'warning-compiler-note))
                                    (when (find-restart 'muffle-warning condition)
                                      (muffle-warning condition)))))
            (setq success (funcall function)))
        (reader-error (condition) (note condition 'read-error-compiler-note))
        (error (condition) (note condition 'error-compiler-note))))
    (values (list success (/ (- (get-internal-real-time) start) internal-time-units-per-second))
            (nreverse notes))))

(defmethod compile-string-for-drei ((image valtan-local-image) (string string) package
                                    (view drei-buffer-view) (buffer-mark mark))
  (declare (ignore image view buffer-mark))
  (let ((*package* (or (find-package package) (error 'package-error :package package))))
    (valtan-compile-with-notes
     (lambda ()
       (with-input-from-string (stream string)
         (let ((eof (gensym)))
           (loop for form = (read stream nil eof) until (eq form eof) do (eval form))))
       t))))

(defmethod compile-file-for-drei ((image valtan-local-image) filepath package &optional load-p)
  (declare (ignore image))
  (let ((*package* (or (find-package package) (error 'package-error :package package))))
    (valtan-compile-with-notes
     (lambda ()
       (multiple-value-bind (artifact warnings failure) (compile-file filepath)
         (declare (ignore warnings))
         (unless failure (when load-p (load artifact)) t))))))
