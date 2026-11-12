(in-package #:drei-lisp-syntax)

(defclass valtan-local-image () ())

;; Keep explicitly selected image objects and upstream Swank configurations.
#+clim-without-swank
(defun default-image () (make-instance 'valtan-local-image))

;; Drei's source walker also evaluates lisp-syntax-swank.lisp, whose
;; SHARED-INITIALIZE method unconditionally selects Swank even with
;; CLIM-WITHOUT-SWANK. Replace that method when restoring this backend.
;; Leave IMAGE untouched: NIL uses DEFAULT-IMAGE, and explicitly selected
;; images must survive REINITIALIZE-INSTANCE.
#+clim-without-swank
(defmethod shared-initialize :after ((syntax lisp-syntax) slot-names &key)
  (declare (ignore syntax slot-names)))

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

;; This backend is part of the target Drei implementation. Include its source
;; after the upstream components so system source walkers restore DEFAULT-IMAGE
;; as well as the editor definitions. In particular, Drei recompiles itself
;; using its Lisp syntax parser and the ASDF component tree. A separate .system
;; dependency alone would leave that newly evaluated editor without its backend.
;; The resource system makes the same source available in browsers, where LOAD
;; cannot fall back to the host filesystem. Preserve every upstream component.
#+clim-without-swank
(let* ((system (asdf:find-system "drei-mcclim"))
       (name "valtan-image")
       (source (asdf:component-pathname
                (asdf:find-component "valtan-drei-source" "drei-image.lisp")))
       (existing (asdf:find-component system name)))
  (if existing
      (assert (equal (asdf:component-pathname existing) source))
      (setf (asdf::component-children system)
            (append (asdf:module-components system)
                    (list (asdf::make-source-file :name name :pathname source
                                                  :parent system))))))
