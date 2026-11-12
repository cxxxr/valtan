;; The driver supplies the unchanged upstream syntax and Swank sources as
;; strings. Source ownership must restore DEFAULT-IMAGE and syntax initialization
;; after self-compilation, including in a browser with no host filesystem.
(defpackage :valtan-drei-image-reload-tests (:use :cl))
(in-package :valtan-drei-image-reload-tests)

(defun known-function (item &optional other) (list item other))

(defun parser-load (source)
  ;; Use the same parser and FORM-TO-OBJECT path as SELF-COMPILATION-TEST.
  ;; Unlike READ, this also handles the unavailable SWANK package as Drei does.
  (let* ((buffer (make-instance 'drei:drei-buffer :initial-contents source))
         (view (make-instance 'drei:textual-drei-syntax-view :buffer buffer))
         (syntax (drei:make-syntax-for-view view 'drei-lisp-syntax:lisp-syntax))
         (*package* *package*))
    (setf (drei:syntax view) syntax)
    (drei-syntax:update-parse syntax)
    (dolist (form (drei-lisp-syntax::children
                  (slot-value syntax 'drei-lisp-syntax::stack-top)))
      (when (drei-lisp-syntax::formp form)
        (eval (drei-lisp-syntax:form-to-object syntax form :read t))))))

(defun check-syntax-image ()
  (let* ((buffer (make-instance 'drei:drei-buffer))
         (view (make-instance 'drei:textual-drei-syntax-view :buffer buffer))
         (syntax (drei:make-syntax-for-view view 'drei-lisp-syntax:lisp-syntax))
         (image (drei-lisp-syntax::get-usable-image syntax)))
    (format t "DREI-SYNTAX-IMAGE ~S~%" (type-of image))
    (assert (typep image 'drei-lisp-syntax::valtan-local-image))
    (assert (equal (drei-lisp-syntax::arglist image 'known-function)
                   '(item &optional other)))
    (assert (equal (drei-lisp-syntax::simple-completions
                    image "multiple-value-b" (find-package :cl-user))
                   '(("multiple-value-bind") "multiple-value-bind")))
    ;; An explicit image must also survive reinitialization.
    (let ((selected (list :selected-image)))
      (setf (drei-lisp-syntax::image syntax) selected)
      (reinitialize-instance syntax)
      (assert (eq (drei-lisp-syntax::get-usable-image syntax) selected)))))

(let* ((system (asdf:find-system "drei-mcclim"))
       (components (copy-list (asdf:module-components system)))
       (backend (asdf:find-component system "valtan-image")))
  (assert backend)
  (assert (eq (car (last components)) backend))
  (assert (= (length components) 35))
  (assert (eq (asdf:component-parent backend) system))
  (assert (eq (asdf:component-system backend) system))
  (assert (probe-file (asdf:component-pathname backend)))
  (assert (typep (drei-lisp-syntax::default-image) 'drei-lisp-syntax::valtan-local-image))
  (check-syntax-image)
  (dotimes (iteration 2)
    (with-input-from-string (stream *upstream-lisp-syntax-source*)
      (load stream))
    (assert (eq (drei-lisp-syntax::default-image) t))
    ;; Source walkers include this file even with CLIM-WITHOUT-SWANK. It
    ;; installs an unguarded SHARED-INITIALIZE method that selects Swank.
    (parser-load *upstream-swank-source*)
    ;; Resolve the extension through the system tree, not a test-only pathname.
    (dolist (component (last (asdf:module-components system)))
      (load (asdf:component-pathname component)))
    (assert (typep (drei-lisp-syntax::default-image) 'drei-lisp-syntax::valtan-local-image))
    (check-syntax-image)
    (assert (equal (asdf:module-components system) components)))
  (format t "DREI-IMAGE-RELOAD-PASS ~D components~%" (length components)))
