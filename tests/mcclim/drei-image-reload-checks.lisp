;; The driver supplies the unchanged upstream lisp-syntax.lisp as a string.
;; Its DEFAULT-IMAGE definition is the one that Drei re-evaluates when compiling
;; itself. Source ownership must make our backend available afterward, including
;; in a browser with no host filesystem.
(defpackage :valtan-drei-image-reload-tests (:use :cl))
(in-package :valtan-drei-image-reload-tests)

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
  (dotimes (iteration 2)
    (with-input-from-string (stream *upstream-lisp-syntax-source*)
      (load stream))
    (assert (eq (drei-lisp-syntax::default-image) t))
    ;; Resolve the extension through the system tree, not a test-only pathname.
    (dolist (component (last (asdf:module-components system)))
      (load (asdf:component-pathname component)))
    (assert (typep (drei-lisp-syntax::default-image) 'drei-lisp-syntax::valtan-local-image))
    (assert (equal (asdf:module-components system) components)))
  (format t "DREI-IMAGE-RELOAD-PASS ~D components~%" (length components)))
