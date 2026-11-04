;; Valtan loader for unmodified upstream tests, using the RT-compatible adapter.
(defpackage :cl-test (:use :cl :regression-test))
(in-package :cl-user)

(defvar *ansi-current-file* "")

(defun ansi-event (kind name detail)
  (funcall (ffi:ref "globalThis" "valtanAnsiEvent")
           (ffi:cl->js kind) (ffi:cl->js name) (ffi:cl->js detail)))

(defun ansi-format-diagnostic (control &rest arguments)
  ;; These bindings apply only after a test has produced its outcome. Avoid
  ;; scanning an entire large failure graph for circularity while reporting it.
  (let ((*print-length* 10) (*print-level* 5) (*print-circle* nil)
        (*print-readably* nil) (*print-pretty* nil))
    (handler-case (apply #'format nil control arguments)
      (error () "<unprintable>"))))

(defun ansi-description (object)
  (ansi-format-diagnostic "~A" object))

(defun ansi-load (path)
  (ansi-event "file-start" path "")
  (let ((*package* *package*)
        (*ansi-current-file* path)
        (eof (gensym)))
    (handler-case
        (let ((source (ffi:js->cl
                       (funcall (ffi:ref "globalThis" "valtanAnsiSource")
                                (ffi:cl->js path)))))
          (with-input-from-string (stream source)
            (do ((form (read stream nil eof) (read stream nil eof))
                 (form-index 1 (1+ form-index)))
                ((eq form eof) (ansi-event "file-complete" path ""))
              (handler-case
                  (eval form)
                (error (condition)
                  (ansi-event "load-error" path
                              (ansi-format-diagnostic "form ~D (~A ~A): ~A"
                                      form-index
                                      (if (consp form) (first form) form)
                                      (if (and (consp form) (consp (cdr form))) (second form) "")
                                      (ansi-description condition))))))))
      (error (condition)
        (ansi-event "read-error" path (ansi-description condition))))))

;; Helper-file loading only. COMPILE inside a test still invokes Valtan's own
;; implementation; compiled mode never falls back to EVAL.
(defun compile-and-load* (path &key force)
  (declare (ignore force))
  (ansi-load (concatenate 'string "auxiliary/" path)))

(defun compile-and-load (path &key force)
  (declare (ignore force))
  (ansi-load (ffi:js->cl
              (funcall (ffi:ref "globalThis" "valtanAnsiResolve")
                       (ffi:cl->js path) (ffi:cl->js *ansi-current-file*)))))

(defun ansi-main ()
  (setq rt::*compile-tests* (string= "compiled" (ffi:js->cl (ffi:ref "globalThis" "valtanAnsiCompile"))))

  (unless (string= "1" (ffi:js->cl (ffi:ref "globalThis" "valtanAnsiFixture")))
    (ansi-load "cl-test-package.lsp"))
  (let ((*package* (find-package :cl-test)))
    (unless (string= "1" (ffi:js->cl (ffi:ref "globalThis" "valtanAnsiFixture")))
      (ansi-load "auxiliary/ansi-aux-macros.lsp")
      (ansi-load "universe.lsp")
      (ansi-load "auxiliary/random-aux.lsp")
      (ansi-load "auxiliary/ansi-aux.lsp")
      (ansi-load "cl-symbol-names.lsp")
      (ansi-load "notes.lsp"))
    (let ((files (ffi:ref "globalThis" "valtanAnsiFiles")))
      (dotimes (i (ffi:ref "globalThis" "valtanAnsiFiles" "length"))
        (ansi-load (ffi:js->cl (ffi:aget files i))))))
  (ansi-event "registered" "" (princ-to-string (length rt::*entries*)))
  ;; Upstream RT:DO-TESTS runs bodies with its copied sandbox as the default.
  (let ((*package* (find-package :cl-test))
        (*default-pathname-defaults*
          (if (string= "1" (ffi:js->cl (ffi:ref "globalThis" "valtanAnsiFixture")))
              *default-pathname-defaults* (truename "sandbox/"))))
  (dolist (entry rt::*entries*)
    (let ((name (if (symbolp (rt::name entry))
                    (symbol-name (rt::name entry))
                    (ansi-description (rt::name entry)))))
      (setq rt::*test* (rt::name entry))
      (ansi-event "test-start" name "")
      (funcall (ffi:ref "globalThis" "valtanAnsiRunTest")
               (ffi:cl->js name)
               (lambda ()
      (handler-case
          (let ((actual (multiple-value-list
                         (if rt::*compile-tests*
                             (funcall (compile nil (list 'lambda nil (rt::form entry))))
                             (eval (rt::form entry))))))
            (if (rt::equalp-with-case actual (rt::vals entry))
                (ansi-event "pass" name "")
                (ansi-event "fail" name
                            (ansi-format-diagnostic
                              "expected ~S; actual ~S [diagnostic display: length 10, depth 5]"
                              (rt::vals entry) actual))))
        (error (condition)
          (ansi-event "fail" name (ansi-description condition))))))))
  (ansi-event "complete" "" "")))

(ansi-main)
