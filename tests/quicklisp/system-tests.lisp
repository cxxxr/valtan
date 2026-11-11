;;;; Standalone tests of target source planning; no Quicklisp/network required.
(require :asdf)
;; Only the target feature variable and the pathname printer's package are used
;; by the loader. Do not load the compiler or any target source in these tests.
(defpackage :valtan-core (:use))
(defparameter valtan-core::*features* '(:valtan :node))
(unless (find-package :ppcre) (make-package :ppcre :use nil))
(export (intern "REGEX-REPLACE-ALL" :ppcre) :ppcre)
(defparameter *root* (truename (merge-pathnames "../../" (uiop:pathname-directory-pathname *load-truename*))))
(asdf:load-asd (merge-pathnames "valtan.asd" *root*))
(load (merge-pathnames "host-src/system.lisp" *root*))
(load (merge-pathnames "host-src/asdf.lisp" *root*))

(in-package :valtan-host.system)
(defvar *checks* 0)
(defun check (value)
  (incf *checks*)
  (assert value () "System loader check ~D failed" *checks*))
(defun write-fixture (directory name content)
  (let ((file (merge-pathnames name directory)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-string content out))))
(defun signals-error (thunk &optional message)
  (handler-case (progn (funcall thunk) nil)
    (error (condition) (if message (search message (princ-to-string condition) :test #'char-equal) t))))

(let* ((directory (merge-pathnames (format nil "valtan-system-tests-~A/" (get-universal-time))
                                  (uiop:temporary-directory)))
       (*system-directories* (list directory))
       (*system-map* (make-hash-table :test 'equal)))
  (unwind-protect
       (progn
         (write-fixture directory "vt-dep.asd"
                        "(asdf:defsystem \"vt-dep\" :version \"1.2\" :components ((:file \"dep\")))")
         (write-fixture directory "vt-sample.asd"
                        "(asdf:defsystem \"vt-sample\"
                           :depends-on ((:version \"vt-dep\" \"1.1\")
                                        (:feature :sbcl \"does-not-exist\"))
                           :components ((:module \"nested\" :pathname \"src/\"
                                         :components ((:file \"last\" :depends-on (\"first\"))
                                                      (:file \"first\" :pathname \"renamed\")
                                                      (:file \"host\" :if-feature :sbcl)
                                                      #+sbcl (:file \"reader-host\")
                                                      #+valtan (:file \"target\")))
                                        (:static-file \"README\")))
                         (asdf:defsystem \"vt-sample/sub\" :depends-on (\"vt-sample\")
                           :components ((:file \"sub\")))")
         (dolist (file '("dep.lisp" "src/last.lisp" "src/renamed.lisp" "src/host.lisp"
                         "src/reader-host.lisp" "src/target.lisp" "sub.lisp"))
           ;; If ASDF ever PERFORMs the target plan, this test must fail.
           (write-fixture directory file "(error \"Target source must not run on the host\")"))
         (write-fixture directory "README" "Static data")
         ;; Register a host-feature version first; the target must not reuse it.
         (asdf:load-asd (merge-pathnames "vt-sample.asd" directory))
         (let* ((host (asdf:find-system "vt-sample"))
                (target (find-system "vt-sample/sub"))
                (files (mapcar (lambda (c) (enough-namestring (component-pathname c) directory))
                               (system-components target))))
           (check (equal files '("dep.lisp" "src/renamed.lisp" "src/last.lisp" "src/target.lisp" "sub.lisp")))
           (check (eq host (asdf:find-system "vt-sample")))
           (check (eq target (find-system :vt-sample/sub)))
           (check (equal (system-depends-on target) '("valtan-core"))))
         (write-fixture directory "vt-version.asd"
                        "(asdf:defsystem \"vt-version\" :depends-on ((:version \"vt-dep\" \"2.0\")))")
         (check (signals-error (lambda () (find-system "vt-version"))))
         (check (null (find-system "vt-missing" nil)))
         (check (signals-error (lambda () (find-system "vt-missing")) "not found"))
         (write-fixture directory "vt-sample.system"
                        "(defsystem :vt-sample :components ((:file \"dep\")))")
         (remhash "vt-sample" *system-map*)
         (check (equal (pathname-type (system-pathname (find-system "vt-sample"))) "system"))
         (check (= 1 (length (system-components (find-system "vt-sample")))))
         (write-fixture directory "vt-direct.asd"
                        "(asdf:defsystem \"vt-direct\" :components ((:file \"dep\")))")
         (check (equal "vt-direct" (system-name (load-system-file (merge-pathnames "vt-direct.asd" directory)))))
         (write-fixture directory "vt-feature.asd"
                        "(pushnew #+valtan :vt-target-feature #-valtan :vt-host-feature *features*)
                         (asdf:defsystem \"vt-feature\" :components ((:file \"dep\")))")
         (let* ((host-features (copy-list *features*))
                (target-features (copy-list valtan-core::*features*))
                (system (load-system-file (merge-pathnames "vt-feature.asd" directory))))
           (check (equal (system-target-features system) '(:vt-target-feature)))
           (check (equal *features* host-features))
           (check (equal valtan-core::*features* target-features)))
         (write-fixture directory "vt-custom.asd"
                        "(defclass vt-custom-file (asdf:cl-source-file) ())
                         (asdf:defsystem \"vt-custom\" :components ((vt-custom-file \"dep\")))")
         (check (signals-error (lambda () (find-system "vt-custom")) "adapter"))
         (write-fixture directory "vt-operation.asd"
                        "(defclass vt-operation (asdf:non-propagating-operation) ())
                         (asdf:defsystem \"vt-operation\"
                           :in-order-to ((asdf:load-op (vt-operation \"vt-operation\"))))")
         (check (signals-error (lambda () (find-system "vt-operation")) "not supported"))
         (write-fixture directory "vt-host.asd"
                        "(asdf:defsystem \"vt-host\" :depends-on (\"asdf\"))")
         (check (signals-error (lambda () (find-system "vt-host")) "no Valtan runtime"))
         (let ((host-uiop (asdf:find-system "uiop")))
           (dolist (name '("uiop" "trivial-gray-streams" "trivial-features"))
             (check (search "library/portability/"
                            (namestring (system-pathname (find-system name))))))
           (check (eq host-uiop (asdf:find-system "uiop"))))
         (let ((a (make-system :name "cycle-a" :depends-on '("cycle-b")))
               (b (make-system :name "cycle-b" :depends-on '("cycle-a"))))
           (setf (gethash "cycle-a" *system-map*) a (gethash "cycle-b" *system-map*) b)
           (check (signals-error (lambda () (compute-system-precedence-list a)) "Circular")))
         (format t "~&~D system loader checks passed.~%" *checks*))
    (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))
