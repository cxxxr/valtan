;; Inspect the upstream ASDF plan on the host; never execute its load actions.
(require :asdf)
(let ((setup (or (uiop:getenv "QUICKLISP_SETUP")
                 (merge-pathnames #p"quicklisp/setup.lisp" (user-homedir-pathname)))))
  (when (probe-file setup) (load setup)))
(asdf:load-system "st-json")
(let ((output (first (uiop:command-line-arguments)))
      (system (or (second (uiop:command-line-arguments)) "mcclim/test"))
      (files nil) (source-resources nil) (seen (make-hash-table :test 'equal)))
  (unless output (error "Expected output manifest pathname"))
  (assert (member system '("mcclim/test" "drei-mcclim/test") :test #'equal))
  (dolist (action (asdf/plan:plan-actions
                  (asdf:make-plan 'asdf/plan:filtered-sequential-plan 'asdf:load-op
                                  (asdf:find-system system) :other-systems t)))
    (let ((component (cdr action)))
      (when (and (typep (car action) 'asdf:load-op) (typep component 'asdf:cl-source-file)
                 (member (asdf:component-name (asdf:component-system component))
                         (if (equal system "mcclim/test")
                             '("mcclim/test" "mcclim/test-util")
                             (list system)) :test #'equal))
        (let ((file (asdf:component-pathname component)))
          (unless (gethash file seen)
            (setf (gethash file seen) t)
            (push (namestring file) files))))))
  (unless files (error "Upstream test plan contains no Lisp files"))
  ;; SELF-COMPILATION-TEST walks the component tree, rather than LOAD-OP's
  ;; feature-filtered plan. Preserve that exact inventory, including disabled
  ;; components; removing one here would silently narrow the upstream test.
  (when (equal system "drei-mcclim/test")
    (labels ((visit (component)
               (typecase component
                 (asdf:module (mapc #'visit (asdf:component-children component)))
                 (asdf:source-file
                  (push (namestring (asdf:component-pathname component)) source-resources)))))
      (visit (asdf:find-system "drei-mcclim"))))
  (with-open-file (out output :direction :output :if-exists :supersede)
    (st-json:write-json
     (st-json:jso "root" (namestring (asdf:system-source-directory "mcclim"))
                  "system" system
                  "definition" (namestring (asdf:system-source-file system))
                  "asdfVersion" (asdf:asdf-version)
                  "files" (nreverse files)
                  "sourceResources" (nreverse source-resources)) out)))
