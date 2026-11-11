;; Installed only in the local test workbench. Requests run in the same target
;; Lisp and event-loop context as the application, preserving its loaded state.
(ffi:set (ffi:ref "globalThis.valtanMcclimKernel") (ffi:ref "lisp"))
(ffi:set (ffi:ref "globalThis.valtanMcclimEvaluate")
  (lambda (source)
    (handler-case
        (let ((*package* (find-package :cl-user)) (*readtable* (copy-readtable nil))
              (last-values nil) (eof (gensym)))
          (with-input-from-string (input (ffi:js->cl source))
            (loop for form = (read input nil eof) until (eq form eof)
                  do (setf last-values (multiple-value-list (eval form)))))
          (ffi:object :ok (ffi:ref "true")
                      :value (ffi:cl->js (prin1-to-string last-values))))
      (error (condition)
        (ffi:object :ok (ffi:ref "false")
                    :error (ffi:cl->js (princ-to-string condition)))))))
