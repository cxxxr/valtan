(in-package :uiop/run-program)

;; Upstream's synchronous I/O reduction implements stream processors, temporary
;; files, output return values and SUBPROCESS-ERROR. Only the actual execution
;; is host-specific. LAUNCH-PROGRAM remains unsupported (it is asynchronous).
(defun %valtan-io-specifier (specifier)
  (cond ((null specifier) (ffi:array #j"ignore"))
        ((eq specifier :interactive) (ffi:array #j"inherit"))
        ((eq specifier :output) (ffi:array #j"stdout"))
        ((typep specifier '(or string pathname))
         (ffi:array #j"file" (ffi:cl->js (native-namestring specifier))))
        (t (parameter-error "Invalid synchronous subprocess I/O: ~S" specifier))))

(defun %system (command &rest keys
                &key (force-shell nil force-shell-supplied-p) directory
                  input (if-input-does-not-exist :error)
                  output (if-output-exists :supersede)
                  error-output (if-error-output-exists :supersede)
                &allow-other-keys)
  (declare (ignore keys))
  (let* ((shell (if force-shell-supplied-p force-shell (stringp command)))
         (command (if (and shell (listp command)) (escape-shell-command command) command)))
    (unless (or (and (stringp command) (plusp (length command)))
                (and (consp command) (every #'stringp command) (plusp (length (first command)))))
      (parameter-error "Invalid subprocess command: ~S" command))
    (when (or (eq input :output) (eq output :output))
      (parameter-error "Only ERROR-OUTPUT can be redirected to :OUTPUT"))
    (%handle-if-does-not-exist input if-input-does-not-exist)
    (%handle-if-exists output if-output-exists)
    (%handle-if-exists error-output if-error-output-exists)
    (system:environment-op :run-program
      (ffi:cl->js (if (stringp command) command (first command)))
      (apply #'ffi:array (mapcar #'ffi:cl->js (if (stringp command) nil (rest command))))
      (if shell (ffi:ref "true") (ffi:ref "false"))
      (if directory (ffi:cl->js (native-namestring directory)) (ffi:ref "null"))
      (%valtan-io-specifier input) (%valtan-io-specifier output)
      (%valtan-io-specifier error-output))))

(defun run-program (command &rest keys &key &allow-other-keys)
  "Run a command synchronously on Node using UIOP's ordinary I/O processors."
  (unless (system:environment-op :subprocess-available)
    (not-implemented-error 'run-program "Subprocess execution requires Node.js"))
  (apply #'%use-system command keys))
