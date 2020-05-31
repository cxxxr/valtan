(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")
(ffi:require js:-code-mirror "react-codemirror2")
(ffi:require "codemirror/keymap/emacs")
(ffi:require "codemirror/mode/commonlisp/commonlisp")

(defpackage :browser-repl
  (:use :cl :valtan.react-utilities))
(in-package :browser-repl)

#|
TODO
- リスタート
|#

(defvar * nil)
(defvar ** nil)
(defvar *** nil)

(defvar + nil)
(defvar ++ nil)
(defvar +++ nil)

(defvar / nil)
(defvar // nil)
(defvar /// nil)

(defvar - nil)

;; TODO: これに時間がかかるようなので最適化を頑張る
(defun split-lines (text)
  (let ((lines '()))
    (do ((start 0))
        ((>= start (length text)))
      (let ((pos (position #\newline text :start start)))
        (when (null pos)
          (push (subseq text start) lines)
          (return))
        (push (subseq text start pos) lines)
        (setq start (1+ pos))))
    (nreverse lines)))

(defun complete-input-p (input)
  (handler-case (values (read-from-string input) t)
    (error () 
      (values nil nil))))

(defun set-editor-value (code-mirror text)
  ((ffi:ref code-mirror :set-value) (ffi:cl->js text)))

(defun get-editor-value (code-mirror)
  (ffi:js->cl ((ffi:ref code-mirror :get-value))))

(defun input-newline (code-mirror)
  ((ffi:ref code-mirror :replace-selection) #j(string #\newline) #j"end"))

(defun on-eval (form printer)
  (let ((values
          (multiple-value-list
           (handler-bind ((error (lambda (e)
                                   (funcall printer (princ-to-string e))
                                   (dolist (line
                                            (split-lines
                                             (with-output-to-string (out)
                                               ;; TODO: パッケージをclではなくvaltan固有のものに変更する
                                               (cl::print-backtrace :stream out))))
                                     (funcall printer line))
                                   (return-from on-eval))))
             (eval form)))))
    (dolist (value values)
      (dolist (line (split-lines (format nil "~S" value)))
        (funcall printer line)))
    (setq - form)
    (setq +++ ++  /// //      *** (first ///)
          ++  +   //  /       **  (first //)
          +   -   /   values  *   (first /))))

(defun on-enter (code-mirror repl-package set-repl-package)
  (let ((*package* repl-package)
        (input (get-editor-value code-mirror)))
    (multiple-value-bind (form ok) (complete-input-p input)
      (cond ((not ok)
             (input-newline code-mirror)
             nil)
            (t
             (let ((lines (list (cons (package-name *package*) input))))
               (on-eval form (lambda (line) (push line lines)))
               (unless (eq repl-package *package*)
                 (funcall set-repl-package *package*))
               (set-editor-value code-mirror "")
               (nreverse lines)))))))

(define-react-component <prompt> (package-name)
  ;; (tag <prompt> (:package-name ...))で受け取ったときにはstringがcl->jsされているので一旦元に戻さないといけない
  (setq package-name (ffi:js->cl package-name))
  (tag :span (:class-name #j"prompt") (format nil "~A>" package-name)))

(define-react-component <backlog> (lines)
  (tag :ul (:class-name #j"back-log")
       (let ((i 0))
         (map 'vector
              (lambda (line)
                (tag :li (:key (incf i))
                     (if (consp line)
                         (destructuring-bind (package-name . code) line
                           (tag :div (:class-name #j"line")
                                (tag <prompt> (:package-name package-name))
                                (tag :span (:class-name #j"code") code)))
                         (tag :span (:class-name #j"code")
                              line))))
              lines))))

(define-react-component <repl> ()
  (with-state ((lines set-lines nil)
               (repl-package set-repl-package (find-package :cl-user))
               (history-index set-history-index 0)
               (history set-history nil))
    (flet ((up (code-mirror)
             (when (<= 0 (1- history-index) (1- (length history)))
               ((ffi:ref code-mirror :set-value)
                (ffi:cl->js (elt history (1- history-index))))
               (set-history-index (1- history-index))))
           (down (code-mirror)
             (let ((next (1+ history-index))
                   (last (1- (length history))))
               (cond ((<= 0 next last)
                      ((ffi:ref code-mirror :set-value)
                       (ffi:cl->js (elt history next)))
                      (set-history-index next))
                     ((< last next)
                      ((ffi:ref code-mirror :set-value)
                       #j"")
                      (set-history-index (1+ last)))))))
      (tag :div ()
           (tag <backlog> (:lines lines))
           (tag :div (:class-name #j"repl-input")
                (tag <prompt> (:package-name (package-name repl-package)))
                (tag (ffi:ref js:-code-mirror :-un-controlled)
                     (:value #j""
                      :options (ffi:object
                                :mode #j"commonlisp"
                                :key-map #j"emacs"
                                :extra-keys (ffi:object
                                             "Enter" (lambda (code-mirror)
                                                       (let ((new-lines
                                                               (on-enter code-mirror
                                                                         repl-package
                                                                         #'set-repl-package)))
                                                         (set-lines (append lines new-lines))))
                                             "Up" #'up
                                             "Ctrl-P" #'up
                                             "Down" #'down
                                             "Ctrl-N" #'down)
                                :autofocus js:true)
                      :editor-did-mount (lambda (editor &rest args)
                                          (declare (ignore args))
                                          ((ffi:ref editor :focus))))))))))

(setup #'<repl> "root")
