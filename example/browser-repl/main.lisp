(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")
(ffi:require js:-code-mirror "react-codemirror2")
(ffi:require "codemirror/keymap/emacs")
(ffi:require "codemirror/mode/commonlisp/commonlisp")

#|
TODO
- * ** *** + +++ +++ / // ///
- リスタート
|#

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

(defun send-eval (code-mirror input repl-package set-repl-package)
  (let ((*package* repl-package))
    (declare (special *package*))
    (let* ((text (ffi:js->cl ((ffi:ref code-mirror :get-value))))
           (incomplete '#:incomplete)
           (form
             (handler-case (read-from-string text)
               (error () incomplete))))
      (if (eq form incomplete)
          ((ffi:ref code-mirror :replace-selection) #j(string #\newline) #j"end")
          (let ((value (block outer
                         (handler-bind ((error (lambda (e)
                                                 (return-from outer
                                                   (with-output-to-string (out)
                                                     (format out "~A~%" e)
                                                     (cl::print-backtrace :stream out))))))
                           (format nil "~S" (eval form))))))
            (funcall input
                     (package-name repl-package)
                     text
                     value)
            ((ffi:ref code-mirror :set-value) #j"")))
      (funcall set-repl-package *package*))))

(define-react-component js:-prompt (package-name)
  ;; (tag js:-prompt (:package-name ...))で受け取ったときにはstringがcl->jsされているので一旦元に戻さないといけない
  (setq package-name (ffi:js->cl package-name))
  (tag :span (:class-name "prompt") (format nil "~A>" package-name)))

(define-react-component js:-backlog (lines)
  (tag :ul (:class-name "back-log")
       (let ((i 0))
         (map 'vector
              (lambda (line)
                (tag :li (:key (incf i))
                     (if (consp line)
                         (destructuring-bind (package-name code) line
                           (tag :div (:class-name "line")
                                (tag js:-prompt (:package-name package-name))
                                (tag :span (:class-name "code") code)))
                         (tag :span (:class-name "code")
                              line))))
              lines))))

(define-react-component js:-repl ()
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
           (tag js:-backlog (:lines lines))
           (tag :div (:class-name "repl-input")
                (tag js:-prompt (:package-name (package-name repl-package)))
                (tag (ffi:ref js:-code-mirror :-un-controlled)
                     (:value ""
                      :options (ffi:object
                                :mode "commonlisp"
                                :key-map "emacs"
                                :extra-keys (ffi:object
                                             "Enter" (lambda (code-mirror)
                                                       (send-eval code-mirror
                                                                  (lambda (package-name text output)
                                                                    (set-lines (append lines
                                                                                       (cons (list package-name text)
                                                                                             (split-lines output))))
                                                                    (let ((history (append history (list text))))
                                                                      (set-history history)
                                                                      (set-history-index (length history))))
                                                                  repl-package
                                                                  #'set-repl-package))
                                             "Up" #'up
                                             "Ctrl-P" #'up
                                             "Down" #'down
                                             "Ctrl-N" #'down)
                                :autofocus js:true)
                      :editor-did-mount (lambda (editor &rest args)
                                          (declare (ignore args))
                                          ((ffi:ref editor :focus))))))))))

(js:react-dom.render
 (js:react.create-element js:-repl)
 (js:document.get-element-by-id #j"root"))
