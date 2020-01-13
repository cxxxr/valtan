(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")
(ffi:require js:-code-mirror "react-codemirror2")
(ffi:require "codemirror/keymap/emacs")
(ffi:require "codemirror/mode/commonlisp/commonlisp")

#|
TODO
- スタックトレースを出す
- REPLに色を付ける
- 履歴機能
- * ** *** + +++ +++ / // ///
- リスタート
|#

(defun send-eval (e append-lines repl-package set-repl-package)
  (let ((*package* repl-package))
    (declare (special *package*))
    (let* ((text (ffi:js->cl ((ffi:ref e :get-value))))
           (incomplete '#:incomplete)
           (form
             (handler-case (read-from-string text)
               (error () incomplete))))
      (js:console.log (ffi:cl->js (package-name *package*)))
      (if (eq form incomplete)
          ((ffi:ref e :replace-selection) #j(string #\newline) #j"end")
          (let ((value (handler-case (format nil "~S" (eval form))
                         (error (e) (princ-to-string e)))))
            (funcall append-lines
                     (list (format nil "~A> ~A" (package-name repl-package) text)
                           value))
            ((ffi:ref e :set-value) #j"")))
      (js:console.log (ffi:cl->js (package-name *package*)))
      (funcall set-repl-package *package*))))

(define-react-component js:-backlog (lines)
  (tag :ul (:class-name "back-log")
       (let ((i 0))
         (map 'vector
              (lambda (line)
                (tag :li (:key (incf i))
                     (tag :span (:class-name "prompt")
                          line)))
              lines))))

(define-react-component js:-repl ()
  (with-state ((lines set-lines nil)
               (repl-package set-repl-package (find-package :cl-user)))
    (tag :div ()
         (tag js:-backlog (:lines lines))
         (tag :div (:class-name "repl-input")
              (tag :span (:class-name "prompt") (format nil "~A>" (package-name repl-package)))
              (tag (ffi:ref js:-code-mirror :-un-controlled)
                   (:value ""
                    :options (ffi:object
                              :mode "commonlisp"
                              :key-map "emacs"
                              :extra-keys (ffi:object
                                           :-enter (lambda (e)
                                                     (send-eval e
                                                                (lambda (new-lines)
                                                                  (set-lines
                                                                   (append
                                                                    lines
                                                                    new-lines)))
                                                                repl-package
                                                                #'set-repl-package)))
                              :autofocus js:true)
                    :editor-did-mount (lambda (editor &rest args)
                                        (declare (ignore args))
                                        ((ffi:ref editor :focus)))))))))

(js:react-dom.render
 (js:react.create-element js:-repl)
 (js:document.get-element-by-id #j"root"))
