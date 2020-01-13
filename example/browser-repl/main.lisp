(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")
(ffi:require js:-code-mirror "react-codemirror2")
(ffi:require "codemirror/keymap/emacs")
(ffi:require "codemirror/mode/commonlisp/commonlisp")

#|
TODO
- *package*の値がおかしい
- in-packageが動かない
- (cons 1 2 3)でエラーハンドリングできてなさそう
- スタックトレースを出す
- REPLに色を付ける
|#

(defvar *repl-package* (find-package :valtan-user))

(defun send-eval (e append-lines)
  (let* ((*package* *repl-package*)
         (text (ffi:js->cl ((ffi:ref e :get-value))))
         (incomplete '#:incomplete)
         (form
           (handler-case (read-from-string text)
             (error () incomplete)))
         (old-package *repl-package*))
    (if (eq form incomplete)
        ((ffi:ref e :replace-selection) #j(string #\newline) #j"end")
        (let ((value (handler-case (format nil "~S" (eval form))
                       (error (e) (princ-to-string e)))))
          (funcall append-lines
                   (list (format nil "~A> ~A" (package-name old-package) text)
                         value))
          ((ffi:ref e :set-value) #j"")))))

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
  (with-state ((lines set-lines nil))
    (tag :div ()
         (tag js:-backlog (:lines lines))
         (tag :div (:class-name "repl-input")
              (tag :span (:class-name "prompt") (format nil "~A>" (package-name *repl-package*)))
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
                                                                    new-lines))))))
                              :autofocus js:true)
                    :editor-did-mount (lambda (editor &rest args)
                                        (declare (ignore args))
                                        ((ffi:ref editor :focus)))))))))

(js:react-dom.render
 (js:react.create-element js:-repl)
 (js:document.get-element-by-id #j"repl"))
