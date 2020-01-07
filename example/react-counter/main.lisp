(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")

(define-react-component js:-number (children)
  (tag :h1 () children))

(define-react-component js:-counter ()
  (with-state ((count set-count 0))
    (let ((handle-click
            (lambda (e)
              (declare (ignore e))
              (set-count (1+ count)))))
      (tag :div ()
           (tag js:-number () count)
           (tag :button
                (:on-Click handle-click)
                "click")))))

(define-react-component js:-repl ()
  (with-state ((text set-text "")
               (result set-result nil))
    (tag :div (:style #j{ :display #j"flex" :flex-Direction #j"column"})
         (tag :input (:option #j"text"
                              :on-change (lambda (e)
                                           (set-text (ffi:js->cl (ffi:ref e "target" "value"))))))
         (tag :button (:on-click (lambda (e)
                                   (declare (ignore e))
                                   (set-result (eval (read-from-string text)))))
              "eval")
         (tag :span () (format nil "~S" result)))))

(define-react-component js:-app ()
  (tag :div ()
       (tag js:-counter ())
       (tag js:-repl ())))

(unless (eq (ffi:typeof js:window) #j"undefined")
  (js:react-dom.render
   (js:react.create-element js:-app)
   (js:document.get-element-by-id #j"example"))
  (ffi:set js:window.lisp js:lisp))
