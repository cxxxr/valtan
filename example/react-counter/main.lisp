(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")

(defmacro define-react-component (name (&rest keys) &body body)
  (let ((garg (gensym))
        (gargs (gensym)))
    `(ffi:define-function ,name (,garg &rest ,gargs)
       (let ,(mapcar (lambda (key)
                       `(,key (ffi:aget ,garg (ffi:cl->js ,(string-downcase key)))))
                     keys)
         ,@body))))

(defmacro with-state (bindings &body body)
  `(let* ,(mapcan (lambda (b)
                    (destructuring-bind (var set-var default) b
                      (let ((state-var (gensym (string var))))
                        `((,state-var (js:react.use-state ,default))
                          (,var (ffi:aget ,state-var 0))
                          (,set-var (ffi:aget ,state-var 1))))))
                  bindings)
     ,@body))

(defmacro tag (tag option &body children)
  `(js:react.create-element (ffi:cl->js
                             ,(if (and (symbolp tag) (eq (find-package :js) (symbol-package tag)))
                                  tag
                                  (string-downcase tag)))
                            (ffi:object . ,option)
                            ,@children))

(define-react-component js:-number (children)
  (tag :h1 () children))

(define-react-component js:-counter ()
  (with-state ((count set-count 0))
    (let ((handle-click
            (lambda (e)
              (declare (ignore e))
              (let ((start (js:-date.now)))
                (funcall set-count (1+ count))
                (ffi:console.log start (js:-date.now) (- (js:-date.now) start))))))
      (tag :div ()
           (tag js:-number () count)
           (tag :button
                (#j"onClick" handle-click)
                #j"click")))))

(define-react-component js:-repl ()
  (with-state ((text set-text "")
               (result set-result nil))
    (tag :div (#j"style" (ffi:object #j"display" #j"flex" #j"flexDirection" #j"column"))
         (tag :input (#j"option" #j"text"
                                 #j"onChange" (lambda (e)
                                                (funcall set-text (ffi:js->cl (ffi:ref e "target" "value"))))))
         (tag :button (#j"onClick" (lambda (e)
                                     (declare (ignore e))
                                     (funcall set-result (eval (read-from-string text)))))
              #j"eval")
         (tag :span () (ffi:cl->js (format nil "~S" result))))))

(define-react-component js:-app ()
  (tag :div ()
       (tag js:-counter ())
       (tag js:-repl ())))

(unless (eq (ffi:typeof js:window) #j"undefined")
  (js:react-dom.render
   (js:react.create-element js:-app)
   (js:document.get-element-by-id #j"example"))
  (ffi:set js:window.lisp js:lisp))
