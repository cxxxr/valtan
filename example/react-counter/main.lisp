(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")

(defmacro define-react-component (name (&rest keys) &body body)
  (let ((garg (gensym))
        (gargs (gensym)))
    `(ffi:define-function ,name (,garg &rest ,gargs)
       (let ,(mapcar (lambda (key)
                       `(,key (ffi:aget ,garg ,(string-downcase key))))
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
  `(js:react.create-element #j,(if (and (symbolp tag) (eq (find-package :js) (symbol-package tag)))
                                             tag
                                             (string-downcase tag))
                            (ffi:object . ,option)
                            ,@children))

(define-react-component js:-number (children)
  (tag :h1 () children))

(define-react-component js:-app ()
  (with-state ((count set-count 0))
    (let ((handle-click
            (lambda (e)
              (declare (ignore e))
              (funcall set-count (1+ count)))))
      (tag :div ()
           (tag js:-number () count)
           (tag :button
                (#j"onClick" handle-click)
                #j"click")))))

(unless (eq (ffi:typeof js:window) #j"undefined")
  (js:react-dom.render
   (js:react.create-element js:-app)
   (js:document.get-element-by-id #j"example"))
  (ffi:set js:window.lisp js:lisp))
