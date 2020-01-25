(ffi:require js:react "react")
(defpackage :valtan.react-utilities
  (:use :cl)
  (:export :define-react-component
           :with-state
           :tag
           :jsx))
(in-package :valtan.react-utilities)

(defmacro define-react-component (name (&rest keys) &body body)
  (let ((garg (gensym))
        (gargs (gensym)))
    `(ffi:define-function ,name (,garg &rest ,gargs)
       (let ,(mapcar (lambda (key)
                       `(,key (ffi:aget ,garg (ffi:cl->js ,(compiler::kebab-to-lower-camel-case (string key))))))
                     keys)
         ,@body))))

(defmacro with-state (bindings &body body)
  (let ((setters '()))
    `(let* ,(mapcan (lambda (b)
                      (destructuring-bind (var setter default) b
                        (let ((state-var (gensym (string var)))
                              (set-fn (gensym)))
                          (push (cons set-fn setter) setters)
                          `((,state-var (js:react.use-state ,default))
                            (,var (ffi:aget ,state-var 0))
                            (,set-fn (ffi:aget ,state-var 1))))))
                    bindings)
       (flet ,(mapcar (lambda (elt)
                        (destructuring-bind (set-fn . setter) elt
                          (let ((arg (gensym)))
                            `(,setter (,arg) (funcall ,set-fn ,arg)))))
                      setters)
         ,@body))))

(eval-when (:compile-toplevel)
  (defun js-symbol-p (symbol)
    (and (symbolp symbol)
         (eq (find-package :js)
             (symbol-package symbol)))))

(defmacro tag (tag option &body children)
  `(js:react.create-element ,(cond ((js-symbol-p tag)
                                    `(ffi:cl->js ,tag))
                                   ((or (stringp tag) (symbolp tag))
                                    `(ffi:cl->js ,(string-downcase tag)))
                                   (t
                                    `(ffi:cl->js ,tag)))
                            (ffi:object . ,option)
                            ,@(mapcar (lambda (c) `(ffi:cl->js ,c))
                                      children)))

(defmacro jsx (form)
  (if (atom form)
      form
      (destructuring-bind (tag-name options &body body) form
        (cond ((or (keywordp tag-name)
                   (js-symbol-p tag-name))
               `(tag ,tag-name ,options
                     ,@(mapcar (lambda (form)
                                 `(jsx ,form))
                               body)))
              (t
               form)))))
