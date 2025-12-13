(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")
(ffi:require js:react-dom-client "react-dom/client")

(defpackage :valtan.react-utilities
  (:use :cl)
  (:export :define-react-component
           :with-state
           :with-effect
           :with-memo
           :with-callback
           :with-ref
           :jsx
           :setup))
(in-package :valtan.react-utilities)

(defmacro define-react-component (name (&rest keys) &body body)
  (let ((garg (gensym))
        (gargs (gensym))
        (v (gensym)))
    `(defun ,name (,garg &rest ,gargs)
       (let ,(mapcar (lambda (key)
                       (if (consp key)
                           (destructuring-bind (key default) key
                             `(,key (let ((,v (ffi:aget ,garg
                                                        (ffi:cl->js ,(compiler::kebab-to-lower-camel-case
                                                                      (string key))))))
                                      (if (eq ,v #j:undefined)
                                          ,default
                                          ,v))))
                           `(,key (ffi:aget ,garg
                                            (ffi:cl->js ,(compiler::kebab-to-lower-camel-case
                                                          (string key)))))))
                     keys)
         (or (progn ,@body)
             #j:null)))))

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

(defmacro with-effect ((&optional deps) &body body)
  "Execute side effects. DEPS is a list of dependencies (nil = every render, () = mount only)."
  (let ((deps-array (cond ((null deps)
                           '#j:undefined)
                          ((eq deps '())
                           '(ffi:array))
                          (t
                           `(ffi:array ,@deps)))))
    `(js:react.use-effect (lambda () ,@body) ,deps-array)))

(defmacro with-memo ((&rest deps) &body body)
  "Return memoized value, recomputed only when DEPS change."
  `(js:react.use-memo (lambda () ,@body)
                      (ffi:array ,@deps)))

(defmacro with-callback ((&rest deps) &body body)
  "Return memoized callback, updated only when DEPS change."
  `(js:react.use-callback (lambda () ,@body)
                          (ffi:array ,@deps)))

(defmacro with-ref (initial-value)
  "Create a mutable ref object with INITIAL-VALUE."
  `(js:react.use-ref ,initial-value))

(eval-when (:compile-toplevel)
  (defun react-component-p (x)
    (and (symbolp x)
         (let ((name (symbol-name x)))
           (and (< 2 (length name))
                (char= #\< (aref name 0))
                (char= #\> (aref name (1- (length name))))))))
  (defun js-symbol-p (symbol)
    (and (symbolp symbol)
         (eq (find-package :js)
             (symbol-package symbol))))
  (defun react-tag-symbol-p (x)
    (or (keywordp x)
        (js-symbol-p x)
        (react-component-p x))))

(defun convert-child (child)
  (cond ((null child)
         #j:null)
        ((consp child)
         (apply #'ffi:array (mapcar #'convert-child child)))
        (t
         (ffi:cl->js child))))

(defmacro tag (tag option &body children)
  `(js:react.create-element ,(cond ((js-symbol-p tag)
                                    `(ffi:cl->js ,tag))
                                   ((or (stringp tag) (keywordp tag))
                                    `(ffi:cl->js ,(string-downcase tag)))
                                   ((symbolp tag)
                                    `(function ,tag))
                                   (t
                                    `(ffi:cl->js ,tag)))
                            (ffi:object . ,option)
                            ,@(mapcar (lambda (c) `(convert-child ,c))
                                      children)))

(defmacro jsx (form)
  (cond ((and (consp form)
              (consp (rest form))
              (react-tag-symbol-p (first form)))
         (destructuring-bind (tag-name options &body body) form
           `(tag ,tag-name ,options
                 ,@(mapcar (lambda (form)
                             `(jsx ,form))
                           body))))
        ((null form)
         #j:null)
        (t
         form)))

(defun ensure-function (value)
  (cond ((symbolp value)
         (symbol-function value))
        ((functionp value)
         value)
        (t
         (error "~S is not a function" value))))

(defun setup (app id &key remote-eval)
  (let ((root (js:react-dom-client.create-root
               (js:document.get-element-by-id (ffi:cl->js id)))))
    (funcall (ffi:ref root "render")
             (js:react.create-element (ensure-function app)))
    (when remote-eval
      (valtan.remote-eval:connect
       (lambda ()
         (setup app id))))))

(ffi:set js:window.lisp js:lisp)
(ffi:set js:window.react js:react)
(ffi:set js:window.react-dom js:react-dom)
