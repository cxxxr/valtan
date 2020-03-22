#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defun js-symbol-p (symbol)
  (cl:and (symbolp symbol)
          (cl:string= (cl:package-name (cl:symbol-package symbol))
                      (cl:load-time-value (cl:coerce '(#\J #\S) 'cl:string)))))

(defmacro ffi:define-function (name arguments &body body)
  `(progn
    (ffi:var ,name)
    (ffi:set
     ,(cl:cond ((cl:stringp name) name)
               ((js-symbol-p name)
                name)
               (t (cl:string name)))
     (lambda ,arguments ,@body))))

(defmacro ffi:define (var value)
  `(progn
    (ffi:var ,var)
    (ffi:set
     ,(cl:cond ((cl:stringp var) var)
               ((js-symbol-p var) var)
               (t (cl:string var)))
     (ffi:cl->js ,value))))

(defun ffi::%object (&rest plist)
  (let ((object (js::-object)))
    (cl:do ((cl:rest plist (cl:cddr cl:rest)))
           ((cl:null cl:rest))
      (let ((key (cl:car cl:rest)) (value (cl:cadr cl:rest)))
        (ffi:set
         (ffi:aget object
                   (cl:cond ((cl:stringp key) (ffi:cl->js key))
                            ((cl:keywordp key)
                             (ffi:cl->js (compiler::kebab-to-lower-camel-case (cl:string key))))
                            (t key)))
         value)))
    object))

(defmacro ffi:object (&rest plist)
  (let ((new-plist 'nil))
    (cl:do ((plist plist (cl:cddr plist)))
           ((cl:null plist))
      (let ((key (cl:car plist)) (value (cl:cadr plist)))
        (cl:push
         (cl:cond ((cl:stringp key) `(ffi:cl->js ,key))
                  ((cl:keywordp key)
                   `(ffi:cl->js ,(compiler::kebab-to-lower-camel-case (cl:string key))))
                  (t key))
         new-plist)
        (cl:push value new-plist)))
    `(ffi::%object ,@(cl:nreverse new-plist))))

(defun ffi:array (&rest args)
  #+valtan
  (cl:apply (ffi:ref "Array") args)
  #-valtan
  (cl:error "unimplemented"))

(defun ffi:js-eval (x)
  #+valtan
  (let* ((code (*:string-append "(function(lisp) { 'use strict'; ~A; });" x))
         (fn (js::eval (ffi:cl->js code))))
    (cl:funcall fn (ffi:ref "lisp")))
  #-valtan
  (cl:error "unimplemented"))

(defun ffi:cl->js (value)
  (cl:cond ((cl:stringp value) (array-contents value))
           ((cl:vectorp value) (array-contents value))
           ;; ((listp value)
           ;;  (*:list-to-js-array value))
           ;; ((eq value t)
           ;;  (ffi:ref "true"))
           ;; ((eq value nil)
           ;;  (ffi:ref "false"))
           ;; ((functionp value)
           ;;  (lambda (&rest args)
           ;;    (apply value (mapcar #'ffi:cl->js args))))
           (t value)))

(defun ffi:js->cl (value)
  (cl:cond ((eq (ffi:typeof value) (*:array-to-js-string "string"))
            (*:js-string-to-array value))
           ((ffi:instanceof value (ffi:ref "Array"))
            (*:js-array-to-array value))
           ;; ((eq value (ffi:ref "true"))
           ;;  t)
           ;; ((eq value (ffi:ref "false"))
           ;;  nil)
           (t value)))
