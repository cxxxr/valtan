(in-package :common-lisp)

(defmacro ffi:define-function (name arguments &body body)
  `(progn
     (ffi:var ,name)
     (ffi:set ,(cond ((stringp name) name)
                     ((and (symbolp name)
                           (string= "JS" (package-name (symbol-package name))))
                      name)
                     (t
                      (string name)))
              (lambda ,arguments ,@body))))

(defmacro ffi:define (var value)
  `(progn
     (ffi:var ,var)
     (ffi:set ,(cond ((stringp var) var)
                     ((and (symbolp var)
                           (string= "JS" (package-name (symbol-package var))))
                      var)
                     (t
                      (string var)))
              (ffi:cl->js ,value))))

(defun ffi::%object (&rest plist)
  (let ((object (js:-object)))
    (do ((rest plist (cddr rest)))
        ((null rest))
      (let ((key (car rest))
            (value (cadr rest)))
        (ffi:set (ffi:aget object
                           (cond ((stringp key)
                                  (ffi:cl->js key))
                                 ((keywordp key)
                                  (ffi:cl->js (compiler::kebab-to-lower-camel-case (string key))))
                                 (t
                                  key)))
                 value)))
    object))

(defmacro ffi:object (&rest plist)
  (let ((new-plist '()))
    (do ((plist plist (cddr plist)))
        ((null plist))
      (let ((key (car plist))
            (value (cadr plist)))
        (push (cond ((stringp key)
                     `(ffi:cl->js ,key))
                    ((keywordp key)
                     `(ffi:cl->js ,(compiler::kebab-to-lower-camel-case (string key))))
                    (t
                     key))
              new-plist)
        (push value new-plist)))
    `(ffi::%object ,@(nreverse new-plist))))

(defun ffi:array (&rest args)
  (apply (ffi:ref "Array") args))

(defmacro ffi:console.log (&rest args)
  `((ffi:ref "console" "log") ,@args))

(defun ffi:js-eval (x)
  (let* ((code (format nil "(function(lisp) { 'use strict'; ~A; });" x))
         (fn (js:eval (ffi:cl->js code))))
    (funcall fn (ffi:ref "lisp"))))

(defun ffi:cl->js (value)
  (cond ((stringp value)
         (system::array-to-js-string value))
        ;; ((listp value)
        ;;  (system::list-to-js-array value))
        ;; ((eq value t)
        ;;  (ffi:ref "true"))
        ;; ((eq value nil)
        ;;  (ffi:ref "false"))
        ;; ((functionp value)
        ;;  (lambda (&rest args)
        ;;    (apply value (mapcar #'ffi:cl->js args))))
        (t
         value)))

(defun ffi:js->cl (value)
  (cond ((eq (ffi:typeof value)
             (system::array-to-js-string "string"))
         (system::js-string-to-array value))
        ;; ((ffi:instanceof value (ffi:ref "Array"))
        ;;  (system::js-array-to-list value))
        ;; ((eq value (ffi:ref "true"))
        ;;  t)
        ;; ((eq value (ffi:ref "false"))
        ;;  nil)
        (t
         value)))
