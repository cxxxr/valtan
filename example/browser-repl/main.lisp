#|
(defun map-index (type fn sequence)
  (let ((i -1))
    (map type
         (lambda (x)
           (funcall fn x (incf i)))
         sequence)))

(define-react-component js:-history (history)
  (tag :ul ()
       (map-index 'vector
                  (lambda (text i)
                    (tag :li (:key i) text))
                  history)))

(define-react-component js:-input (on-return)
  (with-state ((text set-text ""))
    (tag :input (:option #j"text"
                 :value (ffi:cl->js text)
                 :on-change (lambda (e)
                              (set-text (ffi:js->cl (ffi:ref e "target" "value"))))
                 :on-key-press (lambda (e)
                                 (when (eq (ffi:ref e "key") #j"Enter")
                                   ((ffi:ref e "preventDefault"))
                                   (set-text #j"")
                                   (funcall on-return text)))))))

(define-react-component js:-listener ()
  (with-state ((result set-result nil)
               (history set-history nil))
    (tag :div (:style (ffi:object :display #j"flex" :flex-Direction #j"column"))
         (tag js:-history (:history history))
         (tag js:-input (:on-return (lambda (text)
                                      (set-history
                                       (append history
                                               (list text
                                                     (eval (read-from-string text)))))))))))

(define-react-component js:-app ()
  (tag js:-listener ()))

(unless (eq (ffi:typeof js:window) #j"undefined")
  (js:react-dom.render
   (js:react.create-element js:-app)
   (js:document.get-element-by-id #j"repl")))
|#

(defun send-eval (e)
  (let ((text (ffi:js->cl ((ffi:ref *code-mirror* :get-value)))))
    (js:alert (ffi:cl->js (eval (read-from-string text))))))

(defparameter *code-mirror*
  (js:-code-mirror
   (js:document.get-element-by-id #j"repl")
   (ffi:object :mode #j"commonlisp"
               :key-map #j"emacs"
               :extra-keys (ffi:object #j"Enter" #'send-eval))))

((ffi:ref *code-mirror* "focus"))

(let ((last-length 0))
  (labels ((handle-change (&rest args)
             (declare (ignore args))
             (let* ((text (ffi:js->cl ((ffi:ref *code-mirror* :get-value))))
                    (length (length text)))
               (when (and (< last-length length)
                          (ignore-errors (read-from-string text) t))
                 ((ffi:ref *code-mirror* :off) #j"change" #'handle-change)
                 (handle-enter text)
                 ((ffi:ref *code-mirror* :on) #j"change" #'handle-change))
               (setq last-length length))))
    ;; ((ffi:ref *code-mirror* :on) #j"change" #'handle-change)
    ((ffi:ref *code-mirror* "focus"))))
