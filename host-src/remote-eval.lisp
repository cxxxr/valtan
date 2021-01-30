(defpackage :valtan-host.remote-eval
  (:use :cl)
  (:export :start
           :js-eval
           :repl))
(in-package :valtan-host.remote-eval)

(defvar *server*)

(defmethod hunchentoot:acceptor-log-access
    ((acceptor hunchensocket:websocket-acceptor)
     &key &allow-other-keys)
  nil)

(defmethod hunchentoot:acceptor-log-message
    ((acceptor hunchensocket:websocket-acceptor)
     log-level format-string &rest format-arguments)
  (declare (ignore format-arguments))
  nil)

(defun on-connect (*server*))

(defun on-disconnect (*server*))

(defun on-message (*server* message)
  (declare (ignore message)))

(defun start ()
  (setf *server*
        (trivial-ws:make-server :on-connect 'on-connect
                                :on-disconnect 'on-disconnect
                                :on-message 'on-message))
  (trivial-ws:start *server* 40000))

(defun js-eval (form)
  (let ((js-string (compiler:compile-toplevel form nil)))
    (dolist (client (trivial-ws:clients *server*))
      (trivial-ws:send client js-string))))

(defun repl ()
  (flet ((input ()
           (format t "~&~A> " (system::%structure-ref
                               (valtan-core::package-name valtan-core::*package*)
                               0))
           (force-output)
           (let ((valtan-core::*standard-input* (valtan-core::make-standard-input-stream)))
             (valtan-host.reader:read-in-valtan))))
    (let ((*package* (find-package :valtan-user))
          (system:*get-stdin-line-function* (lambda ()
                                              (system::make-structure-array! (read-line)))))
      (loop :for form := (input)
            :do (js-eval form)))))

#|
;;; example

(ql:quickload :valtan)
(in-package :valtan-host.remote-eval)
(valtan-host:build-system #p"example/remote-eval-demo/remote-eval-demo.system")
(start)

;; ここでindex.htmlをブラウザで開く

(repl)
COMMON-LISP-USER> (js:alert #j"Hello World")
|#
