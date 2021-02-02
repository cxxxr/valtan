(defpackage :valtan-host.remote-eval
  (:use :cl)
  (:export :start
           :stop
           :js-eval
           :repl))
(in-package :valtan-host.remote-eval)

(defvar *server* nil)

(defclass server (trivial-ws:server)
  ((event-queue
    :initform (lem::make-event-queue)
    :reader server-event-queue)
   (handler
    :initform nil
    :accessor server-handler)))

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
  (let* ((json (st-json:read-json-from-string message))
         (value (st-json:getjso "value" json)))
    (lem:send-event value (server-event-queue *server*))))

(defun start ()
  (when *server*
    (error "The server is already running"))
  (setf *server*
        (make-instance 'server
                       :on-connect 'on-connect
                       :on-disconnect 'on-disconnect
                       :on-message 'on-message))
  (setf (server-handler *server*)
        (trivial-ws:start *server* 40000))
  *server*)

(defun stop ()
  (when *server*
    (trivial-ws:stop (server-handler *server*))
    (setf *server* nil)
    t))

(defun js-eval (form &key use-return-value)
  (let* ((js-string (compiler:compile-toplevel form t))
         (message (st-json:write-json-to-string
                   (st-json:jso "code" js-string
                                "return" (if use-return-value
                                             :true
                                             :false)))))
    (dolist (client (trivial-ws:clients *server*))
      (trivial-ws:send client message))))

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
      (loop
        (let ((form (input)))
          (cond ((trivial-ws:clients *server*)
                 (js-eval form)
                 (write-line (lem::dequeue-event nil (server-event-queue *server*))))
                (t
                 (uiop:println "No connection" *error-output*))))))))

#|
;;; example

(ql:quickload :valtan)
(valtan-host:start #p"example/remote-eval-demo/remote-eval-demo.system")

;; ここでindex.htmlをブラウザで開く

(repl)
COMMON-LISP-USER> (js:alert #j"Hello World")
|#
