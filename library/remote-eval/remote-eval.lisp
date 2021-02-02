(defpackage :valtan.remote-eval
  (:use :cl)
  (:export :connect))
(in-package :valtan.remote-eval)

(defun connect (&optional eval-hook)
  (labels ((connect-1 (&rest args)
             (declare (ignore args))
             (let ((ws (ffi:new #j:WebSocket #j"ws://0.0.0.0:40000/")))
               (ffi:set (ffi:ref ws :onmessage)
                        (lambda (e)
                          (let* ((json (#j:JSON:parse (ffi:ref e :data)))
                                 (unbound '#:error)
                                 (value unbound))
                            (unwind-protect
                                 (progn
                                   (setf value (ffi:js-eval (ffi:js->cl (ffi:ref json :code))))
                                   (when eval-hook
                                     (funcall eval-hook)))
                              (when (ffi:ref json :return)
                                ((ffi:ref ws :send)
                                 (#j:JSON:stringify
                                  (ffi:object :value
                                              (ffi:cl->js
                                               (prin1-to-string value))))))))))
               (ffi:set (ffi:ref ws :onopen)
                        (lambda (&rest args)
                          (declare (ignore args))))
               (ffi:set (ffi:ref ws :onclose)
                        (lambda (&rest args)
                          (declare (ignore args))
                          (#j:setTimeout #'connect-1 1))))))
    (connect-1)))
