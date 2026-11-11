;; Run the second frame through the real frame top level and event queue.
(defpackage #:mcclim-threaded-window-checks (:use #:cl))
(in-package #:mcclim-threaded-window-checks)

(defvar *secondary-thread* nil)
(defclass threaded-window-pane (clim:application-pane) ())

(defun display-secondary (frame pane)
  (mcclim-window-checks::display-secondary frame pane)
  (clim:draw-text* pane (format nil "Keys: ~D" (slot-value frame 'count))
                   10 65 :ink clim:+white+))

(clim:define-application-frame threaded-window-frame ()
  ((count :initform 0))
  (:menu-bar nil)
  (:panes (content (clim:make-pane 'threaded-window-pane
                    :display-function #'display-secondary :scroll-bars nil
                    :width 220 :height 160 :min-width 1 :min-height 1)))
  (:layouts (default content))
  (:top-level (clime:simple-event-loop)))

(defmethod clim:handle-event ((pane threaded-window-pane)
                             (event clim:key-press-event))
  (let ((frame (clim:pane-frame pane)))
    (assert (eq clim:*application-frame* frame))
    (assert (eq (bt:current-thread) *secondary-thread*))
    (when (eql (clim:keyboard-event-character event) #\+)
      (incf (slot-value frame 'count))
      (clim:redisplay-frame-pane frame pane :force-p t)
      (force-output pane)
      (mcclim-browser::host-call "status"
        (ffi:cl->js (format nil "Secondary: ~D" (slot-value frame 'count)))))))

(defun start ()
  (assert (member :clim-mp *features*))
  (assert (eq clim:*application-frame* mcclim-window-checks::*primary*))
  (assert (bt:thread-alive-p
           (climi::port-event-process (clim:find-port :server-path '(:browser)))))
  (let ((frame mcclim-window-checks::*secondary*))
    (setf *secondary-thread*
          (bt:make-thread (lambda () (clim:run-frame-top-level frame))
                          :name "McCLIM second window")))
  :started)

(defun counts ()
  (assert (bt:thread-alive-p *secondary-thread*))
  (values (slot-value mcclim-window-checks::*secondary* 'count)
          (slot-value mcclim-window-checks::*primary* 'valtan-clim-demo::count)))

(defun stop ()
  (let ((frame mcclim-window-checks::*secondary*))
    (bt:interrupt-thread *secondary-thread* (lambda () (clim:frame-exit frame)))
    (bt:join-thread *secondary-thread*)
    (assert (not (bt:thread-alive-p *secondary-thread*)))
    (assert (eq (clim:frame-state frame) :disabled)))
  :stopped)

:threaded-window-checks-loaded
