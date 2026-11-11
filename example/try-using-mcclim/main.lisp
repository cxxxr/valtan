(defpackage #:valtan-clim-demo (:use #:clim #:clim-lisp))
(in-package #:valtan-clim-demo)

(defclass counter-pane (clim:application-pane) ())
(defun refresh-counter (frame)
  (redisplay-frame-pane frame (find-pane-named frame 'canvas) :force-p t)
  (force-output (find-pane-named frame 'canvas))
  (mcclim-browser::host-call "status"
    (ffi:cl->js (format nil "Counter: ~D" (slot-value frame 'count)))))
(defun increment-counter (frame)
  (incf (slot-value frame 'count))
  (refresh-counter frame))
(defun draw-counter (frame pane)
  (draw-rectangle* pane 0 0 600 240 :ink +white+ :filled t)
  (draw-text* pane "McCLIM on Valtan" 24 35 :text-size 24 :text-face :bold)
  (draw-text* pane "Click the button, or press + / - in this pane." 24 70 :text-size 16)
  (draw-text* pane (format nil "Counter: ~D" (slot-value frame 'count)) 24 120 :text-size 26)
  (draw-rectangle* pane 24 155 (+ 64 (* 10 (mod (slot-value frame 'count) 30))) 200
                   :ink +royal-blue+ :filled t)
  (draw-circle* pane 450 170 25 :ink +orange+ :filled t))
(defmethod handle-event ((pane counter-pane) (event key-press-event))
  (let ((character (keyboard-event-character event)) (frame (pane-frame pane)))
    (cond ((eql character #\+) (increment-counter frame))
          ((eql character #\-) (decf (slot-value frame 'count)) (refresh-counter frame)))))
(defun counter-top-level (frame)
  (redisplay-frame-panes frame :force-p t)
  (force-output (find-pane-named frame 'canvas))
  (mcclim-browser:ready)
  (clime:simple-event-loop frame))
(define-application-frame counter-frame ()
  ((count :initform 0))
  (:menu-bar nil)
  (:panes
   (canvas (make-pane 'counter-pane :display-function #'draw-counter
                     :scroll-bars nil :width 600 :height 240))
   (increment :push-button :label "Increment"
              :activate-callback (lambda (gadget)
                                   (increment-counter (pane-frame gadget)))))
  (:layouts (default (vertically () canvas (40 increment))))
  (:top-level (counter-top-level)))

(defun run-counter ()
  (let* ((port (find-port :server-path '(:browser)))
         (manager (find-frame-manager :port port))
         (frame (make-application-frame 'counter-frame :frame-manager manager
                                        :pretty-name "McCLIM on Valtan")))
    (run-frame-top-level frame)))

#+clim-mp
(bt:make-thread #'run-counter :name "McCLIM counter")
#-clim-mp
(run-counter)
