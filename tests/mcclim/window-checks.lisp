;; These checks run inside the real application's event loop. The second frame
;; exercises programmatic lifecycle operations; it does not claim a concurrent
;; frame event loop or threading support.
(defpackage #:mcclim-window-checks (:use #:cl))
(in-package #:mcclim-window-checks)

(defvar *primary* clim:*application-frame*)
(defvar *secondary* nil)
(defun top-sheet (frame) (clim:frame-top-level-sheet frame))
(defun mirror (frame) (clim:sheet-direct-mirror (top-sheet frame)))
(defun flush () (clim-backend:port-force-output (clim:port (top-sheet *primary*))))

(defun check-geometry (frame x y width height)
  (multiple-value-bind (actual-x actual-y)
      (clim:transform-position (clim:sheet-transformation (top-sheet frame)) 0 0)
    (assert (= actual-x x)) (assert (= actual-y y)))
  (assert (= (clim:bounding-rectangle-width (top-sheet frame)) width))
  (assert (= (clim:bounding-rectangle-height (top-sheet frame)) height))
  (let ((host (mirror frame)))
    (assert (= (mcclim-browser::property host "x") x))
    (assert (= (mcclim-browser::property host "y") y))
    (let ((canvas (mcclim-browser::property host "canvas")))
      (assert (= (mcclim-browser::property canvas "width") width))
      (assert (= (mcclim-browser::property canvas "height") height))))
  :geometry-matches)

(defun display-secondary (frame pane)
  (declare (ignore frame))
  (clim:draw-rectangle* pane 0 0 220 160 :ink clim:+royal-blue+ :filled t)
  (clim:draw-text* pane "Second frame" 10 30 :ink clim:+white+))

(clim:define-application-frame window-test-frame () ()
  (:menu-bar nil)
  (:panes (content :application :display-function 'display-secondary
                               :scroll-bars nil :width 220 :height 160
                               :min-width 1 :min-height 1))
  (:layouts (default content)))

(defun setup (&optional (frame-class 'window-test-frame))
  (clim:move-and-resize-sheet (top-sheet *primary*) 20 40 600 280)
  (check-geometry *primary* 20 40 600 280)
  (setf *secondary*
        (clim:make-application-frame frame-class
          :frame-manager (clim:frame-manager *primary*) :enable t
          :pretty-name "Second frame" :left 320 :top 80 :width 220 :height 160))
  (clim:redisplay-frame-panes *secondary* :force-p t)
  (check-geometry *secondary* 320 80 220 160)
  (flush)
  :windows-created)

(defun shrink-primary ()
  (clim:shrink-frame *primary*)
  (assert (eq (clim:frame-state *primary*) :shrunk))
  (assert (not (clim:sheet-enabled-p (top-sheet *primary*))))
  (flush)
  :shrunk)

(defun check-restored ()
  (assert (eq (clim:frame-state *primary*) :enabled))
  (assert (clim:sheet-enabled-p (top-sheet *primary*)))
  :restored)

(defun destroy-secondary ()
  (let* ((sheet (top-sheet *secondary*)) (port (clim:port sheet))
         (id (mcclim-browser::property (mirror *secondary*) "id")))
    (clim:destroy-frame *secondary*)
    (assert (eq (clim:frame-state *secondary*) :disowned))
    (assert (null (gethash id (mcclim-browser::port-mirrors port)))))
  (flush)
  :destroyed)

:window-checks-loaded
