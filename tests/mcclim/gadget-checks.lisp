;; Exercise upstream gadgets through their normal event handlers and callbacks.
;; The browser driver supplies real pointer and keyboard events.
(defpackage #:mcclim-gadget-checks (:use #:clim #:clim-lisp))
(in-package #:mcclim-gadget-checks)

(defvar *frame* nil)
(defvar *changes* nil)
(defvar *activations* 0)
(defvar *previous-focus* nil)

(defun changed (gadget value)
  (push (list (pane-name gadget) value) *changes*))

(defun top-level (frame)
  (redisplay-frame-panes frame :force-p t)
  (clim-backend:port-force-output (port frame))
  (mcclim-browser::host-call "status" (ffi:cl->js "Gadgets ready"))
  (clime:simple-event-loop frame))

(define-application-frame gadget-frame () ()
  (:menu-bar nil)
  (:panes
   (editor :text-field :value "" :ncolumns 25
           :value-changed-callback #'changed
           :activate-callback (lambda (gadget)
                                (declare (ignore gadget))
                                (incf *activations*)))
   (slider :slider :min-value 0 :max-value 100 :value 0
           :orientation :horizontal :show-value-p t
           :value-changed-callback #'changed)
   (toggle :toggle-button :label "Enabled" :value nil
           :value-changed-callback #'changed)
   (quit :push-button :label "Close test frame"
         :activate-callback (lambda (gadget) (frame-exit (pane-frame gadget)))))
  (:layouts (default (vertically () (40 editor) (60 slider) (40 toggle) (40 quit))))
  (:top-level (top-level)))

(defun run ()
  (setf *previous-focus* (climi::port-keyboard-input-focus (port *application-frame*))
        *frame* (make-application-frame 'gadget-frame
                  :frame-manager (frame-manager *application-frame*)
                  :pretty-name "McCLIM gadget checks"
                  :left 40 :top 35 :width 420 :height 180)
        *changes* nil *activations* 0)
  (unwind-protect (run-frame-top-level *frame*)
    (destroy-frame *frame*))
  :gadget-frame-closed)

(defun gadget (name) (find-pane-named *frame* name))
(defun prepare-focus-transfer ()
  (assert *previous-focus*)
  (setf (gadget-value (gadget 'editor)) ""
        *changes* nil
        (climi::port-keyboard-input-focus (port *frame*)) *previous-focus*)
  :focus-reset)
(defun bounds (name)
  (let* ((pane (gadget name))
         (mirror-sheet (sheet-mirrored-ancestor pane))
         (transformation (sheet-delta-transformation pane mirror-sheet)))
    (multiple-value-bind (x y) (transform-position transformation 0 0)
      (list (mcclim-browser::property (sheet-direct-mirror mirror-sheet) "id")
            x y (bounding-rectangle-width pane) (bounding-rectangle-height pane)))))

(defun check-editor (expected &optional (activations 0))
  (assert (string= expected (gadget-value (gadget 'editor))))
  (assert (= *activations* activations))
  (assert (find expected *changes* :key #'second :test #'equal))
  :editor-passed)

(defun check-toggle (expected)
  (assert (eql expected (gadget-value (gadget 'toggle))))
  (assert (find (list 'toggle expected) *changes* :test #'equal))
  :toggle-passed)

(defun check-slider (minimum maximum)
  (let ((value (gadget-value (gadget 'slider))))
    (assert (<= minimum value maximum))
    (assert (find (list 'slider value) *changes* :test #'equal)))
  :slider-passed)

:gadget-checks-loaded
