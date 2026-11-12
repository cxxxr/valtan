;; This observer and launcher follow the unchanged upstream calculator source.
(in-package #:clim-demo.calculator)

(defmethod (setf gadget-value) :after (value (pane text-field-pane)
                                     &key invoke-callback)
  (declare (ignore invoke-callback))
  (when (and (typep (pane-frame pane) 'calculator-app)
             (eq pane (find-pane-named (pane-frame pane) 'screen)))
    (mcclim-browser::host-call "status"
      (ffi:cl->js (format nil "Calculator: ~A" value)))))

(defmethod calculator-frame-top-level :before
    ((frame calculator-app) &key command-parser command-unparser
                                partial-command-parser prompt)
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (mcclim-browser::host-call "status"
    (ffi:cl->js
      (format nil "{~{~A~^,~}}"
        (loop for name in '(one two three four five six seven eight nine zero
                           plus dash multiply divide result ac ce)
              for pane = (find-pane-named frame name)
              collect
              (multiple-value-bind (x1 y1 x2 y2)
                  (bounding-rectangle* (sheet-region pane))
                (multiple-value-bind (x y)
                    (transform-position
                      (sheet-delta-transformation pane (frame-top-level-sheet frame))
                      (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
                  (format nil "~S:[~F,~F]" (string-downcase name) x y)))))))
  (mcclim-browser:ready))

(bt:make-thread
  (lambda ()
    (let* ((port (find-port :server-path '(:browser)))
           (manager (find-frame-manager :port port))
           (frame (make-application-frame 'calculator-app :frame-manager manager
                                          :pretty-name "Upstream McCLIM calculator")))
      (run-frame-top-level frame)))
  :name "Upstream McCLIM calculator")
