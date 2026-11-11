(in-package #:mcclim-browser)

(defun canvas-bezier-path (surface design)
  (let ((segments (mcclim-bezier:segments design)))
    (canvas-call surface "beginPath")
    (when (plusp (length segments))
      (let ((start (slot-value (elt segments 0) 'mcclim-bezier::p0)))
        (canvas-call surface "moveTo" (point-x start) (point-y start)))
      (map nil
           (lambda (segment)
             (with-slots (mcclim-bezier::p1 mcclim-bezier::p2 mcclim-bezier::p3) segment
               (canvas-call surface "bezierCurveTo"
                            (point-x mcclim-bezier::p1) (point-y mcclim-bezier::p1)
                            (point-x mcclim-bezier::p2) (point-y mcclim-bezier::p2)
                            (point-x mcclim-bezier::p3) (point-y mcclim-bezier::p3))))
           segments))))

(defmethod mcclim-bezier:medium-draw-bezier-design* :around
    ((medium browser-medium) design)
  ;; The upstream Bezier around method transforms the design but, unlike CLIM's
  ;; primitive drawing methods, leaves the medium transformation in place.
  ;; Consume it here so detached media are not transformed twice, and scale the
  ;; line style by the same rules as other CLIM drawing operations.
  (let ((transformation (medium-transformation medium))
        (style (medium-line-style medium)))
    (if (identity-transformation-p transformation)
        (call-next-method)
        (let ((transformed (transform-region transformation design))
              (effective-style (climi::medium-effective-line-style medium)))
          (unwind-protect
               (progn
                 (setf (medium-line-style medium) effective-style
                       (medium-transformation medium) +identity-transformation+)
                 (call-next-method medium transformed))
            (setf (medium-transformation medium) transformation
                  (medium-line-style medium) style))))))

(defmethod mcclim-bezier:medium-draw-bezier-design*
    ((medium browser-medium) (design mcclim-bezier:bezier-curve))
  (with-canvas (medium)
    (canvas-bezier-path medium design)
    (canvas-call medium "stroke")))

(defmethod mcclim-bezier:medium-draw-bezier-design*
    ((medium browser-medium) (design mcclim-bezier:bezier-area))
  (with-canvas (medium)
    (canvas-bezier-path medium design)
    (canvas-call medium "closePath")
    (canvas-call medium "fill")))

(defmethod canvas-region-mask ((region mcclim-bezier:bezier-curve) width height)
  ;; A path has no area, even though drawing it with a line style has coverage.
  (host-call "createSurface" width height))

(defmethod canvas-region-mask ((region mcclim-bezier:bezier-area) width height)
  (let ((mask (host-call "createSurface" width height)))
    (canvas-set mask "fillStyle" "white")
    (canvas-bezier-path mask region)
    (canvas-call mask "closePath")
    (canvas-call mask "fill")
    mask))

(defun canvas-bezier-set-mask (positives negatives width height)
  ;; Compose coverage, independently of winding and ink. Recursing through the
  ;; region protocol also permits these designs inside ordinary region trees.
  (let ((mask (host-call "createSurface" width height)))
    (dolist (area positives)
      (canvas-call mask "drawImage" (property (canvas-region-mask area width height) "canvas") 0 0))
    (canvas-set mask "globalCompositeOperation" "destination-out")
    (dolist (area negatives)
      (canvas-call mask "drawImage" (property (canvas-region-mask area width height) "canvas") 0 0))
    mask))

(defmethod canvas-region-mask ((region mcclim-bezier:bezier-union) width height)
  (canvas-bezier-set-mask (mcclim-bezier:areas region) nil width height))

(defmethod canvas-region-mask ((region mcclim-bezier:bezier-difference) width height)
  (canvas-bezier-set-mask (mcclim-bezier:positive-areas region)
                         (mcclim-bezier:negative-areas region) width height))

(defun canvas-bezier-areas (medium design)
  (with-canvas (medium)
    (let* ((destination (canvas-surface medium))
           (canvas (property destination "canvas"))
           (width (property canvas "width")) (height (property canvas "height"))
           (mask (canvas-region-mask
                  (transform-region (medium-device-transformation medium) design) width height))
           (paint (host-call "createSurface" width height)))
      ;; Construct coverage independently of ink and the existing destination.
      ;; Opposite windings must not cancel a union; overlapping positives must
      ;; not apply translucent ink twice; holes must preserve prior drawing.
      ;; WITH-CANVAS supplies white coverage for composite or flipping inks.
      ;; Use its actual style rather than resolving MEDIUM-INK a second time.
      (canvas-set paint "fillStyle" (property (property destination "context") "fillStyle"))
      (canvas-call paint "fillRect" 0 0 width height)
      (canvas-set paint "globalCompositeOperation" "destination-in")
      (canvas-call paint "drawImage" (property mask "canvas") 0 0)
      (canvas-call medium "save")
      (unwind-protect
           (progn
             (canvas-call medium "resetTransform")
             (canvas-call medium "drawImage" (property paint "canvas") 0 0))
        (canvas-call medium "restore")))))

(defmethod mcclim-bezier:medium-draw-bezier-design*
    ((medium browser-medium) (design mcclim-bezier:bezier-union))
  (canvas-bezier-areas medium design))

(defmethod mcclim-bezier:medium-draw-bezier-design*
    ((medium browser-medium) (design mcclim-bezier:bezier-difference))
  (canvas-bezier-areas medium design))
