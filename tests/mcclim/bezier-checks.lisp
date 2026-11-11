(in-package #:cl-user)

;; The upstream Bezier smoke test exercises both subdivision and the degenerate
;; tiny-area case. Loading alone would not prove these functions execute.
(let* ((original (mcclim-bezier:make-bezier-area*
                   '(0 0 1 1 2 1 3 0 2 -1 1 -1 0 0)))
       (transform (clim:make-scaling-transformation .01 .01))
       (transformed (clim:transform-region transform original)))
  (assert (typep (mcclim-bezier:polygonalize original) 'clim:standard-polygon))
  (assert (clim:region-equal clim:+nowhere+ (mcclim-bezier:polygonalize transformed))))
(write-line "MCCLIM-BEZIER-CHECKS-PASS")
