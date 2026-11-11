(in-package #:mcclim-browser)

(climi::define-bitmap-file-writer :png (pattern destination)
  ;; Collapse through McCLIM so indexed palettes, alpha and transformed
  ;; patterns use the same sampling rules as other bitmap backends.
  (let* ((rgba (climi::pattern-array
                (climi::%collapse-pattern pattern 0 0
                                          (pattern-width pattern)
                                          (pattern-height pattern))))
         (width (array-dimension rgba 1))
         (height (array-dimension rgba 0))
         (png (make-instance 'zpng:png :width width :height height
                             :color-type :truecolor-alpha))
         (data (zpng:data-array png)))
    (dotimes (y height)
      (dotimes (x width)
        (let ((pixel (aref rgba y x)))
          (setf (aref data y x 0) (ldb (byte 8 16) pixel)
                (aref data y x 1) (ldb (byte 8 8) pixel)
                (aref data y x 2) (ldb (byte 8 0) pixel)
                (aref data y x 3) (ldb (byte 8 24) pixel)))))
    (if (streamp destination)
        (progn (zpng:write-png-stream png destination) destination)
        (zpng:write-png png destination))))

;; Match the standard bitmap backend's default output format. A backend that
;; already registered a fallback keeps control of unspecified formats.
(unless (gethash :fallback climi::*bitmap-file-writers*)
  (setf (gethash :fallback climi::*bitmap-file-writers*)
        (gethash :png climi::*bitmap-file-writers*)))
