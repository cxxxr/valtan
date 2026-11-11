(defpackage #:mcclim-png-write-checks (:use #:clim #:clim-lisp))
(in-package #:mcclim-png-write-checks)

(defvar *checks* 0)
(defvar *images* nil)

(defun check (value)
  (assert value () "PNG writer assertion ~D failed" (1+ *checks*))
  (incf *checks*))

(defun check-image (name pattern width height expected &optional (format :png))
  (let ((bytes
          (flexi-streams:with-output-to-sequence (stream)
            (check (eq stream (climi::write-bitmap-file pattern stream :format format)))
            (check (open-stream-p stream)))))
    (flexi-streams:with-input-from-sequence (stream bytes)
      (let* ((png (png-read:read-png-datastream stream))
             (data (png-read:image-data png)))
        (check (and (= (png-read:width png) width) (= (png-read:height png) height)))
        (check (eq (png-read:colour-type png) :truecolor-alpha))
        (check (equal expected
                      (loop for y below height append
                        (loop for x below width collect
                          (loop for c below 4 collect (aref data x y c))))))))
    (push (list name width height (coerce bytes 'list) expected) *images*)))

(defun run ()
  (setf *checks* 0 *images* nil)
  (check (climi::bitmap-output-supported-p :png))
  (let* ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                           :initial-contents '((#xffff0000 #x8000ff00 #x00010203)
                                               (#xff0000ff #x01123456 #xfeabcdef))))
         (image (make-instance 'climi::image-pattern :array array))
         (expected '((255 0 0 255) (0 255 0 128) (1 2 3 0)
                     (0 0 255 255) (18 52 86 1) (171 205 239 254))))
    (check-image "rgba" image 3 2 expected)
    (check-image "default" image 3 2 expected :bitmap)
    (check-image "rgba-repeat" image 3 2 expected))
  (let* ((indices (make-array '(2 2) :initial-contents '((0 1) (2 3))))
         (palette (vector +red+ +green+ +blue+ +transparent-ink+))
         (pattern (make-pattern indices palette)))
    (check-image "palette" pattern 2 2
                 '((255 0 0 255) (0 255 0 255) (0 0 255 255) (0 0 0 0)))
    (setf (aref palette 0) +blue+ (aref indices 1 1) 0)
    (check-image "palette-mutation" pattern 2 2
                 '((0 0 255 255) (0 255 0 255) (0 0 255 255) (0 0 255 255))))
  (let ((pattern (make-pattern #2A((0 1)) (vector +red+ +blue+))))
    (check-image "tile" (make-rectangular-tile pattern 4 2) 4 2
                 '((255 0 0 255) (0 0 255 255) (0 0 0 0) (0 0 0 0)
                   (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
    ;; Integral float dimensions exercise the collapse-pattern rounding path.
    (check-image "scaled" (transform-region (make-scaling-transformation 2.0 1.0) pattern)
                 4 1 '((255 0 0 255) (0 0 255 255) (0 0 255 255) (0 0 0 0))))
  (let ((array (make-array '(32 64) :element-type '(unsigned-byte 32)))
        (expected nil))
    (dotimes (y 32)
      (dotimes (x 64)
        (let ((r (mod (+ (* x 17) y) 256)) (g (mod (+ x (* y 31)) 256))
              (b (mod (* (+ x 1) (+ y 7)) 256)) (a (mod (+ x (* y 7)) 256)))
          (setf (aref array y x) (logior (ash a 24) (ash r 16) (ash g 8) b))
          (push (list r g b a) expected))))
    (check-image "multirow" (make-instance 'climi::image-pattern :array array)
                 64 32 (nreverse expected)))
  (let ((stream (flexi-streams:make-in-memory-output-stream)))
    (unwind-protect
         (progn
           (check (handler-case
                      (progn (climi::write-bitmap-file
                               (make-instance 'climi::image-pattern
                                              :array (make-array '(0 1) :element-type '(unsigned-byte 32)))
                               stream :format :png) nil)
                    (zpng::invalid-size () t)))
           (check (open-stream-p stream))
           (check (zerop (length (flexi-streams:get-output-stream-sequence stream)))))
      (close stream)))
  (setf *images* (nreverse *images*))
  (format t "~&MCCLIM-PNG-WRITE-CHECKS-PASS ~D~%" *checks*)
  (values :passed *checks*))

(defun run-files (pathname)
  (let* ((pattern (make-instance 'climi::image-pattern
                                :array (make-array '(1 2) :element-type '(unsigned-byte 32)
                                                   :initial-contents '((#xff123456 #x80765432)))))
         (written (climi::write-bitmap-file pattern pathname :format :png)))
    (check (pathnamep written))
    (check (equal written (truename pathname)))
    (let ((png (png-read:read-png-file written)))
      (check (= (png-read:width png) 2))
      (check (equal '(18 52 86 255)
                    (loop for c below 4 collect (aref (png-read:image-data png) 0 0 c)))))
    ;; The default :BITMAP writer must replace an existing file completely.
    (let ((replacement (make-pattern #2A((0)) (vector +blue+))))
      (check (equal (climi::write-bitmap-file replacement pathname) written))
      (let ((png (png-read:read-png-file written)))
        (check (= (png-read:width png) 1))
        (check (equal '(0 0 255 255)
                      (loop for c below 4 collect (aref (png-read:image-data png) 0 0 c)))))))
  (format t "~&MCCLIM-PNG-WRITE-FILES-PASS ~D~%" *checks*)
  (values :passed *checks*))
