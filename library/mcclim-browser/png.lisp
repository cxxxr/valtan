(in-package #:mcclim-browser)

;; PNG-READ 0.3.1 reconstructs interlaced RGB samples but leaves INDEX-DATA
;; containing only the last Adam7 pass. Its subsequent tRNS expansion then
;; indexes that small array with full-image coordinates. Preserve the palette
;; indices alongside the upstream decoder's samples. These hooks are active
;; only while the browser bitmap reader is decoding a file.
(defvar *png-reader-active* nil)
(defvar *png-index-array* nil)
(defvar *png-index-passes* nil)

(defmethod png-read::parse-critical-chunk :after
    ((type (eql 'png-read::|IHDR|)) bytes)
  (declare (ignore bytes))
  (let ((state png-read::*png-state*))
    (when (and *png-reader-active*
               (eq (png-read:colour-type state) :indexed-colour)
               (eq (png-read::interlace-method state) :adam7-interlace))
      (setf *png-index-array* (make-array (list (png-read:width state) (png-read:height state)))
            *png-index-passes* (coerce (png-read::make-deinterlace-arrays
                                       (png-read:width state) (png-read:height state)) 'list)))))

(defmethod png-read::decode-data :after
    ((type (eql :indexed-colour)) data state)
  (declare (ignore data))
  (when *png-index-array*
    (let* ((indices (png-read::index-data state))
           (height (array-dimension indices 1)))
      (loop for (x y) in (pop *png-index-passes*) for i from 0
            do (setf (aref *png-index-array* x y)
                     (aref indices (floor i height) (mod i height))))
      (setf (png-read::index-data state) *png-index-array*))))

(defun png-pixels (png)
  ;; PNG-READ indexes X before Y; CLIM image patterns contain rows of packed
  ;; ARGB pixels. Palette components and transparency maps are already expanded.
  (let* ((width (png-read:width png)) (height (png-read:height png))
         (kind (png-read:colour-type png))
         (depth (png-read:bit-depth png))
         (data (png-read:image-data png))
         (transparency (png-read:transparency png))
         (pixels (make-array (list height width) :element-type '(unsigned-byte 32))))
    (labels ((component (sample &optional (bits depth))
               (round (* sample 255) (1- (ash 1 bits))))
             (alpha (x y)
               (if transparency
                   (component (aref transparency x y)
                              (if (or (eq kind :indexed-colour) (< depth 16)) 8 16))
                   255)))
      (dotimes (y height)
        (dotimes (x width)
          (multiple-value-bind (red green blue opacity)
              (ecase kind
                (:greyscale
                 (let ((grey (component (aref data x y))))
                   (values grey grey grey (alpha x y))))
                (:greyscale-alpha
                 (let ((grey (component (aref data x y 0))))
                   (values grey grey grey (component (aref data x y 1)))))
                ((:truecolor :indexed-colour)
                 (let ((bits (if (eq kind :indexed-colour) 8 depth)))
                   (values (component (aref data x y 0) bits)
                           (component (aref data x y 1) bits)
                           (component (aref data x y 2) bits) (alpha x y))))
                (:truecolor-alpha
                 (values (component (aref data x y 0))
                         (component (aref data x y 1))
                         (component (aref data x y 2))
                         (component (aref data x y 3)))))
            (setf (aref pixels y x)
                  (logior (ash opacity 24) (ash red 16) (ash green 8) blue))))))
    pixels))

(define-bitmap-file-reader :png (pathname)
  (let ((*png-reader-active* t) (*png-index-array* nil) (*png-index-passes* nil))
    (values (png-pixels (png-read:read-png-file pathname)) nil)))

;; The standard API defaults to :BITMAP. Select a registered reader by suffix
;; when no backend has supplied a fallback (for example, McCLIM's Opticl port).
(defun browser-bitmap-file-reader (pathname)
  (let* ((name (pathname-type pathname))
         (format (and name (find-symbol (string-upcase name) :keyword)))
         (reader (and (not (member format '(:bitmap :fallback)))
                      (gethash format climi::*bitmap-file-readers*))))
    (unless reader (error 'unsupported-bitmap-format))
    (funcall reader pathname)))

(unless (gethash :fallback climi::*bitmap-file-readers*)
  (setf (gethash :fallback climi::*bitmap-file-readers*) #'browser-bitmap-file-reader))
