;;; Make foreign libraries used by valtan work in a saved executable.
;;;
;;; async-process loads "libasyncprocess.so" by bare file name.  SBCL records
;;; that name in the image and tries to dlopen it again at startup, which
;;; fails unless the library happens to be on the dynamic loader's search
;;; path.  To make the executable self-contained, we embed the library's
;;; bytes into the image at dump time, drop it from the list of shared
;;; objects SBCL reopens, and at startup write it to a cache directory and
;;; load it from that absolute path.

(defpackage :valtan-cli.foreign-libraries
  (:use :cl))
(in-package :valtan-cli.foreign-libraries)

(defparameter *embedded-library-names* '(async-process::async-process))

(defvar *embedded-libraries* '()
  "List of (library-name file-name . octets) captured at image dump time.")

(defun library-truename (library-name)
  (let* ((library (cffi::get-foreign-library library-name))
         (pathname (cffi:foreign-library-pathname library)))
    (or (and pathname
             (uiop:absolute-pathname-p pathname)
             (probe-file pathname))
        (cffi::find-file pathname cffi:*foreign-library-directories*)
        (error "Cannot locate the file of foreign library ~S (~S)."
               library-name pathname))))

(defun read-octets (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun write-octets (octets pathname)
  (ensure-directories-exist pathname)
  (let ((temp (make-pathname :type (format nil "~A.tmp~D"
                                           (pathname-type pathname)
                                           (random 1000000000 (make-random-state t)))
                             :defaults pathname)))
    (with-open-file (out temp :element-type '(unsigned-byte 8)
                              :direction :output
                              :if-exists :supersede)
      (write-sequence octets out))
    (rename-file temp pathname)))

(defun checksum (octets)
  "FNV-1a 32-bit hash, used to name the extracted file per library content."
  (let ((hash #x811c9dc5))
    (loop :for byte :across octets
          :do (setf hash (logand #xffffffff (* (logxor hash byte) #x01000193))))
    hash))

(defun embed-foreign-libraries ()
  (setf *embedded-libraries*
        (loop :for name :in *embedded-library-names*
              :for truename := (library-truename name)
              :collect (list* name (file-namestring truename) (read-octets truename))
              :do (cffi:close-foreign-library name))))

(defun extracted-library-pathname (file-name octets)
  (uiop:xdg-cache-home "valtan" "lib"
                       (format nil "~(~8,'0X~)/" (checksum octets))
                       file-name))

(defun restore-foreign-libraries ()
  (loop :for (nil file-name . octets) :in *embedded-libraries*
        :for pathname := (extracted-library-pathname file-name octets)
        :do (unless (and (probe-file pathname)
                         (with-open-file (in pathname :element-type '(unsigned-byte 8))
                           (= (file-length in) (length octets))))
              (write-octets octets pathname))
            (cffi:load-foreign-library pathname)))

(uiop:register-image-dump-hook 'embed-foreign-libraries)
(uiop:register-image-restore-hook 'restore-foreign-libraries nil)
