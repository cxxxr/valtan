(in-package :valtan-core)

(cl:defvar *valtan-readtable* (cl:copy-readtable cl:nil))

(cl:defmethod cl:make-load-form ((object structure) cl:&optional environment)
  (cl:declare (cl:ignore environment))
  (cl:make-load-form-saving-slots object))

(cl:set-macro-character #\" (cl:lambda (s c)
                              (cl:declare (cl:ignore c))
                              (cl:unread-char #\" s)
                              (cl:let* ((cl:*readtable* (cl:copy-readtable cl:nil))
                                        (raw-string (cl:read s cl:t cl:nil cl:t)))
                                (system:make-structure 'array
                                                       raw-string
                                                       nil
                                                       1
                                                       (cl:length raw-string)
                                                       'character)))
                        nil
                        *valtan-readtable*)

(cl:set-dispatch-macro-character #\# #\(
                                 (cl:lambda (s c n)
                                   (cl:declare (cl:ignore c n))
                                   (cl:unread-char #\( s)
                                   (cl:let* ((cl:*readtable* (cl:copy-readtable cl:nil))
                                             (raw-array (cl:read s cl:t cl:nil cl:t)))
                                     (system:make-structure 'array
                                                            raw-array
                                                            nil
                                                            1
                                                            (cl:length raw-array)
                                                            't)))
                                 *valtan-readtable*)
