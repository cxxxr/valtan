(in-package :valtan-core)

(cl:defvar *valtan-readtable* (cl:copy-readtable cl:nil))
(cl:defvar *plain-readtable* (cl:copy-readtable cl:nil))

(cl:defmethod cl:make-load-form ((object structure) cl:&optional environment)
  (cl:declare (cl:ignore environment))
  (cl:make-load-form-saving-slots object))

(cl:set-macro-character
 #\"
 (cl:lambda (s c)
   (cl:declare (cl:ignore c))
   (cl:unread-char #\" s)
   (cl:let* ((cl:*readtable* *plain-readtable*)
             (raw-string (cl:read s cl:t cl:nil cl:t)))
     (system::make-structure-array! raw-string)))
 nil
 *valtan-readtable*)

(cl:set-dispatch-macro-character
 #\# #\(
 (cl:lambda (s c n)
   (cl:declare (cl:ignore c n))
   (cl:unread-char #\( s)
   (cl:let* ((cl:*readtable* *plain-readtable*)
             (raw-array (cl:read s cl:t cl:nil cl:t)))
     (system::make-structure-array! (cl:coerce raw-array 'cl:vector) t)))
 *valtan-readtable*)

(cl:set-dispatch-macro-character
 #\# #\*
 (cl:lambda (s c n)
   (cl:declare (cl:ignore c n))
   (cl:let* ((cl:*readtable* *plain-readtable*)
             (bits (cl:coerce
                    (cl:loop
                      :while (cl:member (cl:peek-char cl:nil s cl:t cl:nil cl:t)
                                        '(#\0 #\1))
                      :collect (cl:ecase (cl:read-char s cl:t cl:nil cl:t)
                                 (#\0 0)
                                 (#\1 1)))
                    'cl:vector)))
     (system::make-structure-array! bits 't)))
 *valtan-readtable*)

(cl:set-dispatch-macro-character
 #\# #\"
 (cl:lambda (s c n)
   (cl:declare (cl:ignore c n))
   (cl:unread-char #\" s)
   (let ((cl:*readtable* *plain-readtable*))
     (cl:read s cl:t cl:nil cl:t)))
 *valtan-readtable*)
