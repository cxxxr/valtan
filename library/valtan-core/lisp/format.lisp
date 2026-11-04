(in-package :common-lisp)

;; Keep the small bootstrap FORMAT in print.lisp until the stream and object
;; systems have loaded. The directive interpreter is shared with
;; FORMATTER so argument consumption has the same semantics in both paths.
(defun format-object-string (object escape colon base)
  (if (and colon (null object))
      "()"
      (let ((*print-base* (or base *print-base*))
            (*print-radix* (if base nil *print-radix*))
            (*print-readably* (and escape *print-readably*)))
        (write-to-string object :escape escape))))

(defun format (destination control &rest arguments)
  (let* ((string-destination (and destination (stringp destination)))
         (stream (cond ((or (null destination) string-destination)
                        (make-string-output-stream))
                       ((eq destination t) *standard-output*)
                       (t destination))))
    (if (functionp control)
        (apply control stream arguments)
        (progn
          (unless (stringp control) (type-error control '(or string function)))
          (system:format-control stream (system:array-to-raw-string control) arguments)))
    (cond ((null destination) (get-output-stream-string stream))
          (string-destination
           (unless (array-has-fill-pointer-p destination)
             (type-error destination '(and string (satisfies array-has-fill-pointer-p))))
           (let ((text (get-output-stream-string stream)))
             (dotimes (i (length text)) (vector-push-extend (char text i) destination)))
           nil)
          (t nil))))

(defun format-write-object (object colon at)
  (let ((*print-pretty* (or colon *print-pretty*))
        (*print-level* (if at nil *print-level*))
        (*print-length* (if at nil *print-length*)))
    (write-to-string object)))

(defmacro formatter (control)
  (unless (stringp control) (type-error control 'string))
  (let ((stream (gensym)) (arguments (gensym)))
    `(lambda (,stream &rest ,arguments)
       (system:format-control ,stream (system:array-to-raw-string ,control) ,arguments))))
