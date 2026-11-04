#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defvar *print-escape* t)
(defvar *print-circle* t)
(defvar *print-case* :upcase)
(defvar *print-readably* nil)
(defvar *print-array* t)
(defvar *print-base* 10)
(defvar *print-gensym* t)
(defvar *print-length* nil)
(defvar *print-level* nil)
(defvar *print-lines* nil)
(defvar *print-miser-width* nil)
(defvar *print-pprint-dispatch* nil)
(defvar *print-pretty* nil)
(defvar *print-radix* nil)
(defvar *print-right-margin* nil)

(defvar *print-circle-table* nil)
(defvar *print-circle-seen* nil)
(defvar *print-depth* 0)

(defun make-circle-table (object)
  (let ((visited '())
        (circle '())
        (label 0))
    (labels ((f (object)
               (cond ((assoc object visited)
                      (push (cons object (incf label)) circle))
                     ((consp object)
                      (push (cons object t) visited)
                      (f (car object))
                      (f (cdr object)))
                     ((and (arrayp object) (not (stringp object)))
                      (push (cons object t) visited)
                      (dotimes (i (array-total-size object))
                        (f (row-major-aref object i))))
                     ((and (symbolp object) (null (symbol-package object)))
                      (push (cons object t) visited))
                     (t
                      object))))
      (f object)
      circle)))

(defmacro print-unreadable-object ((object stream &key type identity) &body body)
  (declare (ignore object type identity))
  (let ((g-stream (gensym)))
    `(let ((,g-stream ,stream))
       (write-string "#<" ,g-stream)
       ,@body
       (write-string ">" ,g-stream)
       nil)))

;;; Helper functions for symbol case conversion
(defun %all-same-case-p (string)
  "Check if string contains only alphabetic chars of the same case (or no alphabetic chars)."
  (let ((has-upper nil)
        (has-lower nil))
    (dotimes (i (length string))
      (let ((c (char string i)))
        (cond ((upper-case-p c) (setq has-upper t))
              ((lower-case-p c) (setq has-lower t)))))
    (not (and has-upper has-lower))))

(defun %has-special-chars-p (string)
  "Check if string contains special characters that require escaping."
  (let ((len (length string)))
    (dotimes (i len nil)
      (let ((c (char string i)))
        (when (or (char= c #\space)
                  (char= c #\()
                  (char= c #\))
                  (char= c #\')
                  (char= c #\`)
                  (char= c #\,)
                  (char= c #\;)
                  (char= c #\")
                  (char= c #\\)
                  (char= c #\|))
          (return-from %has-special-chars-p t))))))

(defun %needs-escape-p (string readtable-case)
  "Check if symbol name needs vertical bar escaping for readability."
  (let ((len (length string)))
    (when (zerop len) (return-from %needs-escape-p t))
    ;; Always escape if contains special characters
    (when (%has-special-chars-p string)
      (return-from %needs-escape-p t))
    ;; Check case compatibility with readtable-case
    (case readtable-case
      (:upcase
       ;; In upcase readtable, lowercase chars need escaping
       (dotimes (i len nil)
         (when (lower-case-p (char string i))
           (return-from %needs-escape-p t))))
      (:downcase
       ;; In downcase readtable, uppercase chars need escaping
       (dotimes (i len nil)
         (when (upper-case-p (char string i))
           (return-from %needs-escape-p t))))
      ((:preserve :invert)
       ;; Preserve and Invert modes: no case-based escaping needed
       ;; In preserve mode, any case is valid
       ;; In invert mode, we output the inverted form which is always valid
       nil))
    nil))

(defun %convert-case (string print-case readtable-case)
  "Convert symbol name according to *print-case* and readtable-case."
  (case readtable-case
    (:preserve string)
    (:invert
     ;; Invert: all-upper -> all-lower, all-lower -> all-upper, mixed -> preserve
     (cond ((every #'(lambda (c) (or (not (alpha-char-p c)) (upper-case-p c))) string)
            (string-downcase string))
           ((every #'(lambda (c) (or (not (alpha-char-p c)) (lower-case-p c))) string)
            (string-upcase string))
           (t string)))
    (otherwise
     ;; :upcase or :downcase readtable
     (case print-case
       (:upcase (string-upcase string))
       (:downcase (string-downcase string))
       (:capitalize (string-capitalize string))
       (otherwise string)))))

(defun %write-symbol-name (name stream print-case readtable-case escape-p)
  "Write symbol name with appropriate escaping and case conversion."
  (let ((needs-escape (and escape-p (%needs-escape-p name readtable-case))))
    (if needs-escape
        (progn
          (write-char #\| stream)
          (write-string name stream)
          (write-char #\| stream))
        (write-string (%convert-case name print-case readtable-case) stream))))

(defun print-symbol (symbol stream)
  (let ((name (symbol-name symbol))
        (readtable-case (readtable-case *readtable*))
        (print-case *print-case*))
    (if *print-escape*
        (cond ((null (symbol-package symbol))
               (write-string "#:" stream)
               (%write-symbol-name name stream print-case readtable-case t))
              ((keywordp symbol)
               (write-string ":" stream)
               (%write-symbol-name name stream print-case readtable-case t))
              ((or (eq (symbol-package symbol) *package*)
                   (eq symbol (find-symbol (symbol-name symbol))))
               (%write-symbol-name name stream print-case readtable-case t))
              (t
               (write-string (package-name (symbol-package symbol)) stream)
               (multiple-value-bind (_symbol status)
                   (find-symbol (symbol-name symbol) (symbol-package symbol))
                 (declare (ignore _symbol))
                 (if (eq :external status)
                     (write-string ":" stream)
                     (write-string "::" stream)))
               (%write-symbol-name name stream print-case readtable-case t)))
        (%write-symbol-name name stream print-case readtable-case nil))))

(defun print-string (string stream)
  (cond (*print-escape*
         (write-char #\" stream)
         (write-string string stream)
         (write-char #\" stream))
        (t
         (write-string string stream))))

(defun print-number (number stream)
  (write-string (system:raw-string-to-array
                 (system:number-print-string number *print-base* *print-radix*)) stream))

(defun print-decimal-integer (number stream)
  (let ((*print-base* 10) (*print-radix* nil))
    (print-number number stream)))

(defun print-character (char stream)
  (cond (*print-escape*
         (write-string "#\\" stream)
         (write-string (or (char-name char) (string char)) stream))
        (t
         (write-char char stream))))

(defun print-label (object stream)
  (let ((label (cdr (assoc object *print-circle-table*))))
    (when label
      (write-char #\# stream)
      (print-decimal-integer label stream)
      (write-char #\# stream))))

(defun print-cons-1 (cons stream)
  (write-char #\( stream)
  (do ((tail cons (cdr tail)) (count 0 (1+ count)))
      ((null tail))
    (when (and (not *print-readably*) *print-length* (>= count *print-length*))
      (when (> count 0) (write-char #\space stream))
      (write-string "..." stream)
      (return))
    (when (> count 0) (write-char #\space stream))
    (write-1 (car tail) :stream stream)
    (cond ((and (consp (cdr tail)) *print-circle* (assoc (cdr tail) *print-circle-table*))
           (write-string " . " stream)
           (print-cons (cdr tail) stream)
           (return))
          ((and (cdr tail) (atom (cdr tail)))
           (write-string " . " stream)
           (write-1 (cdr tail) :stream stream)
           (return))))
  (write-char #\) stream))

(defun print-cons (cons stream)
  (let (label)
    (cond ((or (null *print-circle*)
               (null (setq label (cdr (assoc cons *print-circle-table*)))))
           (print-cons-1 cons stream))
          ((member cons *print-circle-seen*)
           (write-char #\# stream)
           (print-decimal-integer label stream)
           (write-char #\# stream))
          (t
           (push cons *print-circle-seen*)
           (write-char #\# stream)
           (print-decimal-integer label stream)
           (write-char #\= stream)
           (force-output stream)
           (print-cons-1 cons stream)))))

(defun print-vector (vector stream)
  (cond ((eq (array-element-type vector) 'bit)
         ;; Print bit-vector as #*0101...
         (write-string "#*" stream)
         (let ((len (length vector)))
           (do ((i 0 (1+ i)))
               ((= i len))
             (write-char (if (zerop (aref vector i)) #\0 #\1) stream))))
        (t
         ;; Print regular vector as #(...)
         (write-string "#(" stream)
         (let ((len (length vector)))
           (do ((i 0 (1+ i)))
               ((= i len))
             (when (and (not *print-readably*) *print-length* (>= i *print-length*))
               (write-string "..." stream)
               (return))
             (write-1 (aref vector i) :stream stream)
             (when (< i (1- len))
               (write-char #\space stream))))
         (write-string ")" stream))))

(defun print-array (array stream)
  (write-char #\# stream)
  (print-decimal-integer (array-rank array) stream)
  (write-char #\A stream)
  (let ((index 0))
    (labels ((contents (dimensions)
               (if (null dimensions)
                   (progn
                     (write-1 (row-major-aref array index) :stream stream)
                     (incf index))
                   (progn
                     (write-char #\( stream)
                     (dotimes (i (car dimensions))
                       (unless (zerop i) (write-char #\space stream))
                       (contents (cdr dimensions)))
                     (write-char #\) stream)))))
      (contents (array-dimensions array)))))

(defun print-function (function stream)
  (cl:print-unreadable-object (function stream)
    (write-string "Function" stream)
    (let ((name (system:function-name function)))
      (when name
        (write-char #\space stream)
        (write-string name stream)))))

(defun print-package (package stream)
  (cl:print-unreadable-object (package stream)
    (write-string "PACKAGE " stream)
    (write-char #\" stream)
    (write-string (package-name package) stream)
    (write-char #\" stream)))

(defun print-structure (structure stream)
  (funcall (structure-printer structure) structure stream))

(defun write-1 (object &key ((:array *print-array*) *print-array*)
                            ((:base *print-base*) *print-base*)
                            ((:case *print-case*) *print-case*)
                            ((:circle *print-circle*) *print-circle*)
                            ((:escape *print-escape*) *print-escape*)
                            ((:gensym *print-gensym*) *print-gensym*)
                            ((:length *print-length*) *print-length*)
                            ((:level *print-level*) *print-level*)
                            ((:lines *print-lines*) *print-lines*)
                            ((:miser-width *print-miser-width*) *print-miser-width*)
                            ((:pprint-dispatch *print-pprint-dispatch*) *print-pprint-dispatch*)
                            ((:pretty *print-pretty*) *print-pretty*)
                            ((:radix *print-radix*) *print-radix*)
                            ((:readably *print-readably*) *print-readably*)
                            ((:right-margin *print-right-margin*) *print-right-margin*)
                            (stream *standard-output*))
  (let ((compound (cond ((consp object) t)
                        ((arrayp object) (not (or (stringp object) (bit-vector-p object))))
                        ((system:structure-p object) t))))
    (when (and compound (not *print-readably*) *print-level* (>= *print-depth* *print-level*))
      (write-char #\# stream)
      (return-from write-1 object))
  (let ((*print-depth* (if compound (1+ *print-depth*) *print-depth*)))
  (when (and *print-circle* (not (consp object)))
    (let ((label (cdr (assoc object *print-circle-table*))))
      (when label
        (when (member object *print-circle-seen*)
          (print-label object stream)
          (return-from write-1 object))
        (push object *print-circle-seen*)
        (write-char #\# stream)
        (print-decimal-integer label stream)
        (write-char #\= stream))))
  (let ((*print-escape* (or *print-readably* *print-escape*)))
  (cond ((symbolp object)
         (print-symbol object stream))
        ((stringp object)
         (print-string object stream))
        ((numberp object)
         (print-number object stream))
        ((characterp object)
         (print-character object stream))
        ((consp object)
         (print-cons object stream))
        ((and (arrayp object) (not *print-array*) (not *print-readably*))
         (write-string "#<ARRAY>" stream))
        ((vectorp object)
         (print-vector object stream))
        ((arrayp object)
         (print-array object stream))
        ((functionp object)
         (print-function object stream))
        ((packagep object)
         (print-package object stream))
        ((*:structure-p object)
         (print-structure object stream))
        (t
         (cl:print-unreadable-object (nil stream)
           (write-string (system:unknown-object-to-string object)
                         stream)))))
  object)))

(defun write (object &rest args)
  (let ((*print-depth* 0)
        (*print-circle-table*
          (when (getf args :circle *print-circle*)
            (make-circle-table object)))
        (*print-circle-seen* nil))
    (apply #'write-1 object args)))

(defun write-to-string (object &rest args
                               &key array base case circle escape gensym length level
                                    lines miser-width pprint-dispatch pretty radix readably right-margin)
  (declare (ignore array base case circle escape gensym length level
                   lines miser-width pprint-dispatch pretty radix readably right-margin))
  (with-output-to-string (stream)
    (apply #'write object :stream stream args)))

(defun princ (object &optional (stream *standard-output*))
  (write object :escape nil :readably nil :stream stream)
  object)

(defun princ-to-string (object)
  (with-output-to-string (stream)
    (princ object stream)))

(defun prin1 (object &optional (stream *standard-output*))
  (write object :escape t :stream stream)
  object)

(defun prin1-to-string (object)
  (with-output-to-string (stream)
    (prin1 object stream)))

(defun print (object &optional (stream *standard-output*))
  (terpri stream)
  (prin1 object stream)
  (write-char #\space stream)
  object)

(defun format (destination control-string &rest format-arguments)
  (flet ((take ()
           (unless format-arguments
             (error "No more arguments"))
           (pop format-arguments)))
    (let ((string
            (with-output-to-string (buffer)
              (do ((i 0 (1+ i)))
                  ((>= i (length control-string)))
                (let ((c (aref control-string i)))
                  (cond ((char= c #\~)
                         (incf i)
                         (when (>= i (length control-string))
                           (error "format error"))
                         (case (aref control-string i)
                           ((#\a #\A)
                            (princ (take) buffer))
                           ((#\d #\D)
                            (princ (take) buffer))
                           ((#\c #\C)
                            (princ (take) buffer))
                           ((#\s #\S)
                            (prin1 (take) buffer))
                           ((#\%)
                            (write-char #\newline buffer))
                           ((#\&)
                            (fresh-line buffer))
                           (otherwise
                            (error "unexpected format directive"))))
                        (t
                         (write-char c buffer))))))))
      (cond ((eq destination t)
             (write-string string *standard-output*)
             (finish-output *standard-output*))
            ((eq destination nil)
             string)
            (t
             (write-string string destination)
             (finish-output destination))))))

(defun %format (string &rest args)
  (ffi:cl->js (apply #'format nil (ffi:js->cl string) args)))

#+valtan
((ffi:ref "lisp" "setLispFormatFunction") #'%format)
