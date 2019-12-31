(in-package :common-lisp)

(defun make-symbol (string)
  (*:%make-symbol (*:array-to-js-string string)))

(defun symbol-name (symbol)
  (*:js-string-to-array (*:%symbol-name symbol)))

(defun symbol-package (symbol)
  (if (eq (*:symbol-package-name symbol) (ffi:ref "null"))
      nil
      (find-package (*:js-string-to-array (*:symbol-package-name symbol)))))

(defun keywordp (x)
  (and (symbolp x)
       (eq (symbol-package x)
           (find-package :keyword))))

(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

(defun (setf symbol-plist) (plist symbol)
  (*:put-symbol-plist symbol plist))

(defsetf get (symbol indicator &optional default)
    (value)
  (let ((g-value (gensym)))
    `(let ((,g-value ,value))
       (*:%put ,symbol ,indicator
                     (progn ,default ,g-value))
       ,g-value)))

(defun remprop (symbol indicator)
  (do ((plist (symbol-plist symbol) (cddr plist))
       (prev nil plist))
      ((null plist))
    (when (eq indicator (car plist))
      (if prev
          (setf (cddr prev) (cddr plist))
          (setf (symbol-plist symbol) (cddr plist)))
      (return plist))))

(defun (setf symbol-value) (value symbol)
  (set symbol value))

(defun (setf symbol-function) (function symbol)
  (*:fset symbol function))

(defvar *gensym-counter* 0)

(defun gensym (&optional (prefix "G"))
  (make-symbol (cond ((and (integerp prefix) (<= 0 prefix))
                      (format nil "G~D" prefix))
                     ((not (stringp prefix))
                      (type-error prefix '(or string (integer 0 *))))
                     (t
                      (prog1 (*:string-append prefix (princ-to-string *gensym-counter*))
                        (incf *gensym-counter*))))))

(defvar *gentemp-counter* 0)

(defun gentemp (&optional (prefix "T") (package *package*))
  (do ()
      (nil)
    (let ((name (*:string-append prefix (princ-to-string *gentemp-counter*))))
      (incf *gentemp-counter*)
      (unless (find-symbol name package)
        (return (intern name package))))))

(defun copy-symbol (symbol &optional copy-props)
  (unless (symbolp symbol) (type-error symbol 'symbol))
  (cond (copy-props
         (let ((new-symbol (make-symbol (string symbol))))
           (when (boundp symbol)
             (setf (symbol-value new-symbol) (symbol-value symbol)))
           (when (fboundp symbol)
             (setf (symbol-function new-symbol) (symbol-function symbol)))
           (setf (symbol-plist new-symbol) (symbol-plist symbol))
           new-symbol))
        (t
         (make-symbol (string symbol)))))
