(in-package :common-lisp)

(defun make-symbol (string)
  (system::%make-symbol (system::array-to-js-string string)))

(defun symbol-name (symbol)
  (system::js-string-to-array (system::%symbol-name symbol)))

(defun symbol-package (symbol)
  (if (eq (system::symbol-package-name symbol) (ffi:ref "null"))
      nil
      (find-package (system::js-string-to-array (system::symbol-package-name symbol)))))

(defun keywordp (x)
  (and (symbolp x)
       (eq (symbol-package x)
           (find-package :keyword))))

(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

(defun (setf symbol-plist) (plist symbol)
  (system::put-symbol-plist symbol plist))

(defun %put (symbol indicator value)
  (let* ((plist (symbol-plist symbol))
         (mem (member indicator plist)))
    (if mem
        (setf (cadr mem) value)
        (setf (symbol-plist symbol)
              (list* indicator value plist)))
    value))

(defsetf get (symbol indicator &optional default)
    (value)
  `(%put ,symbol ,indicator
         (progn ,default ,value)))

(defun (setf symbol-value) (value symbol)
  (set symbol value))

(defun (setf symbol-function) (function symbol)
  (system::fset symbol function))

(defvar *gensym-counter* 0)

(defun gensym (&optional (prefix "G"))
  (make-symbol (cond ((and (integerp prefix) (<= 0 prefix))
                      (princ-to-string prefix))
                     ((not (stringp prefix))
                      (error "~S is not a string or non-negative integer"))
                     (t
                      (prog1 (system::string-append prefix (princ-to-string *gensym-counter*))
                        (incf *gensym-counter*))))))

(defvar *gentemp-counter* 0)

(defun gentemp (&optional (prefix "T") (package *package*))
  (do ()
      (nil)
    (let ((name (system::string-append prefix (princ-to-string *gentemp-counter*))))
      (incf *gentemp-counter*)
      (unless (find-symbol name package)
        (return (intern name package))))))

(defun copy-symbol (symbol &optional copy-props)
  (cond (copy-props
         (let ((new-symbol (make-symbol (string symbol))))
           (when (boundp symbol)
             (setf (symbol-function new-symbol) (symbol-value symbol)))
           (when (fboundp symbol)
             (setf (symbol-function new-symbol) (symbol-function symbol)))
           new-symbol))
        (t
         (make-symbol (string symbol)))))
