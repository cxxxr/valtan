#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defgeneric documentation (x doc-type))

(defmethod documentation ((x symbol) (doc-type (eql 'method-combination)))
  (declare (ignore doc-type))
  (get x 'method-combination-documentation))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'method-combination)))
  (declare (ignore doc-type))
  (setf (get x 'method-combination-documentation) new-value))

(defvar *object-documentation* (make-hash-table :test 'equal))

(defun documentation-storage-key (object kind)
  (cond ((and (functionp object) (member kind '(t function)))
         (setq kind 'function))
        ((and (or (symbolp object) (listp object)) (eq kind 'function))
         (setq object (or (and (symbolp object) (macro-function object))
                          (and (fboundp object) (fdefinition object)) object)))
        ((and (typep object 'class) (member kind '(t type)))
         (setq kind 'type))
        ((and (symbolp object) (eq kind 'type))
         (setq object (or (find-class object nil) object))))
  (list object kind))

(defmacro define-documentation-storage (class kind)
  `(progn
     (defmethod documentation ((object ,class) (kind (eql ',kind)))
       (values (gethash (documentation-storage-key object kind) *object-documentation*)))
     (defmethod (setf documentation) (new-value (object ,class) (kind (eql ',kind)))
       (setf (gethash (documentation-storage-key object kind) *object-documentation*) new-value))))

(define-documentation-storage function t)
(define-documentation-storage function function)
(define-documentation-storage list function)
(define-documentation-storage list compiler-macro)
(define-documentation-storage symbol function)
(define-documentation-storage symbol compiler-macro)
(define-documentation-storage symbol setf)
(define-documentation-storage symbol type)
(define-documentation-storage symbol structure)
(define-documentation-storage class t)
(define-documentation-storage class type)
(define-documentation-storage standard-method t)

(defmethod documentation ((x symbol) (doc-type (eql 'variable)))
  (declare (ignore doc-type))
  (getf (get x 'doc-plist) 'variable))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'variable)))
  (declare (ignore doc-type))
  (setf (getf (get x 'doc-plist) 'variable) new-value))

(defmethod documentation ((package package) (doc-type (eql t)))
  (declare (ignore doc-type))
  (values (gethash package *package-documentation*)))

(defmethod (setf documentation) (new-value (package package) (doc-type (eql t)))
  (declare (ignore doc-type))
  (setf (gethash package *package-documentation*) new-value))
