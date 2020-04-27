(in-package :common-lisp)

(defgeneric documentation (x doc-type))

#+(or)
(defmethod documentation ((x function) (doc-type (eql t)))
  )

#+(or)
(defmethod documentation ((x function) (doc-type (eql 'functin)))
  )

#+(or)
(defmethod documentation ((x list) (doc-type (eql 'function)))
  )

#+(or)
(defmethod documentation ((x list) (doc-type (eql 'compiler-macro)))
  )

#+(or)
(defmethod documentation ((x symbol) (doc-type (eql 'function)))
  )

#+(or)
(defmethod documentation ((x symbol) (doc-type (eql 'compiler-macro)))
  )

#+(or)
(defmethod documentation ((x symbol) (doc-type (eql 'cl:setf)))
  )

#+(or)
(defmethod (cl:setf documentation) (new-value (x function) (doc-type (eql 't)))
  )

#+(or)
(defmethod (cl:setf documentation) (new-value (x function) (doc-type (eql 'function)))
  )

#+(or)
(defmethod (cl:setf documentation) (new-value (x list) (doc-type (eql 'function)))
  )

#+(or)
(defmethod (cl:setf documentation) (new-value (x list) (doc-type (eql 'compiler-macro)))
  )

#+(or)
(defmethod (cl:setf documentation) (new-value (x symbol) (doc-type (eql 'function)))
  )

#+(or)
(defmethod (cl:setf documentation) (new-value (x symbol) (doc-type (eql 'compiler-macro)))
  )

#+(or)
(defmethod (cl:setf documentation) (new-value (x symbol) (doc-type (eql 'cl:setf)))
  )

(defmethod documentatin ((x symbol) (doc-type (eql 'variable)))
  (getf (get x 'doc-plist) 'variable))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'variable)))
  (setf (getf (get x 'doc-plist) 'variable) new-value))
