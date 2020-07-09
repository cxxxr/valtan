#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defgeneric documentation (x doc-type))

;; #+valtan(js:console.time #j"documentation")
;; #+valtan((ffi:ref "lisp" "startProfile"))
(defmethod documentation ((x function) (doc-type (eql t)))
  )
;; #+valtan((ffi:ref "lisp" "finishProfile"))
;; #+valtan(js:console.time-end #j"documentation")

(defmethod documentation ((x function) (doc-type (eql 'functin)))
  )

(defmethod documentation ((x list) (doc-type (eql 'function)))
  )

(defmethod documentation ((x list) (doc-type (eql 'compiler-macro)))
  )

(defmethod documentation ((x symbol) (doc-type (eql 'function)))
  )

(defmethod documentation ((x symbol) (doc-type (eql 'compiler-macro)))
  )

(defmethod documentation ((x symbol) (doc-type (eql 'setf)))
  )

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 't)))
  )

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 'function)))
  )

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'function)))
  )

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'compiler-macro)))
  )

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'function)))
  )

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'compiler-macro)))
  )

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'setf)))
  )

(defmethod documentatin ((x symbol) (doc-type (eql 'variable)))
  (declare (ignore doc-type))
  (getf (get x 'doc-plist) 'variable))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'variable)))
  (declare (ignore doc-type))
  (setf (getf (get x 'doc-plist) 'variable) new-value))
