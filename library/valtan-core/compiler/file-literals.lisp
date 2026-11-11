(in-package :compiler)

;; Only target COMPILE-FILE binds this registry. EVAL captures live objects,
;; and the host's bootstrap emitter must work before the target CLOS exists.
(defstruct file-literal
  object index creation initialization creation-dependencies init-dependencies
  aggregate-p state)

(defun p2-file-literal (object)
  (let ((entry (or (gethash object *p2-file-literals*)
                   (setf (gethash object *p2-file-literals*)
                         (let ((entry (make-file-literal
                                       :object object
                                       :index (hash-table-count *p2-file-literals*))))
                           (push entry *p2-file-pending-literals*)
                           entry)))))
    (when *p2-record-file-literal-dependencies*
      (pushnew entry *p2-file-literal-dependencies* :test #'eq))
    (format nil "fileLiterals[~D]" (file-literal-index entry))))

(defun p2-emit-file-literal-pool (stream)
  (when (p2-file-literals-p)
    ;; Functions retain this particular LOAD's pool after its dynamic binding
    ;; ends. Nested LOADs and a later LOAD of the same artifact get new pools.
    (write-line "const fileLiterals=lisp.symbolValue(lisp.intern('*COMPILED-FILE-LITERALS*','SYSTEM'));" stream)))

(defun p2-file-literal-form (form destination)
  ;; A load form is evaluated in a null lexical environment. In particular,
  ;; EVAL-WHEN in it must not execute while we compile the enclosing file.
  (let* ((hir (let ((*file-compilation-p* nil)
                   (*local-macro-compilation-environment* nil))
               (pass1-toplevel `(funcall (function (lambda () ,form))) t nil)))
         (*p2-load-time-value-stream* (make-emitter-stream *p2-emit-stream*))
         (*p2-emit-stream* (make-emitter-stream *p2-emit-stream*)))
    (let ((value (p2-form hir)))
      (format *p2-emit-stream* "~A~A;~%" (or destination "") value))
    (with-output-to-string (out)
      (join-emitter-stream out *p2-load-time-value-stream*)
      (join-emitter-stream out *p2-emit-stream*))))

(defun p2-prepare-file-literal (entry)
  (let* ((object (file-literal-object entry))
         (reference (format nil "fileLiterals[~D]" (file-literal-index entry)))
         (creation nil) (initialization nil) (creation-form-p nil)
         (*p2-record-file-literal-dependencies* t)
         (*p2-file-literal-dependencies* nil))
    (cond
      ((symbolp object)
       (setq creation (format nil "lisp.makeSymbol('~A')"
                              (p2-js-escape-string (symbol-name object)))))
      ((and (stringp object) (= (length object) (array-total-size object)))
       (setq creation (p2-literal-1 object nil nil)))
      ((consp object)
       (setf (file-literal-aggregate-p entry) t)
       (setq creation "lisp.makeCons(lisp.S_nil,lisp.S_nil)"))
      ((arrayp object)
       (setf (file-literal-aggregate-p entry) t)
       (setq creation
             (format nil "~A(lisp.jsArrayToList([~{~D~^,~}]),~A,~A)"
                     (p2-symbol-to-call-value 'make-array) (array-dimensions object)
                     (p2-symbol-to-js-value :element-type)
                     (p2-literal (array-element-type object)))))
      ((packagep object)
       (setq creation (format nil "~A(~A)" (p2-symbol-to-call-value 'find-package)
                              (p2-literal (package-name object)))))
      ((pathnamep object)
       (setq creation-form-p t)
       (setq creation
             (p2-file-literal-form
              `(make-pathname :host ',(pathname-host object)
                              :device ',(pathname-device object)
                              :directory ',(pathname-directory object)
                              :name ',(pathname-name object)
                              :type ',(pathname-type object)
                              :version ',(pathname-version object))
              (format nil "~A=" reference))))
      ((or (typep object 'standard-object) (typep object 'structure-object)
           (typep object 'condition))
       (setq creation-form-p t)
       ;; Exactly one invocation for this identity in the entire source file.
       (multiple-value-bind (create initialize) (make-load-form object nil)
         (setq creation (p2-file-literal-form create (format nil "~A=" reference)))
         (setf (file-literal-creation-dependencies entry) *p2-file-literal-dependencies*)
         (let ((*p2-file-literal-dependencies* nil))
           (when initialize (setq initialization (p2-file-literal-form initialize nil)))
           (setf (file-literal-init-dependencies entry) *p2-file-literal-dependencies*))))
      (t (error "Cannot externalize literal ~S" object)))
    (unless creation-form-p
      (setq creation (format nil "~A=~A;~%" reference creation)))
    (setf (file-literal-creation entry) creation
          (file-literal-creation-dependencies entry) *p2-file-literal-dependencies*)
    (when (file-literal-aggregate-p entry)
      (let ((*p2-file-literal-dependencies* nil))
        (setq initialization
              (with-output-to-string (out)
                (if (consp object)
                    (format out "~A.car=~A;~%~A.cdr=~A;~%"
                            reference (p2-literal (car object)) reference (p2-literal (cdr object)))
                    (dotimes (i (array-total-size object))
                      (format out "lisp.intern('(SETF ROW-MAJOR-AREF)','COMMON-LISP').func(~A,~A,~D);~%"
                              (p2-literal (row-major-aref object i)) reference i)))))
        (setf (file-literal-init-dependencies entry) *p2-file-literal-dependencies*)))
    (setf (file-literal-initialization entry) initialization)))

(defun p2-file-literal-references (entries)
  ;; Quoting an aggregate also references its contents. A creation form that
  ;; quotes a list containing the object being created is a creation cycle.
  ;; A MAKE-LOAD-FORM object's initialization dependencies are different: they
  ;; may refer back to that object and must not be expanded here.
  (let ((seen (make-hash-table :test 'eq)) (result nil))
    (labels ((visit (entry)
               (unless (gethash entry seen)
                 (setf (gethash entry seen) t)
                 (push entry result)
                 (when (file-literal-aggregate-p entry)
                   (mapc #'visit (file-literal-init-dependencies entry))))))
      (mapc #'visit entries))
    (nreverse result)))

(defun p2-emit-file-literals (stream)
  (when (p2-file-literals-p)
    (let ((entries nil))
      ;; Discover the whole graph before ordering it. Compiling a user load
      ;; form can introduce more literals, including references to itself.
      (loop while *p2-file-pending-literals*
            for entry = (pop *p2-file-pending-literals*)
            do (push entry entries) (p2-prepare-file-literal entry))
      (dolist (entry entries)
        (setf (file-literal-creation-dependencies entry)
              (p2-file-literal-references (file-literal-creation-dependencies entry)))
        (unless (file-literal-aggregate-p entry)
          (setf (file-literal-init-dependencies entry)
                (p2-file-literal-references (file-literal-init-dependencies entry)))))
      (labels
          ((can-create-p (entry seen)
             (case (file-literal-state entry)
               (:creating nil)
               ((:created :initializing :initialized) t)
               (otherwise
                (when (member entry seen :test #'eq)
                  (error "Circular MAKE-LOAD-FORM creation dependency"))
                (every (lambda (dependency) (can-create-p dependency (cons entry seen)))
                       (file-literal-creation-dependencies entry)))))
           (initialize (entry)
             (case (file-literal-state entry)
               ((:initializing :initialized) t)
               (:created
                (let ((dependencies (file-literal-init-dependencies entry)))
                  (when (every (lambda (dependency) (can-create-p dependency nil)) dependencies)
                    (setf (file-literal-state entry) :initializing)
                    (dolist (dependency dependencies)
                      (create dependency) (initialize dependency))
                    (when (file-literal-initialization entry)
                      (write-string (file-literal-initialization entry) stream))
                    (setf (file-literal-state entry) :initialized))))))
           (create (entry)
             (case (file-literal-state entry)
               (:creating (error "Circular MAKE-LOAD-FORM creation dependency"))
               ((:created :initializing :initialized) nil)
               (otherwise
                (setf (file-literal-state entry) :creating)
                (dolist (dependency (file-literal-creation-dependencies entry))
                  (create dependency) (initialize dependency))
                (write-string (file-literal-creation entry) stream)
                (setf (file-literal-state entry) :created)
                (initialize entry)))))
        (dolist (entry (reverse entries)) (create entry))
        (dolist (entry entries) (initialize entry))))))
