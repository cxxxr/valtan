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
  (let ((seen (make-hash-table :test 'eq)) (result nil)
        (pending (copy-list entries)))
    (loop while pending for entry = (pop pending)
          unless (gethash entry seen)
            do (setf (gethash entry seen) t)
               (push entry result)
               (when (file-literal-aggregate-p entry)
                 (dolist (dependency (reverse (file-literal-init-dependencies entry)))
                   (push dependency pending))))
    (nreverse result)))

(defun p2-file-literals-creatable-p (entries)
  ;; Initialization may be deferred when a referenced object's creation is
  ;; still in progress. Visit each dependency once without growing the host
  ;; stack. Actual creation actions below diagnose creation cycles.
  (let ((seen (make-hash-table :test 'eq)) (pending (copy-list entries)))
    (loop while pending for entry = (pop pending)
          do (case (file-literal-state entry)
               (:creating (return-from p2-file-literals-creatable-p nil))
               ((:created :initializing :initialized) nil)
               (otherwise
                (unless (gethash entry seen)
                  (setf (gethash entry seen) t)
                  (dolist (dependency (file-literal-creation-dependencies entry))
                    (push dependency pending))))))
    t))

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
      ;; Use an explicit stack for the depth-first creation/initialization
      ;; order. Large quoted lists (such as McCLIM's package exports) must not
      ;; consume a JavaScript stack frame for each cons cell.
      (let ((actions nil))
        (dolist (entry (reverse entries)) (push (cons :initialize entry) actions))
        (dolist (entry entries) (push (cons :create entry) actions))
        (labels ((dependencies (entries)
                   (dolist (entry (reverse entries))
                     (push (cons :initialize entry) actions)
                     (push (cons :create entry) actions))))
          (loop while actions
                for action = (pop actions)
                for entry = (cdr action)
                do (case (car action)
                     (:create
                      (case (file-literal-state entry)
                        (:creating (error "Circular MAKE-LOAD-FORM creation dependency"))
                        ((:created :initializing :initialized) nil)
                        (otherwise
                         (setf (file-literal-state entry) :creating)
                         (push (cons :finish-creation entry) actions)
                         (dependencies (file-literal-creation-dependencies entry)))))
                     (:finish-creation
                      (write-string (file-literal-creation entry) stream)
                      (setf (file-literal-state entry) :created)
                      (push (cons :initialize entry) actions))
                     (:initialize
                      (when (and (eq (file-literal-state entry) :created)
                                 (p2-file-literals-creatable-p
                                  (file-literal-init-dependencies entry)))
                        (setf (file-literal-state entry) :initializing)
                        (push (cons :finish-initialization entry) actions)
                        (dependencies (file-literal-init-dependencies entry))))
                     (:finish-initialization
                      (when (file-literal-initialization entry)
                        (write-string (file-literal-initialization entry) stream))
                      (setf (file-literal-state entry) :initialized)))))))))
