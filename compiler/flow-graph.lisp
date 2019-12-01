(in-package :compiler)

(defparameter +start-node-name+ (make-symbol "START"))

(defstruct compiland
  vars
  functions
  start-basic-block
  basic-blocks)

(defstruct basic-block
  id
  code
  succ
  pred)

(defun check-basic-block-succ-pred (bb)
  (mapc (lambda (pred)
          (let ((count (count (basic-block-id bb)
                              (mapcar #'basic-block-id (basic-block-succ pred))
                              :test #'equal)))
            (assert (= 1 count))))
        (basic-block-pred bb))
  (mapc (lambda (succ)
          (let ((count (count (basic-block-id bb)
                              (mapcar #'basic-block-id (basic-block-pred succ))
                              :test #'equal)))
            (assert (= 1 count))))
          (basic-block-succ bb)))

(defun show-basic-block (bb)
  (format t "~A ~A~%" (basic-block-id bb) (mapcar #'basic-block-id (basic-block-pred bb)))
  (do-vector (lir (basic-block-code bb))
    (format t "  ~A~%" lir))
  (let ((succ (basic-block-succ bb)))
    (format t " ~A~%" (mapcar #'basic-block-id succ)))
  (handler-case (check-basic-block-succ-pred bb)
    (error ()
      (format t "ERROR~%"))))

(defun show-basic-blocks (compiland)
  (show-basic-block (compiland-start-basic-block compiland))
  (mapc #'show-basic-block (compiland-basic-blocks compiland))
  (values))

(defun create-compiland (hir)
  (multiple-value-bind (code vars functions)
      (hir-to-lir hir)
    (multiple-value-bind (basic-blocks start-basic-block)
        (split-basic-blocks code)
      (make-compiland :vars vars
                      :functions functions
                      :basic-blocks basic-blocks
                      :start-basic-block start-basic-block))))

(defun split-basic-blocks (code)
  (let ((current-block '())
        (basic-blocks '())
        (basic-block-counter 0))
    (flet ((add-block ()
             (unless (null current-block)
               (let ((code (coerce (nreverse current-block) 'vector)))
                 (push (make-basic-block :id (prog1 basic-block-counter
                                               (incf basic-block-counter))
                                         :code code
                                         :succ nil)
                       basic-blocks)))))
      (do-vector (lir code)
        (case (lir-op lir)
          ((label)
           (add-block)
           (setq current-block (list lir)))
          ((jump fjump)
           (push lir current-block)
           (add-block)
           (setf current-block '()))
          (otherwise
           (push lir current-block))))
      (add-block)
      (let (prev)
        (dolist (bb basic-blocks)
          (let ((last (vector-last (basic-block-code bb))))
            (case (lir-op last)
              ((jump fjump)
               (let* ((jump-label (lir-jump-label last))
                      (to (find-if (lambda (bb2)
                                     (let ((lir (vector-first (basic-block-code bb2))))
                                       (and (eq (lir-op lir) 'label)
                                            (eq (lir-arg1 lir) jump-label))))
                                   basic-blocks)))
                 (setf (basic-block-succ bb)
                       (let ((succ '()))
                         (when to
                           (push bb (basic-block-pred to))
                           (push to succ))
                         (when (and prev (eq (lir-op last) 'fjump))
                           (push bb (basic-block-pred prev))
                           (push prev succ))
                         succ))))
              (otherwise
               (when prev
                 (push bb (basic-block-pred prev))
                 (setf (basic-block-succ bb)
                       (list prev))))))
          (setf prev bb)))
      (let ((basic-blocks (nreverse basic-blocks))
            (start-basic-block (make-basic-block :id +start-node-name+)))
        (setf (basic-block-succ start-basic-block) (list (first basic-blocks)))
        (push start-basic-block (basic-block-pred (first basic-blocks)))
        (values basic-blocks start-basic-block)))))

(defun flatten-basic-blocks (basic-blocks)
  (coerce (mapcan (lambda (bb)
                    (coerce (basic-block-code bb) 'list))
                  basic-blocks)
          'vector))

(defun remove-basic-block (bb)
  (dolist (pred (basic-block-pred bb))
    (setf (basic-block-succ pred)
          (mapcan (lambda (succ)
                    (if (eq succ bb)
                        (basic-block-succ bb)
                        (list succ)))
                  (basic-block-succ pred))))
  (dolist (succ (basic-block-succ bb))
    (setf (basic-block-pred succ)
          (mapcan (lambda (pred)
                    (if (eq pred bb)
                        (basic-block-pred bb)
                        (list pred)))
                  (basic-block-pred succ))))
  (values))

(defun remove-unused-block (basic-blocks)
  (delete-if (lambda (bb)
               (when (null (basic-block-pred bb))
                 (remove-basic-block bb)
                 t))
             basic-blocks))

(defun remove-unused-label (basic-blocks)
  (let ((label-table '()))
    (dolist (bb basic-blocks)
      (let* ((code (basic-block-code bb))
             (lir (vector-last code)))
        (when (lir-jump-p lir)
          (pushnew (lir-jump-label lir) label-table))))
    (delete-if (lambda (bb)
                 (let* ((code (basic-block-code bb))
                        (lir (aref code 0)))
                   (when (and (eq (lir-op lir) 'label)
                              (not (member (lir-arg1 lir) label-table)))
                     (cond ((= 1 (length code))
                            (remove-basic-block bb)
                            t)
                           (t
                            (setf (basic-block-code bb)
                                  (subseq code 1))
                            nil)))))
               basic-blocks)))

(defun create-dominator-table (compiland)
  (let ((d-table (make-hash-table))
        (all-nodes (cons (compiland-start-basic-block compiland)
                         (compiland-basic-blocks compiland))))
    (dolist (bb all-nodes)
      (setf (gethash (basic-block-id bb) d-table)
            (mapcar #'basic-block-id all-nodes)))
    (dolist (bb all-nodes)
      (let ((pred (basic-block-pred bb))
            (self-id (basic-block-id bb)))
        (if (null pred)
            (setf (gethash self-id d-table)
                  (list self-id))
            (let ((set (gethash (basic-block-id (first pred)) d-table)))
              (dolist (p (rest pred))
                (setq set (intersection set (gethash (basic-block-id p) d-table))))
              (setf (gethash self-id d-table)
                    (adjoin self-id set))))))
    (hash-table-to-alist d-table)))

(defun create-dominator-tree (d-table)
  ;; この関数は間違った結果を返す
  (flet ((finish-p ()
           (dolist (elt d-table t)
             (and elt
                  (destructuring-bind (n . dominators) elt
                    (declare (ignore n))
                    (when (length>1 dominators)
                      (return nil)))))))
    (setf d-table (delete +start-node-name+ d-table :key #'car))
    (dolist (elt d-table)
      (destructuring-bind (n . dominators) elt
        (let ((dominators (delete n dominators :count 1)))
          (setf (cdr elt) dominators))))
    (do ()
        ((finish-p))
      (let ((idoms '()))
        (dolist (elt d-table)
          (destructuring-bind (n . dominators) elt
            (declare (ignore n))
            (when (length=1 dominators)
              (push (car dominators) idoms))))
        (dolist (elt d-table)
          (destructuring-bind (n . dominators) elt
            (declare (ignore n))
            (unless (length=1 dominators)
              (setf (cdr elt) (nset-difference dominators idoms))))))))
  d-table)

(defun create-loop-graph (compiland d-table)
  (let ((visited (make-hash-table))
        (loop-graph '()))
    (labels ((f (bb)
               (unless (gethash (basic-block-id bb) visited)
                 (setf (gethash (basic-block-id bb) visited) t)
                 (dolist (s (basic-block-succ bb))
                   (if (and (gethash (basic-block-id s) visited)
                            (member (basic-block-id s)
                                    (cdr (assoc (basic-block-id bb) d-table))))
                       (push (cons (basic-block-id s)
                                   (basic-block-id bb))
                             loop-graph))
                   (f s)))))
      (f (compiland-start-basic-block compiland))
      loop-graph)))

(defun graphviz (compiland &optional (name "valtan") (open-viewer-p t))
  (let ((dot-filename (format nil "/tmp/~A.dot" name))
        (img-filename (format nil "/tmp/~A.png" name)))
    (with-open-file (out dot-filename
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (let ((basic-blocks (cons (compiland-start-basic-block compiland)
                                (compiland-basic-blocks compiland))))
        (write-line "digraph graph_name {" out)
        (write-line "graph [ labeljust = l; ]" out)
        (write-line "node [ shape = box; ]" out)
        (dolist (bb basic-blocks)
          (format out "~A [label = \"~A\\l" (basic-block-id bb) (basic-block-id bb))
          (do-vector (lir (basic-block-code bb))
            (write-string (princ-to-string (cons (lir-op lir) (lir-args lir))) out)
            (write-string "\\l" out))
          (format out "\"];~%")
          (dolist (succ (basic-block-succ bb))
            (format out "~A -> ~A~%" (basic-block-id bb) (basic-block-id succ))))
        (write-line "}" out)))
    #+sbcl
    (progn
      (uiop:run-program (format nil "dot -Tpng '~A' > '~A'" dot-filename img-filename))
      (when open-viewer-p
        #+linux (uiop:run-program (format nil "xdg-open '~A'" img-filename))
        #+os-macosx (uiop:run-program (format nil "open '~A'" img-filename))))))

(defun test (&optional (open-viewer-p t))
  (let* ((hir (let ((*gensym-counter* 0))
                (pass1-toplevel #+(or)
                                '(tagbody
                                  a
                                  (if x (go b) (go c))
                                  b
                                  (print 1)
                                  (go d)
                                  c
                                  (print 2)
                                  (go d)
                                  d
                                  (print 3)
                                  (go a))
                                #+(or)
                                '(dotimes (i 10)
                                  (dotimes (j 20)
                                    (f i j)))
                                ;#+(or)
                                '(dotimes (i 10)
                                  (print i)))))
         (compiland (create-compiland hir)))
    (pprint (reduce-hir hir))
    (show-basic-blocks (progn (setf (compiland-basic-blocks compiland)
                                    (compiland-basic-blocks compiland))
                              compiland))
    (graphviz compiland "valtan-0" nil)
    (write-line "1 ==================================================")
    (show-basic-blocks (progn (setf (compiland-basic-blocks compiland)
                                    (remove-unused-block (compiland-basic-blocks compiland)))
                              compiland))
    (graphviz compiland "valtan-1" nil)
    (write-line "2 ==================================================")
    (show-basic-blocks (progn (setf (compiland-basic-blocks compiland)
                                    (remove-unused-label (compiland-basic-blocks compiland)))
                              compiland))
    (graphviz compiland "valtan-2" open-viewer-p)
    (create-loop-graph compiland (create-dominator-table compiland))))
