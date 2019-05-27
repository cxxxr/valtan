(in-package :common-lisp)

(defun length (sequence)
  (cond ((listp sequence)
         (length sequence))
        ((vectorp sequence)
         )
        (t
         (error "type error"))))

(defun find-if (predicate sequence &key #+(or)from-end #+(or)start #+(or)end #+(or)key)
  (cond ((listp sequence)
         (dolist (x sequence)
           (when (funcall predicate x)
             (return x))))
        ((vectorp sequence)
         (let ((length sequence))
           (do ((i 0 (1+ i)))
               ((= i (length sequence)))
             (let ((x (aref sequence i)))
               (when (funcall predicate x)
                 (return x))))))
        (t
         (error "type error"))))
