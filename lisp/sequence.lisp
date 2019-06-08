(in-package :common-lisp)

(defun length (sequence)
  (cond ((listp sequence)
         (list-length sequence))
        ((vectorp sequence)
         (array-length sequence))
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

(defun remove-if-not (test sequence &key #|from-end start end count key|#)
  (cond ((listp sequence)
         (let ((head nil)
               (tail nil))
           (dolist (x sequence)
             (when (funcall test x)
               (if (null head)
                   (setf head
                         (setf tail
                               (list x)))
                   (progn
                     (setf (cdr tail) (list x))
                     (setf tail (cdr tail))))))
           head))
        #+(or)
        ((vectorp sequence)
         )
        (t
         (error "type error"))))
