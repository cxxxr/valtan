(in-package :common-lisp)

(defun length (sequence)
  (cond ((listp sequence)
         (list-length sequence))
        ((vectorp sequence)
         (array-length sequence))
        (t
         (error "type error"))))

(defun reverse-list (list)
  (let ((reversed-list '()))
    (dolist (x list)
      (push x reversed-list))
    reversed-list))

(defun reverse-vector (vector)
  (let* ((length (length vector))
         (new-vector (make-array length :element-type (array-element-type vector))))
    (do ((i 0 (1+ i))
         (j (1- length) (1- j)))
        ((= i length))
      (setf (aref new-vector i)
            (aref vector j)))
    new-vector))

(defun reverse (sequence)
  (cond ((listp sequence)
         (reverse-list sequence))
        ((vectorp sequence)
         (reverse-vector sequence))
        (t
         (error "type error"))))

(defun nreverse-list (list)
  (do ((1st (cdr list) (if (endp 1st) 1st (cdr 1st)))
       (2nd list 1st)
       (3rd '() 2nd))
      ((atom 2nd) 3rd)
    (rplacd 2nd 3rd)))

(defun nreverse-vector (vector)
  (let ((length (length vector)))
    (dotimes (i (floor length 2))
      (let ((j (- length i 1)))
        (let ((x (aref vector i))
              (y (aref vector (- length i 1))))
          (setf (aref vector i) y
                (aref vector j) x)))))
  vector)

(defun nreverse (sequence)
  (cond ((listp sequence)
         (nreverse-list sequence))
        ((vectorp sequence)
         (nreverse-vector sequence))
        (t
         (error "type error"))))

(defun subseq-list (list start end)
  (when (and start
             (< 0 start))
    (setq list (nthcdr (1- start) list))
    (when (null list)
      (error "index error"))
    (setq list (cdr list)))
  (cond ((null end)
         (copy-list list))
        ((eql start end)
         nil)
        (t
         (setq end (- end start))
         (let ((new-list (copy-list list)))
           (setf (cdr (nthcdr (1- end) new-list)) nil)
           new-list))))

(defun subseq-vector (vector start end)
  (let ((length (length vector)))
    (when (and end (< length end))
      (error "index error"))
    (unless end
      (setq end length))
    ;; XXX: javascriptのArrayのメソッドを使わず新しいベクタに一つずつ代入していて効率が悪い
    (let ((new-vector
            (make-array (- end start))))
      (do ((i start (1+ i))
           (j 0 (1+ j)))
          ((= i end))
        (setf (aref new-vector j)
              (aref vector i)))
      new-vector)))

(defun subseq (sequence start &optional end)
  (when (and (not (null end)) (> start end))
    (error "start > end"))
  (cond ((listp sequence)
         (subseq-list sequence start end))
        ((vectorp sequence)
         (subseq-vector sequence start end))
        (t
         (error "type error"))))

(defun map-sequence (function sequence from-end start end key)
  (when start
    (setq sequence (subseq sequence start end)))
  (when from-end
    ;; XXX: startと同時に使うとコピーを二度するので無駄がある
    (setq sequence (reverse sequence)))
  (cond ((listp sequence)
         (if key
             (dolist (x sequence)
               (funcall function (funcall key x)))
             (dolist (x sequence)
               (funcall function x))))
        ((vectorp sequence)
         (if key
             (dotimes (i (length sequence))
               (funcall function (funcall key (aref sequence i))))
             (dotimes (i (length sequence))
               (funcall function (aref sequence i)))))
        (t
         (error "type error"))))

(defun find-if (predicate sequence &key from-end start end key)
  (map-sequence (lambda (x)
                  (when (funcall predicate x)
                    (return-from find-if x)))
                sequence
                from-end
                start
                end
                key)
  nil)

(defun remove-if-not (test sequence &key from-end start end #+(or)count key)
  (with-accumulate ()
    (map-sequence (lambda (x)
                    (when (funcall test x)
                      (collect x)))
                  sequence
                  from-end
                  start
                  end
                  key)))

(defun remove (item sequence)
  (with-accumulate ()
    (map-sequence (lambda (x)
                    (unless (eql item x)
                      (collect x)))
                  sequence
                  nil
                  0
                  nil
                  nil)))

(defun map (result-type function sequence &rest more-sequences)
  (cond ((and (null result-type) (null more-sequences))
         (map-sequence function sequence nil nil nil nil))
        (t
         (error "trap"))))
