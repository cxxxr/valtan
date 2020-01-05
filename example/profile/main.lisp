(defmacro time (form)
  (let ((start (gensym)))
    `(let ((,start (js:-date.now)))
       ,form
       (format t "~&time: ~A~%" (- (js:-date.now) ,start)))))

#+(or)
(dotimes (i 100)
  (compiler::pass1-toplevel
   '(let ((chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
")
          c)
     (dotimes (i (length chars) t)
       (setq c (schar chars i))
       (cond
         ((upper-case-p c)
          (unless (and (both-case-p c)
                       (not (lower-case-p c))
                       (char= (char-upcase c) c)
                       (not (char= (char-downcase c) c)))
            (return nil)))
         ((lower-case-p c)
          (unless (and (both-case-p c)
                       (char= (char-downcase c) c)
                       (not (char= (char-upcase c) c)))
            (return nil)))
         (t
          (unless (and (not (upper-case-p c))
                       (not (lower-case-p c))
                       (not (both-case-p c))
                       (char= (char-upcase c) c)
                       (char= (char-downcase c) c))
            (return nil))))))))

(time (dotimes (i 1000) (mapcar #'1+ '(1 2 3 4 5))))
(time (dotimes (i 1000) (*::%mapcar #'1+ '(1 2 3 4 5))))
