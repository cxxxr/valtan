(defmacro time (form)
  (let ((start (gensym)))
    `(let ((,start (js:-date.now)))
       ,form
       (format t "~&time: ~A~%" (- (js:-date.now) ,start)))))

(time (eval '(defun fact (n)
              (if (zerop n)
                  1
                  (* n (fact (1- n)))))))
