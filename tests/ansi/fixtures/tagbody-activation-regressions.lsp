(in-package :cl-test)

(deftest recursive-go-reaches-lexical-activation
  (let ((visited nil))
    (labels ((visit (depth jump)
               (tagbody
                  (if (zerop depth)
                      (funcall jump)
                      (visit (1- depth) (or jump (lambda () (go done)))))
                  (push :fell-through visited)
                done
                  (push depth visited))))
      (visit 3 nil))
    visited)
  (3))

(deftest recursive-handler-case-resignals-outward
  (let ((handlers 0))
    (labels ((visit (depth)
               (handler-case
                   (if (zerop depth) (error "origin") (visit (1- depth)))
                 (error (condition)
                   ;; Bound a broken transfer so the regression fails rather
                   ;; than trapping the test process in an infinite loop.
                   (incf handlers)
                   (when (> handlers 4) (return-from visit :looped))
                   (error condition)))))
      (values (handler-case (visit 3) (error () :caught)) handlers)))
  :caught 4)

(deftest nonlocal-go-unwinds-dynamic-bindings
  (let ((*print-base* 10) (seen nil))
    (tagbody
       (let ((jump (lambda () (go done))))
         (let ((*print-base* 16))
           (unwind-protect (funcall jump) (push *print-base* seen))))
     done
       (push *print-base* seen))
    seen)
  (10 16))
