(in-package :cl-test)

(defgeneric keyword-eql-call (object &key))
(defmethod keyword-eql-call ((object t) &key common) (list object common))
(defmethod keyword-eql-call ((object (eql 7)) &key special)
  (list special (call-next-method)))
(deftest keyword-policy-distinguishes-eql-methods
  (values (keyword-eql-call 7 :special 1)
          (handler-case (keyword-eql-call 8 :special 2) (program-error () :rejected))
          (keyword-eql-call 7 :special 3)
          (handler-case (keyword-eql-call 7 :unknown 4) (program-error () :rejected)))
  (1 (7 nil)) :rejected (3 (7 nil)) :rejected)

(defgeneric keyword-changing-call (object &key))
(defmethod keyword-changing-call ((object t) &key common) (list object common))
(deftest keyword-policy-follows-method-lifecycle
  (let* ((before (handler-case (keyword-changing-call 4 :extra 1)
                   (program-error () :rejected)))
         (method (eval '(defmethod keyword-changing-call ((object integer) &key extra) extra)))
         (added (keyword-changing-call 4 :extra 2)))
    (remove-method #'keyword-changing-call method)
    (let ((removed (handler-case (keyword-changing-call 4 :extra 3)
                     (program-error () :rejected))))
      (add-method #'keyword-changing-call method)
      (let ((restored (keyword-changing-call 4 :extra 4)))
        (eval '(defmethod keyword-changing-call ((object integer) &key replacement) replacement))
        (values before added removed restored
                (handler-case (keyword-changing-call 4 :extra 5) (program-error () :rejected))
                (keyword-changing-call 4 :replacement 6)))))
  :rejected 2 :rejected 4 :rejected 6)

(defgeneric keyword-rest-call (object &key))
(defmethod keyword-rest-call ((object t) &key common) common)
(deftest keyword-policy-follows-permissive-around-method
  (let* ((before (handler-case (keyword-rest-call 1 :unknown 3)
                   (program-error () :rejected)))
         (method (eval '(defmethod keyword-rest-call :around ((object integer) &rest args &key &allow-other-keys)
                          (declare (ignore args)) (call-next-method))))
         (allowed (keyword-rest-call 1 :unknown 3 :common 5)))
    (remove-method #'keyword-rest-call method)
    (values before allowed
            (handler-case (keyword-rest-call 1 :unknown 3 :common 5)
              (program-error () :rejected))))
  :rejected 5 :rejected)

;; &REST makes a method congruent with a keyword-taking GF; it does not
;; authorize arbitrary keywords for calls to that GF (CLHS 7.6.5).
(defgeneric keyword-rest-only-call (object &key))
(defmethod keyword-rest-only-call ((object t) &key common) common)
(defmethod keyword-rest-only-call :around ((object integer) &rest args)
  (declare (ignore args)) (call-next-method))
(deftest keyword-rest-method-does-not-disable-validation
  (values (keyword-rest-only-call 1 :common 5)
          (handler-case (keyword-rest-only-call 1 :unknown 3) (program-error () :rejected))
          (keyword-rest-only-call 1 :common 6 :unknown 3 :allow-other-keys t))
  5 :rejected 6)

(defgeneric keyword-argument-call (object &key))
(defmethod keyword-argument-call ((object t) &key common) common)
(deftest keyword-policy-still-checks-each-argument-list
  (values (keyword-argument-call 1 :common 2)
          (keyword-argument-call 1 :common 3)
          (handler-case (keyword-argument-call 1 :unknown 2) (program-error () :rejected))
          (keyword-argument-call 1 :unknown 2 :allow-other-keys t)
          (handler-case (keyword-argument-call 1 :unknown 2 :allow-other-keys nil
                                              :allow-other-keys t)
            (program-error () :rejected))
          (handler-case (keyword-argument-call 1 :common) (program-error () :rejected)))
  2 3 :rejected nil :rejected :rejected)

(defclass keyword-first-parent () ())
(defclass keyword-second-parent () ())
(defclass keyword-changing-child (keyword-first-parent) ())
(defgeneric keyword-parent-call (object &key))
(defmethod keyword-parent-call ((object keyword-first-parent) &key first) first)
(defmethod keyword-parent-call ((object keyword-second-parent) &key second) second)
(deftest keyword-policy-follows-class-redefinition
  (let* ((object (make-instance 'keyword-changing-child))
         (before (keyword-parent-call object :first 3)))
    (eval '(defclass keyword-changing-child (keyword-second-parent) ()))
    (values before (keyword-parent-call object :second 4)
            (handler-case (keyword-parent-call object :first 5) (program-error () :rejected))))
  3 4 :rejected)
