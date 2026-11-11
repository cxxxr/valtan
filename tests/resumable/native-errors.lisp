(in-package :cl-user)

(defvar *native-error-binding* :global)
(defvar *native-error-unbound*)
(defvar *native-error-cleanups* 0)

(ffi:set
 (ffi:ref "globalThis" "valtanNativeErrorTestSetup")
 (lambda ()
   (makunbound '*native-error-unbound*)
   (setf *native-error-cleanups* 0)
   (let ((missing (make-symbol "NATIVE-ERROR-MISSING")))
     (ffi:object
      "run"
      (lambda (thunk promise marker expected detail)
        (let ((*native-error-binding* marker)
              (cleaned nil))
          (multiple-value-prog1
              (unwind-protect
                   (restart-case
                       (handler-bind
                           ((error
                              (lambda (condition)
                                (assert (typep condition expected))
                                (when detail
                                  (assert
                                   (eql detail
                                        (if (typep condition 'type-error)
                                            (type-error-datum condition)
                                            (cell-error-name condition)))))
                                (assert (not cleaned))
                                (assert (eq *native-error-binding* marker))
                                (system::%resumable-await promise)
                                (assert (not cleaned))
                                (assert (eq *native-error-binding* marker))
                                (invoke-restart 'native-error-recover))))
                         (funcall thunk)
                         :missed)
                     (native-error-recover () (values :caught marker)))
                (system::%resumable-await ((ffi:ref "Promise" "resolve") nil))
                (setf cleaned t)
                (incf *native-error-cleanups*))
            (assert cleaned))))
      "symbolValue" (lambda () (symbol-value missing))
      "globalValue" (lambda () *native-error-unbound*)
      "undefinedFunction" (lambda () (funcall missing))
      "symbolFunction" (lambda () (symbol-function missing))
      "arity" (lambda () (funcall (symbol-function 'identity) 1 2))
      "nativeArity" (lambda () (funcall (symbol-function 'symbolp)))
      "resumableNativeArity" (lambda () (funcall (symbol-function 'symbol-value)))
      "symbolValueType" (lambda () (symbol-value 17))
      "symbolFunctionType" (lambda () (symbol-function 17))
      "functionType" (lambda () (funcall (ffi:ref "globalThis" "valtanNativeInvalidFunction")))
      "integerDivide" (lambda () (/ 1 0))
      "ratioDivide" (lambda () (/ 1/2 0))
      "floatDivide" (lambda () (/ 1d0 0d0))
      "complexDivide" (lambda () (/ #c(1d0 2d0) #c(0d0 0d0)))
      "floorZero" (lambda () (floor 1 0))
      "roundZero" (lambda () (round 1 0))
      "truncateZero" (lambda () (truncate 1 0))
      "ceilingZero" (lambda () (ceiling 1 0))
      "remZero" (lambda () (rem 1 0))
      "floatPrototypeType" (lambda () (float 17 17))
      "realType" (lambda () (floor #c(1 2)))
      "overflow" (lambda () (* most-positive-double-float 2d0))
      "underflow" (lambda () (/ least-positive-double-float 2d0))
      "expOverflow" (lambda () (exp 1000d0))
      "scaleOverflow" (lambda () (scale-float most-positive-double-float 1))
      "rationalNonfinite"
      (lambda () (rational (ffi:new (ffi:ref "lisp" "Float") (ffi:ref "Infinity"))))
      "decodeNonfinite"
      (lambda () (decode-float (ffi:new (ffi:ref "lisp" "Float") (ffi:ref "Infinity"))))
      "numericValues" (lambda () (floor 7 3))
      "ordinaryNumeric"
      (lambda ()
        (catch 'native-error-ordinary
          (handler-bind ((division-by-zero
                           (lambda (condition)
                             (declare (ignore condition))
                             (throw 'native-error-ordinary :caught))))
            (/ 1 0))))
      "foreignNumeric"
      (lambda ()
        (let ((number (ffi:new (ffi:ref "lisp" "Float") 6))
              (handled nil))
          ;; Native arithmetic calls VALUEOF synchronously. The callback's
          ;; own Lisp handler must receive the nested numerical error before
          ;; that callback unwinds, even inside a scheduled outer operation.
          (ffi:set (ffi:ref number "valueOf")
                   (lambda ()
                     (catch 'native-error-callback
                       (handler-bind ((division-by-zero
                                        (lambda (condition)
                                          (declare (ignore condition))
                                          (setf handled t)
                                          (throw 'native-error-callback 6))))
                         (/ 1 0)))))
          (assert (= 3d0 (/ number 2d0)))
          (assert handled)
          :passed))
      "ordinary"
      (lambda ()
        (catch 'native-error-ordinary
          (handler-bind ((unbound-variable
                           (lambda (condition)
                             (assert (eq (cell-error-name condition) missing))
                             (throw 'native-error-ordinary :caught))))
            (symbol-value missing))))
      "missing" missing
      "globalMissing" '*native-error-unbound*
      "binding" (lambda () *native-error-binding*)
      "cleanups" (lambda () *native-error-cleanups*)))))
