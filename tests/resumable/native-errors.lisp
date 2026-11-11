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
