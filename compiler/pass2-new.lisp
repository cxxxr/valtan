(in-package :compiler)

(defvar *p2-emit-stream* *standard-output*)
(defvar *p2-literal-symbols* (make-hash-table))
(defvar *p2-context*)

(defmacro p2-emit-try-finally (try finally)
  `(progn
     (write-line "try{" *p2-emit-stream*)
     ,try
     (write-line "}finally{" *p2-emit-stream*)
     ,finally
     (write-line "}" *p2-emit-stream*)))

(defmacro define-p2-emit (op (hir) &body body)
  (let ((name (make-symbol (format nil "~A:~A" (package-name (symbol-package op)) (symbol-name op)))))
    `(progn
       (defun ,name (,hir)
         (declare (ignorable hir))
         ,@body)
       (setf (get ',op 'p2-emit)
             ',name))))

(defun p2 (hir *p2-context*)
  (assert (member *p2-context* '(:expr :stmt)))
  (assert (eq (if (hir-return-value-p hir) :expr :stmt) *p2-context*))
  (let ((fn (get (hir-op hir) 'p2-emit)))
    (assert fn)
    (funcall fn hir)))

(defun p2-genvar (&optional (prefix "TMP"))
  (genvar prefix))

(defun p2-escape-string (string)
  (setq string (string string))
  (flet ((f (c)
           (or (cdr (assoc c *character-map*))
               (string c))))
    (with-output-to-string (out)
      (map nil (lambda (c)
                 (write-string (f c) out))
           string))))

(defun p2-local-var (symbol)
  (concatenate 'string "L_" (p2-escape-string symbol)))

(defun p2-local-function (symbol)
  (concatenate 'string "F_" (p2-escape-string symbol)))

(defun p2-symbol-to-js-value (symbol)
  (or (gethash symbol *p2-literal-symbols*)
      (setf (gethash symbol *p2-literal-symbols*)
            (genvar "G"))))

(defun p2-encode-string (string)
  (with-output-to-string (s)
    (write-char #\[ s)
    (let ((len (length string)))
      (do ((i 0 (1+ i)))
          ((>= i len))
        (princ (char-code (aref string i)) s)
        (when (< (1+ i) len)
          (write-string ", " s))))
    (write-char #\] s)))

(defun p2-literal (x)
  (cond ((null x)
         "lisp.S_nil")
        ((symbolp x)
         (p2-symbol-to-js-value x))
        ((stringp x)
         (format nil "CL_SYSTEM_JS_STRING_TO_ARRAY(lisp.codeArrayToString(~A))" (p2-encode-string x)))
        ((numberp x)
         (princ-to-string x))
        ((characterp x)
         (format nil "lisp.makeCharacter(~D)" (char-code x)))
        ((consp x)
         (format nil "lisp.makeCons(~A, ~A)"
                 (p2-literal (car x))
                 (p2-literal (cdr x))))
        ((vectorp x)
         (with-output-to-string (out)
           (write-string "CL_COMMON_LISP_VECTOR" out)
           (if (zerop (length x))
               (write-string "(" out)
               (dotimes (i (length x))
                 (if (zerop i)
                     (write-string "(" out)
                     (write-string "," out))
                 (write-string (p2-literal (aref x i)) out)))
           (write-string ")" out)))
        (t
         (error "unexpected literal: ~S" x))))

(defun p2-form (form)
  (cond ((hir-multiple-values-p form)
         (p2 form :expr))
        (t
         (let ((result (p2 form (if (hir-return-value-p form) :expr :stmt))))
           (format nil "lisp.values1(~A)" result)))))

(defun p2-forms (forms)
  (do ((forms forms (cdr forms)))
      ((length=1 forms)
       (p2-form (car forms)))
    (p2 (car forms) :stmt)))

(define-p2-emit const (hir)
  (p2-literal (hir-arg1 hir)))

(define-p2-emit lref (hir)
  (let ((binding (hir-arg1 hir)))
    (ecase (binding-type binding)
      ((:function)
       (p2-local-function (binding-name binding)))
      ((:variable)
       (p2-local-var (binding-id (hir-arg1 hir)))))))

(define-p2-emit gref (hir)
  (let ((ident (p2-symbol-to-js-value (hir-arg1 hir))))
    (format nil "lisp.symbolValue(~A)" ident)))

(define-p2-emit lset (hir)
  (let ((lhs (hir-arg1 hir))
        (rhs (hir-arg2 hir)))
    (let ((result (p2-local-var (binding-id lhs)))
          (value (p2 rhs :expr)))
      (format *p2-emit-stream* "~A = ~A;~%" result value)
      result)))

(define-p2-emit gset (hir)
  (let ((lhs (hir-arg1 hir))
        (rhs (hir-arg2 hir)))
    (let ((ident (p2-symbol-to-js-value lhs))
          (value (p2 rhs :expr)))
      (format *p2-emit-stream* "lisp.setSymbolValue(~A, ~A);~%" ident value)
      ident)))

(define-p2-emit if (hir)
  (let ((test (hir-arg1 hir))
        (then (hir-arg2 hir))
        (else (hir-arg3 hir)))
    ;; TODO: elseが省略できる場合は省略する
    (if (hir-return-value-p hir)
        (let ((test-result (p2 test :expr))
              (if-result (p2-genvar)))
          (format *p2-emit-stream* "let ~A;~%" if-result)
          (format *p2-emit-stream* "if(~A !== lisp.S_nil){~%" test-result)
          (format *p2-emit-stream* "~A=~A;~%" if-result (p2-form then))
          (format *p2-emit-stream* "}else{~%")
          (format *p2-emit-stream* "~A=~A;~%" if-result (p2-form else))
          (format *p2-emit-stream* "}~%")
          if-result)
        (let ((test-result (p2 test :expr)))
          (format *p2-emit-stream* "if(~A !== lisp.S_nil){~%" test-result)
          (p2 then :stmt)
          (format *p2-emit-stream* "}else{~%")
          (p2 else :stmt)
          (format *p2-emit-stream* "}~%")
          (values)))))

(defun p2-emit-declvar (binding value)
  (ecase (binding-type binding)
    ;; ((:special))
    ((:variable)
     (format *p2-emit-stream* "let ~A = ~A;~%" (p2-local-var (binding-id binding)) value))
    ((:function)
     (format *p2-emit-stream* "let ~A = ~A;~%" (p2-local-function (binding-name binding)) value))))

(define-p2-emit progn (hir)
  (p2-forms (hir-arg1 hir)))

(define-p2-emit lambda (hir)
  )

(define-p2-emit let (hir)
  (let ((bindings (hir-arg1 hir))
        (body (hir-arg2 hir)))
    (dolist (binding bindings)
      (let ((value (p2 (binding-init-value binding) :expr)))
        (p2-emit-declvar binding value)))
    ;; TODO: スペシャル変数の後始末
    (p2-forms body)))

(defun p2-prepare-args (args)
  (mapcar (lambda (arg)
            (p2 arg :expr))
          args))

(defun p2-emit-args (args)
  (do ((args args (cdr args)))
      ((null args))
    (princ (car args) *p2-emit-stream*)
    (when (cdr args)
      (write-string "," *p2-emit-stream*)))
  (write-line ");" *p2-emit-stream*))

(define-p2-emit lcall (hir)
  (let ((args (p2-prepare-args (hir-arg2 hir)))
        (result nil))
    (when (hir-return-value-p hir)
      (setq result (p2-genvar))
      (format *p2-emit-stream* "let ~A=" result))
    (format *p2-emit-stream* "~A(" (p2-local-function (binding-name (hir-arg1 hir))))
    (p2-emit-args args)
    result))

(define-p2-emit call (hir)
  ;; TODO: 組み込み関数の場合は効率の良いコードを出力する
  (let ((symbol (hir-arg1 hir))
        (args (p2-prepare-args (hir-arg2 hir)))
        (result nil))
    (when (hir-return-value-p hir)
      (setq result (p2-genvar))
      (format *p2-emit-stream* "let ~A=" result))
    (format *p2-emit-stream* "lisp.callFunctionWithCallStack(~A" (p2-symbol-to-js-value symbol))
    (when args (write-string "," *p2-emit-stream*))
    (p2-emit-args args)
    result))

(define-p2-emit unwind-protect (hir)
  (let ((protected-form (hir-arg1 hir))
        (cleanup-form (hir-arg2 hir))
        (saved-return-var (when (hir-return-value-p hir) (p2-genvar "saved")))
        (result))
    (when saved-return-var
      (format *p2-emit-stream* "let ~A;~%" saved-return-var))
    (p2-emit-try-finally (setq result
                               (cond ((hir-return-value-p hir)
                                      (let ((protect-form-result (p2-form protected-form)))
                                        (format *p2-emit-stream* "~A=lisp.currentValues();~%" saved-return-var)
                                        protect-form-result))
                                     (t
                                      (p2 protected-form :stmt)
                                      nil)))
                         (progn
                           (p2 cleanup-form :stmt)
                           (when (hir-return-value-p hir)
                             (format *p2-emit-stream*
                                     "lisp.restoreValues(~A);~%"
                                     saved-return-var))))
    result))

(define-p2-emit block (hir)
  (let ((name (hir-arg1 hir))
        (body (hir-arg2 hir)))
    (cond ((eql 0 (binding-escape-count name))
           (format *p2-emit-stream* "~A: for(;;){" (binding-id name))
           (p2-forms body)
           (write-line "break;" *p2-emit-stream*)
           (write-line "}" *p2-emit-stream*))
          (t
           (error "Not yet implemented")))))

(define-p2-emit return-from (hir)
  #+(or)
  (let ((name (hir-arg1 hir))
        (form (hir-arg2 hir)))
    (format *p2-emit-stream* "break ~A;~%" (binding-id name))))

(define-p2-emit tagbody (hir)
  )

(define-p2-emit go (hir)
  )

(define-p2-emit catch (hir)
  )

(define-p2-emit catch (hir)
  )

(define-p2-emit *:%defun (hir)
  )

(define-p2-emit *:%defpackage (hir)
  )

(define-p2-emit *:%in-package (hir)
  )

(define-p2-emit ffi:ref (hir)
  )

(define-p2-emit ffi:set (hir)
  )

(define-p2-emit ffi:var (hir)
  )

(define-p2-emit ffi:typeof (hir)
  )

(define-p2-emit ffi:aget (hir)
  )

(define-p2-emit js-call (hir)
  )

(define-p2-emit module (hir)
  )

(defun p2-toplevel (hir)
  (let ((*p2-literal-symbols* (make-hash-table)))
    (p2 hir (if (hir-return-value-p hir) :expr :stmt))))
