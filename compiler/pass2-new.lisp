(in-package :compiler)

(defparameter *character-map*
  '((#\! . "BANG")
    (#\" . "QUOTATION")
    (#\# . "HASH")
    (#\$ . "DOLLAR")
    (#\% . "PERCENT")
    (#\& . "AMPERSAND")
    (#\' . "QUOTE")
    (#\( . "LPAREN")
    (#\) . "RPAREN")
    (#\* . "STAR")
    (#\+ . "PLUS")
    (#\, . "COMMA")
    (#\- . "_")
    (#\. . "DOT")
    (#\/ . "SLASH")
    (#\: . "COLON")
    (#\; . "SEMICOLON")
    (#\< . "LESS")
    (#\= . "EQUAL")
    (#\> . "GREATER")
    (#\? . "QUESTION")
    (#\space . "SPACE")
    (#\@ . "AT")
    (#\[ . "LBRACKET")
    (#\\ . "BACKSLASH")
    (#\] . "RBRACKET")
    (#\^ . "CARET")
    (#\_ . "__")
    (#\` . "BACKQUOTE")
    (#\{ . "LBRACE")
    (#\| . "PIPE")
    (#\} . "RBRACE")
    (#\~ . "TILDE")
    (#\newline . "NEWLINE")
    (#\return . "RETURN")
    (#\backspace . "BACK")
    (#\page . "PAGE")
    (#\tab . "TAB")))

(defvar *p2-emit-stream* *standard-output*)
(defvar *p2-literal-symbols* (make-hash-table))
(defvar *p2-toplevel-defun-stream*)
(defvar *p2-defun-names*)
(defvar *p2-temporary-variables*)

(defmacro p2-emit-try-finally (try finally)
  `(progn
     (write-line "try{" *p2-emit-stream*)
     ,try
     (write-line "}finally{" *p2-emit-stream*)
     ,finally
     (write-line "}" *p2-emit-stream*)))

(defmacro p2-emit-try-catch (try-form ((error-var) &body catch-form))
  `(progn
     (write-line "try {" *p2-emit-stream*)
     ,try-form
     (format *p2-emit-stream* "}catch(~A){" ,error-var)
     ,@catch-form
     (write-line "}" *p2-emit-stream*)))

(defun p2-emit-for-aux (loop-var start end step function)
  (write-string "for (let " *p2-emit-stream*)
  (write-string loop-var *p2-emit-stream*)
  (write-string " = " *p2-emit-stream*)
  (princ start *p2-emit-stream*)
  (write-string "; " *p2-emit-stream*)
  (write-string loop-var *p2-emit-stream*)
  (write-string " < " *p2-emit-stream*)
  (write-string end *p2-emit-stream*)
  (write-string "; " *p2-emit-stream*)
  (write-string loop-var *p2-emit-stream*)
  (if (= step 1)
      (write-string " ++" *p2-emit-stream*)
      (progn
        (write-string " += " *p2-emit-stream*)
        (princ step *p2-emit-stream*)))
  (write-line ") {" *p2-emit-stream*)
  (funcall function)
  (write-line "}" *p2-emit-stream*))

(defmacro p2-emit-for ((loop-var start end step) &body body)
  `(p2-emit-for-aux ,loop-var ,start ,end ,step (lambda () ,@body)))

(defmacro p2-with-unwind-special-vars (form unwind-code)
  (let ((unwind-code-var (gensym)))
    `(let ((,unwind-code-var ,unwind-code))
       (if (string= ,unwind-code-var "")
           ,form
           (p2-emit-try-finally ,form (write-string ,unwind-code-var *p2-emit-stream*))))))

(defmacro p2-with-emit-paren (&body body)
  `(progn
     (write-string "(" *p2-emit-stream*)
     ,@body
     (write-string ")" *p2-emit-stream*)))

(defmacro define-p2-emit (op (hir) &body body)
  (let ((name (make-symbol (format nil "~A:~A" (package-name (symbol-package op)) (symbol-name op)))))
    `(progn
       (defun ,name (,hir)
         (declare (ignorable hir))
         ,@body)
       (setf (get ',op 'p2-emit)
             ',name))))

(defun p2 (hir context)
  (assert (member context '(:expr :stmt)))
  (assert (eq (if (hir-return-value-p hir) :expr :stmt) context))
  (let ((fn (get (hir-op hir) 'p2-emit)))
    (assert fn)
    (funcall fn hir)))

(defun p2-emit-declare-temporary-variables ()
  (dolist (var (remove-duplicates *p2-temporary-variables* :test #'equal))
    (format *p2-emit-stream* "let ~A;~%" var)))

(defun p2-genvar (&optional (prefix "TMP"))
  (genvar prefix))

(defun p2-temporary-var (&optional (prefix "TMP"))
  (let ((var (p2-genvar prefix)))
    (push var *p2-temporary-variables*)
    var))

(defun p2-escape-string (string &optional prefix)
  (setq string (string string))
  (flet ((f (c)
           (or (cdr (assoc c *character-map*))
               (string c))))
    (with-output-to-string (out)
      (when prefix (write-string prefix out))
      (map nil (lambda (c)
                 (write-string (f c) out))
           string))))

(defun p2-local-var (symbol &optional (prefix "L_"))
  (p2-escape-string symbol prefix))

(defun p2-local-function (symbol &optional (prefix "F_"))
  (p2-escape-string symbol prefix))

(defun p2-symbol-to-js-value (symbol)
  (when *debug*
    (write-line "=== debug ===")
    (genvar "G")
    (write-line "=== end ==="))
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

(defun %p2-form (form)
  (p2 form (if (hir-return-value-p form) :expr :stmt)))

(defun p2-form (form)
  (cond ((hir-multiple-values-p form)
         (p2 form :expr))
        (t
         (let ((result (%p2-form form)))
           (format nil "lisp.values1(~A)" result)))))

(defun p2-forms (forms)
  (do ((forms forms (cdr forms)))
      ((length=1 forms)
       (p2-form (car forms)))
    (p2 (car forms) :stmt)))

(defun p2-no-return ()
  (p2-form (make-hir 'const t nil nil)))

(define-p2-emit const (hir)
  (p2-literal (hir-arg1 hir)))

(define-p2-emit lref (hir)
  (let ((binding (hir-arg1 hir)))
    (ecase (binding-type binding)
      ((:function)
       (p2-local-function (binding-id binding)))
      ((:variable)
       (p2-local-var (binding-id (hir-arg1 hir)))))))

(define-p2-emit gref (hir)
  (let ((ident (p2-symbol-to-js-value (hir-arg1 hir))))
    (format nil "lisp.symbolValue(~A)" ident)))

(define-p2-emit lset (hir)
  (let ((lhs (hir-arg1 hir))
        (rhs (hir-arg2 hir)))
    (let ((result (p2-local-var (binding-id lhs)))
          (value (p2-form rhs)))
      (cond ((hir-return-value-p hir)
             (format nil "(~A=~A)" result value))
            (t
             (format *p2-emit-stream* "~A=~A;~%" result value)
             #+(or)(p2-no-return))))))

(define-p2-emit gset (hir)
  (let ((lhs (hir-arg1 hir))
        (rhs (hir-arg2 hir)))
    (let ((ident (p2-symbol-to-js-value lhs))
          (value (p2-form rhs)))
      (cond ((hir-return-value-p hir)
             (format nil "(~A=~A)" ident value))
            (t
             (format *p2-emit-stream* "lisp.setSymbolValue(~A, ~A);~%" ident value)
             #+(or)(p2-no-return))))))

(define-p2-emit if (hir)
  (let ((test (hir-arg1 hir))
        (then (hir-arg2 hir))
        (else (hir-arg3 hir)))
    ;; TODO: elseが省略できる場合は省略する
    (if (hir-return-value-p hir)
        (let ((test-result (p2-form test))
              (if-result (p2-temporary-var)))
          (format *p2-emit-stream* "if(~A !== lisp.S_nil){~%" test-result)
          (format *p2-emit-stream* "~A=~A;~%" if-result (p2-form then))
          (format *p2-emit-stream* "}else{~%")
          (format *p2-emit-stream* "~A=~A;~%" if-result (p2-form else))
          (format *p2-emit-stream* "}~%")
          if-result)
        (let ((test-result (p2-form test)))
          (format *p2-emit-stream* "if(~A !== lisp.S_nil){~%" test-result)
          (p2 then :stmt)
          (format *p2-emit-stream* "}else{~%")
          (p2 else :stmt)
          (format *p2-emit-stream* "}~%")
          (values)))))

(define-p2-emit progn (hir)
  (p2-forms (hir-arg1 hir)))

(defun p2-emit-check-arguments (name parsed-lambda-list)
  (let ((min (parsed-lambda-list-min parsed-lambda-list))
        (max (parsed-lambda-list-max parsed-lambda-list)))
    (cond ((null max)
           (format *p2-emit-stream* "if(arguments.length < ~D) {~%" min))
          ((= min max)
           (format *p2-emit-stream* "if(arguments.length !== ~D) {~%" min))
          (t
           (format *p2-emit-stream* "if(arguments.length < ~D || ~D < arguments.length) {~%" min max)))
    (let ((symbol-var (p2-literal name)))
      (format *p2-emit-stream* "lisp.argumentsError(~A, arguments.length);~%" symbol-var))
    (write-line "}" *p2-emit-stream*)))

(defun p2-make-save-var (var)
  (p2-local-var (binding-id var) "save_"))

(defun p2-emit-unwind-var (var finally-stream)
  (when (eq (binding-type var) :special)
    (let ((js-var (p2-symbol-to-js-value (binding-name var)))
          (save-var (p2-make-save-var var)))
      (format finally-stream "~A.value=~A;~%" js-var save-var))))

(defun p2-emit-declvar (var finally-stream)
  (ecase (binding-type var)
    ((:special)
     (let ((js-var (p2-symbol-to-js-value (binding-name var)))
           (save-var (p2-make-save-var var)))
       (push save-var *p2-temporary-variables*)
       (format *p2-emit-stream* "~A=~A.value;~%" save-var js-var)
       (format *p2-emit-stream* "~A.value=" js-var))
     (when finally-stream
       (p2-emit-unwind-var var finally-stream)))
    ((:variable)
     (let ((var (p2-local-var (binding-id var))))
       (push var *p2-temporary-variables*)
       (format *p2-emit-stream* "~A=" var)))
    ((:function)
     (let ((var (p2-local-function (binding-id var))))
       (push var *p2-temporary-variables*)
       (format *p2-emit-stream* "~A=" var)))))

(defun p2-emit-lambda-list (parsed-lambda-list finally-stream)
  (let ((i 0))
    (dolist (var (parsed-lambda-list-vars parsed-lambda-list))
      (p2-emit-declvar var finally-stream)
      (format *p2-emit-stream* "arguments[~D];~%" i)
      (incf i))
    (dolist (opt (parsed-lambda-list-optionals parsed-lambda-list))
      (let ((var (first opt))
            (value (second opt))
            (supplied-binding (third opt)))
        (let ((result (p2-form value)))
          (p2-emit-declvar var finally-stream)
          (format *p2-emit-stream* "arguments.length > ~D ? arguments[~D] : " i i)
          (format *p2-emit-stream* "(~A);~%" result))
        (when supplied-binding
          (p2-emit-declvar supplied-binding finally-stream)
          (format *p2-emit-stream* "(arguments.length > ~D ? lisp.S_t : lisp.S_nil);~%" i))
        (incf i)))
    (when (parsed-lambda-list-keys parsed-lambda-list)
      (let ((keyword-vars '()))
        (dolist (opt (parsed-lambda-list-keys parsed-lambda-list))
          (let* ((var (first opt))
                 (value (second opt))
                 (supplied-binding (third opt))
                 (keyword-var (p2-symbol-to-js-value (fourth opt)))
                 (supplied-var (p2-local-var (binding-id var) "supplied_")))
            (push keyword-var keyword-vars)
            (format *p2-emit-stream* "let ~A;~%" supplied-var)
            (let ((loop-var (p2-genvar)))
              (p2-emit-for (loop-var i "arguments.length" 2)
                (format *p2-emit-stream* "if(arguments[~D] === ~A){~%" loop-var keyword-var)
                (format *p2-emit-stream* "~A=arguments[~D+1];~%" supplied-var loop-var)
                (write-line "break;" *p2-emit-stream*)
                (write-line "}" *p2-emit-stream*)))
            (let ((result (p2-form value)))
              (p2-emit-declvar var finally-stream)
              (format *p2-emit-stream*
                      "~A !== undefined ? ~A : (~A);~%"
                      supplied-var
                      supplied-var
                      result))
            (when supplied-binding
              (p2-emit-declvar supplied-binding finally-stream)
              (format *p2-emit-stream* "(~A !== undefined ? lisp.S_t : lisp.S_nil);~%" supplied-var))))
        (format *p2-emit-stream* "if((arguments.length-~D)%2===1)" i)
        (write-line "{lisp.programError('odd number of &KEY arguments');}" *p2-emit-stream*)
        (when (and keyword-vars
                   (null (parsed-lambda-list-allow-other-keys parsed-lambda-list)))
          (let ((loop-var (p2-genvar)))
            (p2-emit-for (loop-var i "arguments.length" 2)
              (write-string "if(" *p2-emit-stream*)
              (do ((keyword-vars keyword-vars (rest keyword-vars)))
                  ((null keyword-vars))
                (format *p2-emit-stream* "arguments[~D]!==~A" loop-var (first keyword-vars))
                (when (rest keyword-vars)
                  (write-string " && " *p2-emit-stream*)))
              (format *p2-emit-stream*
                      ") { lisp.programError('Unknown &KEY argument: ' + arguments[~A].name); }~%"
                      loop-var))))))
    (let ((rest-var (parsed-lambda-list-rest-var parsed-lambda-list)))
      (when rest-var
        (p2-emit-declvar rest-var finally-stream)
        (format *p2-emit-stream* "lisp.jsArrayToList(arguments, ~D);~%" i)))))

(define-p2-emit lambda (hir)
  (let ((name (hir-arg1 hir))
        (parsed-lambda-list (hir-arg2 hir))
        (body (hir-arg3 hir))
        lambda-result)
    (when (hir-return-value-p hir)
      (setq lambda-result (p2-temporary-var))
      (format *p2-emit-stream* "~A=" lambda-result))
    (write-line "(function(){" *p2-emit-stream*)
    (p2-emit-check-arguments name parsed-lambda-list)
    (let ((*p2-temporary-variables* '()))
      (let ((code
              (with-output-to-string (*p2-emit-stream*)
                (let ((finally-code
                        (with-output-to-string (finally-stream)
                          (p2-emit-lambda-list parsed-lambda-list finally-stream))))
                  (p2-with-unwind-special-vars
                   (let ((result (p2-forms body)))
                     (format *p2-emit-stream* "return ~A;~%" result))
                   finally-code)))))
        (p2-emit-declare-temporary-variables)
        (write-string code *p2-emit-stream*)))
    (write-line "});" *p2-emit-stream*)
    (or lambda-result
        (values))))

(define-p2-emit let (hir)
  (let ((bindings (hir-arg1 hir))
        (body (hir-arg2 hir)))
    (write-line "{" *p2-emit-stream*)
    (dolist (binding bindings)
      (let ((value (p2-form (binding-init-value binding))))
        (p2-emit-declvar binding nil)
        (format *p2-emit-stream* "~A;~%" value)))
    (let (result)
      (p2-with-unwind-special-vars
       (setq result (p2-forms body))
       (with-output-to-string (output)
         (dolist (binding (reverse bindings))
           (p2-emit-unwind-var binding output))))
      (write-line "}" *p2-emit-stream*)
      result)))

(defun p2-prepare-args (args)
  (mapcar #'%p2-form args))

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
      (setq result (p2-temporary-var))
      (format *p2-emit-stream* "~A=" result))
    (format *p2-emit-stream* "~A(" (p2-local-function (binding-id (hir-arg1 hir))))
    (p2-emit-args args)
    result))

(define-p2-emit call (hir)
  ;; TODO: 組み込み関数の場合は効率の良いコードを出力する
  (let ((symbol (hir-arg1 hir))
        (args (p2-prepare-args (hir-arg2 hir)))
        (result nil))
    (format *p2-emit-stream* "// ~A~%" symbol)
    (when (hir-return-value-p hir)
      (setq result (p2-temporary-var))
      (format *p2-emit-stream* "~A=" result))
    (format *p2-emit-stream*
            "lisp.callFunctionWithCallStack(~A"
            (p2-symbol-to-js-value symbol))
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
                                        (format *p2-emit-stream*
                                                "~A=lisp.currentValues();~%"
                                                saved-return-var)
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

(defun p2-block-result-var-name (name)
  (concatenate 'string "BLOCK_" (string (binding-id name))))

(define-p2-emit block (hir)
  (let ((name (hir-arg1 hir))
        (body (hir-arg2 hir)))
    (cond ((eql 0 (binding-escape-count name))
           (let ((block-result (p2-block-result-var-name name)))
             (format *p2-emit-stream* "let ~A;~%" block-result)
             (format *p2-emit-stream* "~A: for(;;){" (binding-id name))
             (let ((result (p2-forms body)))
               (format *p2-emit-stream* "~A=~A;~%" block-result result))
             (write-line "break;" *p2-emit-stream*)
             (write-line "}" *p2-emit-stream*)
             block-result))
          (t
           (let ((error-var (p2-genvar "E_"))
                 (result-var (p2-temporary-var)))
             (p2-emit-try-catch
              (format *p2-emit-stream* "~A=~A~%" result-var (p2-forms body))
              ((error-var)
               (format *p2-emit-stream*
                       "if(~A instanceof lisp.BlockValue && ~A.name === ~A){~A=~A.value;}~%"
                       error-var
                       error-var
                       (p2-symbol-to-js-value (binding-name name))
                       result-var
                       error-var)
               (format *p2-emit-stream*
                       "else{throw ~A;}~%" error-var)))
             result-var)))))

(define-p2-emit return-from (hir)
  (let ((name (hir-arg1 hir))
        (form (hir-arg2 hir)))
    (cond ((eql 0 (binding-escape-count name))
           (let ((result (p2-form form)))
             (format *p2-emit-stream* "~A=~A;~%" (p2-block-result-var-name name) result))
           (format *p2-emit-stream* "break ~A;~%" (binding-id name)))
          (t
           (let ((result (p2-form form)))
             (format *p2-emit-stream*
                     "throw new lisp.BlockValue(~A,~A);"
                     (p2-symbol-to-js-value (binding-name name))
                     result))))
    (p2-no-return)))

(define-p2-emit loop (hir)
  (let ((body (hir-arg1 hir)))
    (write-line "for(;;){" *p2-emit-stream*)
    (p2 body :stmt)
    (write-line "break;" *p2-emit-stream*)
    (write-line "}" *p2-emit-stream*)
    (p2-no-return)))

(define-p2-emit recur (hir)
  (write-line "continue;" *p2-emit-stream*)
  (p2-no-return))

(defun tagbody-state-name (tagbody-name)
  (format nil "~A_STATE" tagbody-name))

(defun tagbody-tag-name (tag)
  (tagbody-value-index (binding-id tag)))

(define-p2-emit tagbody (hir)
  (let* ((tagbody-name (hir-arg1 hir))
         (tag-body-pairs (hir-arg2 hir))
         (exist-escape-p (not (hir-arg3 hir)))
         (error-var (when exist-escape-p (p2-genvar "ERR"))))
    (let ((state-var (tagbody-state-name tagbody-name)))
      (format *p2-emit-stream* "let ~A = '~A';~%" state-var (tagbody-tag-name (car (first tag-body-pairs))))
      (progn
        (format *p2-emit-stream* "~A: for(;;){~%" tagbody-name)
        (progn
          (when exist-escape-p
            (write-line "try{" *p2-emit-stream*))
          (progn
            (format *p2-emit-stream* "switch(~A){~%" state-var)
            (dolist (pair tag-body-pairs)
              (destructuring-bind (tag . body) pair
                (format *p2-emit-stream* "case '~A':~%" (tagbody-tag-name tag))
                (p2 body :stmt)))
            (write-line "}" *p2-emit-stream*)
            (write-line "break;" *p2-emit-stream*))
          (when exist-escape-p
            (format *p2-emit-stream* "}catch(~A){~%" error-var)
            (format *p2-emit-stream*
                    "if(~A instanceof lisp.TagValue && ~A.id==='~A'){~A=~A.index;}~%"
                    error-var
                    error-var
                    tagbody-name
                    state-var
                    error-var)
            (format *p2-emit-stream* "else{throw ~A;}" error-var)
            (write-line "}" *p2-emit-stream*)))
        (write-line "}" *p2-emit-stream*)
        (p2-no-return)))))

(define-p2-emit go (hir)
  (let ((tag (hir-arg1 hir)))
    (cond ((eql 0 (binding-escape-count tag))
           (let* ((name (tagbody-value-id (binding-id tag)))
                  (state-var (tagbody-state-name name)))
             (format *p2-emit-stream* "~A='~A';~%" state-var (tagbody-tag-name tag))
             (format *p2-emit-stream* "continue ~A;~%" name)
             (values)))
          (t
           (let ((tagbody-value (binding-id tag)))
             (format *p2-emit-stream*
                     "throw new lisp.TagValue('~A', '~A');~%"
                     (tagbody-value-id tagbody-value)
                     (tagbody-value-index tagbody-value))))))
  (p2-no-return))

(define-p2-emit *:%defun (hir)
  (let ((name (hir-arg1 hir))
        (function (hir-arg2 hir)))
    (let ((var (p2-local-function name
                                  (if (symbol-package name)
                                      (format nil
                                              "CL_~A_"
                                              (p2-escape-string
                                               (package-name
                                                (symbol-package name))))
                                      "CL_"))))
      (pushnew var *p2-defun-names* :test #'equal)
      (let ((*p2-temporary-variables* '()))
        (let ((code (with-output-to-string (*p2-emit-stream*)
                      (format *p2-emit-stream* "~A=~A;~%" var (p2-form function)))))
          (format *p2-toplevel-defun-stream*
                  "~A~A"
                  (with-output-to-string (*p2-emit-stream*)
                    (p2-emit-declare-temporary-variables))
                  code)))
      (let ((name-var (p2-symbol-to-js-value name)))
        (format *p2-emit-stream* "lisp.setSymbolFunction(~A, ~A);~%" name-var var)
        name-var))))

(define-p2-emit *:%defpackage (hir)
  (let ((name (hir-arg1 hir))
        (specs (hir-arg2 hir)))
    (destructuring-bind (export-names use-package-names nicknames) specs
      (format *p2-toplevel-defun-stream* "lisp.defpackage('~A', {" name)
      (let ((first t))
        (write-string "exportNames: [" *p2-toplevel-defun-stream*)
        (dolist (name export-names)
          (if first
              (setq first nil)
              (write-string ", " *p2-toplevel-defun-stream*))
          (format *p2-toplevel-defun-stream* "'~A'" name))
        (write-string "]" *p2-toplevel-defun-stream*))
      (write-string ", " *p2-toplevel-defun-stream*)
      (let ((first t))
        (write-string "usePackageNames: [" *p2-toplevel-defun-stream*)
        (dolist (name use-package-names)
          (if first
              (setq first nil)
              (write-string ", " *p2-toplevel-defun-stream*))
          (format *p2-toplevel-defun-stream* "'~A'" name))
        (write-string "]" *p2-toplevel-defun-stream*))
      (write-string ", " *p2-toplevel-defun-stream*)
      (let ((first t))
        (write-string "nicknames: [" *p2-toplevel-defun-stream*)
        (dolist (name nicknames)
          (if first
              (setq first nil)
              (write-string ", " *p2-toplevel-defun-stream*))
          (format *p2-toplevel-defun-stream* "'~A'" name))
        (write-string "]" *p2-toplevel-defun-stream*)))
    (write-line "});" *p2-toplevel-defun-stream*)
    (let ((result (when (hir-return-value-p hir) (p2-temporary-var))))
      (when (hir-return-value-p hir)
        (format *p2-emit-stream* "~A=" result))
      (format *p2-emit-stream* "lisp.ensurePackage('~A');" name)
      (if (hir-return-value-p hir)
          result
          (p2-no-return)))))

(define-p2-emit *:%in-package (hir)
  (let ((name (hir-arg1 hir)))
    (let ((result (when (hir-return-value-p hir) (p2-temporary-var))))
      (when (hir-return-value-p hir)
        (format *p2-emit-stream* "~A=" result))
      (format *p2-emit-stream* "lisp.changeCurrentPackage('~A');" name)
      (if (hir-return-value-p hir)
          result
          (p2-no-return)))))

(defun p2-emit-ref (args)
  (let ((code
          (with-output-to-string (out)
            (destructuring-bind (object . keys) args
              (if (hir-p object)
                  (let ((result (p2-form object)))
                    (princ result out))
                  (write-string object out))
              (when keys
                (write-string "." out))
              (do ((keys keys (rest keys)))
                  ((null keys))
                (write-string (p2-escape-string (first keys)) out)
                (when (rest keys)
                  (write-string "." out)))))))
    code))

(define-p2-emit ffi:ref (hir)
  (p2-emit-ref (hir-arg1 hir)))

(define-p2-emit ffi:set (hir)
  (let ((lhs (hir-arg1 hir))
        (rhs (hir-arg2 hir)))
    (let ((result (p2-form rhs)))
      (format *p2-emit-stream*
              "~A=~A;~%"
              (%p2-form lhs)
              result)
      result)))

(defun p2-convert-var (var)
  (if (stringp var)
      (p2-escape-string var)
      (p2-emit-ref (hir-arg1 var))))

(define-p2-emit ffi:var (hir)
  (write-string "var " *p2-emit-stream*)
  (do ((vars (hir-arg1 hir) (rest vars)))
      ((null vars))
    (write-string (p2-convert-var (first vars)) *p2-emit-stream*)
    (when (rest vars)
      (write-string ", " *p2-emit-stream*)))
  (write-line ";" *p2-emit-stream*)
  (when (hir-return-value-p hir)
    (p2-no-return)))

(define-p2-emit ffi:typeof (hir)
  (let ((value (p2-form (hir-arg1 hir)))
        (result (when (hir-return-value-p hir) (p2-temporary-var))))
    (when (hir-return-value-p hir)
      (format *p2-emit-stream* "~A=" result))
    (format *p2-emit-stream* "(typeof ~A);~%" value)
    (if (hir-return-value-p hir)
        result
        (p2-no-return))))

(define-p2-emit ffi:new (hir)
  (let ((expr (hir-arg1 hir))
        (args (hir-arg2 hir)))
    (let ((fn (%p2-form expr))
          (args (p2-prepare-args args))
          (result nil))
      (when (hir-return-value-p hir)
        (setq result (p2-temporary-var))
        (format *p2-emit-stream* "~A=" result))
      (format *p2-emit-stream* "new ~A(" fn)
      (p2-emit-args args)
      (or result (p2-no-return)))))

(define-p2-emit ffi:aget (hir)
  (let ((array (hir-arg1 hir))
        (indexes (hir-arg2 hir)))
    (let ((value (p2-form array))
          (indexes (mapcar #'p2-form indexes)))
      (with-output-to-string (out)
        (princ value out)
        (dolist (index indexes)
          (format out "[~A]" index))))))

(define-p2-emit js-call (hir)
  (let ((fn-place (hir-arg1 hir))
        (args (hir-arg2 hir)))
    (let ((args (p2-prepare-args args))
          (fn-code (p2-emit-ref fn-place))
          (result nil))
      (when (hir-return-value-p hir)
        (setq result (p2-temporary-var))
        (format *p2-emit-stream* "~A=" result))
      (format *p2-emit-stream* "~A(" fn-code)
      (p2-emit-args args)
      result)))

(define-p2-emit module (hir)
  (let ((name (hir-arg1 hir))
        (forms (hir-arg2 hir))
        (export-modules (hir-arg3 hir)))
    (format *p2-emit-stream* "(function() { // *** module: ~A ***~%" name)
    (p2-forms forms)
    (dolist (export-module export-modules)
      (destructuring-bind (name . as) export-module
        (if as
            (format *p2-emit-stream*
                    "module.exports = ~A~%"
                    (p2-convert-var name))
            (format *p2-emit-stream*
                    "module.exports.~A = ~A~%"
                    (p2-convert-var name)
                    (p2-convert-var as)))))
    (write-line "})();" *p2-emit-stream*)))

(defun p2-toplevel-1 (hir)
  (p2 hir (if (hir-return-value-p hir) :expr :stmt)))

(defun p2-toplevel (hir)
  (let ((*p2-literal-symbols* (make-hash-table)))
    (p2 hir (if (hir-return-value-p hir) :expr :stmt))))

(defun p2-test (hir)
  (let (result)
    (let* ((*p2-toplevel-defun-stream* (make-string-output-stream))
           (*p2-defun-names* '())
           (*p2-temporary-variables* '())
           (text
             (with-output-to-string (*p2-emit-stream*)
               (setq result (p2-toplevel hir)))))
      (setq text
            (with-output-to-string (out)
              (p2-emit-initialize-vars out)
              (write-string text out)
              (p2-emit-initialize-symbols out)))
      (handler-case (js-beautify
                     (concatenate 'string
                                  (get-output-stream-string *p2-toplevel-defun-stream*)
                                  text))
        (error () (write-line text)))
      result)))

(defun p2-emit-initialize-vars (stream)
  (maphash (lambda (symbol ident)
             (declare (ignore symbol))
             (format stream "let ~A;~%" ident))
           *p2-literal-symbols*)
  (dolist (name *p2-defun-names*)
    (format stream "let ~A;~%" name))
  (let ((*p2-emit-stream* stream))
    (p2-emit-declare-temporary-variables)))

(defun p2-emit-initialize-symbols (stream)
  (maphash (lambda (symbol ident)
             (if (symbol-package symbol)
                 (format stream
                         "~A = lisp.intern('~A', '~A');~%"
                         ident
                         symbol
                         (package-name (symbol-package symbol)))
                 (format stream "~A = lisp.makeSymbol(\"~A\");" ident symbol)))
           *p2-literal-symbols*))

(defun p2-toplevel-forms (hir-forms &optional (stream *standard-output*))
  (let ((*p2-literal-symbols* (make-hash-table))
        (*p2-defun-names* '())
        (*p2-temporary-variables* '())
        (*p2-toplevel-defun-stream* (make-string-output-stream))
        (*p2-emit-stream* (make-string-output-stream))
        (*genvar-counter* 0))
    (let ((err (p2-genvar)))
      (p2-emit-try-catch
       (dolist (hir hir-forms)
         (p2-toplevel-1 hir))
       ((err)
        (write-line "CL_COMMON_LISP_FINISH_OUTPUT();" *p2-emit-stream*)
        (format *p2-emit-stream* "console.log(~A);~%" err))))
    (write-line "// initialize-vars" stream)
    (p2-emit-initialize-vars stream)
    (write-line "// toplevel defun" stream)
    (write-string (get-output-stream-string *p2-toplevel-defun-stream*) stream)
    (write-line "// initialize symbols" stream)
    (p2-emit-initialize-symbols stream)
    (write-line "// main" stream)
    (write-string (get-output-stream-string *p2-emit-stream*) stream)
    (write-line "CL_COMMON_LISP_FINISH_OUTPUT();" stream)))
