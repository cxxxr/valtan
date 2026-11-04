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

#-valtan
(defparameter *builtin-function-table*
  (let ((table (make-hash-table)))
    (setf (gethash (read-from-string "CL:SYMBOLP") table) (list "lisp.CL_symbolp" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::MAKE-SYMBOL") table)
          (list "lisp.CL_makeSymbol" (list 1)))
    (setf (gethash (read-from-string "CL:SYMBOL-PLIST") table)
          (list "lisp.CL_symbolPlist" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::PUT-SYMBOL-PLIST") table)
          (list "lisp.CL_setSymbolPlist" (list 2)))
    (setf (gethash (read-from-string "CL:BOUNDP") table) (list "lisp.CL_boundp" (list 1)))
    (setf (gethash (read-from-string "CL:FBOUNDP") table) (list "lisp.CL_fboundp" (list 1)))
    (setf (gethash (read-from-string "CL:SYMBOL-VALUE") table)
          (list "lisp.CL_symbolValue" (list 1)))
    (setf (gethash (read-from-string "CL:SYMBOL-FUNCTION") table)
          (list "lisp.CL_symbolFunction" (list 1)))
    (setf (gethash (read-from-string "SYSTEM:%SET") table) (list "lisp.CL_set" (list 2)))
    (setf (gethash (read-from-string "CL:MAKUNBOUND") table) (list "lisp.CL_makunbound" (list 1)))
    (setf (gethash (read-from-string "CL:FMAKUNBOUND") table) (list "lisp.CL_fmakunbound" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::SYMBOL-NAME") table)
          (list "lisp.CL_symbolName" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::SYMBOL-PACKAGE-NAME") table)
          (list "lisp.CL_symbolPackage" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::FSET") table)
          (list "lisp.CL_setSymbolFunction" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::MAP-PACKAGE-SYMBOLS") table)
          (list "lisp.CL_mapPackageSymbols" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::PUT") table) (list "lisp.CL_put" (list 3)))
    (setf (gethash (read-from-string "CL:PACKAGEP") table) (list "lisp.CL_packagep" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::PACKAGE-NAME") table)
          (list "lisp.CL_packageName" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::PACKAGE-NICKNAMES") table)
          (list "lisp.CL_packageNicknames" (list 1)))
    (setf (gethash (read-from-string "CL:LIST-ALL-PACKAGES") table)
          (list "lisp.CL_listAllPackages" (list 0)))
    (setf (gethash (read-from-string "SYSTEM::INTERN") table) (list "lisp.CL_intern" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::FIND-SYMBOL") table)
          (list "lisp.CL_findSymbol" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::MAKE-PACKAGE") table)
          (list "lisp.CL_makePackage" (list 3)))
    (setf (gethash (read-from-string "CL:NUMBERP") table) (list "lisp.CL_numberp" (list 1)))
    (setf (gethash (read-from-string "CL:COMPLEXP") table) (list "lisp.CL_complexp" (list 1)))
    (setf (gethash (read-from-string "CL:COMPLEX") table) (list "lisp.CL_complex" (list 1 2)))
    (setf (gethash (read-from-string "CL:REALPART") table) (list "lisp.CL_realpart" (list 1)))
    (setf (gethash (read-from-string "CL:IMAGPART") table) (list "lisp.CL_imagpart" (list 1)))
    (setf (gethash (read-from-string "CL:INTEGERP") table) (list "lisp.CL_integerp" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::%ADD") table) (list "lisp.CL_add" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%SUB") table) (list "lisp.CL_sub" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%NEGATE") table) (list "lisp.CL_negate" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::%MUL") table) (list "lisp.CL_mul" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%REM") table) (list "lisp.CL_rem" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%=") table) (list "lisp.CL_numberEqual" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%/=") table) (list "lisp.CL_numberNotEqual" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%>") table) (list "lisp.CL_greaterThan" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%<") table) (list "lisp.CL_lessThan" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%>=") table) (list "lisp.CL_greaterEqual" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%<=") table) (list "lisp.CL_lessEqual" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%FLOOR") table) (list "lisp.CL_floor" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::APPLY") table) (list "lisp.CL_apply" (list 2 nil)))
    (setf (gethash (read-from-string "CL:FUNCTIONP") table) (list "lisp.CL_functionp" (list 1)))
    (setf (gethash (read-from-string "CL:CONSP") table) (list "lisp.CL_consp" (list 1)))
    (setf (gethash (read-from-string "CL:CONS") table) (list "lisp.CL_cons" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%CAR") table) (list "lisp.CL_car" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::%CDR") table) (list "lisp.CL_cdr" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::%RPLACA") table) (list "lisp.CL_rplaca" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%RPLACD") table) (list "lisp.CL_rplacd" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::JS-ARRAY-TO-LIST") table)
          (list "lisp.CL_jsArrayToList" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::LIST-TO-JS-ARRAY") table)
          (list "lisp.CL_listToJsArray" (list 1)))
    (setf (gethash (read-from-string "CL:VALUES") table) (list "lisp.CL_values" (list 0 nil)))
    (setf (gethash (read-from-string "*:MULTIPLE-VALUE-CALL") table)
          'p2-multiple-value-call)
    (setf (gethash (read-from-string "SYSTEM::MAKE-STRUCTURE") table)
          (list "lisp.CL_makeStructure" (list 1 nil)))
    (setf (gethash (read-from-string "SYSTEM::%COPY-STRUCTURE") table)
          (list "lisp.CL_copyStructure" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::STRUCTURE-P") table)
          (list "lisp.CL_structurep" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::%STRUCTURE-NAME") table)
          (list "lisp.CL_structureName" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::%STRUCTURE-SLOT-COUNT") table)
          (list "lisp.CL_structureSlotCount" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::%STRUCTURE-REF") table)
          (list "lisp.CL_structureRef" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::%STRUCTURE-SET") table)
          (list "lisp.CL_structureSet" (list 3)))
    (setf (gethash (read-from-string "CL:EQ") table) (list "lisp.CL_eq" (list 2)))
    (setf (gethash (read-from-string "SYSTEM::ERROR") table) (list "lisp.CL_error" (list nil)))
    (setf (gethash (read-from-string "CL:CHARACTERP") table) (list "lisp.CL_characterp" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::%CODE-CHAR") table)
          (list "lisp.CL_codeChar" (list 1)))
    (setf (gethash (read-from-string "SYSTEM::%CHAR-CODE") table)
          (list "lisp.CL_charCode" (list 1)))
    (setf (gethash (read-from-string "FFI::INSTANCEOF") table)
          (list "lisp.CL_instanceof" (list nil)))

    (setf (gethash 'funcall table) 'p2-funcall)
    (setf (gethash (read-from-string "SYSTEM::%LOGAND") table) 'p2-logand)

    ;; REVIEW
    (setf (gethash (read-from-string "FFI:CL->JS") table)
          'p2-cl->js)

    table))

#+valtan
(defparameter *builtin-function-table*
  (let ((table (make-hash-table)))
    ;; Register multiple-value-call for runtime evaluation
    (setf (gethash (read-from-string "*:MULTIPLE-VALUE-CALL") table)
          'p2-multiple-value-call)
    table))

(defun p2-cl->js (hir)
  (let ((args (hir-arg2 hir)))
    (cond ((and (length=1 args)
                (eq 'const (hir-op (first args)))
                (stringp (hir-arg1 (first args))))
           (format nil "lisp.codeArrayToString(~A)" (p2-encode-string (hir-arg1 (first args)))))
          (t
           (p2-call-default hir)))))

(defvar *p2-emit-stream* *standard-output*)
(defvar *p2-literal-symbols* (make-hash-table))
(defvar *p2-runtime-literals* nil)
(defvar *p2-runtime-literal-values* nil)
(defvar *p2-toplevel-defun-stream*)
(defvar *p2-defun-names*)
(defvar *p2-temporary-variables*)
(defvar *p2-in-simple-loop* nil)

(def-interface make-emitter-stream (base-stream)
  (declare (ignore base-stream))
  (make-string-output-stream))

(def-interface join-emitter-stream (base-stream forked-stream)
  (write-string (get-output-stream-string forked-stream) base-stream))

(def-interface set-source-map (hir)
  (declare (ignore hir)))

(defmacro embed-source-map (hir)
  #+valtan
  nil
  #-valtan
  `(set-source-map ,hir))

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
  (or (gethash symbol *p2-literal-symbols*)
      (setf (gethash symbol *p2-literal-symbols*)
            (genvar "G"))))

(defun p2-symbol-to-call-value (symbol)
  (let ((value (p2-symbol-to-js-value symbol)))
    (format nil "~A.func" value)))

(defun p2-symbol-to-js-function-name (symbol)
  (p2-local-function
   symbol
   (if (symbol-package symbol)
       (format nil
               "CL_~A_"
               (p2-escape-string
                (package-name
                 (symbol-package symbol))))
       "CL_")))

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

(defun make-circle-table (x)
  (let ((visited (make-hash-table))
        (circle (make-hash-table)))
    (labels ((f (x)
               (if (gethash x visited)
                   (progn
                     (setf (gethash x circle) t)
                     x)
                   (progn
                     (cond ((consp x)
                            (setf (gethash x visited) t)
                            (f (car x))
                            (f (cdr x)))
                           (t
                            x))))))
      (f x)
      circle)))

(defun p2-literal-1 (x circle seen)
  (cond ((assoc x seen)
         (cdr (assoc x seen)))
        ((null x)
         "lisp.S_nil")
        ((symbolp x)
         (p2-symbol-to-js-value x))
        ((stringp x)
         (format nil "~A(lisp.codeArrayToString(~A))"
                 (p2-symbol-to-call-value '*:raw-string-to-array)
                 (p2-encode-string x)))
        ((packagep x)
         (format nil "~A(~A)" (p2-symbol-to-call-value 'find-package)
                 (p2-literal-1 (package-name x) circle seen)))
        ((pathnamep x)
         (format nil "~A(~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A)"
                 (p2-symbol-to-call-value 'make-pathname)
                 (p2-literal-1 :host circle seen) (p2-literal-1 (pathname-host x) circle seen)
                 (p2-literal-1 :device circle seen) (p2-literal-1 (pathname-device x) circle seen)
                 (p2-literal-1 :directory circle seen) (p2-literal-1 (pathname-directory x) circle seen)
                 (p2-literal-1 :name circle seen) (p2-literal-1 (pathname-name x) circle seen)
                 (p2-literal-1 :type circle seen) (p2-literal-1 (pathname-type x) circle seen)
                 (p2-literal-1 :version circle seen) (p2-literal-1 (pathname-version x) circle seen)))
        ((complexp x)
         (format nil "lisp.CL_complex(~A, ~A)"
                 (p2-literal-1 (realpart x) circle seen)
                 (p2-literal-1 (imagpart x) circle seen)))
        ((and (integerp x) (or (> x 9007199254740991) (< x -9007199254740991)))
         (format nil "BigInt('~A')" (princ-to-string x)))
        ((and (rationalp x) (not (integerp x)))
         (format nil "lisp.makeRatio(~A, ~A)"
                 (p2-literal-1 (numerator x) circle seen)
                 (p2-literal-1 (denominator x) circle seen)))
        ((numberp x)
         ;; Wrap floats with Float constructor to preserve float type
         (if (floatp x)
             (format nil "lisp.toFloat(~A)"
                     (substitute #\e #\d
                       (substitute #\e #\D (princ-to-string x))))
             (princ-to-string x)))
        ((characterp x)
         (format nil "lisp.makeCharacter(~D)" (char-code x)))
        ((consp x)
         (cond ((gethash x circle)
                (let ((elt (assoc x seen)))
                  (if elt
                      (cdr elt)
                      (with-output-to-string (out)
                        (let* ((var (genvar "LITERAL"))
                               (seen (acons x var seen)))
                          (write-line "(function () {" out)
                          (format out "const ~A = lisp.makeCons(null,null);~%" var)
                          (format out "~A.car = ~A~%" var (p2-literal-1 (car x) circle seen))
                          (format out "~A.cdr = ~A~%" var (p2-literal-1 (cdr x) circle seen))
                          (format out "return ~A;~%" var)
                          (write-line "})()" out))))))
               (t
                (format nil "lisp.makeCons(~A, ~A)"
                        (p2-literal-1 (car x) circle seen)
                        (p2-literal-1 (cdr x) circle seen)))))
        ((arrayp x)
         ;; Allocate before filling so references back to this array are valid.
         (let* ((var (genvar "LITERAL"))
                (seen (acons x var seen)))
           (with-output-to-string (out)
             (write-line "(function () {" out)
             (format out "const ~A = ~A(~A, ~A, ~A);~%"
                     var (p2-symbol-to-call-value 'cl:make-array)
                     (p2-literal-1 (array-dimensions x) circle seen)
                     (p2-symbol-to-js-value :element-type)
                     (p2-literal-1 (array-element-type x) circle seen))
             (dotimes (i (array-total-size x))
               (format out "lisp.intern(\"(SETF ROW-MAJOR-AREF)\", \"COMMON-LISP\").func(~A, ~A, ~D);~%"
                       (p2-literal-1 (row-major-aref x i) circle seen) var i))
             (format out "return ~A;~%" var)
             (write-string "})()" out))))
        (t
         (error "unexpected literal: ~S" x))))

(defun p2-runtime-literal (object)
  (multiple-value-bind (index found) (gethash object *p2-runtime-literals*)
    (unless found
      (setq index (hash-table-count *p2-runtime-literals*))
      (setf (gethash object *p2-runtime-literals*) index)
      (push object *p2-runtime-literal-values*))
    (format nil "constants[~D]" index)))

(defun p2-literal (x)
  (if (and *p2-runtime-literals* (not (or (null x) (numberp x) (characterp x))))
      (p2-runtime-literal x)
      (p2-literal-1 x (make-circle-table x) nil)))

(defun %p2-form (form)
  (p2 form (if (hir-return-value-p form) :expr :stmt)))

(defun p2-form (form)
  (cond ((hir-multiple-values-p form)
         (p2 form :expr))
        ((hir-return-value-p form)
         (format nil "lisp.values1(~A)" (%p2-form form)))
        (t
         (%p2-form form)
         (p2-no-return))))

(defun p2-forms (forms)
  (do ((forms forms (cdr forms)))
      ((length=1 forms)
       (p2-form (car forms)))
    (p2 (car forms) :stmt)))

(defun p2-no-return ()
  (p2-form (make-hir 'const t nil nil)))

(define-p2-emit const (hir)
  (embed-source-map hir)
  (p2-literal (hir-arg1 hir)))

(define-p2-emit lref (hir)
  (embed-source-map hir)
  (let ((binding (hir-arg1 hir)))
    (ecase (binding-kind binding)
      ((:function)
       (p2-local-function (binding-id binding)))
      ((:variable)
       (p2-local-var (binding-id (hir-arg1 hir)))))))

(define-p2-emit gref (hir)
  (embed-source-map hir)
  (let ((ident (p2-symbol-to-js-value (hir-arg1 hir))))
    (format nil "lisp.symbolValue(~A)" ident)))

(define-p2-emit lset (hir)
  (let ((lhs (hir-arg1 hir))
        (rhs (hir-arg2 hir)))
    (let ((result (p2-local-var (binding-id lhs)))
          (value (p2-form rhs)))
      (embed-source-map hir)
      (cond ((hir-return-value-p hir)
             (let ((tmp-var (p2-temporary-var)))
               (format *p2-emit-stream* "~A=(~A=~A);~%" result tmp-var value)
               tmp-var))
            (t
             (format *p2-emit-stream* "~A=~A;~%" result value)
             #+(or)(p2-no-return))))))

(define-p2-emit gset (hir)
#|
NOTE:
下のgsetでhir-return-value-pがtのときに(1)の方法だと

(compile-toplevel
 '(let ((*foo* *bar*))
    (declare (special *foo* *bar*))
    (setq *foo* 0)
    (setq *bar* *foo*)))

の出力結果が

// initialize-vars
let G_1;
let G_2;
let save_STARFOOSTAR__11;
// toplevel defun
// initialize symbols
G_1 = lisp.intern('*BAR*', 'COMMON-LISP-USER');
G_2 = lisp.intern('*FOO*', 'COMMON-LISP-USER');
// main
{
    save_STARFOOSTAR__11 = G_2.value;
    G_2.value = lisp.values1(lisp.symbolValue(G_1));
    try {
        lisp.setSymbolValue(G_2, lisp.values1(0));
    } finally {
        G_2.value = save_STARFOOSTAR__11;
    }
}
return lisp.values1(lisp.setSymbolValue(G_1, lisp.values1(lisp.symbolValue(G_2))););

になり*foo*の値が元に戻った後に*bar*に*foo*をセットしてしまう

なので(2)の*p2-emit-stream*で*bar*に値をセットするコードを出力してから、返り値として*bar*の値を返すようにする必要がある
|#
  (let ((lhs (hir-arg1 hir))
        (rhs (hir-arg2 hir)))
    (let ((ident (p2-symbol-to-js-value lhs))
          (value (p2-form rhs)))
      (embed-source-map hir)
      (cond ((hir-return-value-p hir)
             ;; (format nil "lisp.setSymbolValue(~A, ~A);~%" ident value) ; (1)
             ;; (2)
             (format *p2-emit-stream* "lisp.setSymbolValue(~A, ~A);~%" ident value)
             (format nil "~A.value" ident)
             )
            (t
             (format *p2-emit-stream* "lisp.setSymbolValue(~A, ~A);~%" ident value)
             #+(or)(p2-no-return))))))

(define-p2-emit if (hir)
  (embed-source-map hir)
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

(defun p2-emit-check-arguments (hir name parsed-lambda-list)
  (let ((min (parsed-lambda-list-min parsed-lambda-list))
        (max (parsed-lambda-list-max parsed-lambda-list)))
    (cond ((null max)
           (format *p2-emit-stream* "if(arguments.length < ~D) {~%" min))
          ((= min max)
           (format *p2-emit-stream* "if(arguments.length !== ~D) {~%" min))
          (t
           (format *p2-emit-stream* "if(arguments.length < ~D || ~D < arguments.length) {~%" min max)))
    (let ((symbol-var (p2-literal name)))
      (embed-source-map hir)
      (format *p2-emit-stream* "lisp.argumentsError(~A, arguments.length);~%" symbol-var))
    (write-line "}" *p2-emit-stream*)))

(defun p2-make-save-var (var)
  (p2-local-var (binding-id var) "save_"))

(defun p2-emit-unwind-var (var finally-stream &optional active-var)
  (when (eq (binding-kind var) :special)
    (let ((js-var (p2-symbol-to-js-value (binding-name var)))
          (save-var (p2-make-save-var var)))
      (when active-var (format finally-stream "if(~A){" active-var))
      (format finally-stream "~A.value=~A;~%" js-var save-var)
      (when active-var (write-line "}" finally-stream)))))

(defun p2-emit-declvar (var finally-stream)
  (ecase (binding-kind var)
    ((:special)
     (let ((js-var (p2-symbol-to-js-value (binding-name var)))
           (save-var (p2-make-save-var var))
           (active-var (when finally-stream (p2-local-var (binding-id var) "bound_"))))
       (push save-var *p2-temporary-variables*)
       (format *p2-emit-stream* "~A=~A.value;~%" save-var js-var)
       (when active-var
         (push active-var *p2-temporary-variables*)
         (format *p2-emit-stream* "~A=true;~%" active-var)
         (p2-emit-unwind-var var finally-stream active-var))
       (format *p2-emit-stream* "~A.value=" js-var)))
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
        (let ((result (p2-temporary-var)))
          (format *p2-emit-stream* "if(arguments.length > ~D){~A=arguments[~D];}else{~%" i result i)
          (format *p2-emit-stream* "~A=~A;~%" result (p2-form value))
          (write-line "}" *p2-emit-stream*)
          (p2-emit-declvar var finally-stream)
          (format *p2-emit-stream* "~A;~%" result))
        (when supplied-binding
          (p2-emit-declvar supplied-binding finally-stream)
          (format *p2-emit-stream* "(arguments.length > ~D ? lisp.S_t : lisp.S_nil);~%" i))
        (incf i)))
    (let ((rest-var (parsed-lambda-list-rest-var parsed-lambda-list)))
      (when rest-var
        (p2-emit-declvar rest-var finally-stream)
        (format *p2-emit-stream* "lisp.jsArrayToList(arguments, ~D);~%" i)))
    (when (parsed-lambda-list-keyp parsed-lambda-list)
      (let ((keyword-vars '()))
        (dolist (opt (parsed-lambda-list-keys parsed-lambda-list))
          (let* ((var (first opt))
                 (value (second opt))
                 (supplied-binding (third opt))
                 (keyword-var (p2-symbol-to-js-value (fourth opt)))
                 (supplied-var (p2-local-var (binding-id var) "supplied_"))
                 (present-var (p2-local-var (binding-id var) "present_")))
            (push keyword-var keyword-vars)
            (format *p2-emit-stream* "let ~A, ~A=false;~%" supplied-var present-var)
            (let ((loop-var (p2-genvar)))
              (p2-emit-for (loop-var i "arguments.length" 2)
                (format *p2-emit-stream* "if(arguments[~D] === ~A){~%" loop-var keyword-var)
                (format *p2-emit-stream* "~A=arguments[~D+1];~A=true;~%" supplied-var loop-var present-var)
                (write-line "break;" *p2-emit-stream*)
                (write-line "}" *p2-emit-stream*)))
            (let ((result (p2-temporary-var)))
              (format *p2-emit-stream* "if(~A){~A=~A;}else{~%" present-var result supplied-var)
              (format *p2-emit-stream* "~A=~A;~%" result (p2-form value))
              (write-line "}" *p2-emit-stream*)
              (p2-emit-declvar var finally-stream)
              (format *p2-emit-stream* "~A;~%" result))
            (when supplied-binding
              (p2-emit-declvar supplied-binding finally-stream)
              (format *p2-emit-stream* "(~A ? lisp.S_t : lisp.S_nil);~%" present-var))))
        (format *p2-emit-stream* "if((arguments.length-~D)%2===1)" i)
        (write-line "{lisp.programError('odd number of &KEY arguments');}" *p2-emit-stream*)
        (unless (parsed-lambda-list-allow-other-keys parsed-lambda-list)
          (let ((loop-var (p2-genvar))
                (allow-var (p2-genvar))
                (allow-key (p2-symbol-to-js-value :allow-other-keys)))
            (format *p2-emit-stream* "let ~A = false;~%" allow-var)
            (p2-emit-for (loop-var i "arguments.length" 2)
              (format *p2-emit-stream*
                      "if(arguments[~A] === ~A){ ~A = arguments[~A+1] !== lisp.S_nil; break; }~%"
                      loop-var allow-key allow-var loop-var))
            (format *p2-emit-stream* "if(!~A){~%" allow-var)
            (p2-emit-for (loop-var i "arguments.length" 2)
              (format *p2-emit-stream* "if(arguments[~A] !== ~A" loop-var allow-key)
              (dolist (keyword keyword-vars)
                (format *p2-emit-stream* " && arguments[~A] !== ~A" loop-var keyword))
              (format *p2-emit-stream*
                      ") { lisp.programError('Unknown &KEY argument'); }~%"))
            (format *p2-emit-stream* "}~%")))))
    (dolist (aux (parsed-lambda-list-auxs parsed-lambda-list))
      (let ((result (p2-temporary-var)))
        (format *p2-emit-stream* "~A=~A;~%" result (p2-form (second aux)))
        (p2-emit-declvar (first aux) finally-stream)
        (format *p2-emit-stream* "~A;~%" result)))))

(define-p2-emit lambda (hir)
  (let ((name (hir-arg1 hir))
        (parsed-lambda-list (hir-arg2 hir))
        (body (hir-arg3 hir))
        lambda-result)
    (embed-source-map hir)
    (when (hir-return-value-p hir)
      (setq lambda-result
            (if name
                (p2-temporary-var (p2-local-function name))
                (p2-temporary-var)))
      (format *p2-emit-stream* "~A=" lambda-result))
    (write-line "(function(){" *p2-emit-stream*)
    (p2-emit-check-arguments hir name parsed-lambda-list)
    (let ((*p2-temporary-variables* '()))
      ;; !!!
      (let* ((base-stream *p2-emit-stream*)
             (emitter-stream (make-emitter-stream base-stream)))
        (let ((*p2-emit-stream* emitter-stream))
          (let* ((initialization-stream (make-emitter-stream emitter-stream))
                 (finally-code
                  (with-output-to-string (finally-stream)
                    (let ((*p2-emit-stream* initialization-stream))
                      (p2-emit-lambda-list parsed-lambda-list finally-stream)))))
            (p2-with-unwind-special-vars
             (progn
               (join-emitter-stream emitter-stream initialization-stream)
               (let ((result (p2-form body)))
                 (format *p2-emit-stream* "return ~A;~%" result)))
             finally-code)))
        (p2-emit-declare-temporary-variables)
        (join-emitter-stream base-stream emitter-stream)))
    (write-line "});" *p2-emit-stream*)
    (or lambda-result
        (values))))

(define-p2-emit let (hir)
  (embed-source-map hir)
  (let ((bindings (hir-arg1 hir))
        (body (hir-arg2 hir)))
    (write-line "{" *p2-emit-stream*)
    (dolist (binding bindings)
      (let ((value (p2-form (binding-init-value binding))))
        (p2-emit-declvar binding nil)
        (format *p2-emit-stream* "~A;~%" value)))
    (let (result)
      (p2-with-unwind-special-vars
       (let ((value (p2-forms body)))
         (setq result (p2-temporary-var))
         (format *p2-emit-stream* "~A=~A;~%" result value))
       (with-output-to-string (output)
         (dolist (binding (reverse bindings))
           (p2-emit-unwind-var binding output))))
      (write-line "}" *p2-emit-stream*)
      result)))

(defun p2-prepare-args (args)
  ;; Later arguments can emit assignments before the call itself. Capture each
  ;; argument now, including bare variable reads, to preserve left-to-right order.
  (mapcar (lambda (arg)
            (let ((value (%p2-form arg))
                  (temporary (p2-temporary-var)))
              (format *p2-emit-stream* "~A=~A;~%" temporary value)
              temporary))
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
    (embed-source-map hir)
    (when (hir-return-value-p hir)
      (setq result (p2-temporary-var))
      (format *p2-emit-stream* "~A=" result))
    (format *p2-emit-stream* "~A(" (p2-local-function (binding-id (hir-arg1 hir))))
    (p2-emit-args args)
    result))

(defun p2-call-default (hir)
  (let ((symbol (hir-arg1 hir))
        (args (p2-prepare-args (hir-arg2 hir)))
        (result nil))
    (embed-source-map hir)
    (when (hir-return-value-p hir)
      (setq result (p2-temporary-var))
      (format *p2-emit-stream* "~A=" result))
    (let (elt)
      (cond ((and (not *enable-profiling*)
                  *in-host-runtime*
                  (setq elt (assoc symbol *known-toplevel-functions*))
                  (let ((min (parsed-lambda-list-min (lambda-hir-parsed-lambda-list (cdr elt))))
                        (max (parsed-lambda-list-max (lambda-hir-parsed-lambda-list (cdr elt)))))
                    (if (parsed-lambda-list-max (lambda-hir-parsed-lambda-list (cdr elt)))
                        (<= min (length args) max)
                        (<= min (length args)))))
             (format *p2-emit-stream*
                     "~A("
                     (p2-symbol-to-call-value symbol)))
            (t
             (format *p2-emit-stream*
                     "lisp.callFunctionWithCallStack(~A"
                     (p2-symbol-to-js-value symbol))
             (when args (write-string "," *p2-emit-stream*)))))
    (p2-emit-args args)
    result))

(defun p2-call-builtin-using-list-spec (hir builtin)
  (flet ((gen (name)
           (let ((args (p2-prepare-args (hir-arg2 hir)))
                 (result nil))
             (embed-source-map hir)
             (when (hir-return-value-p hir)
               (setq result (p2-temporary-var))
               (format *p2-emit-stream* "~A=" result))
             (format *p2-emit-stream* "~A(" name)
             (p2-emit-args args)
             result)))
    (let ((nargs (length (hir-arg2 hir))))
      (destructuring-bind (name (&optional min (max min))) builtin
        (cond ((or (null min) (= min 0) (null max))
               (gen name))
              ((eql min max)
               (if (= nargs min)
                   (gen name)
                   (p2-call-default hir)))
              ((or (null max) (= max 0))
               (if (<= min nargs)
                   (gen name)
                   (p2-call-default hir)))
              (t
               (if (<= min nargs max)
                   (gen name)
                   (p2-call-default hir))))))))

;; Share these specializations between the host compiler and runtime COMPILE.
;; Keep unsupported arities on the ordinary path, including its arity checks.
(dolist (name '(+ - * / = /= < > <= >= 1+ 1- zerop))
  (setf (gethash name *builtin-function-table*) 'p2-numeric-call))

(dolist (entry '((car "lisp.CL_checkedCar" (1))
                 (cdr "lisp.CL_checkedCdr" (1))
                 (null "lisp.CL_null" (1))
                 (not "lisp.CL_null" (1))
                 (listp "lisp.CL_listp" (1))
                 (atom "lisp.CL_atom" (1))
                 (char= "lisp.CL_charEqual" (2))))
  (setf (gethash (first entry) *builtin-function-table*) (rest entry)))

(defun p2-numeric-call (hir)
  (let* ((name (hir-arg1 hir))
         (args (hir-arg2 hir))
         (binary (cdr (assoc name '((+ . "lisp.CL_add") (- . "lisp.CL_sub")
                                   (* . "lisp.CL_mul") (/ . "lisp.CL_div")
                                   (= . "lisp.CL_numberEqual") (/= . "lisp.CL_numberNotEqual")
                                   (< . "lisp.CL_lessThan") (> . "lisp.CL_greaterThan")
                                   (<= . "lisp.CL_lessEqual") (>= . "lisp.CL_greaterEqual")))))
         (unary (cdr (assoc name '((1+ . "lisp.CL_add") (1- . "lisp.CL_sub")
                                  (zerop . "lisp.CL_numberEqual"))))))
    (cond ((and binary (= (length args) 2))
           (p2-call-builtin-using-list-spec hir (list binary '(2))))
          ((and unary (= (length args) 1))
           (let ((prepared (p2-prepare-args args))
                 (result (when (hir-return-value-p hir) (p2-temporary-var))))
             (when result (format *p2-emit-stream* "~A=" result))
             (format *p2-emit-stream* "~A(~A,~D);~%" unary (first prepared)
                     (if (eq name 'zerop) 0 1))
             result))
          (t (p2-call-default hir)))))
(define-p2-emit call (hir)
  (let* ((symbol (hir-arg1 hir))
         (builtin (gethash symbol *builtin-function-table*)))
    (cond ((null builtin)
           (p2-call-default hir))
          ((consp builtin)
           (p2-call-builtin-using-list-spec hir builtin))
          ((or (symbolp builtin)
               (functionp builtin))
           ;; (embed-source-map hir)
           (funcall builtin hir))
          (t
           (error "internal error")))))

(define-p2-emit unwind-protect (hir)
  (embed-source-map hir)
  (let ((protected-form (hir-arg1 hir))
        (cleanup-form (hir-arg2 hir))
        (saved-return-var (p2-genvar "saved"))
        (result))
    (format *p2-emit-stream* "let ~A;~%" saved-return-var)
    (p2-emit-try-finally (setq result
                               (cond ((hir-return-value-p hir)
                                      (let ((protect-form-result (p2-form protected-form)))
                                        (setq result (p2-temporary-var))
                                        (format *p2-emit-stream* "~A=~A;~%" result protect-form-result)
                                        result))
                                     (t
                                      (p2 protected-form :stmt)
                                      nil)))
                         (progn
                           ;; Save values on entry to cleanup, including values
                           ;; being transferred by RETURN-FROM or THROW. Saving
                           ;; only after normal completion leaves this undefined
                           ;; on a nonlocal exit and corrupts later VALUES calls.
                           (format *p2-emit-stream* "~A=lisp.currentValues();~%" saved-return-var)
                           (p2 cleanup-form :stmt)
                           (format *p2-emit-stream*
                                   "lisp.restoreValues(~A);~%"
                                   saved-return-var)))
    result))

(defun p2-block-result-var-name (name)
  (concatenate 'string "BLOCK_RESULT_" (p2-escape-string (binding-id name))))

(defun p2-escape-block-name (name)
  (concatenate 'string "BLOCK_" (p2-escape-string (binding-id name))))

(define-p2-emit block (hir)
  (embed-source-map hir)
  (let ((name (hir-arg1 hir))
        (body (hir-arg2 hir)))
    (cond ((eql 0 (binding-escape-count name))
           (let ((block-result (p2-block-result-var-name name)))
             (push block-result *p2-temporary-variables*)
             (format *p2-emit-stream* "~A: for(;;){" (p2-escape-block-name name))
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
  (embed-source-map hir)
  (let ((name (hir-arg1 hir))
        (form (hir-arg2 hir)))
    (cond ((eql 0 (binding-escape-count name))
           (let ((result (p2-form form)))
             (format *p2-emit-stream* "~A=~A;~%" (p2-block-result-var-name name) result))
           (format *p2-emit-stream* "break ~A;~%" (p2-escape-block-name name)))
          (t
           (let ((result (p2-form form)))
             (format *p2-emit-stream*
                     "throw new lisp.BlockValue(~A,~A);"
                     (p2-symbol-to-js-value (binding-name name))
                     result))))
    (p2-no-return)))

(define-p2-emit loop (hir)
  (embed-source-map hir)
  (let ((body (hir-arg1 hir))
        (*p2-in-simple-loop* t))
    (write-line "for(;;){" *p2-emit-stream*)
    (p2 body :stmt)
    (write-line "break;" *p2-emit-stream*)
    (write-line "}" *p2-emit-stream*)
    (p2-no-return)))

(define-p2-emit recur (hir)
  (embed-source-map hir)
  (let ((binding (hir-arg1 hir)))
    (cond (*p2-in-simple-loop*
           ;; In a simple loop, just continue
           (write-line "continue;" *p2-emit-stream*))
          (binding
           ;; In a tagbody with multiple tags, need to set state before continue
           (let* ((name (tagbody-value-id (binding-id binding)))
                  (state-var (tagbody-state-name name)))
             (format *p2-emit-stream* "~A='~A';~%" state-var (tagbody-tag-name binding))
             (format *p2-emit-stream* "continue ~A;~%" name)))
          (t
           ;; Fallback for old code without binding info
           (write-line "continue;" *p2-emit-stream*))))
  (p2-no-return))

(defun tagbody-state-name (tagbody-name)
  (format nil "~A_STATE" tagbody-name))

(defun tagbody-tag-name (tag)
  (tagbody-value-index (binding-id tag)))

(define-p2-emit tagbody (hir)
  (embed-source-map hir)
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
  (embed-source-map hir)
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
  (embed-source-map hir)
  (let ((name (hir-arg1 hir))
        (function (hir-arg2 hir)))
    (let ((var (p2-symbol-to-js-function-name name)))
      (pushnew var *p2-defun-names* :test #'equal)
      ;; !!!
      (let ((*p2-temporary-variables* '()))
        (let ((emitter-stream (make-emitter-stream *p2-emit-stream*)))
          (let ((*p2-emit-stream* emitter-stream))
            (format *p2-emit-stream* "~A=~A;~%" var (p2-form function)))
          (let ((*p2-emit-stream* *p2-toplevel-defun-stream*))
            (p2-emit-declare-temporary-variables))
          (join-emitter-stream *p2-toplevel-defun-stream* emitter-stream)))
      (let ((name-var (p2-symbol-to-js-value name)))
        (format *p2-emit-stream* "~A.lisp_name = '~A'~%" var name)
        (format *p2-emit-stream* "lisp.setSymbolFunction(~A, ~A);~%" name-var var)
        name-var))))

(define-p2-emit *:%defpackage (hir)
  (embed-source-map hir)
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
  (embed-source-map hir)
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
                (write-string (first keys) out)
                (when (rest keys)
                  (write-string "." out)))))))
    code))

(define-p2-emit ffi:ref (hir)
  (embed-source-map hir)
  (p2-emit-ref (hir-arg1 hir)))

(define-p2-emit ffi:set (hir)
  (embed-source-map hir)
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
  (embed-source-map hir)
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
  (embed-source-map hir)
  (let ((value (p2-form (hir-arg1 hir)))
        (result (when (hir-return-value-p hir) (p2-temporary-var))))
    (when (hir-return-value-p hir)
      (format *p2-emit-stream* "~A=" result))
    (format *p2-emit-stream* "(typeof ~A);~%" value)
    (if (hir-return-value-p hir)
        result
        (p2-no-return))))

(define-p2-emit ffi:new (hir)
  (embed-source-map hir)
  (let ((expr (hir-arg1 hir))
        (args (hir-arg2 hir)))
    (let ((fn (%p2-form expr))
          (args (p2-prepare-args args))
          (result nil))
      (when (hir-return-value-p hir)
        (setq result (p2-temporary-var))
        (format *p2-emit-stream* "~A=" result))
      (format *p2-emit-stream* "new ~A" fn)
      (cond (args
             (write-string "(" *p2-emit-stream*)
             (p2-emit-args args))
            (t
             (write-line ";" *p2-emit-stream*)))
      (or result (p2-no-return)))))

(define-p2-emit ffi:aget (hir)
  (embed-source-map hir)
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
      (embed-source-map hir)
      (when (hir-return-value-p hir)
        (setq result (p2-temporary-var))
        (format *p2-emit-stream* "~A=" result))
      (format *p2-emit-stream* "~A(" fn-code)
      (p2-emit-args args)
      result)))

(define-p2-emit module (hir)
  (embed-source-map hir)
  (let ((name (hir-arg1 hir))
        (forms (hir-arg2 hir))
        (export-modules (hir-arg3 hir)))
    (declare (ignore name))
    (when forms
      (p2-forms forms))
    (dolist (export-module export-modules)
      (destructuring-bind (name . as) export-module
        (if as
            (format *p2-emit-stream*
                    "module.exports.~A = ~A~%"
                    (p2-convert-var name)
                    (p2-convert-var as))
            (format *p2-emit-stream*
                    "module.exports = ~A~%"
                    (p2-convert-var name)))))))

(defun escape (form)
  (with-output-to-string (out)
    (loop :for c :across (prin1-to-string form)
          :do (princ (case c
                       (#\newline "\\n")
                       (#\' "\\'")
                       (#\\ "\\\\")
                       (t c))
                     out))))

(define-p2-emit system::form-time (hir)
  (let ((hir2 (hir-arg1 hir))
        (form (hir-arg2 hir)))
    (write-line "(function () {" *p2-emit-stream*)
    (write-line "const __start = Date.now();" *p2-emit-stream*)
    (p2-form hir2)
    (write-line "const __end = Date.now();" *p2-emit-stream*)
    (format *p2-emit-stream*
            "if (__end - __start > 1) console.log(__end - __start, '~A');~%"
            (escape form))
    (write-line "})();" *p2-emit-stream*)))


;;; builtin function emitter
(defun p2-logand (hir)
  (let ((args (p2-prepare-args (hir-arg2 hir))))
    (embed-source-map hir)
    (destructuring-bind (lhs rhs) args
      (format nil "lisp.CL_logand(~A, ~A)" lhs rhs))))

(defun p2-funcall (hir)
  (let ((args (hir-arg2 hir)))
    (cond ((<= 1 (length args))
           (embed-source-map hir)
           (destructuring-bind (fn . args) (p2-prepare-args args)
             (let (result)
               (when (hir-return-value-p hir)
                 (setq result (p2-temporary-var))
                 (format *p2-emit-stream* "~A=" result))
               (format *p2-emit-stream* "lisp.CL_funcall(~A" fn)
               (when args
                 (write-char #\, *p2-emit-stream*))
               (p2-emit-args args)
               result)))
          (t
           (p2-call-default hir)))))

(defun hir-definitely-single-value-p (hir)
  "Check if HIR definitely returns a single value (not multiple values).
   Returns T for literals, variable references, and forms known to return single values.
   Returns NIL for function calls (which may return multiple values) and unknown forms."
  (case (hir-op hir)
    ;; These definitely return single values
    ((const lref gref fref the) t)
    ;; Progn returns what its last form returns
    ((progn)
     (let ((forms (hir-arg1 hir)))
       (if forms
           (hir-definitely-single-value-p (car (last forms)))
           t)))  ; empty progn returns nil (single value)
    ;; If returns what its branches return - conservative: assume might be multiple
    ((if) nil)
    ;; Function calls may return multiple values - don't assume single
    ((call lcall) nil)
    ;; Unknown forms - conservative, assume might be multiple
    (t nil)))

(defun p2-multiple-value-call (hir)
  "Generate code for MULTIPLE-VALUE-CALL that properly collects all values from all arguments.
   Uses block scope instead of IIFE to avoid potential scoping issues."
  (let ((args (hir-arg2 hir)))
    (cond ((<= 1 (length args))
           (embed-source-map hir)
           (destructuring-bind (fn-hir . arg-hirs) args
             (let ((mv-array (p2-genvar "mvall"))
                   (fn-var (p2-genvar "mvfn"))
                   result)
               ;; Use block scope with unique variable names
               (format *p2-emit-stream* "{~%")
               ;; Evaluate function first
               (let ((fn-ref (%p2-form fn-hir)))
                 (format *p2-emit-stream* "let ~A=~A;~%" fn-var fn-ref))
               ;; Create array to collect all values
               (format *p2-emit-stream* "let ~A=[];~%" mv-array)
               ;; Process each argument
               (dolist (arg-hir arg-hirs)
                 (let ((arg-ref (%p2-form arg-hir)))
                   ;; For forms that definitely return a single value (literals, variables),
                   ;; we need to set currentValues explicitly. For function calls and other
                   ;; forms that may return multiple values, let them set currentValues naturally.
                   (if (hir-definitely-single-value-p arg-hir)
                       ;; Single value form - wrap in values1 to set currentValues
                       (format *p2-emit-stream* "lisp.values1(~A);~%" arg-ref)
                       ;; May return multiple values - just evaluate, it sets currentValues
                       (format *p2-emit-stream* "~A;~%" arg-ref))
                   ;; Collect values from this argument
                   (format *p2-emit-stream* "~A.push(...lisp.currentValues());~%" mv-array)))
               ;; Call the function and assign result
               (when (hir-return-value-p hir)
                 (setq result (p2-temporary-var))
                 (format *p2-emit-stream* "~A=~A(...~A);~%" result fn-var mv-array))
               (unless (hir-return-value-p hir)
                 (format *p2-emit-stream* "~A(...~A);~%" fn-var mv-array))
               (format *p2-emit-stream* "}~%")
               result)))
          (t
           (p2-call-default hir)))))


(defun p2-toplevel-1 (hir)
  (p2 hir (if (hir-return-value-p hir) :expr :stmt)))

(defun p2-emit-initialize-vars (stream)
  (maphash (lambda (symbol ident)
             (declare (ignore symbol))
             (format stream "let ~A;~%" ident))
           *p2-literal-symbols*)
  (dolist (name *p2-defun-names*)
    (format stream "let ~A;~%" name))
  (let ((*p2-emit-stream* stream))
    (p2-emit-declare-temporary-variables)))

(defun p2-js-escape-string (string)
  "Escape STRING for use as a JavaScript string literal (without quotes)."
  (with-output-to-string (out)
    (dotimes (i (length string))
      (let ((c (char string i)))
        (case c
          (#\\ (write-string "\\\\" out))
          (#\' (write-string "\\'" out))
          (#\" (write-string "\\\"" out))
          (#\newline (write-string "\\n" out))
          (#\return (write-string "\\r" out))
          (#\tab (write-string "\\t" out))
          (otherwise (write-char c out)))))))

(defun p2-emit-initialize-symbols (stream)
  (maphash (lambda (symbol ident)
             (if *p2-runtime-literals*
                 (format stream "~A = ~A;~%" ident (p2-runtime-literal symbol))
             (let ((name (p2-js-escape-string (symbol-name symbol))))
               (if (symbol-package symbol)
                   (format stream
                           "~A = lisp.intern('~A', '~A');~%"
                           ident
                           name
                           (package-name (symbol-package symbol)))
                   (format stream "~A = lisp.makeSymbol(\"~A\");~%" ident name)))))
           *p2-literal-symbols*))

(defun p2-finish-output (stream)
  (let ((fn (p2-symbol-to-call-value 'cl:finish-output)))
    (format stream "~A();~%" fn)))

(defun p2-toplevel (hir &optional (stream *standard-output*) (return-value-p t))
  (let ((*p2-literal-symbols* (make-hash-table))
        (*p2-defun-names* '())
        (*p2-temporary-variables* '())
        (*p2-toplevel-defun-stream* (make-emitter-stream stream))
        (*p2-emit-stream* (make-emitter-stream stream))
        (*genvar-counter* 0))
    (let ((result (p2-forms (list hir))))
      (when return-value-p
        (format *p2-emit-stream* "return ~A;" result)))
    (write-line "// initialize-vars" stream)
    (p2-emit-initialize-vars stream)
    (write-line "// toplevel defun" stream)
    (join-emitter-stream stream *p2-toplevel-defun-stream*)
    (write-line "// initialize symbols" stream)
    (p2-emit-initialize-symbols stream)
    (write-line "// main" stream)
    (join-emitter-stream stream *p2-emit-stream*)))

(defun p2-toplevel-forms (hir-forms &optional (stream *standard-output*))
  (let ((*p2-literal-symbols* (make-hash-table))
        (*p2-defun-names* '())
        (*p2-temporary-variables* '())
        (*p2-toplevel-defun-stream* (make-emitter-stream stream))
        (*p2-emit-stream* (make-emitter-stream stream))
        (*genvar-counter* 0))
    (let ((err (p2-genvar)))
      (p2-emit-try-catch
       (dolist (hir hir-forms)
         (p2-toplevel-1 hir))
       ((err)
        ;; Note: Don't call finish-output here as it may not be loaded yet
        (format *p2-emit-stream* "console.error('Valtan error:', ~A);~%" err))))
    (write-line "// initialize-vars" stream)
    (p2-emit-initialize-vars stream)
    (write-line "// toplevel defun" stream)
    (join-emitter-stream stream *p2-toplevel-defun-stream*)
    (write-line "// initialize symbols" stream)
    (p2-emit-initialize-symbols stream)
    (write-line "// main" stream)
    (join-emitter-stream stream *p2-emit-stream*)))
