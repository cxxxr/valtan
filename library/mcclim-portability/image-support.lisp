(defpackage #:valtan-image-support
  (:use #:cl)
  (:export #:arglist #:completions #:class-keywords))
(in-package #:valtan-image-support)

;; Editor descriptions of standard body forms. These describe indentation and
;; destructuring, rather than the implementation's compiler expander arguments.
(defparameter *body-form-arglists*
  '((block name &body body) (catch tag &body body)
    (eval-when situations &body body) (flet definitions &body body)
    (labels definitions &body body) (let bindings &body body)
    (let* bindings &body body) (locally &body body) (progn &body forms)
    (progv symbols values &body body) (symbol-macrolet bindings &body body)
    (tagbody &body statements) (unwind-protect protected-form &body cleanup-forms)
    (multiple-value-prog1 first-form &body forms)
    (macrolet definitions &body body) (lambda lambda-list &body body)
    (defun name lambda-list &body body) (defmacro name lambda-list &body body)
    (prog1 first-form &body forms) (prog2 first-form second-form &body forms)
    (prog bindings &body body) (prog* bindings &body body)
    (multiple-value-bind variables values-form &body body)
    (when test &body forms) (unless test &body forms)))

(defparameter *special-form-arglists*
  '((if test then &optional else) (function name) (go tag)
    (load-time-value form &optional read-only-p)
    (multiple-value-call function &rest forms) (quote object)
    (return-from name &optional result) (setq &rest pairs)
    (the type value) (throw tag result)))

(defun arglist (name)
  (let ((description (or (assoc name *body-form-arglists*)
                         (assoc name *special-form-arglists*))))
    (if description
        (values (copy-tree (cdr description)) t)
        (system::operator-lambda-list name))))

(defun decode-token (text)
  ;; Retain an escape mask so an unfinished |Mixed Case| prefix can be matched
  ;; without invoking the reader, interning symbols or evaluating reader forms.
  (let ((bar nil) (escaped nil) (mask nil))
    (values
     (with-output-to-string (output)
       (loop for char across text do
         (cond (escaped (write-char char output) (push t mask) (setq escaped nil))
               ((char= char #\\) (setq escaped t))
               ((char= char #\|) (setq bar (not bar)))
               (t (write-char char output) (push bar mask)))))
     (nreverse mask))))

(defun qualifier-position (text)
  (let ((bar nil) (escaped nil))
    (loop for char across text for index from 0 do
      (cond (escaped (setq escaped nil))
            ((char= char #\\) (setq escaped t))
            ((char= char #\|) (setq bar (not bar)))
            ((and (not bar) (char= char #\:)) (return index))))))

(defun compound-prefix-p (query mask candidate &optional (delimiters '(#\-)))
  (let ((position 0))
    (loop for char across query for escaped in mask
          always (and (< position (length candidate))
                      (if (and (not escaped) (member char delimiters))
                          (setq position (position char candidate :start position))
                          (funcall (if escaped #'char= #'char-equal)
                                   char (char candidate position))))
          do (incf position))))

(defun printable-name (name text)
  (let ((*print-gensym* nil) (*print-escape* t)
        (*print-case* (if (some #'upper-case-p text) :upcase :downcase)))
    (write-to-string (make-symbol name))))

(defun common-prefix (strings fallback)
  (if (null strings) fallback
      (let* ((first (first strings)) (length (length first)))
        (dolist (string (rest strings))
          (setq length (min length (length string)))
          (loop for index below length
                unless (char= (char first index) (char string index))
                  do (setq length index) (return)))
        (subseq first 0 length))))

(defun completions (text default-package)
  "Return (sorted-completions common-prefix) for a symbol designator."
  (let* ((*package* (or (find-package default-package)
                        (error 'package-error :package default-package)))
         (colon (qualifier-position text))
         (internal (and colon (< (1+ colon) (length text))
                        (char= (char text (1+ colon)) #\:)))
         (start (if colon (+ colon (if internal 2 1)) 0))
         (prefix (subseq text 0 start))
         (token (subseq text start))
         (package
           (if colon
               (if (zerop colon) (find-package :keyword)
                   (multiple-value-bind (name mask) (decode-token (subseq text 0 colon))
                     (find-package
                      (coerce (loop for char across name for escaped in mask
                                    collect (if escaped char (char-upcase char))) 'string))))
               *package*))
         (external-only (and (not internal) (or colon (eq package (find-package :cl)))))
         (results nil))
    (multiple-value-bind (query mask) (decode-token token)
      (when package
        (do-symbols (symbol package)
          ;; CL contains Valtan's private compiler helpers too. Its public
          ;; completion surface is the exported API; CL:: requests internals.
          (when (and (or (not external-only)
                         (eq :external (nth-value 1 (find-symbol (symbol-name symbol) package))))
                     (compound-prefix-p query mask (symbol-name symbol)))
            (push (concatenate 'string prefix (printable-name (symbol-name symbol) token)) results))))
      (unless colon
        (dolist (package (list-all-packages))
          (dolist (name (cons (package-name package) (package-nicknames package)))
            (when (compound-prefix-p query mask name '(#\- #\.))
              (push (concatenate 'string (printable-name name token) ":") results))))))
    (setq results (sort (remove-duplicates results :test #'string=) #'string-lessp))
    (list results (common-prefix results text))))

(defun method-keywords (method)
  (let ((state nil) (result nil))
    (dolist (item (closer-mop:method-lambda-list method) (nreverse result))
      (cond ((eq item '&key) (setq state t))
            ((member item lambda-list-keywords) (setq state nil))
            (state
             (let* ((spec (if (consp item) (first item) item))
                    (name (if (consp spec) (second spec) spec))
                    (keyword (if (consp spec) (first spec)
                                 (intern (symbol-name name) :keyword))))
               (push (list (list keyword name) (when (consp item) (second item))) result)))))))

(defun class-keywords (designator)
  "Describe slots, class defaults and applicable initialization method keys.
Default forms are returned as forms, without running their initfunctions."
  (let* ((name (if (and (consp designator) (eq (first designator) 'quote)
                        (= (length designator) 2))
                   (second designator) designator))
         (class (if (symbolp name) (find-class name nil)
                    (when (typep name 'class) name))))
    (unless (typep class 'standard-class) (return-from class-keywords nil))
    (unless (closer-mop:class-finalized-p class) (closer-mop:finalize-inheritance class))
    (let ((defaults (closer-mop:class-default-initargs class)) (seen nil) (result nil))
      (flet ((add (key variable form)
               (unless (member key seen)
                 (push key seen)
                 (let ((default (assoc key defaults)))
                   (push (list (list key variable) (if default (second default) form)) result)))))
        (dolist (slot (closer-mop:class-slots class))
          (dolist (key (closer-mop:slot-definition-initargs slot))
            (add key (closer-mop:slot-definition-name slot) (closer-mop:slot-definition-initform slot))))
        (dolist (default defaults) (add (first default) (first default) (second default)))
        (let ((prototype (closer-mop:class-prototype class)))
          (dolist (call (list (list #'make-instance class) (list #'allocate-instance class)
                             (list #'initialize-instance prototype) (list #'shared-initialize prototype t)))
            (dolist (method (compute-applicable-methods (first call) (rest call)))
              (dolist (item (method-keywords method))
                (add (first (first item)) (second (first item)) (second item)))))))
      (nreverse result))))
