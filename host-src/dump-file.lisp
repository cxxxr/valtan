(defpackage :valtan-host.dump-file
  (:use :cl))
(in-package :valtan-host.dump-file)

(defun collect-builtin-functions ()
  (with-open-file (in (asdf:system-relative-pathname :valtan "./kernel/lisp.js"))
    (loop :with eof := '#:eof
          :and cl-symbols := '()
          :and system-symbols := '()
          :for line := (read-line in nil eof)
          :until (eq line eof)
          :do (ppcre:register-groups-bind (package-name function-name)
                  ("^registerFunction\\((\\w+),\\s*'([^']+)'" line)
                (when (and package-name function-name)
                  (cond ((equal "cl_package" package-name)
                         (push function-name cl-symbols))
                        ((equal "system_package" package-name)
                         (push function-name system-symbols))
                        ((equal "ffi_package" package-name))
                        (t
                         (error "unexpected name: ~A" package-name)))))
          :finally (return (list :common-lisp (nreverse cl-symbols) :system (nreverse system-symbols))))))

(defun dump-file (file)
  (with-open-file (in file)
    (loop :with eof := '#:eof
          :for form := (let ((*package* (find-package :cl)))
                         (valtan-host.reader:read-in-valtan in nil eof *readtable*))
          :until (eq form eof)
          :do (let ((*package* (find-package :valtan-core))
                    (*print-case* :downcase)
                    (*print-right-margin* 100))
                (pprint form)
                (terpri)))))

(defun shortest-package-name (package)
  (let ((shortest-name (package-name package)))
    (dolist (name (package-nicknames package))
      (when (> (length shortest-name) (length name))
        (setq shortest-name name)))
    shortest-name))

(in-package :sb-impl)

(defun output-symbol (symbol package stream)
  (let* ((readably *print-readably*)
         (readtable (if readably *standard-readtable* *readtable*))
         (out-fun (choose-symbol-out-fun *print-case* (%readtable-case readtable))))
    (flet ((output-token (name)
             (declare (type simple-string name))
             (cond ((or (and (readtable-normalization readtable)
                             (not (sb-unicode:normalized-p name :nfkc)))
                        (symbol-quotep name readtable))
                    ;; Output NAME surrounded with |'s,
;; and with any embedded |'s or \'s escaped.
                    (write-char #\| stream)
                    (dotimes (index (length name))
                      (let ((char (char name index)))
                        ;; Hmm. Should these depend on what characters
                        ;; are actually escapes in the readtable ?
                        ;; (See similar remark at DEFUN QUOTE-STRING)
                        (when (or (char= char #\\) (char= char #\|))
                          (write-char #\\ stream))
                        (write-char char stream)))
                    (write-char #\| stream))
                   (t
                    (funcall (truly-the function out-fun) name stream readtable)))))
      (let ((name (symbol-name symbol))
            (current (sane-package)))
        (cond
          ;; The ANSI spec "22.1.3.3.1 Package Prefixes for Symbols"
          ;; requires that keywords be printed with preceding colons
          ;; always, regardless of the value of *PACKAGE*.
          ((eq package *keyword-package*)
           (write-char #\: stream))
          ;; Otherwise, if the symbol's home package is the current
          ;; one, then a prefix is never necessary.
          ((eq package current))
          ;; Uninterned symbols print with a leading #:.
          ((null package)
           (when (or *print-gensym* readably)
             (write-string "#:" stream)))
          (t
           (multiple-value-bind (found accessible) (find-symbol name current)
             ;; If we can find the symbol by looking it up, it need not
             ;; be qualified. This can happen if the symbol has been
             ;; inherited from a package other than its home package.
             ;;
             ;; To preserve print-read consistency, use the local nickname if
             ;; one exists.
             (unless (and accessible (eq found symbol))
               (unless (and (string= "COMMON-LISP" (package-name package))
                            (eql (find-external-symbol name package) 0))
                 (output-token (valtan-host.dump-file::shortest-package-name package))
                 (write-string (if (eql (find-external-symbol name package) 0) "::" ":")
                               stream))))))
        (output-token name)))))
