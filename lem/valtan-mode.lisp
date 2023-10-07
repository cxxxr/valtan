(defpackage :lem-valtan.valtan-mode
  (:use :cl :lem)
  (:import-from :lem-valtan/remote-eval
                :js-eval))
(in-package :lem-valtan.valtan-mode)

(define-major-mode valtan-mode lem-lisp-mode:lisp-mode
    (:name "valtan"
     :keymap *valtan-mode-keymap*)
  ())

(define-key *valtan-mode-keymap* "C-x C-e" 'valtan-eval-last-expression)
(define-key *valtan-mode-keymap* "C-M-x" 'valtan-eval-defun)

(defun current-valtan-package ()
  (lem-lisp-mode/internal::update-buffer-package)
  (or (ignore-errors
        (find-package
         (read-from-string
          (buffer-value (current-buffer) "package"))))
      (find-package :valtan-user)))

(defun eval-string (string)
  (let ((*package* (current-valtan-package)))
    (js-eval (valtan-host.reader::read-from-string-in-valtan string) :use-return-value nil)))

(define-command valtan-eval-last-expression () ()
  (with-point ((start (current-point))
               (end (current-point)))
    (form-offset start -1)
    (lem-lisp-mode/internal::highlight-evaluation-region start end)
    (eval-string (points-to-string start end))))

(define-command valtan-eval-defun () ()
  (with-point ((point (current-point)))
    (lem-lisp-syntax:top-of-defun point)
    (with-point ((start point)
                 (end point))
      (scan-lists end 1 0)
      (lem-lisp-mode/internal::highlight-evaluation-region start end)
      (eval-string (points-to-string start end)))))
