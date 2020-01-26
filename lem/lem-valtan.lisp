(defpackage :lem-valtan
  (:use :cl :lem)
  (:import-from :valtan-host.remote-eval
                :js-eval))
(in-package :lem-valtan)

(define-major-mode valtan-mode lem-lisp-mode:lisp-mode
    (:name "valtan"
     :keymap *valtan-mode-keymap*)
  ())

(define-key *valtan-mode-keymap* "C-x C-e" 'valtan-eval-last-expression)
(define-key *valtan-mode-keymap* "C-M-x" 'valtan-eval-defun)

(defun current-valtan-package ()
  (lem-lisp-mode::update-buffer-package)
  (or (ignore-errors
        (find-package
         (read-from-string
          (buffer-value (current-buffer) "package"))))
      (find-package :valtan-user)))

(defun read-in-valtan-from-string (string)
  (with-input-from-string (stream string)
    (valtan-host.reader:read-in-valtan stream)))

(defun eval-string (string)
  (let ((*package* (current-valtan-package)))
    (js-eval (read-in-valtan-from-string string))))

(define-command valtan-eval-last-expression () ()
  (with-point ((start (current-point))
               (end (current-point)))
    (form-offset start -1)
    (lem-lisp-mode::highlight-evaluation-region start end)
    (eval-string (points-to-string start end))))

(define-command valtan-eval-defun () ()
  (with-point ((point (current-point)))
    (lem-lisp-syntax:top-of-defun point)
    (with-point ((start point)
                 (end point))
      (scan-lists end 1 0)
      (lem-lisp-mode::highlight-evaluation-region start end)
      (eval-string (points-to-string start end)))))
