(defpackage :lem-valtan
  (:use :cl :lem)
  (:import-from :valtan-host.remote-eval
                :js-eval))
(in-package :lem-valtan)

(defun current-valtan-package ()
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
    (eval-string (points-to-string start end))))
