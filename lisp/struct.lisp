(in-package :common-lisp)

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (let ((name (if (consp name-and-options)
                  (first name-and-options)
                  name-and-options))
        (options (if (consp name-and-options)
                     (rest name-and-options))))
    (check-type name symbol)
    (let* (conc-name-option
           constructor-option
           predicate-option
           included-option
           type-option
           named-option
           (constructor-name
             (intern (format nil "MAKE-~A" name)
                     (symbol-package name)))
           (copier-function-name
             (intern (format nil "COPY-~A" name)
                     (symbol-package name))))
      (dolist (option options)
        (unless (consp option) (setq option (list option)))
        (ecase (first option)
          (:conc-name
           (setq conc-name-option (second option)))
          (:constructor
           (setq constructor-name (second option)
                 constructor-option (rest option)))
          (:copier
           (unless (null (rest option))
             (setq copier-function-name (second option))))
          (:predicate
           (setq predicate-option (second option)))
          (:include
           (setq included-option (cons (second option) (cddr option))))
          (:print-object)
          (:print-function)
          (:type
           (setq type-option (second option)))
          (:named
           (setq named-option t))))
      `(progn
         ,@(unless (null copier-function-name)
             `((defun ,copier-function-name (x)
                 (copy-structure x))))
         (defun ,constructor-name ,(if (and constructor-option
                                            (rest constructor-option))
                                       (second constructor-option)
                                       `(&key ,@(mapcar (lambda (slot-desc)
                                                          (if (consp slot-desc)
                                                              (list (first slot-desc)
                                                                    (second slot-desc))
                                                              slot-desc))
                                                        slot-descriptions)))
           (system:make-structure ',name
                                  ,@(mapcar (lambda (slot-desc)
                                              (let ((slot-name
                                                      (if (consp slot-desc)
                                                          (first slot-desc)
                                                          slot-desc)))
                                                slot-name))
                                            slot-descriptions)))
         ,@(let ((i -1))
             (mapcar (lambda (slot-desc)
                       (let* ((slot-name (if (consp slot-desc)
                                             (first slot-desc)
                                             slot-desc))
                              (accessor (intern (format nil "~A-~A" name slot-name))))
                         (incf i)
                         `(progn
                            (defun ,accessor (structure)
                              (system:structure-ref structure ,i))
                            (defun (setf ,accessor) (value structure)
                              (system:structure-set structure ,i value)))))
                     slot-descriptions))))))
