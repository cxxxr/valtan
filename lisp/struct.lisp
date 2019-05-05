(in-package :common-lisp)

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (let ((name (if (consp name-and-options)
                  (first name-and-options)
                  name-and-options))
        (options (if (consp name-and-options)
                     (rest name-and-options))))
    (check-type name symbol)
    (let (conc-name-option
          constructor-option
          copier-option
          predicate-option
          included-option
          type-option
          named-option)
      (dolist (option options)
        (unless (consp option) (setq option (list option)))
        (ecase (first option)
          (:conc-name
           (setq conc-name-option (second option)))
          (:constructor
           (setq constructor-option (rest option)))
          (:copier-option
           (setq copier-option (second option)))
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
      (let ((constructor-name (if constructor-option
                                  (first constructor-option)
                                  (intern (format nil "MAKE-~A" name)))))
        `(progn
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
                           `(defun ,accessor (structure)
                              (system:structure-index structure ,i))))
                       slot-descriptions)))))))
