(in-package :common-lisp)

(defun copy-structure (x)
  (system::copy-structure x))

(defmacro defstruct! (name-and-options &rest slot-descriptions)
  (let ((structure-name (if (consp name-and-options)
                            (first name-and-options)
                            name-and-options))
        (options (if (consp name-and-options)
                     (rest name-and-options))))
    (check-type structure-name symbol)
    (let* ((conc-name (format nil "~A-" structure-name))
           constructor-option
           included-option
           type-option
           named-option
           (constructor-name
             (intern (format nil "MAKE-~A" structure-name)
                     (symbol-package structure-name)))
           (copier-name
             (intern (format nil "COPY-~A" structure-name)
                     (symbol-package structure-name)))
           (predicate-name
             (intern (format nil "~A-P" structure-name)
                     (symbol-package structure-name)))
           print-function)
      (dolist (option options)
        (unless (consp option) (setq option (list option)))
        (ecase (first option)
          (:conc-name
           (setq conc-name (second option)))
          (:constructor
           (setq constructor-name (second option)
                 constructor-option (rest option)))
          (:copier
           (unless (null (rest option))
             (setq copier-name (second option))))
          (:predicate
           (unless (null (rest option))
             (setq predicate-name (second option))))
          (:include
           (setq included-option (cons (second option) (cddr option))))
          ((:print-object :print-function)
           (setq print-function (second option)))
          (:type
           (setq type-option (second option)))
          (:named
           (setq named-option t))))
      `(progn
         ,@(unless (null copier-name)
             `((defun ,copier-name (x)
                 (system::copy-structure x))))
         ,@(unless (null predicate-name)
             `((defun ,predicate-name (x)
                 (typep x ',structure-name))))
         (defun ,constructor-name ,(if (and constructor-option
                                            (rest constructor-option))
                                       (second constructor-option)
                                       `(&key ,@(mapcar (lambda (slot-desc)
                                                          (if (consp slot-desc)
                                                              (list (first slot-desc)
                                                                    (second slot-desc))
                                                              slot-desc))
                                                        slot-descriptions)))
           (system:make-structure ',structure-name
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
                              (accessor (intern (format nil "~A~A" conc-name slot-name))))
                         (incf i)
                         (destructuring-bind (&key type read-only)
                             (if (consp slot-desc)
                                 (cddr slot-desc)
                                 nil)
                           (declare (ignore type))
                           `(progn
                              (defun ,accessor (structure)
                                (system:structure-ref structure ,i))
                              ,@(unless read-only
                                  `((defun (setf ,accessor) (value structure)
                                      (system:structure-set structure ,i value))))))))
                     slot-descriptions))
         (setf (get ',structure-name 'structure-printer)
               ,(if print-function
                    (let ((structure (gensym))
                          (stream (gensym)))
                      `(lambda (,structure ,stream)
                         (,print-function ,structure ,stream 0)))
                    `(lambda (structure stream)
                       (write-string "#S(" stream)
                       (write-string ,(string structure-name) stream)
                       (write-string " " stream)
                       ,@(let ((i -1))
                           (mapcar (lambda (slot-desc)
                                     `(progn
                                        ,(unless (= i -1) '(write-string " " stream))
                                        (prin1 ,(intern (string (if (consp slot-desc)
                                                                    (first slot-desc)
                                                                    slot-desc))
                                                        :keyword)
                                               stream)
                                        (write-string " " stream)
                                        (prin1 (system::structure-ref structure ,(incf i)) stream)))
                                   slot-descriptions))
                       (write-string ")" stream))))
         ',structure-name))))

(defun structure-printer (structure)
  (get (system::structure-name structure) 'structure-printer))
