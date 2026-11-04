#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defun copy-structure (x)
  (unless (*:structure-p x)
    (type-error x 'structure-object))
  (*:%copy-structure x))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-structure-name-and-options (name-and-options)
    (cl:multiple-value-bind (structure-name options)
        (if (cl:consp name-and-options)
            (cl:values (cl:first name-and-options) (cl:rest name-and-options))
            (cl:values name-and-options nil))
      (let* ((conc-name (cl:format nil #"~A-" (cl:symbol-name structure-name)))
             constructor-option
             included-option
             type-option
             named-option
             (constructor-name
              (cl:intern (cl:format nil #"MAKE-~A" (cl:symbol-name structure-name))))
             (copier-name
              (cl:intern (cl:format nil #"COPY-~A" (cl:symbol-name structure-name))))
             (predicate-name
              (cl:intern (cl:format nil #"~A-P" (cl:symbol-name structure-name))))
             print-function)
        (cl:dolist (option options)
          (cl:unless (cl:consp option) (setq option (cl:list option)))
          (cl:ecase (cl:first option)
            (:conc-name (setq conc-name (cl:second option)))
            (:constructor
             (cl:when (cl:rest option)
               (setq constructor-name (cl:second option)
                     constructor-option (cl:rest option))))
            (:copier (cl:unless (cl:null (cl:rest option)) (setq copier-name (cl:second option))))
            (:predicate
             (cl:unless (cl:null (cl:rest option)) (setq predicate-name (cl:second option))))
            (:include (setq included-option (cons (cl:second option) (cl:cddr option))))
            ((:print-object :print-function) (setq print-function (cl:second option)))
            (:type (setq type-option (cl:second option)))
            (:named (setq named-option t))))
        (cl:list :structure-name structure-name :conc-name conc-name :constructor-option
                 constructor-option :included-option included-option :type-option type-option
                 :named-option named-option :constructor-name constructor-name :copier-name
                 copier-name :predicate-name predicate-name :print-function print-function)))))

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (cl:destructuring-bind
        (&key structure-name conc-name constructor-option included-option type-option named-option
              constructor-name copier-name predicate-name print-function)
      (parse-structure-name-and-options name-and-options)
    (declare (ignore included-option named-option))
    (cl:when (cl:stringp (cl:first slot-descriptions)) (cl:pop slot-descriptions))
    ;; CHECK-TYPE is defined later in the bootstrap sequence.
    (cl:unless (cl:symbolp structure-name)
      (cl:error 'cl:type-error :datum structure-name :expected-type 'cl:symbol))
    `(progn
       ,@(cl:unless type-option
           `((cl:setf (cl:get ',structure-name 'structure-defined) t
                      (cl:get ',structure-name 'structure-slots)
                      ',(cl:mapcar (lambda (slot) (if (cl:consp slot) (cl:car slot) slot))
                                   slot-descriptions))))
       ,@(cl:unless (cl:null copier-name)
           `((defun ,copier-name (x)
               (cl:unless (cl:and (*:structure-p x)
                                 (cl:eq (*:%structure-name x) ',structure-name))
                 (type-error x ',structure-name))
               (cl:copy-structure x))))
       ,@(cl:unless (cl:null predicate-name)
           `((defun ,predicate-name (x) (typep x ',structure-name))))
       ,@(cl:when constructor-name
         `((defun ,constructor-name
           ,(if (cl:and constructor-option (cl:rest constructor-option))
                (cl:second constructor-option)
                `(&key
                  ,@(cl:mapcar
                     (lambda (slot-desc)
                       (if (cl:consp slot-desc)
                           (cl:list (cl:first slot-desc) (cl:second slot-desc))
                           slot-desc))
                     slot-descriptions)))
         (*:make-structure ',structure-name
                           ,@(cl:mapcar
                              (lambda (slot-desc)
                                (let ((slot-name
                                        (if (cl:consp slot-desc)
                                            (cl:first slot-desc)
                                            slot-desc)))
                                  slot-name))
                              slot-descriptions)))))
       ,@(let ((i -1))
           (cl:mapcar
            (lambda (slot-desc)
              (let* ((slot-name
                       (if (cl:consp slot-desc)
                           (cl:first slot-desc)
                           slot-desc))
                     (accessor (if conc-name
                                   (cl:intern (cl:format nil #"~A~A" (cl:string conc-name) (cl:symbol-name slot-name)))
                                   slot-name)))
                (cl:incf i)
                (cl:destructuring-bind
                      (&key cl:type read-only)
                    (if (cl:consp slot-desc)
                        (cl:cddr slot-desc)
                        nil)
                  (declare (ignore cl:type))
                  `(progn
                     (defun ,accessor (cl:structure)
                       (cl:unless (cl:and (*:structure-p cl:structure)
                                         (cl:eq (*:%structure-name cl:structure) ',structure-name))
                         (type-error cl:structure ',structure-name))
                       (*:%structure-ref cl:structure ,i))
                     ,@(cl:unless read-only
                         `((defun (cl:setf ,accessor) (value cl:structure)
                             (cl:unless (cl:and (*:structure-p cl:structure)
                                               (cl:eq (*:%structure-name cl:structure) ',structure-name))
                               (type-error cl:structure ',structure-name))
                             (*:%structure-set cl:structure ,i value))))))))
            slot-descriptions))
       (cl:setf (cl:get ',structure-name 'structure-printer)
                ,(if print-function
                     (let ((cl:structure (cl:gensym)) (cl:stream (cl:gensym)))
                       `(lambda (,cl:structure ,cl:stream)
                          (,print-function ,cl:structure ,cl:stream 0)))
                     `(lambda (cl:structure cl:stream)
                        (cl:write-string #"#S(" cl:stream)
                        (cl:write-string ,(cl:string structure-name) cl:stream)
                        (cl:write-string #" " cl:stream)
                        ,@(let ((i -1))
                            (cl:mapcar
                             (lambda (slot-desc)
                               `(progn
                                  ,(cl:unless (cl:= i -1) '(cl:write-string #" " cl:stream))
                                  (cl:prin1
                                   ,(cl:intern
                                     (cl:string
                                      (if (cl:consp slot-desc)
                                          (cl:first slot-desc)
                                          slot-desc))
                                     :keyword)
                                   cl:stream)
                                  (cl:write-string #" " cl:stream)
                                  (cl:prin1 (*:%structure-ref cl:structure ,(cl:incf i))
                                            cl:stream)))
                             slot-descriptions))
                        (cl:write-string #")" cl:stream))))
       ',structure-name)))

(defun structure-printer (structure)
  (get (*:%structure-name structure) 'structure-printer))
