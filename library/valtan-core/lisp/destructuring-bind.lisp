#-valtan(cl:in-package :valtan-core)
#+valtan(cl:in-package :common-lisp)

(cl:defun %db-length (list)
  (cl:do
      ((l list (cl:cdr l))
       (count 0 (cl:+ count 1)))
      ((cl:atom l) count)))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defvar *db-bindings*)
  (cl:defvar *tmp-db-vars*)
  (cl:defun parse-db-lambda-list (lambda-list arg)
    (cl:macrolet ((%push (x cl:list)
                    `(cl:setq ,cl:list
                              (cl:cons ,x
                                       ,cl:list)))
                  (%pop (cl:list)
                    `(cl:prog1 (cl:car ,cl:list)
                       (cl:setq ,cl:list
                                (cl:cdr ,cl:list))))
                  (%incf
                      (sb-debug:var cl:&optional (n 1))
                    `(cl:setq ,sb-debug:var
                              (cl:+ ,sb-debug:var
                                    ,n))))
      (cl:flet ((db-gensym (cl:&optional arg)
                  (cl:car
                   (%push
                    (cl:if arg
                           (cl:gensym arg)
                           (cl:gensym))
                    *tmp-db-vars*)))
                (make-keyword (x)
                  (cl:intern (cl:string x) :keyword))
                (invalid-lambda-list ()
                  (cl:error "Invalid lambda list: ~S"
                            lambda-list)))
        (cl:let ((path arg)
                 (min 0)
                 (max 0)
                 (state cl:nil)
                 (optionalp cl:nil)
                 (restp cl:nil)
                 (keyp cl:nil)
                 (keys cl:nil)
                 (allow-other-keys-p cl:nil)
                 (check-arg-placeholder)
                 (check-key-placeholder))
          (%push
           (cl:setq check-arg-placeholder
                    (cl:list (db-gensym) cl:nil))
           *db-bindings*)
          (cl:when
              (cl:eq 'cl:&whole
                     (cl:first lambda-list))
            (%pop lambda-list)
            (cl:unless lambda-list
              (invalid-lambda-list))
            (%push
             (cl:list (%pop lambda-list)
                      arg)
             *db-bindings*))
          (cl:do ((ll lambda-list
                      (cl:rest ll)))
              ((cl:atom ll)
               (cl:when ll
                 (cl:setq restp cl:t)
                 (%push
                  (cl:list ll path)
                  *db-bindings*)))
            (cl:let ((x (cl:first ll)))
              (cl:cond
                ((cl:eq state :aux)
                 (cl:cond
                   ((cl:symbolp x)
                    (%push (cl:list x cl:nil)
                           *db-bindings*))
                   ((cl:consp x)
                    (cl:case (cl:length x)
                      ((1 2)
                       (%push
                        (cl:list (cl:first x)
                                 (cl:second x))
                        *db-bindings*))
                      (cl:otherwise (invalid-lambda-list))))
                   (cl:t (invalid-lambda-list))))
                ((cl:eq x 'cl:&aux)
                 (cl:setq state :aux))
                ((cl:eq x 'cl:&allow-other-keys)
                 (cl:unless (cl:eq state :key)
                   (invalid-lambda-list))
                 (cl:setq allow-other-keys-p cl:t))
                ((cl:eq x 'cl:&key)
                 (cl:when keyp (invalid-lambda-list))
                 (%push
                  (cl:setq check-key-placeholder
                           (cl:list (db-gensym)
                                    path))
                  *db-bindings*)
                 (cl:setq state :key)
                 (cl:setq keyp cl:t))
                ((cl:eq state :key)
                 (%incf max 2)
                 (cl:cond
                   ((cl:symbolp x)
                    (cl:let ((key
                               (make-keyword x)))
                      (%push key keys)
                      (%push
                       (cl:list x
                                `(cl:getf ,path
                                          ,key))
                       *db-bindings*)))
                   ((cl:consp x)
                    (cl:let ((len
                               (cl:length x)))
                      (cl:cond
                        ((cl:<= 1 len 3)
                         (cl:let ((key
                                    (make-keyword
                                     (cl:first x))))
                           (%push key keys)
                           (cl:let ((supplied-value
                                      (db-gensym))
                                    (default ''#:default))
                             (%push
                              (cl:list supplied-value
                                       `(cl:getf ,path
                                                 ,key
                                                 ,default))
                              *db-bindings*)
                             (cl:when (cl:= len 3)
                               (%push
                                (cl:list (cl:third x)
                                         `(cl:eq ,supplied-value
                                                 ,default))
                                *db-bindings*))
                             (%push
                              (cl:list (cl:first x)
                                       `(cl:if (cl:eq
                                                ,supplied-value
                                                ,default)
                                               ,(cl:second
                                                 x)
                                               ,supplied-value))
                              *db-bindings*))))
                        (cl:t (invalid-lambda-list)))))
                   (cl:t (invalid-lambda-list))))
                ((cl:member x '(cl:&rest cl:&body))
                 (cl:setq restp cl:t)
                 (cl:cond
                   ((cl:and (cl:rest ll)
                            (cl:symbolp
                             (cl:second ll)))
                    (%push
                     (cl:list (cl:second ll)
                              path)
                     *db-bindings*)
                    (%pop ll))
                   ((cl:and (cl:rest ll)
                            (cl:consp (cl:second ll)))
                    (cl:let ((tmp (db-gensym "TMP")))
                      (%push
                       (cl:list tmp path)
                       *db-bindings*)
                      (parse-db-lambda-list
                       (cl:second ll) tmp)))
                   (cl:t (invalid-lambda-list))))
                ((cl:eq x 'cl:&optional)
                 (cl:when optionalp
                   (invalid-lambda-list))
                 (cl:setq state :optional)
                 (cl:setq optionalp cl:t))
                ((cl:eq state :optional)
                 (%incf max)
                 (cl:cond
                   ((cl:symbolp x)
                    (%push
                     (cl:list x
                              `(cl:first ,path))
                     *db-bindings*)
                    (cl:let ((cdr-var
                               (db-gensym "TMP")))
                      (%push
                       (cl:list cdr-var
                                `(cl:rest ,path))
                       *db-bindings*)
                      (cl:setq path cdr-var)))
                   ((cl:consp x)
                    (cl:let ((len
                               (cl:length x)))
                      (cl:cond
                        ((cl:<= 1 len 3)
                         (cl:when (cl:= len 3)
                           (%push
                            (cl:list (cl:third x)
                                     `(cl:if ,path
                                             cl:t
                                             cl:nil))
                            *db-bindings*))
                         (%push
                          (cl:list (cl:first x)
                                   `(cl:if ,path
                                           (cl:first
                                            ,path)
                                           ,(cl:second
                                             x)))
                          *db-bindings*)
                         (cl:let ((cdr-var
                                    (db-gensym "TMP")))
                           (%push
                            (cl:list cdr-var
                                     `(cl:rest ,path))
                            *db-bindings*)
                           (cl:setq path cdr-var)))
                        (cl:t (invalid-lambda-list)))))
                   (cl:t (invalid-lambda-list))))
                ((cl:listp x) (%incf min)
                              (%incf max)
                              (cl:let ((car-var (db-gensym "TMP")))
                                (%push
                                 (cl:list car-var
                                          `(cl:first ,path))
                                 *db-bindings*)
                                (parse-db-lambda-list x
                                                      car-var))
                              (cl:let ((cdr-var (db-gensym "TMP")))
                                (%push
                                 (cl:list cdr-var
                                          `(cl:rest ,path))
                                 *db-bindings*)
                                (cl:setq path cdr-var)))
                (cl:t
                 (cl:unless (cl:symbolp x)
                   (invalid-lambda-list))
                 (%incf min)
                 (%incf max)
                 (%push
                  (cl:list x
                           `(cl:first ,path))
                  *db-bindings*)
                 (cl:let ((cdr-var (db-gensym "TMP")))
                   (%push
                    (cl:list cdr-var
                             `(cl:rest ,path))
                    *db-bindings*)
                   (cl:setq path cdr-var))))))
          (cl:rplaca (cl:cdr check-arg-placeholder)
                     `(cl:unless
                          ,(cl:if (cl:or restp
                                         keyp)
                                  `(cl:<= ,min
                                          (%db-length
                                           ,arg))
                                  `(cl:<= ,min
                                          (%db-length
                                           ,arg)
                                          ,max))
                        (cl:error
                         "Invalid number of arguments for destructuring-bind: ~S ~S"
                         ',lambda-list ,arg)))
          (cl:when
              (cl:and check-key-placeholder
                      (cl:not allow-other-keys-p))
            (cl:rplaca (cl:cdr check-key-placeholder)
                       (cl:let ((plist (cl:gensym)))
                         `(cl:do ((,plist ,(cl:second
                                            check-key-placeholder)
                                          (cl:cddr
                                           ,plist)))
                              ((cl:null ,plist))
                            (cl:unless
                                (cl:or
                                 ,@(cl:mapcar
                                    (cl:lambda (key)
                                      `(cl:eq ,key
                                              (cl:first
                                               ,plist)))
                                    keys))
                              (cl:error "Unknown &key argument: ~S"
                                        (cl:first
                                         ,plist)))))))))))
  (cl:defun expand-destructuring-bind
      (lambda-list expression
       body)
    (cl:let ((*db-bindings* 'cl:nil)
             (*tmp-db-vars* 'cl:nil)
             (g-expression (cl:gensym)))
      (parse-db-lambda-list lambda-list
                            g-expression)
      `(cl:let ((,g-expression ,expression))
         (cl:let* ,(cl:nreverse *db-bindings*)
           (cl:declare (cl:ignorable . ,*tmp-db-vars*))
           ,@body)))))

(*:defmacro* destructuring-bind (lambda-list expression cl:&rest body)
  (expand-destructuring-bind lambda-list expression body))
