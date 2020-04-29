;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: must-condition.lisp,v 1.7 2004/02/20 07:23:42 yuji Exp $
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;;  * Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; signal
(eq (signal "test signal") nil)
(eq (signal 'simple-error :format-control "simple-error" :format-arguments nil)
    nil)
(eq (signal 'simple-warning
	    :format-control "simple-warning" :format-arguments nil)
    nil)
(handler-case (signal "test simple-condition")
  (simple-condition () t)
  (condition () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(handler-case (signal 'simple-warning :format-control "simple warning"
		      :format-arguments nil)
  (simple-warning () t)
  (warning () nil)
  (condition () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(handler-case (signal 'type-error :datum nil :expected-type 'vector)
  (type-error () t)
  (error () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(let ((*break-on-signals* 'arithmetic-error))
  (handler-case (signal 'type-error :datum nil :expected-type 'vector)
    (type-error () t)
    (error () nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil)))


;; error
(handler-case (error "simple-error test")
  (simple-error () t)
  (error () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(handler-case (error 'type-error :datum nil :expected-type 'vector)
  (type-error () t)
  (error () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(handler-case (error 'no-such-error!!)
  (type-error () t)
  (error () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(handler-case (error 'simple-condition :format-control "simple-condition test")
  (simple-condition () t)
  (error () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(handler-case (error 'simple-warning :format-control "simple-warning test")
  (simple-warning () t)
  (error () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))



;; cerror
(handler-case (cerror "Continue." "error test")
  (simple-error () t)
  (error () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(handler-case (cerror "Continue." 'type-error :datum nil :expected-type 'vector)
  (type-error () t)
  (error () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(handler-bind ((simple-error #'(lambda (condition)
				 (declare (ignore condition))
				 (invoke-restart 'continue))))
  (eq (cerror "Continue." "error test") nil))
(handler-bind ((type-error #'(lambda (condition)
			       (declare (ignore condition))
			       (invoke-restart 'continue))))
  (eq (cerror "Continue." 'type-error :datum nil :expected-type 'vector) nil))



;; warn
(let ((*error-output* (make-string-output-stream)))
  (and (eq (warn "I warn you!") nil)
       (get-output-stream-string *error-output*)))
(handler-bind ((warning #'(lambda (condition)
			    (declare (ignore condition))
			    (invoke-restart 'muffle-warning))))
  (eq (warn "I warn you!") nil))
(let ((*error-output* (make-string-output-stream)))
  (handler-bind ((warning #'(lambda (condition)
			      (declare (ignore condition))
			      (invoke-restart 'muffle-warning))))
    (and (eq (warn "I warn you!") nil)
	 (string= (get-output-stream-string *error-output*) ""))))
(block tag
  (handler-case (warn 'simple-error
		      :format-control "boom!" :format-arguments nil)
    (type-error () t)
    (simple-error () nil)
    (error () nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil)))
(block tag
  (handler-case (warn 'simple-condition
		      :format-control "boom!" :format-arguments nil)
    (type-error () t)
    (simple-condition () nil)
    (error () nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil)))
(block tag
  (let ((condition (make-condition 'simple-condition
				   :format-control "boom!"
				   :format-arguments nil)))
    (handler-case (warn condition)
      (type-error () t)
      (simple-condition () nil)
      (error () nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil))))
(block tag
  (let ((condition (make-condition 'simple-error
				   :format-control "boom!"
				   :format-arguments nil)))
    (handler-case (warn condition)
      (type-error () t)
      (simple-error () nil)
      (error () nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil))))
(block tag
  (let ((condition (make-condition 'simple-warning
				   :format-control "boom!"
				   :format-arguments nil)))
    (handler-case (warn condition)
      (type-error () nil)
      (simple-warning () t)
      (error () nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil))))
(block tag
  (let ((condition (make-condition 'simple-warning
				   :format-control "boom!"
				   :format-arguments nil)))
    (handler-case (warn condition :format-control "boom!" :format-arguments nil)
      (type-error () t)
      (simple-warning () nil)
      (error () nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil))))



;; handler-bind
(null (handler-bind ()))
(handler-bind () t)
(equal (multiple-value-list (handler-bind () 1 2 3 (values 4 5 6))) '(4 5 6))
(eq 'handled
    (block tag (handler-bind ((type-error #'(lambda (c)
					      (declare (ignore c))
					      (return-from tag 'handled))))
		 (error 'type-error :datum nil :expected-type 'vector))))
(eq 'handled
    (block tag (handler-bind ((error #'(lambda (c)
					 (declare (ignore c))
					 (return-from tag 'handled))))
		 (error 'type-error :datum nil :expected-type 'vector))))
(eq 'handled
    (block tag (handler-bind ((condition #'(lambda (c)
					 (declare (ignore c))
					 (return-from tag 'handled))))
		 (error 'type-error :datum nil :expected-type 'vector))))
(eq 'outer-handler
    (block tag
      (handler-bind ((type-error #'(lambda (c)
				     (declare (ignore c))
				     (return-from tag 'outer-handler))))
	(handler-bind ((type-error #'(lambda (c) (error c)))
		       (type-error #'(lambda (c)
				       (declare (ignore c))
				       (return-from tag 'inner-handler))))
	  (error 'type-error :datum nil :expected-type 'vector)))))
(eq 'outer-handler
    (block tag
      (handler-bind ((error #'(lambda (c)
				(declare (ignore c))
				(return-from tag 'outer-handler))))
	(handler-bind ((type-error #'(lambda (c) (error c)))
		       (type-error #'(lambda (c)
				       (declare (ignore c))
				       (return-from tag 'inner-handler))))
	  (error 'type-error :datum nil :expected-type 'vector)))))
(eq 'left-handler
    (block tag
      (handler-bind ((type-error #'(lambda (c)
				     (declare (ignore c))
				     (return-from tag 'left-handler)))
		     (type-error #'(lambda (c)
				     (declare (ignore c))
				     (return-from tag 'right-handler))))
	(error 'type-error :datum nil :expected-type 'vector))))
(eq 'left-handler
    (block tag
      (handler-bind ((error #'(lambda (c)
				(declare (ignore c))
				(return-from tag 'left-handler)))
		     (type-error #'(lambda (c)
				     (declare (ignore c))
				     (return-from tag 'right-handler))))
	(error 'type-error :datum nil :expected-type 'vector))))
(eq 'left-handler
    (block tag
      (handler-bind ((type-error #'(lambda (c)
				     (declare (ignore c))
				     (return-from tag 'left-handler)))
		     (error #'(lambda (c)
				(declare (ignore c))
				(return-from tag 'right-handler))))
	(error 'type-error :datum nil :expected-type 'vector))))
(let ((handler-declined nil))
  (and (eq (handler-bind ((type-error #'(lambda (c)
					  (declare (ignore c))
					  (setq handler-declined t))))
	     (signal 'type-error :datum nil :expected-type 'vector))
	   nil)
       handler-declined))
(let ((handler-declined nil))
  (and (eq (handler-bind ((type-error #'(lambda (c)
					  (declare (ignore c))
					  (push 'outer handler-declined))))
	     (handler-bind ((type-error #'(lambda (c)
					    (declare (ignore c))
					    (push 'inner handler-declined))))
	       (signal 'type-error :datum nil :expected-type 'vector)))
	   nil)
       (equal handler-declined '(outer inner))))
(let ((handler-declined nil))
  (and (eq (handler-bind
	       ((type-error #'(lambda (c)
				(declare (ignore c))
				(push 'outer-left-handler handler-declined)))
		(type-error #'(lambda (c)
				(declare (ignore c))
				(push 'outer-right-handler handler-declined))))
	     (handler-bind
		 ((type-error #'(lambda (c)
				  (declare (ignore c))
				  (push 'inner-left-handler handler-declined)))
		  (type-error #'(lambda (c)
				  (declare (ignore c))
				  (push 'inner-right-handler handler-declined))))
	       (signal 'type-error :datum nil :expected-type 'vector)))
	   nil)
       (equal handler-declined '(outer-right-handler outer-left-handler
				 inner-right-handler inner-left-handler))))
(let ((handler-declined nil))
  (and (eq (handler-bind
	       ((type-error #'(lambda (c)
				(declare (ignore c))
				(push 'outer-left-handler handler-declined)))
		(type-error #'(lambda (c)
				(declare (ignore c))
				(push 'outer-right-handler handler-declined))))
	     (handler-bind
		 ((type-error #'(lambda (c)
				  (declare (ignore c))
				  (push 'inner-left-handler handler-declined)))
		  (type-error #'(lambda (c)
				  (signal c)
				  (push 'inner-right-handler handler-declined))))
	       (signal 'type-error :datum nil :expected-type 'vector)))
	   nil)
       (equal handler-declined '(outer-right-handler
				 outer-left-handler
				 inner-right-handler

				 outer-right-handler
				 outer-left-handler

				 inner-left-handler))))
(let ((*dynamic-var* nil))
  (declare (special *dynamic-var*))
  (block tag
    (handler-bind ((type-error #'(lambda (c)
				   (declare (ignore c))
				   (return-from tag *dynamic-var*))))
      (let ((*dynamic-var* t))
	(declare (special *dynamic-var*))
	(signal 'type-error :datum nil :expected-type 'vector)))))
(let ((declined nil))
  (and (eq nil
	   (handler-bind ((simple-condition #'(lambda (c)
						(declare (ignore c))
						(push 'specific declined))))
	     (handler-bind ((condition #'(lambda (c)
					   (declare (ignore c))
					   (push 'general declined))))
	       (signal "error"))))
       (equal declined '(specific general))))
(block tag
  (handler-bind ((error #'(lambda (c) (return-from tag (typep c 'error)))))
    (error "error")))
(eq 'ok
    (block tag
      (handler-bind ((error #'(lambda (c)
				(declare (ignore c))
				(return-from tag 'ok))))
	(handler-bind ((error #'(lambda (c)
				  (declare (ignore c))
				  (error "error3"))))
	  (handler-bind ((error #'(lambda (c)
				    (declare (ignore c))
				    (error "error2"))))
	    (error "error"))))))
(eq 'ok
    (block tag
      (handler-bind
	  ((error
	    #'(lambda (c)
		(declare (ignore c))
		(handler-bind
		    ((error #'(lambda (c)
				(declare (ignore c))
				(handler-bind
				    ((error #'(lambda (c)
						(declare (ignore c))
						(return-from tag 'ok))))
				  (error "error2")))))
		  (error "error1")))))
	(error "error0"))))


;; handler-case
(handler-case t)
(handler-case nil
  (:no-error (&rest rest) (declare (ignore rest)) t))
(equal (multiple-value-list (handler-case (values 0 1 2 3 4)))
       '(0 1 2 3 4))
(equal (handler-case (values 0 1 2 3 4)
	 (:no-error (&rest rest) rest))
       '(0 1 2 3 4))
(equal (multiple-value-list (handler-case (values 0 1 2 3 4)
			      (:no-error (&rest rest) (values rest 5 6 7 8))))
       '((0 1 2 3 4) 5 6 7 8))
(eq t (handler-case t
	(type-error () 'type-error)
	(error () 'error)))
(eq 'simple-error
    (handler-case (error "error!")
      (simple-error () 'simple-error)
      (error () 'error)))
(eq 'error
    (handler-case (error "error!")
      (error () 'error)
      (simple-error () 'simple-error)))
(eq 'error
    (handler-case (error "error!")
      (error () 'error)
      (condition () 'condition)
      (simple-error () 'simple-error)))
(eq 'condition
    (handler-case (error "error!")
      (condition () 'condition)
      (error () 'error)
      (simple-error () 'simple-error)))
(eq 'simple-error
    (handler-case (signal 'simple-error
			  :format-control "error!" :format-arguments nil)
      (simple-error () 'simple-error)
      (error () 'error)))
(eq 'simple-error-left
    (handler-case (signal 'simple-error
			  :format-control "error!" :format-arguments nil)
      (simple-error () 'simple-error-left)
      (simple-error () 'simple-error-right)))
(eq 'no-one-handled
    (handler-case (progn
		    (signal 'simple-warning
			    :format-control "warning!" :format-arguments nil)
		    'no-one-handled)
      (simple-error () 'simple-error)
      (error () 'error)))
(equal (handler-case (progn
		       (signal 'simple-warning
			       :format-control "warning!" :format-arguments nil)
		       'no-one-handled)
	 (:no-error (&rest rest) (cons 'no-error rest))
	 (simple-error () 'simple-error)
	 (error () 'error))
       '(no-error no-one-handled))
(let ((where 'out))
  (eq (handler-case (let ((where 'in))
		      (declare (ignorable where))
		      (error "error!"))
	(error () where))
      'out))
(let ((where 'out))
  (declare (special where))
  (eq (handler-case (let ((where 'in))
		      (declare (special where))
		      (error "~S" where))
	(error () where))
      'out))
(typep (handler-case (error "error!")
	 (error (c) c))
       'simple-error)
(typep (handler-case (error "error!")
	 (condition (c) c))
       'simple-error)
(typep (handler-case (signal "condition")
	 (condition (c) c))
       'simple-condition)
(typep (handler-case (warn "warning")
	 (condition (c) c))
       'simple-warning)



;; restart-bind
(null (restart-bind ()))
(restart-bind () t)
(= (restart-bind () 0 1 2) 2)
(equal (multiple-value-list (restart-bind () 0 1 2 (values 3 4 5))) '(3 4 5))
(block tag
  (restart-bind ((continue #'(lambda (&rest rest)
			       (declare (ignore rest))
			       (return-from tag t))))
    (handler-case (signal "testing simple-signal")
      (simple-condition () (invoke-restart 'continue)))))
(block tag
  (handler-bind ((simple-condition #'(lambda (condition)
				       (declare (ignore condition))
				       (invoke-restart 'continue))))
    (restart-bind ((continue #'(lambda (&rest rest)
				 (declare (ignore rest))
				 (return-from tag t))))
      (signal "testing simple-condition"))))
(block tag
  (restart-bind ((continue  #'(lambda (&rest rest)
				 (declare (ignore rest))
				 (return-from tag nil))))
    (handler-bind ((simple-condition #'(lambda (condition)
					 (declare (ignore condition))
					 (invoke-restart 'continue))))
      (restart-bind ((continue #'(lambda (&rest rest)
				   (declare (ignore rest))
				   (return-from tag t))))
	(signal "testing simple-condition")))))
(block tag
  (restart-bind ((continue #'(lambda (&rest rest)
			       (declare (ignore rest))
			       (return-from tag t)))
		 (continue #'(lambda (&rest rest)
			       (declare (ignore rest))
			       (return-from tag nil))))
    (handler-case (signal "testing simple-signal")
      (simple-condition () (invoke-restart 'continue)))))
(block tag
  (restart-bind ((continue #'(lambda (&rest rest)
			       (declare (ignore rest))
			       (return-from tag t))
		   :report-function #'(lambda (stream)
					(format stream "Continue"))))
    (handler-case (signal "testing simple-signal")
      (simple-condition () (invoke-restart 'continue)))))
(block tag
  (restart-bind ((continue #'(lambda (x) (return-from tag x))
		   :report-function
		   #'(lambda (stream) (format stream "Continue"))
		   :interactive-function #'(lambda () (list t))))
    (handler-case (signal "testing simple-signal")
      (simple-condition () (invoke-restart-interactively 'continue)))))
(eq 'ok
    (block tag
      (restart-bind ((continue #'(lambda (x) (return-from tag x))))
	(handler-case (signal "testing simple-signal")
	  (simple-condition () (invoke-restart 'continue 'ok))))))
(block tag
  (restart-bind ((continue #'(lambda (x) (return-from tag x))
		   :report-function
		   #'(lambda (stream) (format stream "Continue"))
		   :interactive-function #'(lambda () (list t))
		   :test-function (constantly t)))
    (handler-case (signal "testing simple-signal")
      (simple-condition () (invoke-restart-interactively 'continue)))))
(block tag
  (restart-bind ((continue #'(lambda (x) (return-from tag x))
		   :report-function
		   #'(lambda (stream) (format stream "Continue"))
		   :interactive-function #'(lambda () (list t))
		   :test-function
		   #'(lambda (c) (or (null c) (typep c 'simple-condition)))))
    (handler-case (signal "testing simple-signal")
      (simple-condition () (invoke-restart-interactively 'continue)))))
(block tag
  (restart-bind ((tb-continue #'(lambda (x) (return-from tag x))
		   :interactive-function #'(lambda () (list t))
		   :test-function (constantly nil)
		   :report-function
		   #'(lambda (stream) (format stream "Continue"))))
    (not (find-restart 'tb-continue))))
(block tag
  (restart-bind ((tb-continue #'(lambda (x) (return-from tag x))
		   :interactive-function #'(lambda () (list t))
		   :test-function (constantly t)
		   :report-function #'(lambda (stream) (format stream "cont."))))
    (handler-case (signal "testing simple-signal")
      (simple-condition () (invoke-restart-interactively 'tb-continue)))))
(null (let ((*dynamic-var* nil))
	(declare (special *dynamic-var*))
	(block tag
	  (restart-bind ((continue #'(lambda (x)
				       (declare (ignore x))
				       (return-from tag *dynamic-var*))
			   :interactive-function #'(lambda () (list t))
			   :test-function (constantly t)
			   :report-function
			   #'(lambda (stream) (format stream "cont."))))
	    (handler-case (let ((*dynamic-var* t))
			    (declare (special *dynamic-var*))
			    (signal "testing simple-signal"))
	      (simple-condition () (invoke-restart-interactively 'continue)))))))
(let ((*dynamic-var* nil))
	(declare (special *dynamic-var*))
	(block tag
	  (restart-bind ((continue #'(lambda (x)
				       (declare (ignore x))
				       (return-from tag *dynamic-var*))
			   :interactive-function #'(lambda () (list t))
			   :test-function (constantly t)
			   :report-function
			   #'(lambda (stream) (format stream "cont."))))
	    (handler-bind ((simple-condition
			    #'(lambda (c)
				(declare (ignore c))
				(invoke-restart-interactively 'continue))))
	      (let ((*dynamic-var* t))
		(declare (special *dynamic-var*))
		(signal "testing simple-signal"))))))
(block tag
  (restart-bind ((nil #'(lambda (&rest rest)
			       (declare (ignore rest))
			       (return-from tag t))))
    (handler-case (signal "testing simple-signal")
      (simple-condition () (invoke-restart 'nil)))))



;; restart-case
(restart-case t)
(restart-case t
  (continue (&rest rest) (declare (ignore rest)) nil))
(equal (multiple-value-list (restart-case (values 0 1 2 3 4))) '(0 1 2 3 4))
(eq 'continued
    (restart-case (continue)
      (continue (&rest rest) (declare (ignore rest)) 'continued)))
(eq nil
    (restart-case (continue)
      (continue (&rest rest) (declare (ignore rest)))))
(eq 'continue-left
    (restart-case (continue)
      (continue (&rest rest) (declare (ignore rest)) 'continue-left)
      (continue (&rest rest) (declare (ignore rest)) 'continue-right)))
(null (restart-case (invoke-restart 'continue)
	(continue (&rest rest)
	  :interactive (lambda () (list 0 1 2 3))
	  rest)))
(equal (restart-case (invoke-restart-interactively 'continue)
	 (continue (&rest rest)
	   :interactive (lambda () (list 0 1 2 3))
	   rest))
       '(0 1 2 3))
(equal (restart-case (invoke-restart-interactively 'continue)
	 (continue (&rest rest)
	   :interactive (lambda () (list 0 1 2 3))
	   :report "continue"
	   rest))
       '(0 1 2 3))
(equal (restart-case (invoke-restart-interactively 'continue)
	 (continue (&rest rest)
	   :interactive (lambda () (list 0 1 2 3))
	   :report "continue"
	   :test  (lambda (c) (declare (ignore c)) t)
	   rest))
       '(0 1 2 3))
(= (restart-case
       (handler-bind ((error #'(lambda (c)
				 (declare (ignore c))
				 (invoke-restart 'my-restart 7))))
	 (error "Foo."))
     (my-restart (&optional v) v))
   7)
(eq (handler-bind ((error #'(lambda (c)
			      (declare (ignore c))
			      (invoke-restart 'my-restart 'restarted))))
      (restart-case (error "Boo.")
	(my-restart (&optional v) v)))
    'restarted)
(eq (handler-bind ((error #'(lambda (c)
			      (invoke-restart (find-restart 'my-restart c)
					      'restarted))))
      (restart-case (error "Boo.")
	(my-restart (&optional v) v)))
    'restarted)

(> (length
    (block tag
      (handler-bind ((error #'(lambda (c)
				(return-from tag (compute-restarts c)))))
	(restart-case (error "Boo.")
	  (my-restart (&optional v) v)
	  (my-restart (&optional v) v)))))
    1)
(eq 'ok
    (restart-case (invoke-restart 'nil)
      (nil (&rest rest) (declare (ignore rest)) 'ok)))






;; compute-restarts
(listp (mapcar #'restart-name (compute-restarts)))
(listp (mapcar #'restart-name
	       (compute-restarts (make-condition 'simple-error
						 :format-control "error"
						 :format-arguments nil))))
(restart-case (let ((list (compute-restarts)))
		(and (member 'my-restart list
			     :test #'string= :key #'restart-name)
		     (member 'your-restart list
			     :test #'string= :key #'restart-name)))
  (my-restart ())
  (your-restart ()))
(restart-case (let ((list (compute-restarts)))
		(member 'my-restart
			(cdr (member 'my-restart list
				     :test #'string= :key #'restart-name))
			:test #'string= :key #'restart-name))
  (my-restart ())
  (my-restart ()))


;; find-restart
(or (find-restart 'continue) t)
(restart-case (find-restart 'my-restart)
  (my-restart ()))
(restart-case (find-restart (find-restart 'my-restart))
  (my-restart ()))
(let ((condition (make-condition 'simple-error
				 :format-control "error" :format-arguments nil)))
  (block tag
    (handler-bind ((error
		    #'(lambda (c)
			(return-from tag (and (eq c condition)
					      (find-restart 'my-restart c))))))
      (restart-case (error condition)
	(my-restart ())))))


;; restart-name
(string= "MY-RESTART"
	 (block tag
	   (handler-bind
	       ((error
		 #'(lambda (c)
		     (return-from tag (restart-name
				       (find-restart 'my-restart c))))))
	     (restart-case (error "error!")
	       (my-restart ())))))
(null (block tag
	(handler-bind
	    ((error
	      #'(lambda (c)
		  (return-from tag (restart-name
				    (find-restart 'nil c))))))
	  (restart-case (error "error!")
	    (nil ())))))


;; with-condition-restarts
(null (with-condition-restarts
	  (make-condition 'simple-error
			  :format-control "error" :format-arguments nil)
	()))
(with-condition-restarts
    (make-condition 'simple-error
		    :format-control "error" :format-arguments nil)
  () t)
(equal
 (multiple-value-list
  (with-condition-restarts
      (make-condition 'simple-error
		      :format-control "error" :format-arguments nil)
    () 0 1 2 (values 3 4 5)))
 '(3 4 5))
(let ((condition (make-condition 'simple-error
				 :format-control "error" :format-arguments nil))
      (other (make-condition 'simple-error
			     :format-control "error" :format-arguments nil)))
  (block tag
    (handler-bind
	((error
	  #'(lambda (c)
	      (return-from tag (and (find-restart 'my-restart c)
				    (null (with-condition-restarts other
					    (compute-restarts)
					    (find-restart 'my-restart c))))))))
      (restart-case (progn 3 2 1 'go (error condition))
	(my-restart ())))))


;; with-simple-restart
(null (with-simple-restart (continue "continue")))
(with-simple-restart (continue "continue") t)
(equal (multiple-value-list
	(with-simple-restart (continue "continue") 0 1 (values 2 3 4)))
       '(2 3 4))
(equal (multiple-value-list
	(with-simple-restart (continue "continue")
	  (continue)))
       '(nil t))
(equal (multiple-value-list
	(with-simple-restart (continue "continue")
	  (handler-case (error "boo")
	    (error (c) (declare (ignore c)) (invoke-restart 'continue)))))
       '(nil t))


;; abort
(eq 'ok
    (restart-case (abort)
      (abort () 'ok)))
(let ((condition (make-condition 'simple-error
				 :format-control "error" :format-arguments nil)))
  (or (find-restart 'abort condition)
      (eq 'handled
	  (handler-case (abort condition)
	    (control-error () 'handled)
	    (condition () nil)))))

;; muffle-warning
(eq 'ok
    (restart-case (muffle-warning)
      (muffle-warning () 'ok)))
(let ((condition (make-condition 'simple-warning
				 :format-control "warning"
				 :format-arguments nil)))
  (or (find-restart 'muffle-warning condition)
      (eq 'handled
	  (handler-case (muffle-warning condition)
	    (control-error () 'handled)
	    (condition () nil)))))

;; continue
(eq 'ok
    (restart-case (continue)
      (continue () 'ok)))
(let ((condition (make-condition 'simple-error
				 :format-control "error"
				 :format-arguments nil)))
  (or (find-restart 'continue condition)
      (null (continue condition))))

;; store-value
(eq 'ok
    (restart-case (store-value 'ok)
      (store-value (value) value)))
(let ((condition (make-condition 'simple-error
				 :format-control "error"
				 :format-arguments nil)))
  (or (find-restart 'store-value condition)
      (null (store-value t condition))))

;; use-value
(eq 'ok
    (restart-case (use-value 'ok)
      (use-value (value) value)))
(let ((condition (make-condition 'simple-error
				 :format-control "error"
				 :format-arguments nil)))
  (or (find-restart 'use-value condition)
      (null (use-value t condition))))





;; assert
(eq (assert t) nil)
(handler-case (assert nil)
  (error () t)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(let ((count 0))
  (and (eq (assert (incf count)) nil)
       (= count 1)))
(handler-case (let ((var nil)) (assert var (var) "VAR should be true."))
  (simple-error () t)
  (error () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(let ((str (copy-seq "ABC"))
      (count 0))
  (and (eq (assert (char= (aref str 0) #\A) ((aref (progn (incf count) str) 0)))
	   nil)
       (zerop count)))
(let ((str (copy-seq "ABC"))
      (count 0))
  (and (eq (assert (and (char= (aref str 0) #\A)
			(char= (aref str 1) #\B))
		   ((aref (progn (incf count) str) 0)
		    (aref (progn (incf count) str) 1)))
	   nil)
       (zerop count)))
(handler-case (let ((var nil))
		(assert var (var) 'type-error :expected-type 'array))
  (type-error () t)
  (error () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))


;; check-type
(null (let ((var nil)) (check-type var null)))
(null (let ((var '(a b c))) (check-type var cons)))
(handler-case (let ((var '(a b c))) (check-type var vector))
  (type-error () t)
  (error () nil)
  (:no-error (&rest rest) (declare (ignore rest)) nil))
(eq 'handled
    (block tag 
      (handler-bind ((type-error
		      #'(lambda (c)
			  (declare (ignore c))
			  (return-from tag 'handled)))
		     (error #'(lambda (c)
				(declare (ignore c))
				(return-from tag nil))))
	(let ((var '(a b c)))
	  (check-type var vector)
	  var))))
(string= (block tag 
	   (handler-bind ((type-error
			   #'(lambda (c)
			       (declare (ignore c))
			       (invoke-restart 'store-value "eat this")))
			  (error #'(lambda (c)
				     (declare (ignore c))
				     (return-from tag nil))))
	     (let ((var '(a b c)))
	       (check-type var vector)
	       var)))
	 "eat this")


;; ignore-errors
(null (ignore-errors))
(ignore-errors t)
(let ((result (multiple-value-list (ignore-errors (error "error")))))
  (and (null (first result))
       (typep (second result) 'simple-error)))
(equal (multiple-value-list (ignore-errors 'a 'b 'c (values 'd 'e)))
       '(d e))
(let ((result (multiple-value-list
	       (ignore-errors (signal 'simple-error
				      :format-control "error"
				      :format-arguments nil)))))
  (and (null (first result))
       (typep (second result) 'simple-error)))
(eq (ignore-errors (signal "only signal") 'ok) 'ok)
(eq (block tag
      (handler-bind ((condition #'(lambda (c)
				    (declare (ignore c))
				    (return-from tag 'handled))))
	(ignore-errors (error 'simple-condition
			      :format-control "only condition"
			      :format-arguments nil))))
    'handled)
(let ((result (multiple-value-list
	       (ignore-errors (warn 'simple-error
				    :format-control "an error, not a warning"
				    :format-arguments nil)))))
  (and (null (first result))
       (typep (second result) 'type-error)))

