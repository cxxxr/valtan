;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: must-eval.lisp,v 1.8 2004/08/09 02:49:54 yuji Exp $
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

(= (funcall (lambda (x) (+ x 3)) 4) 7)
(= (funcall (lambda (&rest args) (apply #'+ args)) 1 2 3 4) 10)
(functionp (lambda (&rest args) (apply #'+ args)))
(functionp (macro-function 'lambda))



(every #'special-operator-p '(block catch eval-when flet function go if labels let let* load-time-value locally macrolet multiple-value-call multiple-value-prog1 progn progv quote return-from setq symbol-macrolet tagbody the throw unwind-protect))
(not (special-operator-p 'car))
(not (special-operator-p 'cdr))
;; Bruno: The spec of MACRO-FUNCTION says that an implementation can have
;; additional special operators provided the macro expander is available through
;; MACRO-FUNCTION.
#-CLISP
(not (special-operator-p 'cond))
(not (special-operator-p 'values))
