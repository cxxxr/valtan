;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: must-cons.lisp,v 1.4 2004/02/20 07:23:42 yuji Exp $
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

(consp (cons 'a 'b))

(consp '(1 . 2))

(consp (list nil))

(not (consp 'a))

(not (consp nil))

(not (consp 1))

(not (consp #\a))

(let ((a (cons 1 2)))
  (and (eql (car a) 1)
       (eql (cdr a) 2)))

(equal (cons 1 nil) '(1))

(equal (cons nil nil) '(nil))

(equal (cons 'a (cons 'b (cons 'c '()))) '(a b c))

(atom 'a)

(atom nil)

(atom 1)

(atom #\a)

(not (atom (cons 1 2)))

(not (atom '(a . b)))

(not (atom (list nil)))


(listp nil)

(listp '(a b c))

(listp '(a . b))

(listp (cons 'a 'b))

(listp '#1=(1 2 . #1#))

(not (listp 1))

(not (listp 't))

(null '())
(null 'nil)
(null nil)
(not (null t))
(null (cdr '(a)))
(not (null (cdr '(1 . 2))))
(not (null 'a))


(endp '())
(not (endp '(1)))
(not (endp '(1 2)))
(not (endp '(1 2 3)))
(not (endp (cons 1 2)))
(endp (cddr '(1 2)))


(let ((a (cons 1 2)))
  (and (eq (rplaca a 0) a)
       (equal a '(0 . 2))))

(let ((a (list 1 2 3)))
  (and (eq (rplaca a 0) a)
       (equal a '(0 2 3))))

(let ((a (cons 1 2)))
  (and (eq (rplacd a 0) a)
       (equal a '(1 . 0))))

(let ((a (list 1 2 3)))
  (and (eq (rplacd a 0) a)
       (equal a '(1 . 0))))

(eq (car '(a . b)) 'a)

(null (car nil))

(let ((a (cons 1 2)))
  (eq (car (list a)) a))

(eq (car '#1=(a . #1#)) 'a)

(eq (cdr '(a . b)) 'b)
(eq (rest '(a . b)) 'b)

(null (cdr nil))
(null (rest nil))

(let ((a (cons 1 2)))
  (eq (cdr (cons 1 a)) a))
(let ((a (cons 1 2)))
  (eq (rest (cons 1 a)) a))

(let ((x '#1=(a . #1#)))
  (eq (cdr x) x))
(let ((x '#1=(a . #1#)))
  (eq (rest x) x))

(eq (caar '((a) b c)) 'a)

(eq (cadr '(a b c)) 'b)

(eq (cdar '((a . aa) b c)) 'aa)

(eq (cddr '(a b . c)) 'c)

(eq (caaar '(((a)) b c)) 'a)

(eq (caadr '(a (b) c)) 'b)

(eq (cadar '((a aa) b c)) 'aa)

(eq (caddr '(a b c)) 'c)

(eq (cdaar '(((a . aa)) b c)) 'aa)

(eq (cdadr '(a (b . bb) c)) 'bb)

(eq (cddar '((a aa . aaa) b c)) 'aaa)

(eq (cdddr '(a b c . d)) 'd)

(eq (caaaar '((((a))) b c)) 'a)

(eq (caaadr '(a ((b)) c)) 'b)

(eq (caadar '((a (aa)) b c)) 'aa)

(eq (caaddr '(a b (c))) 'c)

(eq (cadaar '(((a aa)) b c)) 'aa)

(eq (cadadr '(a (b bb) c)) 'bb)

(eq (caddar '((a aa aaa) b c)) 'aaa)

(eq (cadddr '(a b c d)) 'd)

(eq (cdaaar '((((a . aa))) b c)) 'aa)

(eq (cdaadr '(a ((b . bb)) c)) 'bb)

(eq (cdadar '((a (aa . aaa)) b c)) 'aaa)

(eq (cdaddr '(a b (c . cc))) 'cc)

(eq (cddaar '(((a aa . aaa)) b c)) 'aaa)

(eq (cddadr '(a (b bb . bbb) c)) 'bbb)

(eq (cdddar '((a aa aaa . aaaa) b c)) 'aaaa)

(eq (cddddr '(a b c d . e)) 'e)

(let ((x (cons 1 2)))
  (and (eql (setf (car x) 0) 0)
       (equal x '(0 . 2))))

(let ((x (cons 1 2)))
  (and (eql (setf (cdr x) 0) 0)
       (equal x '(1 . 0))))

(let ((x (copy-tree '((a) b c))))
  (and (eql (setf (caar x) 0) 0)
       (equal x '((0) b c))))

(let ((x (list 'a 'b 'c)))
  (and (eql (setf (cadr x) 0) 0)
       (equal x '(a 0 c))))

(let ((x (copy-tree '((a . aa) b c))))
  (and (eql (setf (cdar x) 0) 0)
       (equal x '((a . 0) b c))))

(let ((x (copy-tree '(a b . c))))
  (and (eql (setf (cddr x) 0) 0)
       (equal x '(a b . 0))))

(let ((x (copy-tree '(((a)) b c))))
  (and (eql (setf (caaar x) 0) 0)
       (equal x '(((0)) b c))))

(let ((x (copy-tree '(a (b) c))))
  (and (eql (setf (caadr x) 0) 0)
       (equal x '(a (0) c))))

(let ((x (copy-tree '((a aa) b c))))
  (and (eql (setf (cadar x) 0) 0)
       (equal x '((a 0) b c))))

(let ((x (list 'a 'b 'c)))
  (and (eql (setf (caddr x) 0) 0)
       (equal x '(a b 0))))

(let ((x (copy-tree '(((a . aa)) b c))))
  (and (eql (setf (cdaar x) 0) 0)
       (equal x '(((a . 0)) b c))))

(let ((x (copy-tree '(a (b . bb) c))))
  (and (eql (setf (cdadr x) 0) 0)
       (equal x '(a (b . 0) c))))

(let ((x (copy-tree '((a aa . aaa) b c))))
  (and (eql (setf (cddar x) 0) 0)
       (equal x '((a aa . 0) b c))))

(let ((x (copy-tree '(a b c . d))))
  (and (eql (setf (cdddr x) 0) 0)
       (equal x '(a b c . 0))))

(let ((x (copy-tree '((((a))) b c))))
  (and (eql (setf (caaaar x) 0) 0)
       (equal x '((((0))) b c))))

(let ((x (copy-tree '(a ((b)) c))))
  (and (eql (setf (caaadr x) 0) 0)
       (equal x '(a ((0)) c))))

(let ((x (copy-tree '((a (aa)) b c))))
  (and (eql (setf (caadar x) 0) 0)
       (equal x '((a (0)) b c))))

(let ((x (copy-tree '(a b (c)))))
  (and (eql (setf (caaddr x) 0) 0)
       (equal x '(a b (0)))))

(let ((x (copy-tree '(((a aa)) b c))))
  (and (eql (setf (cadaar x) 0) 0)
       (equal x '(((a 0)) b c))))

(let ((x (copy-tree '(a (b bb) c))))
  (and (eql (setf (cadadr x) 0) 0)
       (equal x '(a (b 0) c))))

(let ((x (copy-tree '((a aa aaa) b c))))
  (and (eql (setf (caddar x) 0) 0)
       (equal x '((a aa 0) b c))))

(let ((x (list 'a 'b 'c 'd)))
  (and (eql (setf (cadddr x) 0) 0)
       (equal x '(a b c 0))))

(let ((x (copy-tree '((((a . aa))) b c))))
  (and (eql (setf (cdaaar x) 0) 0)
       (equal x '((((a . 0))) b c))))

(let ((x (copy-tree '(a ((b . bb)) c))))
  (and (eql (setf (cdaadr x) 0) 0)
       (equal x '(a ((b . 0)) c))))

(let ((x (copy-tree '((a (aa . aaa)) b c))))
  (and (eql (setf (cdadar x) 0) 0)
       (equal x '((a (aa . 0)) b c))))

(let ((x (copy-tree '(a b (c . cc)))))
  (and (eql (setf (cdaddr x) 0) 0)
       (equal x '(a b (c . 0)))))

(let ((x (copy-tree '(((a aa . aaa)) b c))))
  (and (eql (setf (cddaar x) 0) 0)
       (equal x '(((a aa . 0)) b c))))

(let ((x (copy-tree '(a (b bb . bbb) c))))
  (and (eql (setf (cddadr x) 0) 0)
       (equal x '(a (b bb . 0) c))))

(let ((x (copy-tree '((a aa aaa . aaaa) b c))))
  (and (eql (setf (cdddar x) 0) 0)
       (equal x '((a aa aaa . 0) b c))))

(let ((x (copy-tree '(a b c d . e))))
  (and (eql (setf (cddddr x) 0) 0)
       (equal x '(a b c d . 0))))

(eq (copy-tree 'a) 'a)

(eq (copy-tree nil) nil)

(let* ((a (list 'a))
       (b (list 'b))
       (c (list 'c))
       (x3 (cons c nil))
       (x2 (cons b x3))
       (x  (cons a x2))
       (y  (copy-tree x)))
  (and (not (eq x y))
       (not (eq (car x) (car y)))
       (not (eq (cdr x) (cdr y)))
       (not (eq (cadr x) (cadr y)))
       (not (eq (cddr x) (cddr y)))
       (not (eq (caddr x) (caddr y)))
       (eq (cdddr x) (cdddr y))
       (equal x y)
       (eq (car x) a) (eq (car a) 'a) (eq (cdr a) nil)
       (eq (cdr x) x2)
       (eq (car x2) b) (eq (car b) 'b) (eq (cdr b) nil)
       (eq (cdr x2) x3)
       (eq (car x3) c) (eq (car c) 'c) (eq (cdr c) nil)
       (eq (cdr x3) nil)))

(let* ((x (list (list 'a 1) (list 'b 2) (list 'c 3)))
       (y (copy-tree x)))
  (and (not (eq (car x) (car y)))
       (not (eq (cadr x) (cadr y)))
       (not (eq (caddr x) (caddr y)))))

(let* ((x (list (list (list 1))))
       (y (copy-tree x)))
  (and (not (eq x y))
       (not (eq (car x) (car y)))
       (not (eq (caar x) (caar y)))))


(let ((x (list 'a 'b 'c 'd)))
  (and (equal (sublis '((a . 1) (b . 2) (c . 3)) x)
	      '(1 2 3 d))
       (equal x '(a b c d))))

(let* ((n (cons 'n nil))
       (m (cons 'm n))
       (l (cons 'l m))
       (x (sublis '((a . 1) (b . 2) (c . 3)) l)))
  (and (eq x l)
       (eq (car l) 'l)
       (eq (cdr l) m)
       (eq (car m) 'm)
       (eq (cdr m) n)
       (eq (car n) 'n)
       (eq (cdr n) nil)))

(eq (sublis '() '()) '())

(equal (sublis '() '(1 2 3)) '(1 2 3))

(eq (sublis '((a . 1) (b . 2)) '()) nil)

(equal (sublis '((a b c) (b c d) (c d e))
	       '(a b c))
       '((b c) (c d) (d e)))

(equal (sublis '((a . 1) (b . 2) (c . 3))
	       '(((a)) (b) c))
       '(((1)) (2) 3))

(equal (sublis '(((a) . 1) ((b) . 2) ((c) . 3))
	       '((((a))) ((b)) (c)))
       '((((a))) ((b)) (c)))

(equal (sublis '(((a) . 1) ((b) . 2) ((c) . 3))
	       '((((a))) ((b)) (c))
	       :test #'equal)
       '(((1)) (2) 3))

(equal (sublis '(((a) . 1) ((b) . 2) ((c) . 3))
	       '((((a))) ((b)) (c))
	       :test-not (complement #'equal))
       '(((1)) (2) 3))

(equal (sublis '((a . 1) (b . 2) (c . 3))
	       '((((a))) ((b)) (c))
	       :key #'car)
       '(((1)) (2) 3))

(equal (sublis '(((a) . 1) ((b) . 2) ((c) . 3))
	       '((((a))) ((b)) (c))
	       :key  #'car
	       :test #'equal)
       '((1) 2 . 3))



(equal (nsublis '((a . 1) (b . 2) (c . 3))
		(list 'a 'b 'c 'd))
       '(1 2 3 d))

(let* ((x (list 'a 'b 'c 'd))
       (y (nsublis '((a . 1) (b . 2) (c . 3)) x)))
  (and (eq x y)
       (equal x '(1 2 3 d))))

(let ((x (list 'l 'm 'n)))
  (and (eq (nsublis '((a . 1) (b . 2) (c . 3)) x) x)
       (equal x '(l m n))))

(let* ((n (cons 'n nil))
       (m (cons 'm n))
       (l (cons 'l m))
       (x (nsublis '((a . 1) (b . 2) (c . 3)) l)))
  (and (eq x l)
       (eq (car l) 'l)
       (eq (cdr l) m)
       (eq (car m) 'm)
       (eq (cdr m) n)
       (eq (car n) 'n)
       (eq (cdr n) nil)))

(eq (nsublis '() '()) '())

(equal (nsublis '() '(1 2 3)) '(1 2 3))

(eq (nsublis '((a . 1) (b . 2)) '()) nil)

(equal (nsublis '((a b c) (b c d) (c d e))
		(list 'a 'b 'c))
       '((b c) (c d) (d e)))

(equal (nsublis '((a . 1) (b . 2) (c . 3))
		(copy-tree '(((a)) (b) c)))
       '(((1)) (2) 3))

(equal (nsublis '(((a) . 1) ((b) . 2) ((c) . 3))
		(copy-tree '((((a))) ((b)) (c))))
       '((((a))) ((b)) (c)))

(equal (nsublis '(((a) . 1) ((b) . 2) ((c) . 3))
		(copy-tree '((((a))) ((b)) (c)))
		:test #'equal)
       '(((1)) (2) 3))

(equal (nsublis '(((a) . 1) ((b) . 2) ((c) . 3))
		(copy-tree '((((a))) ((b)) (c)))
		:test-not (complement #'equal))
       '(((1)) (2) 3))


(equal (nsublis '((a . 1) (b . 2) (c . 3))
		(copy-tree '((((a))) ((b)) (c)))
		:key #'car)
       '(((1)) (2) 3))

(equal (nsublis '(((a) . 1) ((b) . 2) ((c) . 3))
		(copy-tree '((((a))) ((b)) (c)))
		:key 'car
		:test #'equal)
       '((1) 2 . 3))


(let ((tree '(old (old) ((old)))))
  (equal (subst 'new 'old tree)
	 '(new (new) ((new)))))

(eq (subst 'new 'old 'old) 'new)

(eq (subst 'new 'old 'not-old) 'not-old)

(equal (subst 'new '(b) '(a ((b))) :test #'equal)
       '(a (new)))

(equal (subst 'new '(b) '(a ((b))) :test-not (complement #'equal))
       '(a (new)))

(equal (subst 'x 3 '(1 (1 2) (1 2 3) (1 2 3 4))
	      :key #'(lambda (y) (and (listp y) (third y))))
       '(1 (1 2) X X))

(equal (subst 'x "D" '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d"))
	      :test #'equalp
	      :key #'(lambda (y) (and (listp y) (fourth y))))
       '("a" ("a" "b") ("a" "b" "c") X))


(equal (subst-if 'new #'(lambda (x) (eq x 'old)) '(old old))
       '(new new))

(eq (subst-if 'new #'(lambda (x) (eq x 'old)) 'old) 'new)

(equal (subst-if 'x #'(lambda (x) (eql x 3)) '(1 (1 2) (1 2 3) (1 2 3 4))
		 :key #'(lambda (y) (and (listp y) (third y))))
       '(1 (1 2) x x))


(let ((tree '(old (old) ((old)))))
  (equal (subst-if 'new #'(lambda (x) (eq x 'old)) tree)
	 '(new (new) ((new)))))

(eq (subst-if 'new #'(lambda (x) (eq x 'old)) 'old)
    'new)

(eq (subst-if 'new #'(lambda (x) (eq x 'old)) 'not-old)
    'not-old)

(equal (subst-if 'new #'(lambda (x) (equal x '(b))) '(a ((b))))
       '(a (new)))

(equal (subst-if 'x
		 #'(lambda (x) (eql x 3)) '(1 (1 2) (1 2 3) (1 2 3 4))
		 :key #'(lambda (y) (and (listp y) (third y))))
       '(1 (1 2) X X))

(equal (subst-if 'x
		 #'(lambda (x) (equalp x "D"))
		 '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d"))
		 :key #'(lambda (y) (and (listp y) (fourth y))))
       '("a" ("a" "b") ("a" "b" "c") X))


(equal (subst-if-not 'new #'(lambda (x) (not (eq x 'old))) '(old old))
       '(new new))

(eq (subst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'old) 'new)

(equal (subst-if-not 'x #'(lambda (x) (not (eql x 3)))
		     '(1 (1 2) (1 2 3) (1 2 3 4))
		     :key #'(lambda (y) (and (listp y) (third y))))
       '(1 (1 2) x x))


(let ((tree '(old (old) ((old)))))
  (equal (subst-if-not 'new #'(lambda (x) (not (eq x 'old))) tree)
	 '(new (new) ((new)))))

(eq (subst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'old)
    'new)

(eq (subst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'not-old)
    'not-old)

(equal (subst-if-not 'new #'(lambda (x) (not (equal x '(b)))) '(a ((b))))
       '(a (new)))

(equal (subst-if-not 'x
		     #'(lambda (x) (not (eql x 3)))
		     '(1 (1 2) (1 2 3) (1 2 3 4))
		     :key #'(lambda (y) (and (listp y) (third y))))
       '(1 (1 2) X X))

(equal (subst-if-not 'x
		     #'(lambda (x) (not (equalp x "D")))
		     '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d"))
		     :key #'(lambda (y) (and (listp y) (fourth y))))
       '("a" ("a" "b") ("a" "b" "c") X))



(let ((tree '(old (old) ((old)))))
  (equal (nsubst 'new 'old (copy-tree tree))
	 '(new (new) ((new)))))

(let* ((tree (copy-tree '(old (old) ((old)))))
       (new-tree (nsubst 'new 'old tree)))
  (and (eq tree new-tree)
       (equal tree '(new (new) ((new))))))

(eq (nsubst 'new 'old 'old) 'new)

(eq (nsubst 'new 'old 'not-old) 'not-old)

(equal (nsubst 'new '(b) (copy-tree '(a ((b)))) :test #'equal)
       '(a (new)))

(equal (nsubst 'new '(b) (copy-tree '(a ((b)))) :test-not (complement #'equal))
       '(a (new)))

(equal (nsubst 'x 3 (copy-tree '(1 (1 2) (1 2 3) (1 2 3 4)))
	       :key #'(lambda (y) (and (listp y) (third y))))
       '(1 (1 2) X X))

(equal (nsubst 'x "D"
	       (copy-tree '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d")))
	       :test #'equalp
	       :key #'(lambda (y) (and (listp y) (fourth y))))
       '("a" ("a" "b") ("a" "b" "c") X))


(equal (nsubst-if 'new #'(lambda (x) (eq x 'old)) (list 'old 'old))
       '(new new))

(eq (nsubst-if 'new #'(lambda (x) (eq x 'old)) 'old) 'new)

(let* ((x (copy-tree '(old (old) ((old)) (old) old)))
       (y (nsubst-if 'new #'(lambda (x) (eq x 'old)) x)))
  (and (eq x y)
       (equal x '(new (new) ((new)) (new) new))))

(equal (nsubst-if 'x
		  #'(lambda (x) (eql x 3))
		  (copy-tree '(1 (1 2) (1 2 3) (1 2 3 4)))
		  :key #'(lambda (y) (and (listp y) (third y))))
       '(1 (1 2) x x))

(let ((tree '(old (old) ((old)))))
  (equal (nsubst-if 'new #'(lambda (x) (eq x 'old)) (copy-tree tree))
	 '(new (new) ((new)))))

(eq (nsubst-if 'new #'(lambda (x) (eq x 'old)) 'old)
    'new)

(eq (nsubst-if 'new #'(lambda (x) (eq x 'old)) 'not-old)
    'not-old)

(equal (nsubst-if 'new #'(lambda (x) (equal x '(b)))
		  (copy-tree '(a ((b)))))
       '(a (new)))

(equal (nsubst-if 'x
		  #'(lambda (x) (eql x 3))
		  (copy-tree '(1 (1 2) (1 2 3) (1 2 3 4)))
		  :key #'(lambda (y) (and (listp y) (third y))))
       '(1 (1 2) X X))

(equal (nsubst-if 'x
		  #'(lambda (x) (equalp x "D"))
		  (copy-tree '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d")))
		  :key #'(lambda (y) (and (listp y) (fourth y))))
       '("a" ("a" "b") ("a" "b" "c") X))


(equal (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old)))
		      (list 'old 'old))
       '(new new))

(eq (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'old) 'new)

(let* ((x (copy-tree '(old (old) ((old)) (old) old)))
       (y (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old))) x)))
  (and (eq x y)
       (equal x '(new (new) ((new)) (new) new))))

(equal (nsubst-if-not 'x #'(lambda (x) (not (eql x 3)))
		      (copy-tree '(1 (1 2) (1 2 3) (1 2 3 4)))
		      :key #'(lambda (y) (and (listp y) (third y))))
       '(1 (1 2) x x))

(let ((tree '(old (old) ((old)))))
  (equal (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old))) (copy-tree tree))
	 '(new (new) ((new)))))

(eq (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'old)
    'new)

(eq (nsubst-if-not 'new #'(lambda (x) (not (eq x 'old))) 'not-old)
    'not-old)

(equal (nsubst-if-not 'new #'(lambda (x) (not (equal x '(b))))
		      (copy-tree '(a ((b)))))
       '(a (new)))

(equal (nsubst-if-not 'x
		      #'(lambda (x) (not (eql x 3)))
		      (copy-tree '(1 (1 2) (1 2 3) (1 2 3 4)))
		      :key #'(lambda (y) (and (listp y) (third y))))
       '(1 (1 2) X X))

(equal
 (nsubst-if-not 'x
		#'(lambda (x) (not (equalp x "D")))
		(copy-tree '("a" ("a" "b") ("a" "b" "c") ("a" "b" "c" "d")))
		:key #'(lambda (y) (and (listp y) (fourth y))))
 '("a" ("a" "b") ("a" "b" "c") X))



(tree-equal 'a 'a)

(not (tree-equal 'a 'b))

(tree-equal '(a (b (c))) '(a (b (c))))

(tree-equal '(a (b (c))) '(a (b (c))) :test #'eq)

(tree-equal '(a (b (c))) '(a (b (c))) :test-not (complement #'eq))

(not (tree-equal '("a" ("b" ("c"))) '("a" ("b" ("c")))))

(tree-equal '("a" ("b" ("c"))) '("a" ("b" ("c"))) :test #'equal)

(tree-equal '("a" ("b" ("c"))) '("a" ("b" ("c")))
	    :test-not (complement #'equal))

(not (tree-equal '(a b) '(a (b))))


(eq (copy-list '()) '())

(equal (copy-list '(a b c))
       '(a b c))

(equal (copy-list '(a . b)) '(a . b))

(let* ((x '(a b c))
       (y (copy-list x)))
  (and (equal x y)
       (not (eq x y))))

(let* ((a (list 'a))
       (b (list 'b))
       (c (list 'c))
       (x (list a b c))
       (y (copy-list x)))
  (and (equal x y)
       (not (eq x y))
       (eq (car x) (car y))
       (eq (cadr x) (cadr y))
       (eq (caddr x) (caddr y))
       (eq (caar x) 'a)
       (eq (caadr x) 'b)
       (eq (caaddr x) 'c)))


(null (list))

(equal (list 1) '(1))

(equal (list 1 2 3) '(1 2 3))

(equal (list* 1 2 '(3)) '(1 2 3))

(equal (list* 1 2 'x) '(1 2 . x))

(equal (list* 1 2 '(3 4)) '(1 2 3 4))

(eq (list* 'x) 'x)


(eql (list-length '()) 0)

(eql (list-length '(1)) 1)

(eql (list-length '(1 2)) 2)

(null (list-length '#1=(1 2 3 4 . #1#)))


(equal (make-list 5) '(nil nil nil nil nil))

(equal (make-list 3 :initial-element 'rah) '(rah rah rah))

(equal (make-list 2 :initial-element '(1 2 3)) '((1 2 3) (1 2 3)))

(null (make-list 0))

(null (make-list 0 :initial-element 'new-element))


(let ((place nil))
  (and (equal (push 0 place) '(0))
       (equal place '(0))))

(let ((place (list 1 2 3)))
  (and (equal (push 0 place) '(0 1 2 3))
       (equal place '(0 1 2 3))))

(let ((a (list (list 1 2 3) 9)))
  (and (equal (push 0 (car a)) '(0 1 2 3))
       (equal a '((0 1 2 3) 9))))

(let ((x (copy-tree '(a (b c) d))))
  (and (equal (push 'aa (cadr x)) '(aa b c))
       (equal x '(a (aa b c) d))))


(let ((place (list 1 2 3)))
  (and (eql (pop place) 1)
       (equal place '(2 3))))

(let ((place '()))
  (and (eql (pop place) nil)
       (equal place '())))

(let ((a (list (list 1 2 3) 9)))
  (and (eql (pop (car a)) 1)
       (equal a '((2 3) 9))))

(let ((x (list 'a 'b 'c)))
  (and (eq (pop (cdr x)) 'b)
       (equal x '(a c))))


(eq (first '(a . b)) 'a)

(null (first nil))

(let ((a (cons 1 2)))
  (eq (first (list a)) a))

(eq (first '#1=(a . #1#)) 'a)

(eql (first   '(1 2 3)) '1)
(eql (second  '(1 2 3)) '2)
(eql (third   '(1 2 3)) '3)
(eql (fourth  '(1 2 3 4)) '4)
(eql (fifth   '(1 2 3 4 5)) '5)
(eql (sixth   '(1 2 3 4 5 6)) '6)
(eql (seventh '(1 2 3 4 5 6 7)) '7)
(eql (eighth  '(1 2 3 4 5 6 7 8)) '8)
(eql (ninth   '(1 2 3 4 5 6 7 8 9)) '9)
(eql (tenth   '(1 2 3 4 5 6 7 8 9 10)) '10)


(let ((x (list 'a 'b 'c)))
  (and (eql (setf (first x) 0) 0)
       (equal x '(0 b c))))

(let ((x (list 'a 'b 'c)))
  (and (eql (setf (second x) 0) 0)
       (equal x '(a 0 c))))

(let ((x (list 'a 'b 'c)))
  (and (eql (setf (third x) 0) 0)
       (equal x '(a b 0))))

(let ((x (list 'a 'b 'c 'd)))
  (and (eql (setf (fourth x) 0) 0)
       (equal x '(a b c 0))))

(let ((x (list 'a 'b 'c 'd 'e)))
  (and (eql (setf (fifth x) 0) 0)
       (equal x '(a b c d 0))))

(let ((x (list 'a 'b 'c 'd 'e 'f)))
  (and (eql (setf (sixth x) 0) 0)
       (equal x '(a b c d e 0))))

(let ((x (list 'a 'b 'c 'd 'e 'f 'g)))
  (and (eql (setf (seventh x) 0) 0)
       (equal x '(a b c d e f 0))))

(let ((x (list 'a 'b 'c 'd 'e 'f 'g 'h)))
  (and (eql (setf (eighth x) 0) 0)
       (equal x '(a b c d e f g 0))))

(let ((x (list 'a 'b 'c 'd 'e 'f 'g 'h 'i)))
  (and (eql (setf (ninth x) 0) 0)
       (equal x '(a b c d e f g h 0))))

(let ((x (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
  (and (eql (setf (tenth x) 0) 0)
       (equal x '(a b c d e f g h i 0))))


(let ((x '(a b c)))
  (eq (nthcdr 0 x) x))

(let ((x '(a b c)))
  (eq (nthcdr 1 x) (cdr x)))

(let ((x '(a b c)))
  (eq (nthcdr 2 x) (cddr x)))

(let ((x '(a b c)))
  (eq (nthcdr 2 x) (cddr x)))

(let ((x '(a b c)))
  (eq (nthcdr 3 x) (cdddr x)))

(equal (nthcdr 0 '(0 1 2)) '(0 1 2))
(equal (nthcdr 1 '(0 1 2)) '(1 2))
(equal (nthcdr 2 '(0 1 2)) '(2))
(equal (nthcdr 3 '(0 1 2)) '())

(eql (nthcdr 1 '(0 . 1)) 1)

(eql (nth 0 '(a b c)) 'a)
(eql (nth 1 '(a b c)) 'b)
(eql (nth 2 '(a b c)) 'c)
(eql (nth 3 '(a b c)) '())
(eql (nth 4 '(a b c)) '())
(eql (nth 5 '(a b c)) '())
(eql (nth 6 '(a b c)) '())

(eq (nth 0 '(a . b)) 'a)

(let ((x (list 'a 'b 'c)))
  (and (eq (setf (nth 0 x) 'z) 'z)
       (equal x '(z b c))))

(let ((x (list 'a 'b 'c)))
  (and (eq (setf (nth 1 x) 'z) 'z)
       (equal x '(a z c))))

(let ((x (list 'a 'b 'c)))
  (and (eq (setf (nth 2 x) 'z) 'z)
       (equal x '(a b z))))

(let ((0-to-3 (list 0 1 2 3)))
  (and (equal (setf (nth 2 0-to-3) "two") "two")
       (equal 0-to-3 '(0 1 "two" 3))))


(eq (nconc) '())
(equal (nconc nil (list 'a 'b 'c) (list 'd 'e 'f))
       '(a b c d e f))

(equal (nconc nil nil (list 'a 'b 'c) (list 'd 'e 'f))
       '(a b c d e f))

(equal (nconc nil nil nil (list 'a 'b 'c) (list 'd 'e 'f))
       '(a b c d e f))


(let* ((x (list 'a 'b 'c)))
  (eq (nconc x) x))

(let* ((x (list 'a 'b 'c))
       (y (list 'd 'e 'f))
       (list (nconc x y)))
  (and (eq list x)
       (eq (nthcdr 3 list) y)
       (equal list '(a b c d e f))))

(let* ((x (list 'a))
       (y (list 'b))
       (z (list 'c))
       (list (nconc x y z)))
  (and (eq x list)
       (eq (first list) 'a)
       (eq y (cdr list))
       (eq (second list) 'b)
       (eq z (cddr list))
       (eq (third list) 'c)))

(equal (append '(a b) '() '(c d) '(e f))
       '(a b c d e f))

(null (append))
(null (append '()))
(null (append '() '()))

(eq (append 'a) 'a)

(eq (append '() 'a) 'a)

(eq (append '() '() 'a) 'a)


(equal (append '(a b) 'c) '(a b . c))

(let* ((x '(a b c))
       (y '(d e f))
       (z (append x y)))
  (and (equal z '(a b c d e f))
       (eq (nthcdr 3 z) y)
       (not (eq x z))))


(equal (revappend '(a b c) '(d e f))
       '(c b a d e f))

(let* ((x '(a b c))
       (y '(d e f))
       (z (revappend x y)))
  (and (equal z '(c b a d e f))
       (not (eq x z))
       (eq (nthcdr 3 z) y)))

(let ((x '(a b c)))
  (eq (revappend '() x) x))

(null (revappend '() '()))

(eq (revappend '() 'a) 'a)

(equal (revappend '(a) 'b) '(a . b))

(equal (revappend '(a) '()) '(a))

(equal (revappend '(1 2 3) '()) '(3 2 1))


(equal (nreconc (list 'a 'b 'c) '(d e f))
       '(c b a d e f))

(let* ((x (list 'a 'b 'c))
       (y '(d e f))
       (z (nreconc x y)))
  (and (equal z '(c b a d e f))
       (eq (nthcdr 3 z) y)))

(let ((x (list 'a 'b 'c)))
  (eq (nreconc '() x) x))

(null (nreconc '() '()))

(eq (nreconc '() 'a) 'a)

(equal (nreconc (list 'a) 'b) '(a . b))

(equal (nreconc (list 'a) '()) '(a))

(equal (nreconc (list 1 2 3) '()) '(3 2 1))


(null (butlast nil))
(null (butlast nil 1))
(null (butlast nil 2))
(null (butlast nil 3))
(equal (butlast '(1 2 3 4 5))   '(1 2 3 4))
(equal (butlast '(1 2 3 4 5) 1) '(1 2 3 4))
(equal (butlast '(1 2 3 4 5) 2) '(1 2 3))
(equal (butlast '(1 2 3 4 5) 3) '(1 2))
(equal (butlast '(1 2 3 4 5) 4) '(1))
(equal (butlast '(1 2 3 4 5) 5) '())
(equal (butlast '(1 2 3 4 5) 6) '())
(equal (butlast '(1 2 3 4 5) 7) '())

(equal (butlast '(1 2 3 4 5 . 6))   '(1 2 3 4))
(equal (butlast '(1 2 3 4 5 . 6) 1) '(1 2 3 4))
(equal (butlast '(1 2 3 4 5 . 6) 2) '(1 2 3))
(equal (butlast '(1 2 3 4 5 . 6) 3) '(1 2))
(equal (butlast '(1 2 3 4 5 . 6) 4) '(1))
(equal (butlast '(1 2 3 4 5 . 6) 5) '())
(equal (butlast '(1 2 3 4 5 . 6) 6) '())
(equal (butlast '(1 2 3 4 5 . 6) 7) '())

(let ((a '(1 2 3 4 5)))
  (equal (butlast a 3) '(1 2))
  (equal a '(1 2 3 4 5)))


(null (nbutlast nil))
(null (nbutlast nil 1))
(null (nbutlast nil 2))
(null (nbutlast nil 3))
(equal (nbutlast (list 1 2 3 4 5))   '(1 2 3 4))
(equal (nbutlast (list 1 2 3 4 5) 1) '(1 2 3 4))
(equal (nbutlast (list 1 2 3 4 5) 2) '(1 2 3))
(equal (nbutlast (list 1 2 3 4 5) 3) '(1 2))
(equal (nbutlast (list 1 2 3 4 5) 4) '(1))
(equal (nbutlast (list 1 2 3 4 5) 5) '())
(equal (nbutlast (list 1 2 3 4 5) 6) '())
(equal (nbutlast (list 1 2 3 4 5) 7) '())

(equal (nbutlast (list* 1 2 3 4 5 6))   '(1 2 3 4))
(equal (nbutlast (list* 1 2 3 4 5 6) 1) '(1 2 3 4))
(equal (nbutlast (list* 1 2 3 4 5 6) 2) '(1 2 3))
(equal (nbutlast (list* 1 2 3 4 5 6) 3) '(1 2))
(equal (nbutlast (list* 1 2 3 4 5 6) 4) '(1))
(equal (nbutlast (list* 1 2 3 4 5 6) 5) '())
(equal (nbutlast (list* 1 2 3 4 5 6) 6) '())
(equal (nbutlast (list* 1 2 3 4 5 6) 7) '())

(let* ((a '(1 2 3 4 5))
       (b (nbutlast a 3)))
  (and (eq a b)
       (equal a '(1 2))))


(let ((x '(0 1 2 3 4 5 6 7 8 9)))
  (eq (last x) (nthcdr 9 x)))

(null (last nil))

(let ((x '(0 1 . 2)))
  (eq (last x) (cdr x)))

(eql (last '(1 . 2) 0) 2)

(let ((x '(0 1 2 3 4)))
  (eq (last x 0) nil))

(let ((x '(0 1 2 3 4)))
  (eq (last x) (nthcdr 4 x)))

(let ((x '(0 1 2 3 4)))
  (eq (last x 1) (nthcdr 4 x)))

(let ((x '(0 1 2 3 4)))
  (eq (last x 2) (cdddr x)))

(let ((x '(0 1 2 3 4)))
  (eq (last x 3) (cddr x)))

(let ((x '(0 1 2 3 4)))
  (eq (last x 4) (cdr x)))

(let ((x '(0 1 2 3 4)))
  (eq (last x 5) x))

(let ((x '(0 1 2 3 4)))
  (eq (last x 6) x))

(let ((x '(0 1 2 3 4)))
  (eq (last x 7) x))

(let ((x '(0 1 2 3 4)))
  (eq (last x 8) x))


(tailp '() '())

(tailp '() '(1))

(tailp '() '(1 2 3 4 5 6 7 8 9))

(let ((x '(1 2 3)))
  (and (tailp x x)
       (tailp (cdr x) x)
       (tailp (cddr x) x)
       (tailp (cdddr x) x)))

(let ((x '(1 . 2)))
  (and (tailp x x)
       (tailp (cdr x) x)))

(not (tailp nil '(1 . 2)))

(not (tailp 'x '(1 2 3 4 5 6)))

(not (tailp (list 1 2 3) '(1 2 3)))

(let ((x '(1 2 3 4 5 . 6)))
  (tailp (last x) x))

(let ((x '(1 2 3 4 5 . 6)))
  (tailp (last x) x))


(null (ldiff '() '()))

(equal (ldiff '(1 . 2) 2) '(1))

(equal (ldiff '(1 2 3 4 5 6 7 8 9) '())
       '(1 2 3 4 5 6 7 8 9))

(let ((x '(1 2 3)))
  (and (null (ldiff x x))
       (equal (ldiff x (cdr x)) '(1))
       (equal (ldiff x (cddr x)) '(1 2))
       (equal (ldiff x (cdddr x)) '(1 2 3))))

(let* ((x '(1 2 3))
       (y '(a b c))
       (z (ldiff x y)))
  (and (not (eq x z))
       (equal z '(1 2 3))))

(equal (member 'a '(a b c d)) '(a b c d))
(equal (member 'b '(a b c d)) '(b c d))
(equal (member 'c '(a b c d)) '(c d))
(equal (member 'd '(a b c d)) '(d))
(equal (member 'e '(a b c d)) '())
(equal (member 'f '(a b c d)) '())

(let ((x '(a b c d)))
  (eq (member 'a x) x)
  (eq (member 'b x) (cdr x))
  (eq (member 'c x) (cddr x))
  (eq (member 'd x) (cdddr x))
  (eq (member 'e x) nil))


(equal (member 'a '(a b c d) :test #'eq) '(a b c d))
(equal (member 'b '(a b c d) :test #'eq) '(b c d))
(equal (member 'c '(a b c d) :test #'eq) '(c d))
(equal (member 'd '(a b c d) :test #'eq) '(d))
(equal (member 'e '(a b c d) :test #'eq) '())
(equal (member 'f '(a b c d) :test #'eq) '())

(null (member 'a '()))

(let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
       (y (member 'd x :key #'cdr :test #'eq)))
  (and (equal y '((4 . d) (5 . e)))
       (eq y (nthcdr 3 x))))

(let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
       (y (member 'd x :key #'cdr)))
  (and (equal y '((4 . d) (5 . e)))
       (eq y (nthcdr 3 x))))

(let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
       (y (member 'd x :key #'cdr :test-not (complement #'eq))))
  (and (equal y '((4 . d) (5 . e)))
       (eq y (nthcdr 3 x))))

(let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
       (y (member 'd x :test-not (complement #'eq))))
  (eq y nil))

(equal (member 2 '((1 . 2) (3 . 4)) :test-not #'= :key #'cdr)
       '((3 . 4)))

(equal (member-if #'(lambda (x) (eql x 'a)) '(a b c d)) '(a b c d))
(equal (member-if #'(lambda (x) (eql x 'b)) '(a b c d)) '(b c d))
(equal (member-if #'(lambda (x) (eql x 'c)) '(a b c d)) '(c d))
(equal (member-if #'(lambda (x) (eql x 'd)) '(a b c d)) '(d))
(equal (member-if #'(lambda (x) (eql x 'e)) '(a b c d)) '())
(equal (member-if #'(lambda (x) (eql x 'f)) '(a b c d)) '())

(null (member-if #'(lambda (x) (eql x 'a)) '()))

(let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
       (y (member-if #'(lambda (p) (eq p 'd)) x :key #'cdr)))
  (and (equal y '((4 . d) (5 . e)))
       (eq y (nthcdr 3 x))))

(equal (member-if #'cdr '((1) (2 . 2) (3 3 . 3)))
       '((2 . 2) (3 3 . 3)))

(null (member-if #'zerop '(7 8 9)))


(equal (member-if-not #'(lambda (x) (not (eql x 'a))) '(a b c d)) '(a b c d))
(equal (member-if-not #'(lambda (x) (not (eql x 'b))) '(a b c d)) '(b c d))
(equal (member-if-not #'(lambda (x) (not (eql x 'c))) '(a b c d)) '(c d))
(equal (member-if-not #'(lambda (x) (not (eql x 'd))) '(a b c d)) '(d))
(equal (member-if-not #'(lambda (x) (not (eql x 'e))) '(a b c d)) '())
(equal (member-if-not #'(lambda (x) (not (eql x 'f))) '(a b c d)) '())

(null (member-if-not #'(lambda (x) (not (eql x 'a))) '()))

(let* ((x '((1 . a) (2 . b) (3 . c) (4 . d) (5 . e)))
       (y (member-if-not #'(lambda (p) (not (eq p 'd))) x :key #'cdr)))
  (and (equal y '((4 . d) (5 . e)))
       (eq y (nthcdr 3 x))))


(let ((x '((1 2) (2 3) (3 4) (4 5)))
      (y nil))
  (and (eq (mapc #'(lambda (a) (push (car a) y)) x) x)
       (equal y '(4 3 2 1))))

(let ((dummy nil)
      (list-1 '(1 2 3 4)))
  (and (eq (mapc #'(lambda (&rest x) (setq dummy (append dummy x)))
		    list-1
		    '(a b c d e)
		    '(x y z))
	   list-1)
       (equal dummy '(1 a x 2 b y 3 c z))))

(let* ((x '(0 1 2 3))
       (y nil)
       (z (mapc #'(lambda (a b c) (push (list a b c) y))
		x '(1 2 3 4) '(2 3 4 5))))
  (and (eq z x)
       (equal y '((3 4 5) (2 3 4) (1 2 3) (0 1 2)))))
       
  
(let* ((x '(0 1 2 3))
       (y nil)
       (z (mapc #'(lambda (a b c) (push (list a b c) y))
		nil x '(1 2 3 4) '(2 3 4 5))))
  (and (null z)
       (null y)))
       
(let ((sum 0))
  (mapc #'(lambda (&rest rest) (setq sum (+ sum (apply #'+ rest))))
	'(0 1 2)
	'(1 2 0)
	'(2 0 1))
  (eql sum 9))

(let ((result 'initial-value)
      (list-1 nil))
  (and (eq (mapc #'(lambda (a b) (setq result (cons (cons a b) result))) list-1) list-1)
       (eq result 'initial-value)))

(let ((result 'initial-value)
      (list-1 nil))
  (and (eq (mapc #'(lambda (a b) (setq result (cons (cons a b) result)))
		 list-1
		 '(1 2 3))
	   list-1)
       (eq result 'initial-value)))

(let ((result 'initial-value)
      (list-1 '(1 2 3)))
  (and (eq (mapc #'(lambda (a b) (setq result (cons (cons a b) result)))
		 list-1
		 '())
	   list-1)
       (eq result 'initial-value)))


(equal (mapcar #'car '((1 2) (2 3) (3 4) (4 5)))
       '(1 2 3 4))

(null (mapcar #'identity '()))

(equal (mapcar #'list '(0 1 2 3) '(a b c d) '(w x y z))
       '((0 a w) (1 b x) (2 c y) (3 d z)))

(null (mapcar #'list '() '(0 1 2 3) '(1 2 3 4) '(2 3 4 5)))
(null (mapcar #'list '(0 1 2 3) '() '(1 2 3 4) '(2 3 4 5)))
(null (mapcar #'list '(0 1 2 3) '(1 2 3 4) '() '(2 3 4 5)))
(null (mapcar #'list '(0 1 2 3) '(1 2 3 4) '(2 3 4 5) '()))

(equal (mapcar #'list '(0) '(a b) '(x y z)) '((0 a x)))
(equal (mapcar #'list '(a b) '(0) '(x y z)) '((a 0 x)))
(equal (mapcar #'list '(a b) '(x y z) '(0)) '((a x 0)))

(equal (mapcar #'cons '(a b c) '(1 2 3))
       '((A . 1) (B . 2) (C . 3)))


(equal (mapcan #'cdr (copy-tree '((1 2) (2 3) (3 4) (4 5))))
       '(2 3 4 5))

(equal (mapcan #'append
	       '((1 2 3) (4 5 6) (7 8 9))
	       '((a) (b c) (d e f))
	       (list (list 'x 'y 'z) (list 'y 'z) (list 'z)))
       '(1 2 3 a x y z 4 5 6 b c y z 7 8 9 d e f z))

(null (mapcan #'append '((1 2 3) (4 5 6) (7 8 9)) '((a) (b c)) '()))
(null (mapcan #'append '((1 2 3) (4 5 6) (7 8 9)) '() '((a) (b c))))
(null (mapcan #'append '() '((1 2 3) (4 5 6) (7 8 9)) '((a) (b c))))

(equal (mapcan #'list
	       (list 1 2 3 4 5)
	       (list 2 3 4 5 6)
	       (list 3 4 5 6 7)
	       (list 4 5 6 7 8))
       '(1 2 3 4 2 3 4 5 3 4 5 6 4 5 6 7 5 6 7 8))

(equal (mapcan #'(lambda (x y) (if (null x) nil (list x y)))
	       '(nil nil nil d e)
	       '(1 2 3 4 5 6))
       '(d 4 e 5))

(equal (mapcan #'(lambda (x) (and (numberp x) (list x)))
	       '(a 1 b c 3 4 d 5))
       '(1 3 4 5))


(equal (maplist #'identity '(a b c d))
       '((a b c d) (b c d) (c d) (d)))

(equal (maplist #'car '((1 2) (2 3) (3 4) (4 5)))
       '((1 2) (2 3) (3 4) (4 5)))

(equal (maplist #'list '(a b c) '(b c d) '(c d e))
       '(((a b c) (b c d) (c d e))
	 ((b c) (c d) (d e))
	 ((c) (d) (e))))

(equal (maplist #'append '(a b c) '(b c d) '(c d e))
       '((a b c b c d c d e) (b c c d d e) (c d e)))

(equal (maplist #'append '(a b c) '(b c) '(c))
       '((a b c b c c)))

(null (maplist #'append '() '(a b c) '(b c) '(c)))
(null (maplist #'append '(a b c) '() '(b c) '(c)))
(null (maplist #'append '(a b c) '(b c) '(c) '()))

(let ((x '((1 2) (2 3) (3 4) (4 5)))
      (y nil))
  (and (eq (mapl #'(lambda (a) (push (car a) y)) x) x)
       (equal y '((4 5) (3 4) (2 3) (1 2)))))

(let ((x nil))
  (and (null (mapl #'(lambda (&rest rest) (push rest x)) '() '(0) '(0 1)))
       (null x)))

(let ((x nil))
  (and (equal (mapl #'(lambda (&rest rest) (push rest x)) '(0) '() '(0 1))
	      '(0))
       (null x)))

(let ((x nil))
  (and (equal (mapl #'(lambda (&rest rest) (push rest x)) '(0) '(0 1) '())
	      '(0))
       (null x)))

(equal (mapcon #'car (copy-tree '((1 2) (2 3) (3 4) (4 5))))
       '(1 2 2 3 3 4 4 5))
       

(equal (mapcon #'list '(0 1 2 3) '(1 2 3 4) '(2 3 4 5) '(3 4 5 6))
       '((0 1 2 3) (1 2 3 4) (2 3 4 5) (3 4 5 6) (1 2 3) (2 3 4) (3 4 5)
	 (4 5 6) (2 3) (3 4) (4 5) (5 6) (3) (4) (5) (6)))


(null (mapcon #'list '() '(0 1 2 3) '(1 2 3 4) '(2 3 4 5) '(3 4 5 6)))
(null (mapcon #'list '(0 1 2 3) '() '(1 2 3 4) '(2 3 4 5) '(3 4 5 6)))
(null (mapcon #'list '(0 1 2 3) '(1 2 3 4) '() '(2 3 4 5) '(3 4 5 6)))
(null (mapcon #'list '(0 1 2 3) '(1 2 3 4) '(2 3 4 5) '() '(3 4 5 6)))
(null (mapcon #'list '(0 1 2 3) '(1 2 3 4) '(2 3 4 5) '(3 4 5 6) '()))


(let* ((x '((apple . 1) (orange . 2) (grapes . 3)))
       (y (acons 'plum 9 x)))
  (and (equal y '((plum . 9) (apple . 1) (orange . 2) (grapes . 3)))
       (eq x (cdr y))))

(equal (acons 'a '0 nil) '((a . 0)))

(equal (acons 'apple 1 (acons 'orange 2 (acons 'grapes '3 nil)))
       '((apple . 1) (orange . 2) (grapes . 3)))

(equal (acons nil nil nil) '((nil)))


(let ((alist '((x . 100) (y . 200) (z . 50))))
  (eq (assoc 'y alist) (cadr alist)))

(null (assoc 'no-such-key '((x . 100) (y . 200) (z . 50))))

(let ((alist '((x . 100) (y . 200) (z . 50))))
  (eq (assoc 'y alist :test #'eq) (cadr alist)))

(null (assoc 'key '()))
(null (assoc 'nil '(())))
(null (assoc 'nil '(() ())))
(let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
  (eq (assoc 'y alist) (car (cddddr alist))))
(let ((alist '((1 . a) nil (2 . b) (nil))))
  (eq (assoc 'nil alist) (cadddr alist)))

(let ((alist '((x . 100) (y . 200) (x . 100) (z . 50))))
  (eq (assoc 'y alist) (cadr alist)))

(let ((alist '((a . 1) (b . 2) (c . 3) (d . 4))))
  (eq (assoc 'a alist :test-not (complement #'eq)) (car alist)))

(let ((alist '((a . 1) (b . 2) (c . 3) (d . 4))))
  (null (assoc 'z alist :test-not (complement #'eq))))

(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (eq (assoc 'aa alist :key #'cadr :test #'eq) (car alist)))

(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (eq (assoc 'bb alist :key #'cadr :test #'eq) (cadr alist)))

(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (eq (assoc 'cc alist :key #'cadr :test #'eq) (caddr alist)))

(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (eq (assoc 'dd alist :key #'cadr :test #'eq) (cadddr alist)))

(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (null (assoc 'ee alist :key #'cadr :test #'eq)))

(let ((alist '(((a aa aaa)) nil ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (eq (assoc 'dd alist :key #'cadr :test #'eq) (car (cddddr alist))))

(let ((alist '(((a aa aaa)) ((b bb bbb)) nil ((c cc ccc)) ((d dd ddd)))))
  (eq (assoc 'dd alist :key #'cadr :test #'eq) (car (cddddr alist))))

(let ((alist '(((a aa aaa)) nil ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (eq (assoc 'dd alist :key #'cadr :test #'eq) (car (cddddr alist))))

(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)) nil)))
  (eq (assoc 'dd alist :key #'cadr :test #'eq) (cadddr alist)))



(let ((alist '((x . 100) (y . 200) (z . 50))))
  (eq (assoc-if #'(lambda (arg) (eq arg 'y)) alist) (cadr alist)))

(null (assoc-if #'consp '((x . 100) (y . 200) (z . 50))))

(null (assoc-if #'(lambda (x) (eq x 'key)) '()))
(null (assoc-if #'identity '(())))
(null (assoc-if #'identity '(() ())))
(let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
  (eq (assoc-if #'(lambda (arg) (eq arg 'y)) alist) (car (cddddr alist))))
(let ((alist '((1 . a) nil (2 . b) (nil))))
  (eq (assoc-if #'(lambda (arg) (null arg)) alist) (cadddr alist)))


(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (eq (assoc-if #'(lambda (x) (eq x 'aa)) alist :key #'cadr) (car alist)))

(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (eq (assoc-if  #'(lambda (x) (eq x 'bb)) alist :key #'cadr) (cadr alist)))

(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (null (assoc-if #'(lambda (x) (eq x 'ee)) alist :key #'cadr)))



(let ((alist '((x . 100) (y . 200) (z . 50))))
  (eq (assoc-if-not #'(lambda (arg) (not (eq arg 'y))) alist) (cadr alist)))

(null (assoc-if-not (complement #'consp) '((x . 100) (y . 200) (z . 50))))

(null (assoc-if-not #'(lambda (x) (not (eq x 'key))) '()))
(null (assoc-if-not #'identity '(())))
(null (assoc-if-not #'identity '(() ())))
(let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
  (eq (assoc-if-not #'(lambda (arg) (not (eq arg 'y))) alist)
      (car (cddddr alist))))
(let ((alist '((1 . a) nil (2 . b) (nil))))
  (eq (assoc-if-not #'identity alist) (cadddr alist)))

(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (eq (assoc-if-not #'(lambda (x) (not (eq x 'aa))) alist :key #'cadr)
      (car alist)))

(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (eq (assoc-if-not #'(lambda (x) (not (eq x 'bb))) alist :key #'cadr)
      (cadr alist)))

(let ((alist '(((a aa aaa)) ((b bb bbb)) ((c cc ccc)) ((d dd ddd)))))
  (null (assoc-if-not #'(lambda (x) (not (eq x 'ee))) alist :key #'cadr)))


(equal (copy-alist '((a . 10) (b . 100) (c . 1000)))
       '((a . 10) (b . 100) (c . 1000)))

(let* ((alist '((a . 10) (b . 100) (c . 1000)))
       (copy (copy-alist alist)))
  (and (not (eq alist copy))
       (not (eq (cdr alist) (cdr copy)))
       (not (eq (cddr alist) (cddr copy)))
       (not (eq (car alist) (car copy)))
       (not (eq (cadr alist) (cadr copy)))
       (not (eq (caddr alist) (caddr copy)))))

(let* ((alist '((a 10 x) (b 100 y) (c 1000 z)))
       (copy (copy-alist alist)))
  (and (not (eq alist copy))
       (not (eq (cdr alist) (cdr copy)))
       (not (eq (cddr alist) (cddr copy)))
       (not (eq (car alist) (car copy)))
       (not (eq (cadr alist) (cadr copy)))
       (not (eq (caddr alist) (caddr copy)))
       (eq (cdar alist) (cdar copy))
       (eq (cdadr alist) (cdadr copy))
       (eq (cdaddr alist) (cdaddr copy))))


(let* ((alist (pairlis '(x y z) '(xx yy zz) '((a . aa) (b . bb)))))
  (and (equal (assoc 'x alist) '(x . xx))
       (equal (assoc 'y alist) '(y . yy))
       (equal (assoc 'z alist) '(z . zz))
       (equal (assoc 'a alist) '(a . aa))
       (equal (assoc 'b alist) '(b . bb))
       (null (assoc 'key alist))))

(let* ((alist (pairlis '(x y z) '(xx yy zz))))
  (and (equal (assoc 'x alist) '(x . xx))
       (equal (assoc 'y alist) '(y . yy))
       (equal (assoc 'z alist) '(z . zz))
       (null (assoc 'key alist))))


(let ((alist '((x . 100) (y . 200) (z . 50))))
  (eq (rassoc '200 alist) (cadr alist)))

(null (rassoc 'no-such-datum '((x . 100) (y . 200) (z . 50))))

(let ((alist '((x . 100) (y . 200) (z . 50))))
  (eq (rassoc '200 alist :test #'=) (cadr alist)))

(null (rassoc 'key '()))
(null (rassoc 'nil '(())))
(null (rassoc 'nil '(() ())))

(let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
  (eq (rassoc '200 alist) (car (cddddr alist))))
(let ((alist '((1 . a) nil (2 . b) (nil))))
  (eq (rassoc 'nil alist) (cadddr alist)))

(let ((alist '((x . 100) (y . 200) (x . 100) (z . 50))))
  (eq (rassoc '200 alist) (cadr alist)))

(let ((alist '((a . 1) (b . 2) (c . 3) (d . 4))))
  (eq (rassoc '1 alist :test-not (complement #'=)) (car alist)))

(let ((alist '((a . 1) (b . 2) (c . 3) (d . 4))))
  (null (rassoc '9 alist :test-not (complement #'=))))

(let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
  (eq (rassoc 'aa alist :key #'car :test #'eq) (car alist)))

(let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
  (eq (rassoc 'ddd alist :key #'cadr :test #'eq) (cadddr alist)))

(let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
  (null (rassoc 'eee alist :key #'cadr :test #'eq)))

(let ((alist '((a aa aaa) nil (b bb bbb) (c cc ccc) (d dd ddd))))
  (eq (rassoc 'ddd alist :key #'cadr :test #'eq) (car (cddddr alist))))

(let ((alist '((a aa aaa) (b bb bbb) nil (c cc ccc) (d dd ddd))))
  (eq (rassoc 'ddd alist :key #'cadr :test #'eq) (car (cddddr alist))))

(let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd) nil)))
  (eq (rassoc 'ddd alist :key #'cadr :test #'eq) (car (cdddr alist))))

(let ((alist '((x . 100) (y . 200) (z . 50))))
  (eq (rassoc-if #'(lambda (arg) (= arg 200)) alist) (cadr alist)))

(null (rassoc-if #'consp '((x . 100) (y . 200) (z . 50))))

(null (rassoc-if #'(lambda (x) (eq x 'key)) '()))
(null (rassoc-if #'identity '(())))
(null (rassoc-if #'identity '(() ())))
(let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
  (eq (rassoc-if #'(lambda (arg) (= arg 200)) alist) (car (cddddr alist))))
(let ((alist '((1 . a) nil (2 . b) (nil))))
  (eq (rassoc-if #'(lambda (arg) (null arg)) alist) (cadddr alist)))

(let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
  (eq (rassoc-if #'(lambda (x) (eq x 'aaa)) alist :key #'cadr) (car alist)))

(let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
  (eq (rassoc-if  #'(lambda (x) (eq x 'bbb)) alist :key #'cadr) (cadr alist)))

(let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
  (null (rassoc-if #'(lambda (x) (eq x 'eee)) alist :key #'cadr)))


(let ((alist '((x . 100) (y . 200) (z . 50))))
  (eq (rassoc-if-not #'(lambda (arg) (not (= arg 200))) alist) (cadr alist)))

(null (rassoc-if-not (complement #'consp) '((x . 100) (y . 200) (z . 50))))

(null (rassoc-if-not #'(lambda (x) (not (eq x 'key))) '()))
(null (rassoc-if-not #'identity '(())))
(null (rassoc-if-not #'identity '(() ())))
(let ((alist '(nil nil nil (x . 100) (y . 200) (z . 50))))
  (eq (rassoc-if-not #'(lambda (arg) (not (= arg 200))) alist)
      (car (cddddr alist))))
(let ((alist '((1 . a) nil (2 . b) (nil))))
  (eq (assoc-if-not #'identity alist) (cadddr alist)))

(let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
  (eq (rassoc-if-not #'(lambda (x) (not (eq x 'aaa))) alist :key #'cadr)
      (car alist)))

(let ((alist '((a aa aaa) (b bb bbb) (c cc ccc) (d dd ddd))))
  (eq (rassoc-if-not #'(lambda (x) (not (eq x 'bbb))) alist :key #'cadr)
      (cadr alist)))

(let ((alist '(((a aa aaa) . 0) ((b bb bbb) . 1) ((c cc ccc) . 2))))
  (eq (rassoc-if-not #'(lambda (x) (not (= x '2))) alist :key #'1+)
      (cadr alist)))


(let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
  (multiple-value-bind (indicator value tail)
      (get-properties plist '(prop3 prop4 propX propY))
    (and (eq indicator 'prop3)
	 (eql value 3)
	 (eq tail (nthcdr 4 plist)))))

(multiple-value-bind (indicator value tail)
    (get-properties '(prop1 1 prop2 2 prop3 3 prop4 4)
		    '(propX propY propZ))
  (and (eq indicator nil)
       (eq value nil)
       (eq tail nil)))

(let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
  (multiple-value-bind (indicator value tail)
      (get-properties plist '(prop1))
    (and (eq indicator 'prop1)
	 (eql value 1)
	 (eq tail plist))))

(let ((plist '(prop1 1 nil nil prop2 2 prop3 3 prop4 4)))
  (multiple-value-bind (indicator value tail)
      (get-properties plist '(nil))
    (and (eq indicator nil)
	 (eql value nil)
	 (eq tail (cddr plist)))))

(let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
  (multiple-value-bind (indicator value tail)
      (get-properties plist '(prop3 prop4 propX propY prop1))
    (and (eq indicator 'prop1)
	 (eql value 1)
	 (eq tail plist))))


(let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
  (eql (getf plist 'prop1) 1))

(let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
  (eql (getf plist 'prop2) 2))

(let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
  (eql (getf plist 'prop3) 3))

(let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
  (eql (getf plist 'prop4) 4))

(let ((plist
       '(prop1 1 prop2 2 prop3 3 prop4 4 prop1 5 prop2 6 prop3 7 prop4 8)))
  (eql (getf plist 'prop1) 1))

(let ((plist
       '(prop1 1 prop2 2 prop3 3 prop4 4 prop1 5 prop2 6 prop3 7 prop4 8)))
  (eql (getf plist 'prop2) 2))

(let ((plist
       '(prop1 1 prop2 2 prop3 3 prop4 4 prop1 5 prop2 6 prop3 7 prop4 8)))
  (eql (getf plist 'prop3) 3))

(let ((plist
       '(prop1 1 prop2 2 prop3 3 prop4 4 prop1 5 prop2 6 prop3 7 prop4 8)))
  (eql (getf plist 'prop4) 4))

(let ((plist
       '(prop1 1 prop2 2 prop3 3 prop4 4 prop1 5 prop2 6 prop3 7 prop4 8)))
  (null (getf plist 'propX)))

(let ((plist '(prop1 1 prop2 2 prop3 3 prop4 4)))
  (eq (getf plist 'weird-property 'not-found) 'not-found))


(let ((plist (copy-list '(prop1 1 prop2 2 prop3 3 prop4 4))))
  (and (eql (setf (getf plist 'prop1) 9) 9)
       (eql (getf plist 'prop1) 9)))

(let ((plist nil))
  (and (eql (setf (getf plist 'prop1) 9) 9)
       (eql (getf plist 'prop1) 9)))

(let ((plist '()))
  (incf (getf plist 'count 0))
  (eql (getf plist 'count) 1))

(let ((x (list nil)))
  (and (eql (setf (getf (car x) 'prop1) 9) 9)
       (eql (getf (car x) 'prop1) 9)))


(let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
  (and (remf plist 'p2)
       (eq (getf plist 'p2 'not-found) 'not-found)))

(let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
  (and (remf plist 'p3)
       (eq (getf plist 'p3 'not-found) 'not-found)))

(let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
  (and (remf plist 'p4)
       (eq (getf plist 'p4 'not-found) 'not-found)))

(let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
  (and (null (remf plist 'pX))
       (equal plist '(p1 1 p2 2 p3 3 p4 4))))

(let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
  (and (remf plist 'p4)
       (remf plist 'p2)
       (remf plist 'p3)
       (remf plist 'p1)
       (null (remf plist 'pX))
       (null (remf plist 'p1))
       (null (remf plist 'p2))
       (null (remf plist 'p3))
       (null (remf plist 'p4))
       (null plist)))


(let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4 'p1 5 'p2 6 'p3 7 'p4 8)))
  (and (remf plist 'p4)
       (remf plist 'p2)
       (remf plist 'p3)
       (remf plist 'p1)
       (null (remf plist 'pX))
       (eql (getf plist 'p1) 5)
       (eql (getf plist 'p2) 6)
       (eql (getf plist 'p3) 7)
       (eql (getf plist 'p4) 8)))

(let ((plist (list 'p1 100 'p1 1 'p2 2 'p3 3 'p4 4)))
  (and (eql (getf plist 'p1) 100)
       (remf plist 'p1)
       (eql (getf plist 'p1) 1)
       (remf plist 'p1)
       (null (getf plist 'p1))))

(let ((plist (list 'p1 1 'p2 2 'p3 3 'p4 4)))
  (and (remf plist 'p4)
       (null (getf plist 'p4))))


(let ((list1 (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d"))
      (list2 (list 1 4 5 'b 'c 'd "a" "B" "c" "D")))
  (null (set-exclusive-or (intersection list1 list2) '(C B 4 1 1)))
  (null (set-exclusive-or (intersection list1 list2 :test 'equal)
			  '("B" C B 4 1 1)
			  :test 'equal))
  (null (set-exclusive-or (intersection list1 list2 :test #'equalp)
			  '("d" "C" "B" "A" C B 4 1 1)
			  :test #'equalp)))

(null (intersection '(0 1 2) '()))
(null (intersection '() '()))
(null (intersection '() '(0 1 2)))
(equal (intersection '(0) '(0)) '(0))
(equal (intersection '(0 1 2 3) '(2)) '(2))
(member 0 (intersection '(0 0 0 0 0) '(0 1 2 3 4 5)))
(null (set-exclusive-or (intersection '(0 1 2 3 4) '(4 3 2 1 0))
			'(4 3 2 1 0)))
(null (set-exclusive-or (intersection '(0 1 2 3 4) '(0 1 2 3 4))
			'(0 1 2 3 4)))
(null (set-exclusive-or (intersection '(0 1 2 3 4) '(4 3 2 1 0))
			'(0 1 2 3 4)))


(let ((list1 (list "A" "B" "C" "d" "e" "F" "G" "h"))
      (list2 (list "a" "B" "c" "D" "E" "F" "g" "h")))
  (null (set-exclusive-or (intersection list1 list2
					:test #'char=
					:key #'(lambda (x) (char x 0)))
			  '("B" "F" "h")
			  :test #'char=
			  :key #'(lambda (x) (char x 0)))))

(let ((list1 (list "A" "B" "C" "d" "e" "F" "G" "h"))
      (list2 (list "a" "B" "c" "D" "E" "F" "g" "h")))
  (null (set-exclusive-or (intersection list1 list2
					:test #'char-equal
					:key #'(lambda (x) (char x 0)))
			  '("A" "B" "C" "d" "e" "F" "G" "h")
			  :test #'char-equal
			  :key #'(lambda (x) (char x 0)))))

(let ((list1 (list "A" "B" "C" "d"))
      (list2 (list             "D" "E" "F" "g" "h")))
  (null (set-exclusive-or (intersection list1 list2
					:test #'char-equal
					:key #'(lambda (x) (char x 0)))
			  '("d")
			  :test #'char-equal
			  :key #'(lambda (x) (char x 0)))))




(let ((list1 (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d"))
      (list2 (list 1 4 5 'b 'c 'd "a" "B" "c" "D")))
  (null (set-exclusive-or (nintersection (copy-list list1) list2) '(C B 4 1 1)))
  (null (set-exclusive-or (nintersection (copy-list list1) list2 :test 'equal)
			  '("B" C B 4 1 1)
			  :test 'equal))
  (null (set-exclusive-or (nintersection (copy-list list1) list2 :test #'equalp)
			  '("d" "C" "B" "A" C B 4 1 1)
			  :test #'equalp)))

(null (nintersection (list 0 1 2) '()))
(null (nintersection '() '()))
(null (nintersection '() '(0 1 2)))
(equal (nintersection (list 0) '(0)) '(0))
(equal (nintersection (list 0 1 2 3) '(2)) '(2))
(member 0 (nintersection (list 0 0 0 0 0) '(0 1 2 3 4 5)))
(null (set-exclusive-or (nintersection (list 0 1 2 3 4) '(4 3 2 1 0))
			'(4 3 2 1 0)))
(null (set-exclusive-or (nintersection (list 0 1 2 3 4) '(0 1 2 3 4))
			'(0 1 2 3 4)))
(null (set-exclusive-or (nintersection (list 0 1 2 3 4) '(4 3 2 1 0))
			'(0 1 2 3 4)))


(let ((list1 (list "A" "B" "C" "d" "e" "F" "G" "h"))
      (list2 (list "a" "B" "c" "D" "E" "F" "g" "h")))
  (null (set-exclusive-or (nintersection list1 list2
					 :test #'char=
					 :key #'(lambda (x) (char x 0)))
			  '("B" "F" "h")
			  :test #'char=
			  :key #'(lambda (x) (char x 0)))))

(let ((list1 (list "A" "B" "C" "d" "e" "F" "G" "h"))
      (list2 (list "a" "B" "c" "D" "E" "F" "g" "h")))
  (null (set-exclusive-or (nintersection list1 list2
					:test #'char-equal
					:key #'(lambda (x) (char x 0)))
			  '("A" "B" "C" "d" "e" "F" "G" "h")
			  :test #'char-equal
			  :key #'(lambda (x) (char x 0)))))

(let ((list1 (list "A" "B" "C" "d"))
      (list2 (list             "D" "E" "F" "g" "h")))
  (null (set-exclusive-or (nintersection list1 list2
					:test #'char-equal
					:key #'(lambda (x) (char x 0)))
			  '("d")
			  :test #'char-equal
			  :key #'(lambda (x) (char x 0)))))


(let ((set '(a b c)))
  (eq (adjoin 'a set) set))

(let* ((set '(a b c))
       (new-set (adjoin 'x set)))
  (and (equal new-set '(x a b c))
       (eq set (cdr new-set))))

(equal (adjoin 1 nil) '(1))
(equal (adjoin nil nil) '(nil))
(equal (adjoin nil '(nil)) '(nil))
(let ((set '((test-item 1))))
  (equal (adjoin '(test-item 1) set) '((test-item 1) (test-item 1))))

(let ((set '((test-item 1))))
  (equal (adjoin '(test-item 1) set)
	 '((test-item 1) (test-item 1))))

(let ((set '((test-item 1))))
  (eq (adjoin '(test-item 1) set :test #'equal) set))

(let ((set '((test-item 1))))
  (eq (adjoin '(test-item) set :key #'car) set))

(let ((set '((test-item 1))))
  (eq (adjoin '(test-item) set :key #'car :test #'eq) set))

(let ((set '(("test-item" 1))))
  (eq (adjoin '("test-item") set :key #'car :test #'equal) set))


(let ((set '((test-item 1))))
  (eq (adjoin '(test-item 1) set :test-not (complement #'equal)) set))

(let ((set '((test-item 1))))
  (eq (adjoin '(test-item) set :test-not (complement #'eql) :key #'car) set))

(let ((set '((test-item 1))))
  (eq (adjoin '(test-item) set :key #'car :test-not (complement #'eq)) set))

(let ((set '(("test-item" 1))))
  (eq (adjoin '("test-item") set :key #'car :test-not (complement #'equal))
      set))


(let ((place nil))
  (and (equal (pushnew 'a place) '(a))
       (equal place '(a))))

(let ((place nil))
  (and (equal (pushnew 'a place) '(a))
       (equal place '(a))))

(let ((place '((a . 1) (b . 2))))
  (and (equal (pushnew '(b . 2) place :test #'= :key #'cdr) '((a . 1) (b . 2)))
       (equal place '((a . 1) (b . 2)))))

(let ((place '((a . 1) (b . 2))))
  (and (equal (pushnew '(b . 2) place :test-not (complement #'=) :key #'cdr)
	      '((a . 1) (b . 2)))
       (equal place '((a . 1) (b . 2)))))

(let ((place '((a . 1) (b . 2))))
  (and (eq (pushnew '(z . 2) place :test #'= :key #'cdr) place)
       (equal place '((a . 1) (b . 2)))))

(let ((place '((a . 1) (b . 2))))
  (and (eq (pushnew '(z . 2) place :test-not (complement #'=) :key #'cdr) place)
       (equal place '((a . 1) (b . 2)))))

(let ((place '("love" "peace")))
  (equal (pushnew "war" place :test #'equal) '("war" "love" "peace")))
       
(let ((place '("love" "peace")))
  (equal (pushnew "war" place :test-not (complement #'equal))
	 '("war" "love" "peace")))
       
(let ((place '("love" "peace")))
  (and (eq (pushnew "peace" place :test #'equal) place)
       (equal place '("love" "peace"))))
       
(let ((place '("love" "peace")))
  (and (eq (pushnew "peace" place :test-not (complement #'equal)) place)
       (equal place '("love" "peace"))))
       
(let ((place '(("love" . l) ("peace" . p))))
  (equal (pushnew '("war" . w) place :test #'equal :key #'car)
	 '(("war" . w) ("love" . l) ("peace" . p))))

(let ((place '(("love" . l) ("peace" . p))))
  (equal (pushnew '("war" . w) place :test-not (complement #'equal) :key #'car)
	 '(("war" . w) ("love" . l) ("peace" . p))))

(let ((place '(("love" . l) ("peace" . p))))
  (and (eq (pushnew '("love" . l) place :test #'equal :key #'car) place)
       (equal place '(("love" . l) ("peace" . p)))))

(let ((place '(("love" . l) ("peace" . p))))
  (and (eq (pushnew '("love" . l) place
		    :test-not (complement #'equal) :key #'car) place)
       (equal place '(("love" . l) ("peace" . p)))))

(let ((place '(("love" . l) ("peace" . p))))
  (and (eq (pushnew '("LOVE" . L) place :test #'equalp :key #'car) place)
       (equal place '(("love" . l) ("peace" . p)))))

(let ((place '(("love" . l) ("peace" . p))))
  (and (eq (pushnew '("LOVE" . L) place
		    :test-not (complement #'equalp) :key #'car) place)
       (equal place '(("love" . l) ("peace" . p)))))

(let ((place '(("love" . l) ("peace" . p))))
  (equal (pushnew '("LOVE" . L) place :test #'equal :key #'car)
	 '(("LOVE" . L) ("love" . l) ("peace" . p))))

(let ((place '(("love" . l) ("peace" . p))))
  (equal (pushnew '("LOVE" . L) place :test-not (complement #'equal) :key #'car)
	 '(("LOVE" . L) ("love" . l) ("peace" . p))))

(let ((list '((1) (1 2) (1 2 3))))
  (and (equal (pushnew '(1) list) '((1) (1) (1 2) (1 2 3)))
       (equal list '((1) (1) (1 2) (1 2 3)))))


(let* ((list '((1) (1 2) (1 2 3)))
       (original list))
  (and (equal (pushnew '(1) list :test #'equal) '((1) (1 2) (1 2 3)))
       (eq list original)))

(let* ((list '((1) (1 2) (1 2 3)))
       (original list))
  (and (equal (pushnew '(1) list :test #'equal :key nil) '((1) (1 2) (1 2 3)))
       (eq list original)))

(let ((list (copy-tree '(1 (2) 3 4))))
  (and (equal (pushnew 4 (cadr list)) '(4 2))
       (equal list '(1 (4 2) 3 4))))

(let ((list (copy-tree '(1 (2) 3 4))))
  (and (equal (pushnew 4 (cadr list) :key nil) '(4 2))
       (equal list '(1 (4 2) 3 4))))


(null (set-difference (set-difference '(1 2 3 4 5 6 7 8 9)
				      '(2 4 6 8))
		      '(1 3 5 7 9)))
(null (nset-difference (set-difference (list 1 2 3 4 5 6 7 8 9)
				       '(2 4 6 8))
		       '(1 3 5 7 9)))

(null (set-difference (set-difference '("1" "2" "3" "4" "5" "6" "7" "8" "9")
				      '("2" "4" "6" "8") :test #'equal)
		      '("1" "3" "5" "7" "9") :test-not (complement #'equal)))

(null (set-difference (set-difference '("1" "2" "3" "4" "5" "6" "7" "8" "9")
				      '("2" "4" "6" "8") :test #'equal)
		      '("1" "3" "5" "7" "9") :test-not (complement #'equal)))

(null (nset-difference (nset-difference
			(list "1" "2" "3" "4" "5" "6" "7" "8" "9")
			'("2" "4" "6" "8") :test #'equal)
		       '("1" "3" "5" "7" "9") :test-not (complement #'equal)))

(null (set-difference (set-difference '(("love") ("hate") ("peace") ("war"))
				      '(("love") ("peace"))
				      :key #'car
				      :test #'equal)
		      '(("hate") ("war"))
		      :key #'car
		      :test-not (complement #'equal)))

(null (nset-difference (nset-difference
			(list '("love") '("hate") '("peace") '("war"))
			'(("love") ("peace"))
			:key #'car
			:test #'equal)
		       '(("hate") ("war"))
		       :key #'car
		       :test-not (complement #'equal)))


(null (set-difference '() '()))
(null (set-difference '() '() :test #'equal :key 'identity))
(null (nset-difference '() '()))
(null (set-difference '() '(1 2 3)))
(null (set-difference '() '(1 2 3) :test #'equal :key 'identity))
(null (nset-difference '() '(1 2 3)))

(null (set-difference '(1 2 3 4) '(4 3 2 1)))
(null (nset-difference (list 1 2 3 4) '(4 3 2 1)))
(null (set-difference '(1 2 3 4) '(2 4 3 1)))
(null (nset-difference (list 1 2 3 4) '(2 4 3 1)))
(null (set-difference '(1 2 3 4) '(1 3 4 2)))
(null (nset-difference (list 1 2 3 4) '(1 3 4 2)))
(null (set-difference '(1 2 3 4) '(1 3 2 4)))
(null (nset-difference (list 1 2 3 4) '(1 3 2 4)))


(eq (set-difference (set-difference '(1 2 3) '())
		    '(1 2 3))
    '())
(eq (nset-difference (nset-difference (list 1 2 3) '())
		     '(1 2 3))
    '())

(eq (set-difference (set-difference '(1 2 3) '(1))
		    '(2 3))
    '())
(eq (nset-difference (nset-difference (list 1 2 3) '(1))
		     '(2 3))
    '())

(eq (set-difference (set-difference '(1 2 3) '(1 2))
		    '(3))
    '())
(eq (nset-difference (nset-difference (list 1 2 3) '(1 2))
		     '(3))
    '())


(null (set-exclusive-or (set-exclusive-or '(1 2 3) '(2 3 4))
			'(1 4)))
(null (nset-exclusive-or (nset-exclusive-or (list 1 2 3) '(2 3 4))
			 '(1 4)))
(null (set-exclusive-or (set-exclusive-or '(1 2 3) '(1 3))
			'(2)))
(null (nset-exclusive-or (nset-exclusive-or (list 1 2 3) '(1 3))
			 '(2)))
(null (set-exclusive-or '() '()))
(null (nset-exclusive-or '() '()))
(null (set-exclusive-or '(1 2 3) '(3 2 1)))
(null (nset-exclusive-or (list 1 2 3) '(3 2 1)))
(null (set-exclusive-or '(1 2 3) '(2 3 1)))
(null (nset-exclusive-or (list 1 2 3) '(2 3 1)))
(null (set-exclusive-or '(1 2 3) '(1 3 2)))
(null (nset-exclusive-or (list 1 2 3) '(1 3 2)))

(null (set-exclusive-or (set-exclusive-or '(1 2 3) '())
			'(3 2 1)))
(null (nset-exclusive-or (nset-exclusive-or (list 1 2 3) '())
			 '(3 2 1)))
(null (set-exclusive-or (set-exclusive-or '() '(1 2 3))
			'(2 1 3)))
(null (nset-exclusive-or (nset-exclusive-or '() '(1 2 3))
			 '(2 1 3)))

(null (set-exclusive-or '("car" "ship" "airplane" "submarine")
			'("car" "ship" "airplane" "submarine")
			:test #'equal))
(null (nset-exclusive-or (copy-list '("car" "ship" "airplane" "submarine"))
			'("car" "ship" "airplane" "submarine")
			:test #'equal))

(null (set-exclusive-or '("car" "ship" "airplane" "submarine")
			'("CAR" "SHIP" "AIRPLANE" "SUBMARINE")
			:test #'equalp))
(null (nset-exclusive-or (copy-list '("car" "ship" "airplane" "submarine"))
			'("CAR" "SHIP" "AIRPLANE" "SUBMARINE")
			:test #'equalp))

(null (set-exclusive-or '("car" "ship" "airplane" "submarine")
			'("ship" "airplane" "submarine" "car")
			:test-not (complement #'equal)))
(null (nset-exclusive-or (copy-list '("car" "ship" "airplane" "submarine"))
			 '("ship" "airplane" "submarine" "car")
			:test-not (complement #'equal)))

(null (set-exclusive-or '(("car") ("ship") ("airplane") ("submarine"))
			'(("car") ("ship") ("airplane") ("submarine"))
			:test #'string=
			:key #'car))
(null (nset-exclusive-or (copy-tree
			  '(("car") ("ship") ("airplane") ("submarine")))
			 '(("car") ("ship") ("airplane") ("submarine"))
			 :test #'string=
			 :key #'car))

(null (set-exclusive-or '(("car") ("ship") ("airplane") ("submarine"))
			'(("car") ("ship") ("airplane") ("submarine"))
			:test-not (complement #'string=)
			:key #'car))
(null (nset-exclusive-or (copy-tree
			  '(("car") ("ship") ("airplane") ("submarine")))
			 '(("car") ("ship") ("airplane") ("submarine"))
			 :test-not (complement #'string=)
			 :key #'car))

(null (set-exclusive-or
       (set-exclusive-or '("car" "ship" "airplane" "submarine")
			 '("car" "ship" "horse" "airplane" "submarine" "camel")
			 :test #'equal)
       '("camel" "horse")
       :test-not (complement #'equal)))

(null (nset-exclusive-or
       (nset-exclusive-or (list "car" "ship" "airplane" "submarine")
			  '("car" "ship" "horse" "airplane" "submarine" "camel")
			  :test #'equal)
       '("camel" "horse")
       :test-not (complement #'equal)))


(subsetp '(1 2 3) '(1 2 3))
(subsetp '(1 2 3) '(3 2 1))
(subsetp '(1 2 3) '(2 1 3))

(null (subsetp '(1 2 3 4) '(2 1 3)))
(subsetp '(1) '(2 1 3))
(subsetp '(1 2) '(1 2 3 4 5 6 7 8))
(subsetp '(1 2 3 4 5) '(8 7 6 5 4 3 2 1))
(null (subsetp '("car" "ship" "airplane" "submarine")
	       '("car" "ship" "horse" "airplane" "submarine" "camel")))

(subsetp '("car" "ship" "airplane" "submarine")
	 '("car" "ship" "horse" "airplane" "submarine" "camel")
	 :test #'equal)

(subsetp '("CAR" "SHIP" "AIRPLANE" "SUBMARINE")
	 '("car" "ship" "horse" "airplane" "submarine" "camel")
	 :test #'equalp)

(subsetp '(("car") ("ship") ("airplane") ("submarine"))
	 '(("car") ("ship") ("horse") ("airplane") ("submarine") ("camel"))
	 :test #'string=
	 :key #'car)


(null (union '() '()))
(null (nunion '() '()))
(null (set-difference (union '(1 2 3) '(2 3 4))
		      '(1 2 3 4)))
(null (set-difference (nunion (list 1 2 3) (list 2 3 4))
		      '(1 2 3 4)))

(null (set-difference (union '(1 2 3) '(1 2 3))
		      '(1 2 3)))
(null (set-difference (nunion (list 1 2 3) (list 1 2 3))
		      '(1 2 3)))

(null (set-difference (union '(1) '(3 2 1))
		      '(1 2 3)))
(null (set-difference (nunion (list 1) (list 3 2 1))
		      '(1 2 3)))

(null (set-difference (union '(1 2 3) '())
		      '(1 2 3)))
(null (set-difference (nunion (list 1 2 3) '())
		      '(1 2 3)))

(null (set-difference (union '() '(1 2 3))
		      '(1 2 3)))
(null (set-difference (nunion '() (list 1 2 3))
		      '(1 2 3)))

(null (set-difference (union '(1 2 3) '(2))
		      '(1 2 3)))
(null (set-difference (nunion (list 1 2 3) (list 2))
		      '(1 2 3)))


(null (set-difference (union '("Alpha" "Bravo" "Charlie")
			     '("Bravo" "Charlie" "Delta" "Echo")
			     :test #'string=)
		      '("Alpha" "Bravo" "Charlie" "Delta" "Echo")
		      :test-not (complement #'string=)))

(null (set-difference (nunion (list "Alpha" "Bravo" "Charlie")
			      (list "Bravo" "Charlie" "Delta" "Echo")
			      :test #'string=)
		      '("Alpha" "Bravo" "Charlie" "Delta" "Echo")
		      :test-not (complement #'string=)))

(null (set-difference
       (union (copy-tree '(("Alpha") ("Bravo") ("Charlie")))
	      (copy-tree '(("Bravo") ("Charlie") ("Delta") ("Echo")))
	      :test #'string=
	      :key #'car)
       '(("Alpha") ("Bravo") ("Charlie") ("Delta") ("Echo"))
       :test-not (complement #'string=)
       :key #'car))

(null (set-difference
       (nunion (copy-tree '(("Alpha") ("Bravo") ("Charlie")))
	       (copy-tree '(("Bravo") ("Charlie") ("Delta") ("Echo")))
	       :test #'string=
	       :key #'car)
       '(("Alpha") ("Bravo") ("Charlie") ("Delta") ("Echo"))
       :test-not (complement #'string=)
       :key #'car))


(null (set-difference (union '("Alpha" "Bravo" "Charlie")
			     '("BRAVO" "CHARLIE" "DELTA" "ECHO")
			     :test #'string-equal)
		      '("ALPHA" "BRAVO" "CHARLIE" "DELTA" "ECHO")
		      :test-not (complement #'string-equal)))

