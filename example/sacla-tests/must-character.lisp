;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: must-character.lisp,v 1.6 2004/02/20 07:23:42 yuji Exp $
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

(char= #\d #\d)
(not (char= #\A #\a))
(not (char= #\d #\x))
(not (char= #\d #\D))
(not (char/= #\d #\d))
(char/= #\d #\x)
(char/= #\d #\D)
(char= #\d #\d #\d #\d)
(not (char/= #\d #\d #\d #\d))
(not (char= #\d #\d #\x #\d))
(not (char/= #\d #\d #\x #\d))
(not (char= #\d #\y #\x #\c))
(char/= #\d #\y #\x #\c)
(not (char= #\d #\c #\d))
(not (char/= #\d #\c #\d))
(char< #\d #\x)
(char<= #\d #\x)
(not (char< #\d #\d))
(char<= #\d #\d)
(char< #\a #\e #\y #\z)
(char<= #\a #\e #\y #\z)
(not (char< #\a #\e #\e #\y))
(char<= #\a #\e #\e #\y)
(char> #\e #\d)
(char>= #\e #\d)
(char> #\d #\c #\b #\a)
(char>= #\d #\c #\b #\a)
(not (char> #\d #\d #\c #\a))
(char>= #\d #\d #\c #\a)
(not (char> #\e #\d #\b #\c #\a))
(not (char>= #\e #\d #\b #\c #\a))
(char-equal #\A #\a)
(equal (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char-lessp)
       '(#\A #\a #\b #\B #\c #\C))

(char= #\a)
(char= #\a #\a)
(char= #\a #\a #\a)
(char= #\a #\a #\a #\a)
(char= #\a #\a #\a #\a #\a)
(char= #\a #\a #\a #\a #\a #\a)
(let ((c #\z))
  (and (eq c c)
       (char= c c)))
(not (char= #\Z #\z))
(not (char= #\z #\z #\z #\a))
(not (char= #\a #\z #\z #\z #\a))
(not (char= #\z #\i #\z #\z))
(not (char= #\z #\z #\Z #\z))

(char/= #\a)
(char/= #\a #\b)
(char/= #\a #\b #\c)
(char/= #\a #\b #\c #\d)
(char/= #\a #\b #\c #\d #\e)
(char/= #\a #\b #\c #\d #\e #\f)
(let ((c #\z))
  (and (eq c c)
       (not (char/= c c))))
(char/= #\Z #\z)
(not (char/= #\z #\z #\z #\a))
(not (char= #\a #\z #\z #\z #\a))
(not (char= #\z #\i #\z #\z))
(not (char= #\z #\z #\Z #\z))
(not (char/= #\a #\a #\b #\c))
(not (char/= #\a #\b #\a #\c))
(not (char/= #\a #\b #\c #\a))

(char< #\a)
(char< #\a #\z)
(char< #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
       #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)
(not (char< #\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n
	    #\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c #\b #\a))
(char< #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
       #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)
(not (char< #\Z #\Y #\X #\W #\V #\U #\T #\S #\R #\Q #\P #\O #\N
	    #\M #\L #\K #\J #\I #\H #\G #\F #\E #\D #\C #\B #\A))
(char< #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
(not (char< #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0))
(or (char< #\9 #\A)
    (char< #\Z #\0))
(or (char< #\9 #\a)
    (char< #\z #\0))
(not (char< #\a #\a #\b #\c))
(not (char< #\a #\b #\a #\c))
(not (char< #\a #\b #\c #\a))
(not (char< #\9 #\0))

(char> #\a)
(not (char> #\a #\z))
(char> #\z #\a)
(not (char> #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
	    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(char> #\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n
       #\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c #\b #\a)
(not (char> #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
	    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
(char> #\Z #\Y #\X #\W #\V #\U #\T #\S #\R #\Q #\P #\O #\N
       #\M #\L #\K #\J #\I #\H #\G #\F #\E #\D #\C #\B #\A)
(not (char> #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(char> #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0)
(or (char> #\A #\9)
    (char> #\0 #\Z))
(or (char> #\a #\9)
    (char> #\0 #\z))
(not (char> #\a #\a #\b #\c))
(not (char> #\a #\b #\a #\c))
(not (char> #\a #\b #\c #\a))
(char> #\9 #\0)

(char<= #\a)
(char<= #\a #\z)
(char<= #\a #\a)
(char<= #\Z #\Z)
(char<= #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
	#\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)
(char<= #\a #\a #\b #\b #\c #\c #\d #\d #\e #\e #\f #\f #\g #\g
        #\h #\h #\i #\i #\j #\j #\k #\k #\l #\l #\m #\m
	#\n #\n #\o #\o #\p #\p #\q #\q #\r #\r #\s #\s
	#\t #\t #\u #\u #\v #\v #\w #\w #\x #\x #\y #\y #\z #\z)
(not (char<= #\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n
	     #\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c #\b #\a))
(char<= #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
	#\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)
(char<= #\A #\B #\B #\C #\D #\E #\E #\F #\G #\H #\I #\I #\J #\K #\L #\M
	#\N #\N #\O #\P #\Q #\R #\S #\T #\T #\U #\V #\W #\X #\Y #\Z)
(not (char<= #\Z #\Y #\X #\W #\V #\U #\T #\S #\R #\Q #\P #\O #\N
	     #\M #\L #\K #\J #\I #\H #\G #\F #\E #\D #\C #\B #\A))
(char<= #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
(char<= #\0 #\1 #\2 #\2 #\3 #\3 #\3 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\9)
(not (char<= #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0))
(or (char<= #\9 #\A)
    (char<= #\Z #\0))
(or (char<= #\9 #\a)
    (char<= #\z #\0))
(char<= #\a #\a #\b #\c)
(not (char<= #\a #\b #\a #\c))
(not (char<= #\a #\b #\c #\a))
(not (char<= #\9 #\0))

(char>= #\a)
(not (char>= #\a #\z))
(char>= #\z #\a)
(char>= #\a #\a)
(char>= #\Z #\Z)
(not (char>= #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
	     #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(char>= #\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n
	#\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c #\b #\a)
(char>= #\z #\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n #\n
	#\m #\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c #\b #\a #\a)
(not (char>= #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
	     #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
(char>= #\Z #\Y #\X #\W #\V #\U #\T #\S #\R #\Q #\P #\O #\N
	#\M #\L #\K #\J #\I #\H #\G #\F #\E #\D #\C #\B #\A)
(char>= #\Z #\Y #\X #\W #\V #\U #\U #\T #\T #\S #\S #\R #\Q #\P #\O #\N
	#\M #\L #\K #\J #\I #\H #\H #\G #\G #\F #\F #\E #\D #\C #\B #\A)
(not (char>= #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(char>= #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0)
(char>= #\9 #\8 #\8 #\8 #\7 #\6 #\5 #\4 #\3 #\3 #\3 #\2 #\1 #\0)
(or (char>= #\A #\9)
    (char>= #\0 #\Z))
(or (char>= #\a #\9)
    (char>= #\0 #\z))
(char>= #\c #\b #\a #\a)
(not (char>= #\c #\b #\a #\a #\b #\c))
(not (char>= #\c #\b #\a #\c))
(not (char>= #\c #\b #\c #\a))
(char>= #\9 #\0)
(not (char>= #\0 #\9))


(char-equal #\a)
(char-equal #\a #\a)
(char-equal #\a #\a #\a)
(char-equal #\a #\a #\a #\a)
(char-equal #\a #\a #\a #\a #\a)
(char-equal #\a #\a #\a #\a #\a #\a)
(char-equal #\a #\A)
(char-equal #\a #\A #\a)
(char-equal #\a #\a #\A #\a)
(char-equal #\a #\a #\a #\A #\a)
(char-equal #\a #\a #\a #\a #\A #\a)
(let ((c #\z))
  (and (eq c c)
       (char-equal c c)))
(char-equal #\Z #\z)
(not (char-equal #\z #\z #\z #\a))
(not (char-equal #\a #\z #\z #\z #\a))
(not (char-equal #\z #\i #\z #\z))
(char-equal #\z #\z #\Z #\z)
(char-equal #\a #\A #\a #\A #\a #\A #\a #\A #\a #\A)


(char-not-equal #\a)
(char-not-equal #\a #\b)
(char-not-equal #\a #\b #\c)
(char-not-equal #\a #\b #\c #\d)
(char-not-equal #\a #\b #\c #\d #\e)
(char-not-equal #\a #\b #\c #\d #\e #\f)
(let ((c #\z))
  (and (eq c c)
       (not (char-not-equal c c))))
(not (char-not-equal #\Z #\z))
(not (char-not-equal #\z #\z #\z #\a))
(not (char= #\a #\z #\z #\z #\a))
(not (char= #\z #\i #\z #\z))
(not (char= #\z #\z #\Z #\z))
(not (char-not-equal #\a #\a #\b #\c))
(not (char-not-equal #\a #\b #\a #\c))
(not (char-not-equal #\a #\b #\c #\a))
(not (char-not-equal #\a #\A #\a #\A))


(char-lessp #\a)
(char-lessp #\a #\z)
(char-lessp #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
       #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)
(not (char-lessp #\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n
	    #\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c #\b #\a))
(char-lessp #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
       #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)
(not (char-lessp #\Z #\Y #\X #\W #\V #\U #\T #\S #\R #\Q #\P #\O #\N
	    #\M #\L #\K #\J #\I #\H #\G #\F #\E #\D #\C #\B #\A))
(char-lessp #\a #\B #\c #\D #\e #\F #\g #\H #\i #\J #\k #\L #\m
       #\N #\o #\P #\q #\R #\s #\T #\u #\V #\w #\X #\y #\Z)
(char-lessp #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
(not (char-lessp #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0))
(or (char-lessp #\9 #\A)
    (char-lessp #\Z #\0))
(or (char-lessp #\9 #\a)
    (char-lessp #\z #\0))
(not (char-lessp #\a #\a #\b #\c))
(not (char-lessp #\a #\b #\a #\c))
(not (char-lessp #\a #\b #\c #\a))
(not (char-lessp #\9 #\0))
(and (char-lessp #\a #\Z)
     (char-lessp #\A #\z))


(char-greaterp #\a)
(not (char-greaterp #\a #\z))
(char-greaterp #\z #\a)
(not (char-greaterp #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
	    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(char-greaterp #\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n
       #\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c #\b #\a)
(not (char-greaterp #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
	    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
(char-greaterp #\Z #\Y #\X #\W #\V #\U #\T #\S #\R #\Q #\P #\O #\N
       #\M #\L #\K #\J #\I #\H #\G #\F #\E #\D #\C #\B #\A)
(char-greaterp #\z #\Y #\x #\W #\v #\U #\t #\S #\r #\Q #\p #\O #\n
       #\M #\l #\K #\j #\I #\h #\G #\f #\E #\d #\C #\b #\A)
(not (char-greaterp #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(char-greaterp #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0)
(or (char-greaterp #\A #\9)
    (char-greaterp #\0 #\Z))
(or (char-greaterp #\a #\9)
    (char-greaterp #\0 #\z))
(not (char-greaterp #\a #\a #\b #\c))
(not (char-greaterp #\a #\b #\a #\c))
(not (char-greaterp #\a #\b #\c #\a))
(char-greaterp #\9 #\0)
(and (char-greaterp #\z #\A)
     (char-greaterp #\Z #\a))



(char-not-greaterp #\a)
(char-not-greaterp #\a #\z)
(char-not-greaterp #\a #\a)
(char-not-greaterp #\Z #\Z)
(char-not-greaterp
 #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
 #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)
(char-not-greaterp
 #\a #\a #\b #\b #\c #\c #\d #\d #\e #\e #\f #\f #\g #\g
 #\h #\h #\i #\i #\j #\j #\k #\k #\l #\l #\m #\m
 #\n #\n #\o #\o #\p #\p #\q #\q #\r #\r #\s #\s
 #\t #\t #\u #\u #\v #\v #\w #\w #\x #\x #\y #\y #\z #\z)
(char-not-greaterp
 #\a #\A #\b #\B #\c #\C #\d #\D #\e #\E #\f #\F #\g #\G
 #\h #\H #\i #\I #\j #\J #\k #\K #\l #\L #\m #\M
 #\n #\N #\o #\O #\p #\P #\q #\Q #\r #\R #\s #\S
 #\t #\T #\u #\U #\v #\V #\w #\W #\x #\X #\y #\Y #\z #\z)
(not (char-not-greaterp
      #\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n
      #\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c #\b #\a))
(char-not-greaterp
 #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
 #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)
(char-not-greaterp
 #\A #\B #\B #\C #\D #\E #\E #\F #\G #\H #\I #\I #\J #\K #\L #\M
 #\N #\N #\O #\P #\Q #\R #\S #\T #\T #\U #\V #\W #\X #\Y #\Z)
(not (char-not-greaterp
      #\Z #\Y #\X #\W #\V #\U #\T #\S #\R #\Q #\P #\O #\N
      #\M #\L #\K #\J #\I #\H #\G #\F #\E #\D #\C #\B #\A))
(char-not-greaterp #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
(char-not-greaterp #\0 #\1 #\2 #\2 #\3 #\3 #\3 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\9)
(not (char-not-greaterp #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0))
(or (char-not-greaterp #\9 #\A)
    (char-not-greaterp #\Z #\0))
(or (char-not-greaterp #\9 #\a)
    (char-not-greaterp #\z #\0))
(char-not-greaterp #\a #\a #\b #\c)
(not (char-not-greaterp #\a #\b #\a #\c))
(not (char-not-greaterp #\a #\b #\c #\a))
(not (char-not-greaterp #\9 #\0))
(and (char-not-greaterp #\A #\z)
     (char-not-greaterp #\a #\Z))


(char-not-lessp #\a)
(not (char-not-lessp #\a #\z))
(char-not-lessp #\z #\a)
(char-not-lessp #\a #\a)
(char-not-lessp #\Z #\Z)
(not (char-not-lessp #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
		     #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(char-not-lessp #\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n
		#\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c #\b #\a)
(char-not-lessp #\z #\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n #\n
		#\m #\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c #\b #\a #\a)
(not (char-not-lessp #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
		     #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
(char-not-lessp #\Z #\Y #\X #\W #\V #\U #\T #\S #\R #\Q #\P #\O #\N
		#\M #\L #\K #\J #\I #\H #\G #\F #\E #\D #\C #\B #\A)
(char-not-lessp #\Z #\Y #\X #\W #\V #\U #\U #\T #\T #\S #\S #\R #\Q #\P #\O #\N
		#\M #\L #\K #\J #\I #\H #\H #\G #\G #\F #\F #\E #\D #\C #\B #\A)
(char-not-lessp #\z #\Z #\y #\x #\w #\V #\v #\u #\t #\s #\r #\q #\p #\o #\n #\n
		#\m #\M #\l #\k #\K #\j #\i #\h #\g #\f #\e #\d #\c #\b #\A #\a)
(not (char-not-lessp #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(char-not-lessp #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0)
(char-not-lessp #\9 #\8 #\8 #\8 #\7 #\6 #\5 #\4 #\3 #\3 #\3 #\2 #\1 #\0)
(or (char-not-lessp #\A #\9)
    (char-not-lessp #\0 #\Z))
(or (char-not-lessp #\a #\9)
    (char-not-lessp #\0 #\z))
(char-not-lessp #\c #\b #\a #\a)
(not (char-not-lessp #\c #\b #\a #\a #\b #\c))
(not (char-not-lessp #\c #\b #\a #\c))
(not (char-not-lessp #\c #\b #\c #\a))
(char-not-lessp #\9 #\0)
(not (char-not-lessp #\0 #\9))
(and (char-not-lessp #\z #\A)
     (char-not-lessp #\Z #\a))

(char= (character #\a) #\a)
(char= (character #\b) #\b)
(char= (character #\Space) #\Space)
(char= (character "a") #\a)
(char= (character "X") #\X)
(char= (character "z") #\z)
(char= (character 'a) #\A)
(char= (character '\a) #\a)

(alpha-char-p #\a)
(every #'alpha-char-p '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
			#\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(every #'alpha-char-p '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
			#\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
(notany #'alpha-char-p '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(not (alpha-char-p #\Newline))

(alphanumericp #\Z)
(alphanumericp #\9)
(every #'alphanumericp '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
			 #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(every #'alphanumericp '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
			 #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
(every #'alphanumericp '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(not (alphanumericp #\Newline))
(not (alphanumericp #\#))

(char= (digit-char 0) #\0)
(char= (digit-char 10 11) #\A)
(null (digit-char 10 10))
(char= (digit-char 7) #\7)
(null (digit-char 12))
(char= (digit-char 12 16) #\C)
(null (digit-char 6 2))
(char= (digit-char 1 2) #\1)
(char= (digit-char 35 36) #\Z)

(do ((radix 2 (1+ radix)))
    ((= radix 37) t)
  (unless (dotimes (i radix t)
	    (unless (char= (digit-char i radix)
			   (svref #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
				    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
				    #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
				    #\U #\V #\W #\X #\Y #\Z) i))
	      (return nil)))
    (return nil)))


(= (digit-char-p #\0) 0)
(= (digit-char-p #\5) 5)
(not (digit-char-p #\5 2))
(not (digit-char-p #\A))
(not (digit-char-p #\a))
(= (digit-char-p #\A 11) 10)
(= (digit-char-p #\a 11) 10)
(equal (mapcar #'(lambda (radix) 
		   (map 'list #'(lambda (x) (digit-char-p x radix)) 
			"059AaFGZ"))
	       '(2 8 10 16 36))
       '((0 NIL NIL NIL NIL NIL NIL NIL)
	 (0 5 NIL NIL NIL NIL NIL NIL)
	 (0 5 9 NIL NIL NIL NIL NIL)
	 (0 5 9 10 10 15 NIL NIL)
	 (0 5 9 10 10 15 16 35)))

(do ((radix 2 (1+ radix)))
    ((= radix 37) t)
  (unless (dotimes (i radix t)
	    (unless (= (digit-char-p (schar
				      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" i)
				     radix)
		       i)
	      (return nil)))
    (return nil)))

(do ((radix 2 (1+ radix)))
    ((= radix 37) t)
  (unless (dotimes (i radix t)
	    (unless (= (digit-char-p (schar
				      "0123456789abcdefghijklmnopqrstuvwxyz" i)
				     radix)
		       i)
	      (return nil)))
    (return nil)))


(graphic-char-p #\G)
(graphic-char-p #\#)
(graphic-char-p #\Space)
(not (graphic-char-p #\Newline))

(standard-char-p #\a)
(standard-char-p #\z)
(standard-char-p #\Newline)
(every #'standard-char-p " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
")


(char= (char-upcase #\a) #\A)
(char= (char-upcase #\A) #\A)
(char= (char-upcase #\-) #\-)
(char= (char-downcase #\A) #\a)
(char= (char-downcase #\a) #\a)
(char= (char-downcase #\-) #\-)
(not (upper-case-p #\a))
(upper-case-p #\A)
(not (upper-case-p #\-))
(not (lower-case-p #\A))
(lower-case-p #\a)
(not (lower-case-p #\-))
(both-case-p #\a)
(both-case-p #\A)
(not (both-case-p #\-))

(let ((chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
")
      c)
  (dotimes (i (length chars) t)
    (setq c (schar chars i))
    (cond
     ((upper-case-p c)
      (unless (and (both-case-p c)
		   (not (lower-case-p c))
		   (char= (char-upcase c) c)
		   (not (char= (char-downcase c) c)))
	(return nil)))
     ((lower-case-p c)
      (unless (and (both-case-p c)
		   (char= (char-downcase c) c)
		   (not (char= (char-upcase c) c)))
	(return nil)))
     (t
      (unless (and (not (upper-case-p c))
		   (not (lower-case-p c))
		   (not (both-case-p c))
		   (char= (char-upcase c) c)
		   (char= (char-downcase c) c))
	(return nil))))))

(every (complement #'minusp)
       (map 'list #'char-code " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
"))

(every (complement #'minusp)
       (map 'list #'char-int " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
"))


(every #'characterp
       (map 'list #'code-char
	    (map 'list #'char-code " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
")))

(dotimes (i char-code-limit t)
  (unless (or (null (code-char i)) (characterp (code-char i)))
    (return nil)))

(char= #\ (name-char (char-name #\ )))
(char= #\Space (name-char (char-name #\Space)))
(char= #\Newline (name-char (char-name #\Newline)))
