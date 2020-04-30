;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: x-sequence.lisp,v 1.11 2004/02/20 07:23:42 yuji Exp $
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

(HANDLER-CASE (PROGN (SUBSEQ '(0 1 . 2) 1))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (SUBSEQ '#1=(0 1 . #1#) 3))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (MAP 'LIST #'+ '(0 1 . 2) '(1 0 -1 -2)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(HANDLER-CASE (PROGN (REDUCE #'LIST '(1 . 2)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REDUCE #'LIST '#1=(1 2 3 4 5 6 7 8 9 . #1#)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REDUCE #'LIST '(A . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REDUCE #'LIST '(A B . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REDUCE #'LIST '(A B C . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REDUCE #'LIST '(A B C D . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (REDUCE #'LIST '(A B C D E . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (REDUCE #'LIST '(A B C D E F . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (REDUCE #'LIST '(A B C D E F G . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (REDUCE #'LIST '(A B C D E F G H . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (REDUCE #'LIST '(A B C D E F G H I . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (REDUCE #'LIST '(A B C D E F G H I J . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN
      (REDUCE #'LIST '(A B C D E F G H I J K . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))


(HANDLER-CASE (PROGN (LENGTH '(A . B)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (LENGTH '#1=(0 1 . #1#)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REDUCE #'LIST '(1 . 2)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REDUCE #'LIST '#1=(1 2 3 4 5 6 7 8 9 . #1#)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (LENGTH '(A . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (LENGTH '(A B . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (LENGTH '(A B C . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (LENGTH '(A B C D . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (LENGTH '(A B C D E . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (LENGTH '(A B C D E F . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (LENGTH '(A B C D E F G . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (LENGTH '(A B C D E F G H . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (LENGTH '(A B C D E F G H I . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (LENGTH '(A B C D E F G H I J . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (LENGTH '(A B C D E F G H I J K . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))


(HANDLER-CASE (PROGN (REVERSE '(A . B)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REVERSE '#1=(0 1 . #1#)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REVERSE '#1=(1 2 3 4 5 6 7 8 9 . #1#)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REVERSE '(A . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REVERSE '(A B . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REVERSE '(A B C . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REVERSE '(A B C D . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REVERSE '(A B C D E . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REVERSE '(A B C D E F . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (REVERSE '(A B C D E F G . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (REVERSE '(A B C D E F G H . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (REVERSE '(A B C D E F G H I . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (REVERSE '(A B C D E F G H I J . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN (REVERSE '(A B C D E F G H I J K . #1=(1 2 3 4 5 6 7 8 9 . #1#))))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))


(HANDLER-CASE (PROGN (NREVERSE (CONS 'A 'B)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE
    (PROGN
      (LET ((A (LIST 0 1)))
	(SETF (CDDR A) A)
	(NREVERSE A)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(HANDLER-CASE
    (PROGN
      (LET ((A (LIST 0 1 2 3)))
	(SETF (CDR (NTHCDR 3 A)) A)
	(NREVERSE A)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(HANDLER-CASE
    (PROGN
      (LET ((A (LIST 0 1 2 3 4)))
	(SETF (CDR (NTHCDR 4 A)) (CDDR A))
	(NREVERSE A)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(HANDLER-CASE (PROGN (FIND 'ITEM '(A B . C)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (FIND-IF (CONSTANTLY NIL) '(A B . C)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (FIND-IF-NOT (CONSTANTLY T) '(A B . C)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(HANDLER-CASE (PROGN (FIND 'ITEM '#1=(A B . #1#)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (FIND-IF (CONSTANTLY NIL) '#1=(A B . #1#)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (FIND-IF-NOT (CONSTANTLY T) '#1=(A B . #1#)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))

(HANDLER-CASE (PROGN (POSITION 'ITEM '#1=(A B . #1#)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (POSITION-IF (CONSTANTLY NIL) '#1=(A B . #1#)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))
(HANDLER-CASE (PROGN (POSITION-IF-NOT (CONSTANTLY T) '#1=(A B . #1#)))
  (TYPE-ERROR NIL T)
  (ERROR NIL NIL)
  (:NO-ERROR (&REST REST) (DECLARE (IGNORE REST)) NIL))