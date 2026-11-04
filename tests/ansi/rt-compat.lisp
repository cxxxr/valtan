;-*-syntax:COMMON-LISP;Package:(RT :use "COMMON-LISP" :colon-mode :external)-*-

#|----------------------------------------------------------------------------|
 | Copyright 1990 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#

;This was the December 19, 1990 version of the regression tester, but
;has since been modified.

;; Valtan adapter for upstream DEFTEST records. Test bodies are not rewritten.
;; EQUALP-WITH-CASE below is copied verbatim from upstream rt.lsp; see
;; submodules/ansi-test/rt.lsp for the MIT copyright and permission notice.
(defpackage :regression-test (:use :cl) (:nicknames :rt)
  (:export :deftest :defnote :my-aref :*compile-tests*))
(in-package :regression-test)
(defvar *entries* nil)
(defvar *entries-tail* nil)
(defvar *entries-table* (make-hash-table :test 'equal))
(defvar *compile-tests* nil)
(defvar *test* nil)
(defun add-entry (name form vals props)
  (let ((entry (gethash name *entries-table*)))
    (if entry
        (setf (cdr entry) (list form vals props))
        (let* ((entry (list name form vals props))
               (tail (list entry)))
          (setf (gethash name *entries-table*) entry)
          (if *entries-tail*
              (setf (cdr *entries-tail*) tail)
              (setq *entries* tail))
          (setq *entries-tail* tail))))
  name)
(defmacro deftest (name &rest body)
  (let ((properties nil))
    (do () ((not (keywordp (car body))))
      (unless (cadr body) (error "Malformed DEFTEST properties for ~S" name))
      (setq properties (append properties (list (pop body) (pop body)))))
    `(add-entry ',name ',(car body) ',(cdr body) ',properties)))
(defmacro defnote (name &rest contents)
  (declare (ignore contents))
  `',name)
(defun name (entry) (first entry))
(defun form (entry) (second entry))
(defun vals (entry) (third entry))
(defun my-aref (a &rest args) (apply #'aref a args))
(defun my-row-major-aref (a index) (row-major-aref a index))
(defun equalp-with-case (x y)
  "Like EQUALP, but doesn't do case conversion of characters.
   Currently doesn't work on arrays of dimension > 2."
  (cond
   ((eq x y) t)
   ((consp x)
    (and (consp y)
         (equalp-with-case (car x) (car y))
         (equalp-with-case (cdr x) (cdr y))))
   ((and (typep x 'array)
         (= (array-rank x) 0))
    (equalp-with-case (my-aref x) (my-aref y)))
   ((typep x 'vector)
    (and (typep y 'vector)
         (let ((x-len (length x))
               (y-len (length y)))
           (and (eql x-len y-len)
                (loop
                 for i from 0 below x-len
                 for e1 = (my-aref x i)
                 for e2 = (my-aref y i)
                 always (equalp-with-case e1 e2))))))
   ((and (typep x 'array)
         (typep y 'array)
         (not (equal (array-dimensions x)
                     (array-dimensions y))))
    nil)

   ((typep x 'array)
    (and (typep y 'array)
         (let ((size (array-total-size x)))
           (loop for i from 0 below size
                 always (equalp-with-case (my-row-major-aref x i)
                                          (my-row-major-aref y i))))))
   ((typep x 'pathname)
    (equal x y))
   (t (eql x y))))


(in-package :cl-user)
