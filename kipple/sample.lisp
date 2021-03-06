(in-package :cl-user)

#|
;; TODO: in-packageをファイル単位で有効にする

;; バッククォートのテスト 今の実装は不十分なので二重のバッククォートは使えずこのテストもこける
;; (DEFMACRO FOO (X)
;;   (SYSTEM:QUASIQUOTE
;;    (SYSTEM:QUASIQUOTE
;;     (LIST (SYSTEM:UNQUOTE (+ 1 2))
;;           (SYSTEM:UNQUOTE '(SYSTEM:UNQUOTE X))))))
;; (defmacro foo (x)
;;   ``(list ,(+ 1 2)
;;           ,',x))
;; (ffi:console.log (foo 10))
;; (defmacro defmacro-macrolet-test (&body body)
;;   (let ((c 10))
;;     `(macrolet ((foo (xxx)
;;                   `(+ ,xxx ,',c)))
;;        ,@body)))
;; (assert (= 20 (defmacro-macrolet-test (foo 10))))

(ffi:console.log "hello world")
(ffi:console.log (list 1 2 3))

(dotimes (i 10)
  (ffi:console.log i))

(dolist (i (list 1 2 3))
  (ffi:console.log i))

(dolist (x (list 'foo 'bar 'baz))
  (ffi:console.log "case" x
                   (case x
                     (foo 100)
                     (bar 200)
                     (otherwise 300))))

(dolist (k '(1 2 3 :four v () t 'other))
  (ffi:console.log
   (case k ((1 2) 'clause1)
         (3 'clause2)
         (nil 'no-keys-so-never-seen)
         ((nil) 'nilslot)
         ((:four v) 'clause4)
         ((t) 'tslot)
         (otherwise 'others))))

(defun fact (n)
  (cond ((= n 0) 1)
        (t (* n (fact (- n 1))))))

(ffi:console.log (fact 5))

(multiple-value-bind (form expanded-p)
    (values 0 nil)
  (assert (eq 0 form)))

(write-line "==================== Number ====================")
(assert (numberp 1))
(assert (not (numberp "foo")))
(assert (numberp 1.2))
(assert (not (numberp nil)))
(assert (integerp 1))
(assert (not (integerp "foo")))
(assert (not (integerp 1.2)))
(assert (not (integerp nil)))
(assert (= 3 3))
(assert (not (= 3 4)))
(assert (< 3 4))
(assert (not (< 3 3)))
(assert (not (< 3 2)))
(assert (<= 3 3))
(assert (<= 3 4))
(assert (not (<= 3 2)))
(assert (> 3 2))
(assert (not (> 3 3)))
(assert (not (> 2 3)))
(assert (>= 4 3))
(assert (>= 3 3))
(assert (not (>= 2 3)))
(assert (= 3 (+ 1 2)))
(assert (= (rem -1 5) -1))
(assert (= (rem 13 4) 1))
(assert (= (rem -13 4) -1))
(assert (= (rem 13 -4) 1))
(assert (= (rem -13 -4) -1))
(assert (evenp 0))
(assert (not (evenp 1)))
(assert (evenp 2))
(assert (not (evenp 3)))
(assert (evenp 4))
(multiple-value-bind (a b)
    (floor 5 2)
  (assert (= a 2))
  (assert (= b 1)))

(write-line "==================== Character ====================")
(assert (characterp #\a))
(assert (not (characterp "a")))
(assert (eql #\a (code-char 97)))
(assert (= 97 (char-code #\a)))
(assert (char= #\a #\a))
(assert (not (char= #\A #\a)))
(assert (not (char= #\a #\b)))
(assert (not (char/= #\a #\a)))
(assert (char/= #\a #\A))
(assert (char/= #\a #\b))
(assert (char< #\a #\b))
(assert (not (char< #\a #\a)))
(assert (char< #\a #\b #\c))
(assert (not (char< #\a #\c #\b)))
(assert (char<= #\a #\a))
(assert (char<= #\a #\b))
(assert (not (char<= #\c #\b)))
(assert (char<= #\a #\a #\b))
(assert (not (char<= #\a #\z #\b)))
(assert (not (char<= #\z #\z #\b)))
(assert (char> #\b #\a))
(assert (not (char> #\b #\b)))
(assert (char> #\c #\b #\a))
(assert (not (char> #\c #\b #\z)))
(assert (not (char> #\a #\b #\a)))
(assert (char>= #\a #\a))
(assert (not (char>= #\a #\b)))
(assert (char>= #\b #\a))
(assert (char>= #\b #\a #\a))
(assert (not (char>= #\b #\a #\z)))

(write-line "==================== Array ====================")
(ffi:console.log (make-array 3))
(ffi:console.log (make-array 3 :initial-element 100))
(ffi:console.log (arrayp (make-array 3)))
(let ((a (make-array 3 :fill-pointer t)))
  (assert (array-has-fill-pointer-p a))
  (assert (= 1 (array-rank a)))
  (assert (vectorp a))
  (assert (not (vectorp 1)))
  (assert (vectorp "test"))
  (dotimes (i 3)
    (setf (aref a i) i))
  (dotimes (i 3)
    (assert (= (aref a i) i))))

(let ((array "abcd"))
  (ffi:console.log array)
  (assert (eql (aref array 0) #\a))
  (assert (eql (aref array 1) #\b))
  (assert (eql (aref array 2) #\c))
  (assert (eql (aref array 3) #\d)))

(let ((x (make-array 10 :initial-element #\a :element-type 'character)))
  (assert (eq (array-element-type x) 'character))
  (assert (stringp x))
  (assert (string= x "aaaaaaaaaa")))

(assert (string= "ABC" (string-upcase "abc")))
(assert (string= "aBC" (string-upcase "abc" :start 1)))
(assert (string= "aBc" (string-upcase "abc" :start 1 :end 2)))
(assert (string= "abc" (string-downcase "ABC")))
(assert (string= "Abc" (string-downcase "ABC" :start 1)))
(assert (string= "AbC" (string-downcase "ABC" :start 1 :end 2)))

(assert (eql (aref "abc" 0) #\a))
(let ((x "abc"))
  (dotimes (i (length x))
    (assert (eql (setf (aref x i) #\@) #\@))
    (ffi:console.log x)
    (assert (eql (aref x i) #\@)))
  (assert (equal x "@@@")))

(let ((x (vector 'a 'b 'c)))
  (assert (= 3 (length x)))
  (assert (eq 'a (aref x 0)))
  (assert (eq 'b (aref x 1)))
  (assert (eq 'c (aref x 2))))

(write-line "==================== ffi ====================")
(let ((x (ffi:new (ffi:ref "Array") 10)))
  (ffi:console.log x)
  (ffi:set (ffi:%aget x 0) "a")
  (ffi:console.log (ffi:%aget x 0)))

(write-line "==================== cons ====================")
(ffi:console.log (cons 1 2))
(assert (eql 1 (car (cons 1 2))))
(assert (eql 2 (cdr (cons 1 2))))
(assert (consp (cons 1 2)))
(let ((x (cons 1 2)))
  (assert (equal '(100 . 2) (rplaca x 100)))
  (assert (equal '(100 . 200) (rplacd x 200)))
  (assert (eql 100 (car x)))
  (assert (eql 200 (cdr x))))
(let ((x (cons 1 2)))
  (setf (car x) 849213482)
  (assert (eql (car x) 849213482)))
(assert (equal (list* 1) 1))
(assert (equal (list* 1 2) '(1 . 2)))
(assert (equal (list* 1 2 3) '(1 2 . 3)))
(assert (equal (member 2 (list 1 2 3)) '(2 3)))

(assert (equal '(1 2 3)
               (mapcan (lambda (x) (list x))
                       (list 1 2 3))))

(assert (null (nthcdr 0 '())))
(assert (equal (nthcdr 0 '(a b c)) '(a b c)))
(assert (equal (nthcdr 1 '(a b c)) '(b c)))
(assert (equal (nthcdr 2 '(a b c)) '(c)))
(assert (equal (nthcdr 3 '(a b c)) '()))
(assert (equal (nthcdr 4 '(a b c)) '()))

(assert (endp nil))
(assert (not (endp '(a))))

(let ((ls nil))
  (push 1 ls)
  (assert (equal ls '(1)))
  (push 2 ls)
  (assert (equal ls '(2 1))))

(let ((x (cons nil nil)))
  (push 1 (car x))
  (assert (equal (car x) '(1)))
  (push 2 (car x))
  (assert (equal (car x) '(2 1))))

(let ((x (list 1 2 3)))
  (assert (eql 1 (pop x)))
  (assert (equal x '(2 3)))
  (assert (eql 2 (pop x)))
  (assert (equal x '(3)))
  (assert (eql 3 (pop x)))
  (assert (equal x nil))
  (assert (null (pop x))))

(let* ((x (list 1 2 3))
       (y (copy-list x)))
  (assert (not (eq x y)))
  (assert (equal x y)))

(let ((a '(1 2 3)))
  (let ((res (mapc (lambda (x)
                     (assert (= (pop a) x)))
                   '(1 2 3))))
    (assert (equal res '(1 2 3)))))

(let ((a '(1 2 3))
      (b '(a b c)))
  (let ((res (mapc (lambda (x y)
                     (assert (eql (pop a) x))
                     (assert (eql (pop b) y)))
                   '(1 2 3)
                   '(a b c))))
    (assert (equal res '(1 2 3)))))

(let ((a '(1 2 3))
      (b '(a b))
      (n 0))
  (let ((res (mapc (lambda (x y)
                     (setq n (+ n 1))
                     (assert (eql (pop a) x))
                     (assert (eql (pop b) y)))
                   '(1 2 3)
                   '(a b))))
    (assert (equal res '(1 2 3)))
    (assert (= n 2))))

(let ((a '(1 2))
      (b '(a b c))
      (n 0))
  (let ((res (mapc (lambda (x y)
                     (setq n (+ n 1))
                     (assert (eql (pop a) x))
                     (assert (eql (pop b) y)))
                   '(1 2)
                   '(a b c))))
    (assert (equal res '(1 2)))
    (assert (= n 2))))

(assert (equal (mapcar #'1+ '(1 2 3)) '(2 3 4)))
(assert (equal (mapcar #'+ '(1 2 3) '(100 200 300 400))
               '(101 202 303)))

(assert (equal (member 2 '(1 2 3)) '(2 3)))
(assert (member "foo" '("foo" "bar" "baz") :test #'equal)
        '("foo" "bar" "baz"))
(let ((mem (member #\b '("foo" "bar" "baz") :test #'equal :key (lambda (x) (aref x 0)))))
  (assert mem '("bar" "baz")))
(let ((mem (member #\b '("foo" "bar" "baz") :key (lambda (x) (aref x 0)))))
  (assert mem '("bar" "baz")))

(assert (equal (assoc 'a '((a 1) (b 2) (c 3))) '(a 1)))

(let ((slist '()))
  (assert (equal (adjoin 'a slist) '(A)))
  (assert (equal (setq slist (adjoin '(test-item 1) slist))
                 '((TEST-ITEM 1))))
  (assert (equal (adjoin '(test-item 1) slist)
                 '((TEST-ITEM 1) (TEST-ITEM 1))))
  (assert (equal (adjoin '(test-item 1) slist :test 'equal)
                 '((TEST-ITEM 1))))
  (assert (equal (adjoin '(new-test-item 1) slist :key #'cadr)
                 '((TEST-ITEM 1))))
  (assert (equal (adjoin '(new-test-item 1) slist)
                 '((NEW-TEST-ITEM 1) (TEST-ITEM 1)))))

(let ((x '()))
  (pushnew 1 x)
  (assert (equal x '(1)))
  (pushnew 2 x)
  (assert (equal x '(2 1)))
  (pushnew 2 x)
  (assert (equal x '(2 1)))
  (pushnew 1 x)
  (assert (equal x '(2 1)))
  (pushnew 3 x)
  (assert (equal x '(3 2 1))))

(write-line "==================== lambda-list ====================")
(defun f1 (&rest args)
  (ffi:console.log args))

(f1 1 2 3)

(defun f2 (x y &rest z)
  (ffi:console.log x y z))

(f2 1 2)
(f2 1 2 3)
(f2 1 2 3 4)

(defun f3 (&key (foo 100))
  foo)

(ffi:console.log (f3))
(ffi:console.log (f3 :foo 0))

(write-line "==================== defstruct ====================")
(defstruct foo
  x y z)
(ffi:console.log (make-foo))
(ffi:console.log (make-foo :x 10 :y 20 :z 30))
(ffi:console.log (make-foo :z 10 :x 20))
(ffi:console.log (make-foo :y 100))
(let ((foo (make-foo :x 12345 :y 200 :z 'foo)))
  (ffi:console.log (foo-x foo))
  (ffi:console.log (foo-y foo))
  (ffi:console.log (foo-z foo)))

(defstruct bar
  (x 100))
(ffi:console.log (make-bar))
(ffi:console.log (make-bar :x 0))

(defstruct (hoge (:constructor %make-hoge))
  x
  y
  z)
(ffi:console.log (%make-hoge :x 1 :y 2 :z 3))

(defstruct (piyo (:constructor %make-piyo (x y z)))
  x
  y
  z)
(let ((piyo (%make-piyo 100 200 300)))
  (ffi:console.log piyo)
  (setf (piyo-x piyo) 'test)
  (ffi:console.log piyo (piyo-x piyo)))

(let ((piyo (%make-piyo 1 2 3)))
  (ffi:console.log (eq piyo piyo))
  (ffi:console.log (eq piyo (copy-structure piyo)))
  (ffi:console.log (eq piyo (copy-piyo piyo)))
  (ffi:console.log (piyo-p piyo)))

(write-line "==================== Symbol ====================")
(let ((symbol '#:foo))
  (assert (equal (symbol-plist symbol) '()))
  (setf (symbol-plist symbol) (list 'a 1 'b 2))
  (assert (equal (symbol-plist symbol) '(a 1 b 2)))
  (setf (get 'aaa 'key1) 100)
  (assert (= (get 'aaa 'key1) 100)))

(dolist (p (list-all-packages))
  (assert (packagep p))
  (assert (stringp (package-name p))))

(let ((p (symbol-package :foo)))
  (assert (packagep p))
  (assert (string= "KEYWORD" (package-name p))))

(assert (keywordp :foo))

(assert (not (keywordp 'foo)))
(assert (not (keywordp 1)))

(let ((name (symbol-name 'abc)))
  (assert (stringp name))
  (assert (string= name "ABC")))

(write-line "==================== Hash Table ====================")
(let ((x (make-hash-table)))
  (ffi:console.log x)
  (ffi:console.log (hash-table-p x))
  (ffi:console.log "hash-table-count" (hash-table-count x))
  (ffi:console.log (gethash "key1" x))
  (ffi:console.log (setf (gethash "key1" x) "value"))
  (ffi:console.log  "hash-table-count" 1 (hash-table-count x))
  (gethash "key1" x)
  (ffi:console.log (remhash "key1" x))
  (ffi:console.log "hash-table-count" 0 (hash-table-count x))
  (ffi:console.log (remhash "key1" x))
  )

(let ((ht (make-hash-table))
      key1
      key2
      key3)
  (setf (gethash "key1" ht) 100)
  (setf (gethash "key2" ht) 200)
  (setf (gethash "key3" ht) 300)
  (maphash (lambda (k v)
             (cond ((equal k "key1")
                    (= v 100)
                    (setq key1 t))
                   ((equal k "key2")
                    (= v 200)
                    (setq key2 t))
                   ((equal k "key3")
                    (= v 300)
                    (setq key3 t))
                   (t
                    (error "error"))))
           ht)
  (assert (and key1 key2 key3)))

(write-line "==================== destructuring-bind ====================")

(destructuring-bind (a) (list 1)
  (ffi:console.log a))

(destructuring-bind ((a)) (list (list 2))
  (ffi:console.log a))

(destructuring-bind ((a &optional (b "default"))) (list (list 2))
  (ffi:console.log a b))

(destructuring-bind ((a &optional (b "default")) x y)
    (list (list 2) 10 20)
  (ffi:console.log a b x y))

(destructuring-bind ((a &optional (b 'bee)) one two three)
    `((alpha) ,@(list 1 2 3))
  (ffi:console.log a b three two one))

(destructuring-bind (a . b) (cons 1 2)
  (ffi:console.log a b))

(write-line "==================== apply ====================")
(ffi:console.log (apply '+ '()))
(ffi:console.log (apply '+ '(1 2 3)))
(ffi:console.log (apply '+ 1 2 3 '(4 5 6)))

(ffi:console.log "(numberp 1)" (numberp 1))
(ffi:console.log "(numberp 1.2)" (numberp 1.2))
(ffi:console.log "(numberp 'a)" (numberp 'a))
(ffi:console.log "(integerp 1)" (integerp 1))
(ffi:console.log "(integerp 1.0)" (integerp 1.0))
(ffi:console.log "(integerp 'a)" (integerp 'a))

(write-line "==================== stream ====================")
(ffi:console.log
 (assert
  (string= "string!"
           (with-output-to-string (out)
             (write-string "string" out)
             (write-char #\! out)))))

(let ((in (make-string-input-stream "abc")))
  (assert (char= #\a (read-char in)))
  (assert (char= #\b (read-char in)))
  (assert (char= #\c (read-char in)))
  (assert (eql :foo (read-char in nil :foo)))
  (assert (eql :bar (read-char in nil :bar))))

(let ((in (make-string-input-stream "abc
")))
  (assert (equal (multiple-value-list (read-line in))
                 '("abc" nil)))
  (assert (equal (multiple-value-list (read-line in nil :foo))
                 '(:foo t))))

(let ((in (make-string-input-stream "abc
d
efg")))
  (assert (equal (multiple-value-list (read-line in))
                 '("abc" nil)))
  (assert (equal (read-char in) #\d))
  (assert (equal (read-char in) #\newline))
  (assert (equal (read-char in) #\e))
  (assert (equal (multiple-value-list (read-line in))
                 '("fg" t))))

(let ((in (make-string-input-stream "  ab")))
  (assert (char= #\a (peek-char t in)))
  (assert (char= #\a (peek-char t in))))

(let ((in (make-string-input-stream "abc")))
  (assert (char= #\a (peek-char nil in)))
  (assert (char= #\a (peek-char t in))))

(let ((in (make-string-input-stream "")))
  (assert (eql :hoge (peek-char nil in nil :hoge))))

(let ((in (make-string-input-stream "")))
  (assert (eql :hoge (peek-char t in nil :hoge))))

(write-line "==================== package ====================")
(assert (equal "COMMON-LISP" (package-name :cl)))
(assert (equal "COMMON-LISP" (package-name 'common-lisp)))
(assert (null (find-package :abcd)))
(let ((packages (mapcar #'package-name (list-all-packages))))
  (assert (find-if (lambda (x) (equal x "COMMON-LISP")) packages))
  (assert (find-if (lambda (x) (equal x "SYSTEM")) packages))
  (assert (find-if (lambda (x) (equal x "FFI")) packages)))
(assert (not (packagep "foo")))
(assert (packagep (first (list-all-packages))))

(let ((symbol-name1 "a908eb86-ec92-4196-a68f-42577766a8fa"))
  (multiple-value-bind (symbol status)
      (intern symbol-name1)
    (assert (symbolp symbol))
    (assert (string= (symbol-name symbol) symbol-name1))
    (assert (eq (symbol-package symbol) *package*))
    (assert (eq status nil)))
  (multiple-value-bind (symbol status)
      (intern symbol-name1)
    (assert (symbolp symbol))
    (assert (string= (symbol-name symbol) symbol-name1))
    (assert (eq (symbol-package symbol) *package*))
    (assert (eq status :internal)))
  (multiple-value-bind (symbol status)
      (find-symbol symbol-name1)
    (assert (symbolp symbol))
    (assert (string= (symbol-name symbol) symbol-name1))
    (assert (eq (symbol-package symbol) *package*))
    (assert (eq status :internal))))
(let ((symbol-name2 "10731476-cf81-4368-910a-e9bad821c082"))
  (multiple-value-bind (symbol status)
      (find-symbol symbol-name2)
    (assert (null symbol))
    (assert (eq status nil))))

(write-line "==================== sequence ====================")
;; length
(assert (= (length "abc") 3))
(assert (= (length (make-array 10)) 10))
(assert (= (length '(a b c)) 3))

;; reverse
(assert (equal (reverse '()) '()))
(assert (equal (reverse '(a)) '(a)))
(assert (equal (reverse '(a b)) '(b a)))
(assert (equal (reverse '(a b c)) '(c b a)))

(defun vector-equal (x y)
  (and (= (length x) (length y))
       (dotimes (i (length x) t)
         (unless (eql (aref x i)
                      (aref y i))
           (return nil)))))

(let ((x (make-array 3 :initial-contents '(1 2 3)))
      (y (make-array 3 :initial-contents '(3 2 1))))
  (assert (vector-equal (reverse x) y)))
(assert (vector-equal (reverse "abc") "cba"))

;; subseq
(assert (equal (subseq '(a b c) 0) '(a b c)))
(assert (equal (subseq '(a b c) 1) '(b c)))
(assert (equal (subseq '(a b c) 3) nil))
;(subseq '(a b c) 4) ; error
(assert (equal (subseq '(a b c) 1 2) '(b)))
(assert (equal (subseq '(a b c) 1 3) '(b c)))
;(subseq #(a b c) 1 4) ; error
(assert (vector-equal (subseq #(a b c) 0) #(a b c)))
(assert (vector-equal (subseq #(a b c) 1) #(b c)))
(assert (vector-equal (subseq #(a b c) 3) #()))
(assert (vector-equal (subseq #(a b c) 1 2) #(b)))
(assert (vector-equal (subseq #(a b c) 1 3) #(b c)))

;; position
(assert (= 1 (position #\. "a.b.c")))
(assert (= 3 (position #\. "a.b.c" :start 2)))
(assert (null (position #\. "a.b.c" :start 4)))
(assert (= 3 (position #\. "a.b.c" :from-end t)))
(assert (= 1 (position #\. "a.b.c" :from-end t :end 3)))

;; find-if
(assert (eql 2 (find-if #'(lambda (x) (= x 2)) '(1 2 3))))

;; remove-if-not
(assert (equal (remove-if-not (lambda (x)
                                (= x 1))
                              '(1 2 3))
               '(1)))
(let ((x (remove-if-not (lambda (x)
                          (not (= x 1)))
                        '(1 2 3))))
  (assert (equal x '(2 3))))
(let ((x (remove-if-not (lambda (x)
                          t)
                        '(1 2 3))))
  (assert (equal x '(1 2 3))))
(let ((x (remove-if-not (lambda (x)
                          t)
                        '())))
  (assert (null x)))
(let ((x (remove-if-not (lambda (x)
                          nil)
                        '(1 2 3))))
  (assert (null x)))

;; remove
(assert (equal (remove 1 '(1 2 3)) '(2 3)))
(assert (equal (remove 1 '(1 2 1 3)) '(2 3)))

;; nreverse
(let ((v (list #\a #\b #\c #\d)))
  (setq v (nreverse v))
  (assert (equal v (list #\d #\c #\b #\a))))

(let ((v (vector #\a #\b #\c #\d)))
  (nreverse v)
  (assert (vector-equal v (vector #\d #\c #\b #\a))))

(write-line "==================================================")
(defun foo ()
  (ffi:console.log "a")
  (return-from foo)
  (ffi:console.log "b"))

(foo)

(ffi:console.log (make-symbol "adlfjasldfkjksj"))

(let ((x 0))
  (defun f ()
    (setq x (+ x 1))))

(ffi:console.log (f))
(ffi:console.log (f))

(ffi:console.log #\a)
(ffi:console.log (characterp #\a))
(ffi:console.log (code-char 70))
(ffi:console.log (char-code #\a))

(ffi:console.log (funcall #'+ 1 2))
(ffi:console.log (apply #'+ '(1 2 3)))
(ffi:console.log (apply #'+ 100 200 '(1 2 3)))

(defun return-values ()
  (values 100 200))

(assert (= 100 (multiple-value-call #'+ 100)))
(assert (= 300 (multiple-value-call #'+ (return-values))))
(assert (= 6 (multiple-value-call #'+ (values 1 2 3))))

(assert (= 300 (multiple-value-call '+ 100 200)))
(assert (= 600 (multiple-value-call '+ 100 200 300)))
(assert (= 106 (multiple-value-call '+ 100 (values 1 2 3))))
(assert (= 400 (multiple-value-call '+ 100 (return-values))))

(assert (equal 1 1))
(assert (not (equal 1 2)))
(assert (equal 'a 'a))
(assert (not (equal 'a 'b)))
(assert (equal (cons 1 2) (cons 1 2)))
(assert (equal '(1 ((a b) 3)) '(1 ((a b) 3))))
(assert (not (equal '(2 ((a b) 3)) '(1 ((a b) 3)))))
(assert (not (equal (list 1 2) (list 1 2 3))))

((lambda (&rest x) (assert (equal x (list 1 2 3)))) 1 2 3)
((lambda (x &rest y)
   (assert (eql x 1))
   (assert (equal y (list 2 3))))
 1 2 3)

((lambda (x)
   (declare (special x))
   (assert (eql x 0)))
 0)

(defun e18153e3-f341-4e18-8114-c98ca80b6835 ()
  (declare (special x))
  (assert (eql x 123)))

(let ((x 123))
  (declare (special x))
  (e18153e3-f341-4e18-8114-c98ca80b6835))

(defvar *e18153e3* nil)
(let ((*e18153e3*))
  (let* ((*e18153e3* (cons 0 *e18153e3*))
         (*e18153e3* (cons 1 *e18153e3*))
         (*e18153e3* (cons 2 *e18153e3*))))
  (assert (null *e18153e3*)))

(defmacro output-test (form string)
  `(assert (string= ,string (with-output-to-string (*standard-output*) ,form))))

(output-test (progn
               (princ 'hoge)
               (write-char #\space)
               (princ t)
               (write-char #\space)
               (princ nil))
             "HOGE T NIL")

(write-line "## princ")
(output-test (princ "hello world") "hello world")
(output-test (princ 123) "123")
(output-test (princ #\a) "a")
(output-test (princ (cons 1 2)) "(1 . 2)")
(output-test (princ (cons 1 (cons 2 3))) "(1 2 . 3)")
(output-test (princ (cons 1 (cons 2 (cons 3 nil)))) "(1 2 3)")
(output-test (princ (vector)) "#()")
(output-test (princ (vector 1 2 3)) "#(1 2 3)")

(write-line "## format")
(output-test (format t "hello") "hello")
(output-test (format t "~A" 10) "10")
(output-test (format t "~A" 'a) "A")
(output-test (format t "~A" #\a) "a")
(output-test (format t "aaa ~A ~C" 'test #\@) "aaa TEST @")

(write-line "## prin1")
(output-test (prin1 'hello) "HELLO")
(output-test (prin1 :hello) ":HELLO")
(output-test (prin1 '#:hello) "#:HELLO")
(output-test (prin1 (find-package :cl)) "#<PACKAGE \"COMMON-LISP\">")
(output-test (prin1 "abc") "\"abc\"")

(defstruct print-test-structure x y z)

(output-test (prin1 (make-print-test-structure :x 100 :y 200 :z 300))
             "#S(PRINT-TEST-STRUCTURE :X 100 :Y 200 :Z 300)")

(assert (eq 'abc (read-from-string "abc")))
(assert (eq 'abc (read-from-string "Abc")))
(assert (= 100 (read-from-string "100")))
(assert (= 0.2 (read-from-string ".2")))
(assert (= 1 (read-from-string "1.")))
(assert (= 123 (read-from-string "+123")))
(assert (= -123 (read-from-string "-123")))
(assert (eq '|a| (read-from-string "\\a")))
(assert (eq '|aB| (read-from-string "\\ab")))
(assert (string= "abc" (string (read-from-string "|abc|"))))
(assert (eq :common-lisp (read-from-string ":common-lisp")))
(assert (eq :eof (read-from-string "  " nil :eof)))
(assert (equal '(abc abc 100 0.2 1 123 -123 |a| |abc|)
               (read-from-string "(abc Abc 100
.2 1. +123
 -123 \\a |abc|)")))
(assert (equal '(a (b) ((c) d))
               (read-from-string "(a (b) ((c) d))")))
(assert (equal '(foo bar) (read-from-string "(foo; comment
bar)")))

(assert (equal '(quote x) (read-from-string "'x")))
(assert (equal '(a . b) (read-from-string "(a . b)")))
(assert (equal '(a . (b . c)) (read-from-string "(a . (b . c))")))
(assert (equal '((a . d) . (b . c)) (read-from-string "((a . d) . (b . c))")))
(assert (eq 'cl:car (read-from-string "cl:car")))
(assert (eq 'compiler::pass1 (read-from-string "compiler::pass1")))
(assert (string= "foo" (read-from-string "\"foo\"")))
(assert (string= "f\"oo" (read-from-string "\"f\\\"oo\"")))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (assert (string= "zebra" (string (read-from-string "ZEBRA"))))
  (assert (string= "Zebra" (string (read-from-string "Zebra"))))
  (assert (string= "ZEBRA" (string (read-from-string "zebra")))))

(assert (char= #\a (read-from-string "#\\a")))
(assert (char= #\A (read-from-string "#\\A")))
(assert (char= #\space (read-from-string "#\\space")))
(assert (equal '(function cons) (read-from-string "#'cons")))
(assert (vector-equal #(1 2 3) (read-from-string "#(1 2 3)")))
(let ((symbol (read-from-string "#:foo")))
  (assert (null (symbol-package symbol)))
  (assert (string= "FOO" symbol)))

(assert (eql 2 (read-from-string "#+(or) 1 2")))

(let ((tree (read-from-string "#1=(a b #1#)")))
  (assert (eq tree (third tree))))

(progn
  (defun #1=#:foo (x) x)
  (assert (functionp (symbol-function '#1#))))

(in-package :compiler)

(defun f ()
  (assert (string= "COMPILER" (package-name *package*))))

(f)

(write-line "==================== compiler ====================")

(assert (= (eval '(+ 1 100)) 101))
(assert (= 1 (eval '(let ((form 1))
                     (multiple-value-bind (form expanded-p)
                         (%macroexpand-1 form)
                       (declare (ignore expanded-p))
                       form)))))

;; (write-line (js:-string.prototype.concat "foo" "bar"))
;; (js:console.log js:global.clear-immediate)
;; (js:console.log (ffi:new js:-array))

(js:console.log (ffi:object "foo" 100 "bar" 200))

(let ((obj (ffi:object "foo" (ffi:object "baz" 100) "bar" 200)))
  (ffi:console.log obj)
  (ffi:console.log (ffi:aget obj "foo"))
  (ffi:console.log (ffi:aget obj "foo" "baz")))

(ffi:define-function js:foo (x) (1+ x))

(ffi:console.log (js:foo 10))

(let ((ht (make-hash-table)))
  (setf (gethash 'cl::set ht) t)
  (ffi:console.log (cl::hash-table-object ht))
  (print (gethash 'ffi::set ht)))

(defun benchmark (function)
  (let ((start ((ffi:ref "Date" "now"))))
    (write-line "start")
    (funcall function)
    (write-line "end")
    (format t "time: ~A ms~%" (- ((ffi:ref "Date" "now")) start))))

(benchmark
 (lambda ()
   (compiler::pass1-toplevel
    '(LET ((PLACE (QUOTE ("love" "peace"))))
      (AND (EQ (PUSHNEW "peace" PLACE :TEST (FUNCTION EQUAL)) PLACE)
       (EQUAL PLACE (QUOTE ("love" "peace")))))
    t)))

(benchmark
 (lambda ()
   (compiler::pass1-toplevel
    '(LET ((PLACE (QUOTE ("love" "peace"))))
      (AND (EQ (PUSHNEW "peace" PLACE :TEST (FUNCTION EQUAL)) PLACE)
       (EQUAL PLACE (QUOTE ("love" "peace")))))
    t)))
|#
