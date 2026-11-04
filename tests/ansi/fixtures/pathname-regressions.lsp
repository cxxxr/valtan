(in-package :cl-test)

(deftest make-pathname-defaults-only-the-host
  (let ((*default-pathname-defaults* #p"/var/tmp/example.lisp"))
    (let ((path (make-pathname :name "new")))
      (values (pathname-directory path) (pathname-name path) (pathname-type path)
              (pathname-version path))))
  nil "new" nil nil)

(deftest pathname-accessors-accept-designators-and-common-case
  (values (pathname-name "/tmp/hello.lisp" :case :common)
          (pathname-directory "/tmp/Mixed/NAME.txt" :case :common)
          (namestring (make-pathname :name "HELLO" :type "LISP" :case :common)))
  "HELLO" (:absolute "TMP" "Mixed") "hello.lisp")

(deftest pathname-matching-and-wildcard-substitution
  (values (pathname-match-p "/tmp/a/b/file.lisp" "/tmp/**/f*.lisp")
          (pathname-match-p "/tmp/file.txt" "/tmp/*.lisp")
          (namestring (translate-pathname "/tmp/a/b/file.lisp" "/tmp/**/f*.lisp"
                                          "/out/**/prefix-*.txt")))
  t nil "/out/a/b/prefix-ile.txt")

(deftest logical-pathname-parse-and-recursive-translation
  (progn
    (setf (logical-pathname-translations "FIXTURE-A")
          '(("**;*.*.*" "/tmp/fixture/**/prefix-*.*")))
    (setf (logical-pathname-translations "FIXTURE-B")
          '(("**;*.*.*" "FIXTURE-A:**;*.*.*")))
    (let ((path (logical-pathname "fixture-b:one;two;name.lisp.3")))
      (values (typep path 'pathname) (typep path 'logical-pathname)
              (pathname-directory path :case :common)
              (namestring path) (namestring (translate-logical-pathname path)))))
  t t (:absolute "ONE" "TWO") "FIXTURE-B:ONE;TWO;NAME.LISP.3"
  "/tmp/fixture/one/two/prefix-name.lisp")

(deftest logical-file-stream-retains-original-designator
  (progn
    (setf (logical-pathname-translations "FIXTURE-FILE")
          (list (list "*.*.*" (merge-pathnames "*.*"))))
    (let ((path (logical-pathname "FIXTURE-FILE:TEXT.DAT.NEWEST")))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (write-line "logical data" out)
        (assert (equal (logical-pathname out) path)))
      (with-open-file (in path) (read-line in))))
  "logical data" nil)

(deftest physical-pathname-version-defaults-designate-the-same-file
  (values (equal (make-pathname :name "file" :type "txt") #p"file.txt")
          (handler-case (probe-file #p"*.txt")
            (file-error (c) (wild-pathname-p (file-error-pathname c)))))
  t t)
