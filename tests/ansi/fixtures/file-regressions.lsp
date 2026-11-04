(in-package :cl-test)

(deftest file-unicode-position-length
  (with-open-file (s "unicode.txt" :direction :io :if-exists :supersede)
    (let* ((text "日本語é") (size (file-string-length s text))
           (start (file-position s)))
      (write-string text s)
      (let ((end (file-position s)))
        (file-position s start)
        (values (= size (- end start)) (read-line s)))))
  t "日本語é")

(deftest file-text-roundtrip
  (let ((path #p"roundtrip.txt"))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-line "first" out) (write-string "last" out))
    (with-open-file (in path)
      (values (typep in 'file-stream) (multiple-value-list (read-line in))
              (multiple-value-list (read-line in)) (read-char in nil :end))))
  t ("first" nil) ("last" t) :end)

(deftest file-binary-roundtrip
  (progn
    (with-open-file (out "signed.bin" :direction :output :if-exists :supersede :element-type '(signed-byte 16))
      (write-sequence '(-32768 -1 0 32767) out))
    (with-open-file (in "signed.bin" :element-type '(signed-byte 16))
      (let ((data (make-array 5 :initial-element :old)))
        (values (read-sequence data in) data))))
  4 #(-32768 -1 0 32767 :old))

(deftest file-binary-read-and-position
  (progn
    (with-open-file (out "position.bin" :direction :output :if-exists :supersede :element-type '(signed-byte 16))
      (write-sequence '(-32768 -1 0 32767) out))
    (with-open-file (in "position.bin" :element-type '(signed-byte 16))
      (values (file-length in) (file-position in 1) (read-byte in)
              (file-position in :end) (file-position in) (read-byte in nil :end))))
  4 t -1 t 4 :end)

(deftest file-large-binary
  (let ((value (1+ (ash 1 100))))
    (with-open-file (s "large.bin" :direction :output :if-exists :supersede :element-type '(unsigned-byte 101))
      (write-byte value s))
    (with-open-file (s "large.bin" :element-type '(unsigned-byte 101)) (= (read-byte s) value)))
  t)

(deftest file-append-overwrite
  (progn
    (with-open-file (s "modes.txt" :direction :output :if-exists :supersede) (write-string "abc" s))
    (with-open-file (s "modes.txt" :direction :output :if-exists :append) (write-string "de" s))
    (with-open-file (s "modes.txt" :direction :output :if-exists :overwrite) (write-char #\X s))
    (with-open-file (s "modes.txt") (read-line s)))
  "Xbcde" t)

(deftest file-abort-restores-previous
  (progn
    (with-open-file (s "abort.txt" :direction :output :if-exists :supersede) (write-string "old" s))
    (catch 'done
      (with-open-file (s "abort.txt" :direction :output :if-exists :supersede)
        (write-string "new" s) (throw 'done nil)))
    (with-open-file (s "abort.txt") (read-line s)))
  "old" t)

(deftest file-path-operations
  (let ((path #p"subdir/deeper/a.txt"))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede) (write-char #\a s))
    (values (pathnamep (probe-file path)) (integerp (file-write-date path))
            (stringp (file-author path)) (length (directory #p"subdir/deeper/*.txt"))
            (length (multiple-value-list (rename-file path "b.txt")))
            (probe-file path) (delete-file #p"subdir/deeper/b.txt")))
  t t t 1 3 nil t)

(deftest file-missing-and-probe
  (progn
    (with-open-file (s "probe.txt" :direction :output :if-exists :supersede) (write-char #\x s))
    (values (open "no-file-here.txt" :if-does-not-exist nil)
            (handler-case (open "no-file-here.txt") (file-error (c) (pathnamep (file-error-pathname c))))
            (let ((s (open "probe.txt" :direction :probe)))
              (and (streamp s) (not (open-stream-p s))))))
  nil t t)

(deftest pathname-component-equality
  (let ((a (parse-namestring "dir/name.txt"))
        (b (parse-namestring "dir/name.txt"))
        (c (parse-namestring "dir/NAME.txt"))
        (table (make-hash-table :test 'equal)))
    (setf (gethash a table) :found)
    (values (equal a b) (equalp a b) (equal a c) (equalp a c) (gethash b table)))
  t t nil nil :found)
