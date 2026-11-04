(in-package :cl-test)
(defvar *stream-fixture-target*)

(deftest stream-designators
  (let ((*standard-input* (make-string-input-stream "ab"))
        (*standard-output* (make-string-output-stream)))
    (values (read-char nil) (peek-char nil nil) (read-char nil)
            (progn (write-string "xy" nil) (get-output-stream-string *standard-output*))
            (read-char nil nil :end)))
  #\a #\b #\b "xy" :end)

(deftest string-stream-ranges
  (let ((index 0) (stream nil))
    (values (with-input-from-string (s "012345" :start 2 :end 4 :index index)
              (setq stream s) (read-line s))
            index (open-stream-p stream)
            (with-output-to-string (s) (write-line "012345" s :start 2 :end 4))))
  "23" 4 nil "23
")

(deftest string-stream-append-target
  (let ((target (make-array 2 :element-type 'character :fill-pointer 1
                             :adjustable t :initial-contents "ab")))
    (values (multiple-value-list (with-output-to-string (s target)
                                  (write-string "xy" s) (values 1 2)))
            target (fill-pointer target)))
  (1 2) "axy" 3)

(deftest read-sequence-bounds-and-end
  (let ((vector (make-array 5 :initial-element :old)) (list (list :old :old :old)))
    (values (read-sequence vector (make-string-input-stream "ab") :start 1 :end 4)
            vector (read-sequence list (make-string-input-stream "z") :start 1)
            list))
  3 #(:old #\a #\b :old :old) 2 (:old #\z :old))

(deftest write-sequence-list-vector
  (let ((out (make-string-output-stream)) (data (list #\a #\b #\c)))
    (values (eq (write-sequence data out :start 1) data)
            (progn (write-sequence "0123" out :end 2) (get-output-stream-string out))))
  t "bc01")

(deftest concatenated-stream-lifetime
  (let* ((a (make-string-input-stream "a")) (b (make-string-input-stream "bc"))
         (s (make-concatenated-stream a b)))
    (values (read-char s) (peek-char nil s) (length (concatenated-stream-streams s))
            (read-line s) (progn (close s) (open-stream-p s))
            (open-stream-p a) (open-stream-p b)))
  #\a #\b 1 "bc" nil t t)

(deftest broadcast-stream-lifetime
  (let* ((a (make-string-output-stream)) (b (make-string-output-stream))
         (s (make-broadcast-stream a b)))
    (write-sequence "abc" s)
    (close s)
    (values (get-output-stream-string a) (get-output-stream-string b)
            (open-stream-p a) (open-stream-p b) (stream-element-type (make-broadcast-stream))))
  "abc" "abc" t t t)

(deftest echo-stream-unread
  (let* ((out (make-string-output-stream)) (s (make-echo-stream (make-string-input-stream "ab") out)))
    (values (peek-char nil s) (get-output-stream-string out)
            (read-char s) (progn (unread-char #\a s) (read-char s))
            (read-char s) (get-output-stream-string out)))
  #\a "" #\a #\a #\b "ab")

(deftest synonym-stream-dynamic-target
  (let ((*stream-fixture-target* (make-string-input-stream "a")))
    (let ((s (make-synonym-stream '*stream-fixture-target*)))
      (values (read-char s)
              (let ((*stream-fixture-target* (make-string-input-stream "b"))) (read-char s))
              (synonym-stream-symbol s))))
  #\a #\b *stream-fixture-target*)

(deftest stream-conditions
  (let ((s (make-string-input-stream "")))
    (values (handler-case (read-char s) (end-of-file (c) (eq (stream-error-stream c) s)))
            (progn (close s)
                   (handler-case (read-char s) (stream-error (c) (eq (stream-error-stream c) s))))))
  t t)

(deftest stream-validation
  (values (handler-case (make-string-input-stream "ab" -1) (type-error () :bad))
          (handler-case (read-sequence "ab" (make-string-input-stream "x") :end -1) (type-error () :bad))
          (handler-case (make-echo-stream 'x (make-string-output-stream)) (type-error () :bad)))
  :bad :bad :bad)

(deftest read-has-one-value
  (multiple-value-list (read (make-string-input-stream "cl:car")))
  (car))

(deftest input-index-on-nonlocal-exit
  (let ((index :unchanged) saved)
    (values (catch 'done
              (with-input-from-string (s "abc" :index index)
                (setq saved s) (throw 'done (read-char s))))
            index (open-stream-p saved)))
  #\a :unchanged nil)

(deftest output-target-ignores-element-type
  (let ((text (make-array 8 :element-type 'character :fill-pointer 0)))
    (values (with-output-to-string (s text :element-type nil) (write-string "abc" s)) text))
  "abc" "abc")

(deftest empty-broadcast-file-properties
  (let ((s (make-broadcast-stream)))
    (values (file-position s) (file-length s) (file-string-length s "abc")
            (stream-element-type s) (stream-external-format s)))
  0 0 1 t :default)
