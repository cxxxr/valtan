(in-package :cl-test)

(deftest character-name-designators
  (values (name-char 'space) (name-char "sPaCe") (name-char nil))
  #\Space #\Space nil)

(deftest character-name-displaced
  (name-char (make-array 5 :element-type 'character
                         :displaced-to "  sPaCe  " :displaced-index-offset 2))
  #\Space)

(deftest hash-iterator-value-count
  (let ((table (make-hash-table)))
    (setf (gethash :key table) :value)
    (with-hash-table-iterator (next table)
      (values (multiple-value-list (next)) (multiple-value-list (next)))))
  (t :key :value) (nil))

(deftest hash-iterator-cleared-table
  (let ((table (make-hash-table)))
    (setf (gethash :a table) 1 (gethash :b table) 2)
    (with-hash-table-iterator (next table)
      (next)
      (clrhash table)
      (multiple-value-list (next))))
  (nil))

(deftest hash-iterator-macro-binding
  (macrolet ((macro-visible-p (name &environment env)
               (not (null (macro-function name env)))))
    (with-hash-table-iterator (next (make-hash-table))
      (macro-visible-p next)))
  t)

(deftest portable-control-character-aliases
  (values (mapcar (lambda (name) (char-code (name-char name)))
                  '("nul" "NULL" "Bell" "BS" "HT" "LF" "VT" "FF" "CR" "Sub" "Esc" "Escape" "Del"))
          (char-code (read-from-string "#\\nul"))
          (char-name (code-char 0)))
  (0 0 7 8 9 10 11 12 13 26 27 27 127) 0 "U0000")

(deftest supplementary-characters-and-strings
  (let* ((face (code-char #x1f600))
         (text (concatenate 'string "A" (string face) "B")))
    (values (characterp face) (char-code face) (length text)
            (char-code (char text 1)) (string= text "A😀B")
            (null (code-char char-code-limit))))
  t 128512 3 128512 t t)

(deftest supplementary-string-mutation-and-displacement
  (let* ((text (copy-seq "A😀B"))
         (view (make-array 1 :element-type 'character :displaced-to text :displaced-index-offset 1)))
    (setf (char view 0) (code-char #x10400))
    (values (length text) (char-code (char text 1))
            (string= (ffi:js->cl (ffi:cl->js view)) "𐐀")
            (length (subseq text 1 2))))
  3 66560 t 1)
