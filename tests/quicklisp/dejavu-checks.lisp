(let* ((names (cl-dejavu:list-fonts))
       (name "DejaVuSans.ttf")
       (path (cl-dejavu:font-pathname name)))
  (assert (= (length names) 22))
  (assert (member name names :test #'string=))
  (assert (string= (pathname-name path) "DejaVuSans"))
  (assert (string= (pathname-type path) "ttf"))
  ;; ASDF static components are embedded. This checks the font header through
  ;; an ordinary binary stream; glyph rendering is a separate integration gate.
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (assert (equal (loop repeat 4 collect (read-byte in)) '(0 1 0 0)))))
(write-line "DEJAVU-METADATA-PASS")
