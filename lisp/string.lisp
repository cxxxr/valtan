(in-package :common-lisp)

(defun stringp (x)
  (and (arrayp x)
       (eq 'character (array-element-type x))))

(defun simple-string-p (x)
  (and (arrayp x)
       (eq 'character (array-element-type x))
       (null (array-fill-pointer x))))

(defun string (x)
  (cond ((stringp x)
         x)
        ((characterp x)
         (system::js-string-to-array ((ffi:ref "String" "fromCharCode") (char-code x))))
        ((symbolp x)
         (symbol-name x))
        (t
         (type-error x 'string-designator))))

(defun system::string-append (x y)
  (unless (stringp x)
    (type-error x 'string))
  (unless (stringp y)
    (type-error y 'string))
  (system::js-string-to-array
   ((ffi:ref (system::array-to-js-string x) "concat")
    (system::array-to-js-string y))))

(defun system::string-append* (string &rest strings)
  (dolist (s strings)
    (setq string (system::string-append string s)))
  string)

(defun string-upcase (string &key (start 0) end)
  (setq string (string string))
  (unless end (setq end (length string)))
  (if (and (= start 0) (= end (length string)))
      (system::js-string-to-array ((ffi:ref (system::array-to-js-string string) "toUpperCase")))
      (system::string-append* (subseq string 0 start)
                              (system::js-string-to-array
                               ((ffi:ref (system::array-to-js-string (subseq string start end))
                                         "toUpperCase")))
                              (subseq string end))))

(defun string-downcase (string &key (start 0) end)
  (setq string (string string))
  (unless end (setq end (length string)))
  (if (and (= start 0) (= end (length string)))
      (system::js-string-to-array ((ffi:ref (system::array-to-js-string string) "toLowerCase")))
      (system::string-append* (subseq string 0 start)
                              (system::js-string-to-array
                               ((ffi:ref (system::array-to-js-string (subseq string start end))
                                         "toLowerCase")))
                              (subseq string end))))

(defun string-capitalize (string &key (start 0) end)
  (setq string (string string))
  (unless end (setq end (length string)))
  (system::string-append* (char-upcase (aref string start))
                          (string-downcase (subseq string (1+ start) end))))

(defun nstring-upcase (string &key (start 0) end)
  (setf (array-contents string)
        (string-upcase string :start start :end end))
  string)

(defun nstring-downcase (string &key (start 0) end)
  (setf (array-contents string)
        (string-downcase string :start start :end end)))

(defun nstring-capitalize (string &key (start 0) end)
  (setf (array-contents string)
        (string-capitalize string :start start :end end)))

(defun start-position-with-bag (character-bag string)
  (dotimes (i (length string))
    (unless (find (aref string i) character-bag)
      (return i))))

(defun end-position-with-bag (character-bag string)
  (do ((i (1- (length string)) (1- i)))
      ((<= i 0) 0)
    (unless (find (aref string i) character-bag)
      (return i))))

(defun string-trim (character-bag string)
  (let ((start (start-position-with-bag character-bag string))
        (end (end-position-with-bag character-bag string)))
    (subseq string start end)))

(defun string-left-trim (character-bag string)
  (subseq string (start-position-with-bag character-bag string)))

(defun string-right-trim (character-bag string)
  (subseq string 0 (end-position-with-bag character-bag string)))

(defun string= (string1 string2 &key start1 end1 start2 end2)
  (setq string1 (string string1))
  (setq string2 (string string2))
  (let ((string1 (if (or start1 end1)
                     (subseq string1 start1 end1)
                     string1))
        (string2 (if (or start2 end2)
                     (subseq string2 start2 end2)
                     string2)))
    (eql (system::array-to-js-string string1)
         (system::array-to-js-string string2))))

(defun string/= (string1 string2 &key start1 end1 start2 end2)
  (not (string= string1 string2 :start1 start :end1 end1 :start2 start2 :end2 end2)))

(defun string< (string1 string2 &key start1 end1 start2 end2)
  )

(defun string> (string1 string2 &key start1 end1 start2 end2)
  )

(defun string<= (string1 string2 &key start1 end1 start2 end2)
  )

(defun string>= (string1 string2 &key start1 end1 start2 end2)
  )

(defun string-equal (string1 string2 &key start1 end1 start2 end2)
  (string= (string-upcase x) (string-upcase y) :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(defun string-not-equal (string1 string2 &key start1 end1 start2 end2)
  )

(defun string-greaterp (string1 string2 &key start1 end1 start2 end2)
  )

(defun string-not-greaterp (string1 string2 &key start1 end1 start2 end2)
  )

(defun string-lessp (string1 string2 &key start1 end1 start2 end2)
  )

(defun string-not-lessp (string1 string2 &key start1 end1 start2 end2)
  )

(defun char (string index)
  (aref string index))

(defun schar (string index)
  (aref string index))

(defun make-string (size &key initial-element (element-type 'character))
  )
