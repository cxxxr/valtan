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
  )

(defun nstring-upcase (string &key (start 0) end)
  )

(defun nstring-downcase (string &key (start 0) end)
  )

(defun nstring-capitalize (string &key (start 0) end)
  )

(defun string-trim (character-bag string)
  )

(defun string-left-trim (character-bag string)
  )

(defun string-right-trim (character-bag string)
  )

(defun string= (string1 string2 &key start1 end1 start2 end2)
  (eql (system::array-to-js-string (string x))
       (system::array-to-js-string (string y))))

(defun string/= (string1 string2 &key start1 end1 start2 end2)
  )

(defun string< (string1 string2 &key start1 end1 start2 end2)
  )

(defun string> (string1 string2 &key start1 end1 start2 end2)
  )

(defun string<= (string1 string2 &key start1 end1 start2 end2)
  )

(defun string>= (string1 string2 &key start1 end1 start2 end2)
  )

(defun string-equal (string1 string2 &key start1 end1 start2 end2)
  (string= (string-upcase x) (string-upcase y)))

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
