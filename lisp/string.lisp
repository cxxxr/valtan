(in-package :common-lisp)

(defun stringp (x)
  (and (arrayp x)
       (eq 'character (array-element-type x))))

(defun string (x)
  (cond ((stringp x)
         x)
        ((characterp x)
         (system::js-string-to-array ((ffi:ref "String" "fromCharCode") (char-code x))))
        ((symbolp x)
         (symbol-name x))
        (t
         (type-error x 'string-designator))))

(defun string= (x y)
  (eql (system::array-to-js-string (string x))
       (system::array-to-js-string (string y))))

(defun string-equal (x y)
  (string= (string-upcase x) (string-upcase y)))

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

(defun string-upcase (string &key (start 0) (end (length string)))
  (if (and (= start 0) (= end (length string)))
      (system::js-string-to-array ((ffi:ref (system::array-to-js-string string) "toUpperCase")))
      (system::string-append* (subseq string 0 start)
                              (system::js-string-to-array
                               ((ffi:ref (system::array-to-js-string (subseq string start end))
                                         "toUpperCase")))
                              (subseq string end))))

(defun string-downcase (string &key (start 0) (end (length string)))
  (if (and (= start 0) (= end (length string)))
      (system::js-string-to-array ((ffi:ref (system::array-to-js-string string) "toLowerCase")))
      (system::string-append* (subseq string 0 start)
                              (system::js-string-to-array
                               ((ffi:ref (system::array-to-js-string (subseq string start end))
                                         "toLowerCase")))
                              (subseq string end))))
