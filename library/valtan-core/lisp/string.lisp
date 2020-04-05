#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

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
         (*:raw-string-to-array (*:code-to-raw-string (char-code x))))
        ((symbolp x)
         (symbol-name x))
        (t
         (type-error x 'string-designator))))

(defun *:string-append (x y)
  (unless (stringp x)
    (type-error x 'string))
  (unless (stringp y)
    (type-error y 'string))
  (*:raw-string-to-array
   (*:concat-raw-string/2 (*:array-to-raw-string x)
                          (*:array-to-raw-string y))))

(defun *:string-append* (string &rest strings)
  (dolist (s strings)
    (setq string (*:string-append string s)))
  string)

(defun string-upcase (string &key (start 0) end)
  (setq string (string string))
  (unless end (setq end (length string)))
  (if (and (= start 0) (= end (length string)))
      (*:raw-string-to-array (*:raw-string-upcase (*:array-to-raw-string string)))
      (*:string-append* (subseq string 0 start)
                        (*:raw-string-to-array
                         (*:raw-string-upcase (*:array-to-raw-string (subseq string start end))))
                        (subseq string end))))

(defun string-downcase (string &key (start 0) end)
  (setq string (string string))
  (unless end (setq end (length string)))
  (if (and (= start 0) (= end (length string)))
      (*:raw-string-to-array (*:raw-string-downcase (*:array-to-raw-string string)))
      (*:string-append* (subseq string 0 start)
                        (*:raw-string-to-array
                         (*:raw-string-downcase (*:array-to-raw-string (subseq string start end))))
                        (subseq string end))))

(defun string-capitalize (string &key (start 0) end)
  (setq string (string string))
  (unless end (setq end (length string)))
  (let ((first t)
        (new-string (make-array (length string) :element-type 'character :initial-contents string)))
    (do ((i start (1+ i)))
        ((>= i end))
      (let ((c (aref string i)))
        (setf (aref new-string i)
              (if first
                  (char-upcase c)
                  (char-downcase c)))
        (cond ((not (alphanumericp c))
               (setq first t))
              (first
               (setq first nil)))))
    new-string))

(defun nstring-upcase (string &key (start 0) end)
  (setq end (or end (length string)))
  (do ((i start (1+ i)))
      ((>= i end))
    (setf (aref string i) (char-upcase (aref string i))))
  string)

(defun nstring-downcase (string &key (start 0) end)
  (setq end (or end (length string)))
  (do ((i start (1+ i)))
      ((>= i end))
    (setf (aref string i) (char-downcase (aref string i))))
  string)

(defun nstring-capitalize (string &key (start 0) end)
  (setq end (or end (length string)))
  (let ((first t))
    (do ((i start (1+ i)))
        ((>= i end))
      (let ((c (aref string i)))
        (setf (aref string i)
              (if first
                  (char-upcase c)
                  (char-downcase c)))
        (cond ((not (alphanumericp c))
               (setq first t))
              (first
               (setq first nil)))))
    string))

(defun start-position-with-bag (character-bag string)
  (let ((length (length string)))
    (dotimes (i length length)
      (unless (find (aref string i) character-bag)
        (return i)))))

(defun end-position-with-bag (character-bag string start)
  (do ((i (1- (length string)) (1- i)))
      ((< i start) start)
    (unless (find (aref string i) character-bag)
      (return (1+ i)))))

(defun string-trim (character-bag string)
  (setq string (string string))
  (let* ((start (start-position-with-bag character-bag string))
         (end (end-position-with-bag character-bag string start)))
    (subseq string start end)))

(defun string-left-trim (character-bag string)
  (setq string (string string))
  (subseq string (start-position-with-bag character-bag string)))

(defun string-right-trim (character-bag string)
  (setq string (string string))
  (subseq string 0 (end-position-with-bag character-bag string 0)))

(defun string-cmp (string1 string2 start1 end1 start2 end2 ignore-case)
  (setq string1 (string string1))
  (setq string2 (string string2))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (do ((i start1 (1+ i))
       (j start2 (1+ j)))
      (nil)
    (cond ((= i end1)
           (return (values (if (= j end2)
                               0
                               -1)
                           end1)))
          ((= j end2)
           (return (values 1 i))))
    (let ((a (aref string1 i))
          (b (aref string2 j)))
      (when ignore-case
        (setq a (char-upcase a))
        (setq b (char-upcase b)))
      (when (char/= a b)
        (return (values (if (char< a b) -1 1) i))))))

(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (= 0 (string-cmp string1 string2 start1 end1 start2 end2 nil)))

(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (cmp pos)
      (string-cmp string1 string2 start1 end1 start2 end2 nil)
    (if (= cmp 0) nil pos)))

(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (cmp pos)
      (string-cmp string1 string2 start1 end1 start2 end2 nil)
    (if (= cmp -1) pos nil)))

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (cmp pos)
      (string-cmp string1 string2 start1 end1 start2 end2 nil)
    (if (= cmp 1) pos nil)))

(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (cmp pos)
      (string-cmp string1 string2 start1 end1 start2 end2 nil)
    (if (= cmp 1) nil pos)))

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (cmp pos)
      (string-cmp string1 string2 start1 end1 start2 end2 nil)
    (if (= cmp -1) nil pos)))

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (= 0 (string-cmp string1 string2 start1 end1 start2 end2 t)))

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (cmp pos)
      (string-cmp string1 string2 start1 end1 start2 end2 t)
    (if (= cmp 0) nil pos)))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (cmp pos)
      (string-cmp string1 string2 start1 end1 start2 end2 t)
    (if (= cmp 1) pos nil)))

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (cmp pos)
      (string-cmp string1 string2 start1 end1 start2 end2 t)
    (if (= cmp 1) nil pos)))

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (cmp pos)
      (string-cmp string1 string2 start1 end1 start2 end2 t)
    (if (= cmp -1) pos nil)))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (cmp pos)
      (string-cmp string1 string2 start1 end1 start2 end2 t)
    (if (= cmp -1) nil pos)))

(defun char (string index)
  (aref string index))

(defun (cl:setf char) (char string index)
  (unless (characterp char)
    (type-error char 'character))
  (setf (aref string index) char))

(defun schar (string index)
  (aref string index))

(defun (cl:setf schar) (char string index)
  (unless (characterp char)
    (type-error char 'character))
  (setf (aref string index) char))

(defun make-string (size &key initial-element (element-type 'character))
  (make-array size :initial-element (or initial-element (code-char 0)) :element-type element-type))
