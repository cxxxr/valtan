(in-package :compiler)

(defun make-keyword (x)
  (intern (princ-to-string x) :keyword))

(defvar *genvar-counter* 0)

(defun genvar (prefix &optional (kind :string))
  (ecase kind
    (:string
     (format nil "~A_~D" prefix (incf *genvar-counter*)))
    (:symbol
     (make-symbol (format nil "~A_~D" prefix (incf *genvar-counter*))))))

(defun split-string (character string &key (start 0))
  (let ((pos (position character string :start start)))
    (if pos
        (cons (subseq string start pos)
              (split-string character string :start (1+ pos)))
        (list (subseq string start)))))

(defun js-symbol-p (symbol)
  (and (symbolp symbol)
       (eq (find-package (symbol-package symbol))
           (find-package "JS"))))

(defun capitalize (string first out)
  (when (< 0 (length string))
    (write-char (if first
                    (char-downcase (aref string 0))
                    (char-upcase (aref string 0)))
                out)
    (let ((len (length string)))
      (do ((i 1 (1+ i)))
          ((= i len))
        (write-char (char-downcase (aref string i)) out)))))

(defun kebab-to-lower-camel-case (string)
  (let ((parts (split-string #\- string))
        (first t))
    (with-output-to-string (out)
      (dolist (part parts)
        (capitalize part first out)
        (setq first nil)))))

(defun parse-js-name (symbol)
  (let ((parts (split-string #\. (symbol-name symbol))))
    (mapcar #'kebab-to-lower-camel-case parts)))

(defmacro do-vector ((var vector) &body body)
  (let ((g-vector (gensym))
        (g-i (gensym)))
    `(let ((,g-vector ,vector))
       (dotimes (,g-i (length ,g-vector))
         (let ((,var (aref ,g-vector ,g-i)))
           ,@body)))))

(defun vector-first (vector)
  (aref vector 0))

(defun (setf vector-first) (value vector)
  (setf (aref vector 0) value))

(defun vector-last (vector)
  (aref vector (1- (length vector))))

(defun (setf vector-last) (value vector)
  (setf (aref vector (1- (length vector))) value))

(defun length=1 (list)
  (and (not (null list))
       (null (cdr list))))

(defun length=n (list n)
  (length=1 (nthcdr (1- n) list)))

(defun length>1 (list)
  (not (null (cdr list))))

(defun hash-table-to-alist (hash-table)
  (let ((alist '()))
    (maphash (lambda (n dominators)
               (push (cons n dominators) alist))
             hash-table)
    alist))
