;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: must-printer.lisp,v 1.16 2004/08/09 02:49:54 yuji Exp $
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;;  * Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; printer control variables
(eql *print-base* 10)
(null *print-radix*)
(eq *print-case* :upcase)
*print-gensym*
(null *print-level*)
(null *print-length*)
(null *print-circle*)
*print-escape*
(null *print-readably*)
*print-pprint-dispatch*
(null *print-lines*)
(null *print-right-margin*)


;; string
(string= "abc" (write-to-string "abc" :escape nil))
(string= "\"abc\"" (write-to-string "abc" :readably t))
(string= "\"abc\"" (write-to-string "abc" :escape nil :readably t))

(string= "ABC" (write-to-string "ABC" :escape nil))
(string= "\"ABC\"" (write-to-string "ABC" :readably t))
(string= "\"ABC\"" (write-to-string "ABC" :escape nil :readably t))

(string= "\"A\\\\B\\\"C\"" (write-to-string "A\\B\"C" :escape nil :readably t))
(string= "\"A\\\\B\\\"C\"" (write-to-string "A\\B\"C"))
(string= "A\\B\"C" (write-to-string "A\\B\"C" :escape nil))
(let ((str "a\\b\""))
  (and (= 4 (length str))
       (string= str (read-from-string (write-to-string str)))))
(let ((str "a\\b\""))
  (and (= 4 (length str))
       (string= str (read-from-string
                     (write-to-string str :escape nil :readably t)))))

(string= "\"\\\"\"" (write-to-string "\""))
(string= "\"\\\"\"" (write-to-string "\"" :escape nil :readably t))
(string= "\"" (read-from-string (write-to-string "\"")))
(string= "\"" (read-from-string (write-to-string "\"" :escape nil :readably t)))

(string= "\"\"" (write-to-string ""))
(string= "\"\"" (write-to-string "" :escape nil :readably t))
(string= "" (write-to-string "" :escape nil))

(string= "\" \"" (write-to-string " "))
(string= "\" \"" (write-to-string " " :escape nil :readably t))
(string= " " (write-to-string " " :escape nil))

(string= "\"	\"" (write-to-string "	"))
(string= "\"	\"" (write-to-string "	" :escape nil :readably t))
(string= "	" (write-to-string "	" :escape nil))

(string= "\"
\"" (write-to-string "
" :escape nil :readably t))
(string= "
" (write-to-string "
" :escape nil))


(string= "\"\\\"\\\"\\\"\\\"\\\"\\\"\""
         (write-to-string "\"\"\"\"\"\"" :readably t))
(string= "\"\"\"\"\"\""
	 (read-from-string (write-to-string "\"\"\"\"\"\"" :readably t)))
(string= "\"\"\"\"\"\""
	 (write-to-string "\"\"\"\"\"\"" :readably nil :escape nil))
(string= "\"	 	\""
	 (write-to-string "	 	" :readably t))

(string= "\"\\\"Hi\\\" \\\"Oh, hi!\\\"\""
	 (write-to-string "\"Hi\" \"Oh, hi!\"" :readably t))
(string= "\"Hi\" \"Oh, hi!\""
	 (write-to-string "\"Hi\" \"Oh, hi!\""
                          :pretty nil :readably nil :escape nil))

(string= "abc"
         (write-to-string "abc" :array nil :escape nil)
         ;; 22.1.3.4 Printing Strings
         ;; http://www.lispworks.com/reference/HyperSpec/Body/22_acd.htm
         ;; The printing of strings is not affected by *print-array*. 
         )



(string= "abc"
         (write-to-string (make-array 10
                                      :element-type 'character
                                      :initial-contents "abcdefghij"
                                      :fill-pointer 3)
                          :escape nil))



;; integer, *print-base*, *print-radix*
(string= (write-to-string 0) "0")
(string= (write-to-string -0) "0")
(string= (write-to-string 9) "9")
(string= (write-to-string -10) "-10")
(string= (write-to-string 1234567890987654321234567890987654321)
         "1234567890987654321234567890987654321")
(let ((*print-radix* t)) (string= (write-to-string 0) "0."))
(let ((*print-radix* t)) (string= (write-to-string -52) "-52."))
(let ((*print-radix* t))
  (string= (write-to-string -1234567890987654321234567890987654321)
           "-1234567890987654321234567890987654321."))

(let ((*print-base* 2)) (string= (write-to-string 0) "0"))
(let ((*print-base* 2)) (string= (write-to-string 10) "1010"))
(let ((*print-base* 2))
  (string= (write-to-string -1234567890987654321234567890987654321)
           "-111011011100010011100101100000010011000101110111101001110100010101110010000101001111011010110110001011000001110010110001"))
(let ((*print-base* 2) (*print-radix* t))
  (string= (write-to-string 11) "#b1011"))
(let ((*print-base* 2) (*print-radix* t))
  (string= (write-to-string -15) "#b-1111"))
(let ((*print-base* 2) (*print-radix* t))
  (string= (write-to-string 1234567890987654321234567890987654321)
           "#b111011011100010011100101100000010011000101110111101001110100010101110010000101001111011010110110001011000001110010110001"))


(let ((*print-base* 8)) (string= (write-to-string 10) "12"))
(let ((*print-base* 8)) (string= (write-to-string -21) "-25"))
(let ((*print-base* 8) (*print-radix* t))
  (string= (write-to-string 11) "#o13"))
(let ((*print-base* 8) (*print-radix* t))
  (string= (write-to-string -13) "#o-15"))
(let ((*print-base* 8))
  (string= (write-to-string 1234567890987654321234567890987654321)
         "7334234540230567516425620517326613016261"))
(let ((*print-base* 8) (*print-radix* t))
  (string= (write-to-string -1234567890987654321234567890987654321)
           "#o-7334234540230567516425620517326613016261"))


(let ((*print-base* 16)) (string= (write-to-string 20) "14"))
(let ((*print-base* 16)) (string= (write-to-string -22) "-16"))
(let ((*print-base* 16)) (string= (string-upcase (write-to-string -30)) "-1E"))
(let ((*print-base* 16) (*print-radix* t))
  (string= (write-to-string 21) "#x15"))
(let ((*print-base* 16) (*print-radix* t))
  (string= (write-to-string -23) "#x-17"))
(let ((*print-base* 16))
  (string= (string-upcase (write-to-string 1234567890987654321234567890987654321))
           "EDC4E5813177A7457214F6B62C1CB1"))
(let ((*print-base* 16) (*print-radix* t))
  (string= (string-upcase (write-to-string -1234567890987654321234567890987654321))
           "#X-EDC4E5813177A7457214F6B62C1CB1"))

(let ((*print-base* 24.)) (string= (write-to-string 9) "9"))
(let ((*print-base* 24.))
  (string= (string-upcase (write-to-string 17)) "H"))
(let ((*print-base* 24.))
  (string= (string-upcase (write-to-string -17)) "-H"))
(let ((*print-base* 24.) (*print-radix* t)) 
  (string= (write-to-string 9.) "#24r9"))
(let ((*print-base* 24.) (*print-radix* t)) 
  (string-equal (write-to-string 23.) "#24rN"))
(let ((*print-base* 24.) (*print-radix* t))
  (string-equal (write-to-string -23.) "#24r-N"))
(let ((*print-base* 24))
  (string= (string-upcase (write-to-string 1234567890987654321234567890987654321))
           "1EDFC9EAF544D8D12FI44J4FMCH"))

(loop for *print-base* from 2 upto 36
      always (string= (write-to-string 0) "0"))
(loop for *print-base* from 2 upto 36
      always (string= (write-to-string -1) "-1"))
(loop for *print-base* from 2 upto 36
      always (string= (string-upcase (write-to-string (1- *print-base*)))
                      (string (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                    (1- *print-base*)))))
(loop for *print-base* from 2 upto 36
      always (string= (write-to-string *print-base*) "10"))
(let ((list nil))
  (equal (dotimes (i 35 (reverse list))
           (let ((*print-base* (+ i 2)))
             ;;collect the decimal number 40 in each base from 2 to 36
             (push (string-upcase (write-to-string 40)) list)))
         '("101000" "1111" "220" "130" "104" "55" "50" "44" "40" "37" "34"
           "31" "2C" "2A" "28" "26" "24" "22" "20" "1J" "1I" "1H" "1G" "1F"
           "1E" "1D" "1C" "1B" "1A" "19" "18" "17" "16" "15" "14")))
(let ((list nil))
  (equal (dotimes (i 35 (reverse list))
           (let ((*print-base* (+ i 2))
                 (*print-radix* t))
             ;;collect the decimal number 40 in each base from 2 to 36
             (push (string-upcase (write-to-string 40)) list)))
         '("#B101000" "#3R1111" "#4R220" "#5R130" "#6R104" "#7R55" "#O50"
           "#9R44" "40." "#11R37" "#12R34" "#13R31" "#14R2C" "#15R2A"
           "#X28" "#17R26" "#18R24" "#19R22" "#20R20" "#21R1J" "#22R1I"
           "#23R1H" "#24R1G" "#25R1F" "#26R1E" "#27R1D" "#28R1C" "#29R1B"
           "#30R1A" "#31R19" "#32R18" "#33R17" "#34R16" "#35R15" "#36R14")))


;; ratio, *print-base*, *print-radix*
(string= (write-to-string 1/3) "1/3")
(string= (write-to-string -1/2) "-1/2")
(string= (write-to-string -3/5) "-3/5")
(let ((*print-radix* t))
  ;; Variable *PRINT-BASE*, *PRINT-RADIX*
  ;; http://www.lispworks.com/reference/HyperSpec/Body/v_pr_bas.htm
  ;; For integers, base ten is indicated by a trailing decimal point instead
  ;; of a leading radix specifier; for ratios, #10r is used.
  (string= (write-to-string 1/15) "#10r1/15"))
(let ((*print-radix* t))
  (string= (write-to-string -4/15) "#10r-4/15"))
(string= (write-to-string 2/1234567890987654321234567890987654321)
         "2/1234567890987654321234567890987654321")
(string= (write-to-string 1234567890987654321234567890987654321/4)
         "1234567890987654321234567890987654321/4")
(let ((*print-radix* t))
  (string= (write-to-string 2/1234567890987654321234567890987654321)
           "#10r2/1234567890987654321234567890987654321"))

(let ((*print-base* 2)) (string= (write-to-string 1/3) "1/11"))
(let ((*print-base* 2)) (string= (write-to-string -1/2) "-1/10"))
(let ((*print-base* 2)) (string= (write-to-string -3/5) "-11/101"))
(let ((*print-base* 2) (*print-radix* t))
  (string= (write-to-string 1/15) "#b1/1111"))
(let ((*print-base* 2) (*print-radix* t))
  (string= (write-to-string -3/16) "#b-11/10000"))
(let ((*print-base* 2))
  (string= (write-to-string 2/1234567890987654321234567890987654321)
           "10/111011011100010011100101100000010011000101110111101001110100010101110010000101001111011010110110001011000001110010110001"))
(let ((*print-base* 2))
  (string= (write-to-string -1234567890987654321234567890987654321/2)
           "-111011011100010011100101100000010011000101110111101001110100010101110010000101001111011010110110001011000001110010110001/10"))
(let ((*print-base* 2) (*print-radix* t))
  (string= (write-to-string 2/1234567890987654321234567890987654321)
           "#b10/111011011100010011100101100000010011000101110111101001110100010101110010000101001111011010110110001011000001110010110001"))

(let ((*print-base* 8)) (string= (write-to-string 1/3) "1/3"))
(let ((*print-base* 8)) (string= (write-to-string -1/4) "-1/4"))
(let ((*print-base* 8)) (string= (write-to-string -3/7) "-3/7"))
(let ((*print-base* 8)
      (*print-radix* t))
  (string= (write-to-string 1/3) "#o1/3"))
(let ((*print-base* 8)
      (*print-radix* t))
  (string= (write-to-string -3/7) "#o-3/7"))
(let ((*print-base* 8)
      (*print-radix* t))
  (string= (write-to-string -15/11) "#o-17/13"))
(let ((*print-base* 8))
  (string= (write-to-string 2/1234567890987654321234567890987654321)
           "2/7334234540230567516425620517326613016261"))
(let ((*print-base* 8)
      (*print-radix* t))
  (string= (write-to-string -1234567890987654321234567890987654321/4)
           "#o-7334234540230567516425620517326613016261/4"))

(let ((*print-base* 16)) (string= (write-to-string 1/8) "1/8"))
(let ((*print-base* 16)) (string= (write-to-string -1/9) "-1/9"))
(let ((*print-base* 16)) (string-equal (write-to-string -9/10) "-9/A"))
(let ((*print-base* 16)
      (*print-radix* t))
  (string= (write-to-string 1/3) "#x1/3"))
(let ((*print-base* 16)
      (*print-radix* t))
  (string= (write-to-string 3/8) "#x3/8"))
(let ((*print-base* 16)
      (*print-radix* t))
  (string= (write-to-string -4/9) "#x-4/9"))
(let ((*print-base* 16))
  (string= (write-to-string 2/1234567890987654321234567890987654321)
           "2/EDC4E5813177A7457214F6B62C1CB1"))
(let ((*print-base* 16)
      (*print-radix* t))
  (string-equal (write-to-string 1234567890987654321234567890987654321/4)
                "#xEDC4E5813177A7457214F6B62C1CB1/4"))
(let ((*print-base* 16)
      (*print-radix* t))
  (string-equal (write-to-string 1234567890987654321234567890987654321/1234)
                "#xEDC4E5813177A7457214F6B62C1CB1/4D2"))

(let ((*print-base* 21)) (string= (write-to-string 1/8) "1/8"))
(let ((*print-base* 21)) (string= (write-to-string -1/9) "-1/9"))
(let ((*print-base* 21)) (string-equal (write-to-string -9/10) "-9/A"))
(let ((*print-base* 21)
      (*print-radix* t))
  (string= (write-to-string 1/4) "#21r1/4"))
(let ((*print-base* 21)
      (*print-radix* t))
  (string-equal (write-to-string -1/20) "#21r-1/K"))
(let ((*print-base* 21))
  (string= (write-to-string 2/1234567890987654321234567890987654321)
           "2/29FADE40CGDJK4D0654KEAD5K6EK"))
(let ((*print-base* 21)
      (*print-radix* t))
  (string-equal (write-to-string 1234567890987654321234567890987654321/1234)
                "#21r29FADE40CGDJK4D0654KEAD5K6EK/2GG"))

(loop for *print-base* from 3 upto 36
      always (string= (write-to-string 1/2) "1/2"))
(loop for *print-base* from 4 upto 36
      always (string= (write-to-string -1/3) "-1/3"))
(loop for *print-base* from 3 upto 36
      always (string=
              (string-upcase (write-to-string (/ 1 (1- *print-base*))))
              (concatenate 'string
                           "1/"
                           (string (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                         (1- *print-base*))))))
(loop for *print-base* from 2 upto 36
      always (string= (write-to-string (/ 1 *print-base*)) "1/10"))
(let ((list nil))
  (equal (dotimes (i 35 (reverse list))
           (let ((*print-base* (+ i 2)))
             ;;collect the decimal number 40 in each base from 2 to 36
             (push (string-upcase (write-to-string 41/40)) list)))
         '("101001/101000" "1112/1111" "221/220" "131/130" "105/104"
           "56/55" "51/50" "45/44" "41/40" "38/37" "35/34"
           "32/31" "2D/2C" "2B/2A" "29/28" "27/26" "25/24" "23/22" "21/20"
           "1K/1J" "1J/1I" "1I/1H" "1H/1G" "1G/1F"
           "1F/1E" "1E/1D" "1D/1C" "1C/1B" "1B/1A" "1A/19" "19/18" "18/17"
           "17/16" "16/15" "15/14")))
(let ((list nil))
  (equal (dotimes (i 35 (reverse list))
           (let ((*print-base* (+ i 2))
                 (*print-radix* t))
             ;;collect the decimal number 40 in each base from 2 to 36
             (push (string-upcase (write-to-string 41/40)) list)))
         '("#B101001/101000" "#3R1112/1111" "#4R221/220" "#5R131/130"
           "#6R105/104" "#7R56/55" "#O51/50" "#9R45/44" "#10R41/40"
           "#11R38/37" "#12R35/34" "#13R32/31" "#14R2D/2C" "#15R2B/2A"
           "#X29/28" "#17R27/26" "#18R25/24" "#19R23/22" "#20R21/20"
           "#21R1K/1J" "#22R1J/1I" "#23R1I/1H" "#24R1H/1G" "#25R1G/1F"
           "#26R1F/1E" "#27R1E/1D" "#28R1D/1C" "#29R1C/1B" "#30R1B/1A"
           "#31R1A/19" "#32R19/18" "#33R18/17" "#34R17/16" "#35R16/15"
           "#36R15/14")))

;; character
(let ((*print-escape* nil))
  (string= (write-to-string #\a) "a"))
(let ((*print-escape* nil)
      (*print-readably* nil))
  (string= (write-to-string #\d) "d"))
(let ((*print-escape* nil))
  (string= (write-to-string #\m) "m"))
(let ((*print-escape* nil))
  (string= (write-to-string #\z) "z"))
(let ((*print-escape* nil)
      (*print-readably* nil))
  (loop for c across " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~"
        always (string= (write-to-string c) (string c))))
(let ((*print-escape* nil))
  (loop for c across " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~"
        always (string= (write-to-string c) (string c))))

(string= (write-to-string #\b) "#\\b")
(string= (write-to-string #\n) "#\\n")
(string= (write-to-string #\x) "#\\x")
(let ((*print-escape* nil)
      (*print-readably* t))
  (string= (write-to-string #\c) "#\\c"))
(loop for c across "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_'abcdefghijklmnopqrstuvwxyz{|}~"
      always (string= (write-to-string c) (concatenate 'string "#\\" (string c))))
(string= (write-to-string #\\) "#\\\\")
(string= (write-to-string #\") "#\\\"")
(let ((*print-readably* t)
      (*print-escape* nil))
  (loop for c across "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_'abcdefghijklmnopqrstuvwxyz{|}~"
        always (string= (write-to-string c) (concatenate 'string "#\\" (string c)))))
(let ((*print-readably* t)
      (*print-escape* t))
  (loop for c across "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_'abcdefghijklmnopqrstuvwxyz{|}~"
        always (string= (write-to-string c) (concatenate 'string "#\\" (string c)))))
(let ((*print-readably* nil)
      (*print-escape* t))
  (loop for c across "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_'abcdefghijklmnopqrstuvwxyz{|}~"
        always (string= (write-to-string c) (concatenate 'string "#\\" (string c)))))

(progn
  (let ((*print-readably* t))
    ;; 22.1.3.2 Printing Characters
    ;; http://www.lispworks.com/reference/HyperSpec/Body/22_acb.htm
    ;; For the graphic standard characters, the character itself is always used
    ;; for printing in #\ notation---even if the character also has a name[5].
    ;;
    ;; http://www.lispworks.com/reference/HyperSpec/Body/26_glo_g.htm#graphic
    ;; graphic adj. -snip- Space is defined to be graphic.
    (string= (write-to-string #\Space) "#\\ "))
    'skipped)


;;; symbol
;; accessible symbol, escaping off, *print-case* :capitalize
(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc" (write-to-string '|abc| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "Abc" (write-to-string '|abc| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "abc" (write-to-string '|abc| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "ABC" (write-to-string '|abc| :escape nil :case :capitalize)))


(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "Abc" (write-to-string '|ABC| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "ABC" (write-to-string '|ABC| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "ABC" (write-to-string '|ABC| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "abc" (write-to-string '|ABC| :escape nil :case :capitalize)))


(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "Abc-abc"
           (write-to-string '|ABC-abc| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "ABC-Abc"
	   (write-to-string '|ABC-abc| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "ABC-abc"
	   (write-to-string '|ABC-abc| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "ABC-abc"
	   (write-to-string '|ABC-abc| :escape nil :case :capitalize)))


(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc-Abc"
	   (write-to-string '|abc-ABC| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "Abc-ABC"
	   (write-to-string '|abc-ABC| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "abc-ABC"
	   (write-to-string '|abc-ABC| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "abc-ABC"
	   (write-to-string '|abc-ABC| :escape nil :case :capitalize)))


;; accessible symbol, escaping off, *print-case* :upcase
(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc" (write-to-string '|abc| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "ABC" (write-to-string '|abc| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "abc" (write-to-string '|abc| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "ABC" (write-to-string '|abc| :escape nil :case :upcase)))


(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "ABC" (write-to-string '|ABC| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "ABC" (write-to-string '|ABC| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "ABC" (write-to-string '|ABC| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "abc" (write-to-string '|ABC| :escape nil :case :upcase)))


(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "ABC-abc" (write-to-string '|ABC-abc| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "ABC-ABC" (write-to-string '|ABC-abc| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "ABC-abc" (write-to-string '|ABC-abc| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "ABC-abc" (write-to-string '|ABC-abc| :escape nil :case :upcase)))


(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc-ABC" (write-to-string '|abc-ABC| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "ABC-ABC" (write-to-string '|abc-ABC| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "abc-ABC" (write-to-string '|abc-ABC| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "abc-ABC" (write-to-string '|abc-ABC| :escape nil :case :upcase)))


;; accessible symbol, escaping off, *print-case* :downcase
(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc" (write-to-string '|abc| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "abc" (write-to-string '|abc| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "abc" (write-to-string '|abc| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "ABC" (write-to-string '|abc| :escape nil :case :downcase)))


(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc" (write-to-string '|ABC| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "ABC" (write-to-string '|ABC| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "ABC" (write-to-string '|ABC| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "abc" (write-to-string '|ABC| :escape nil :case :downcase)))


(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc-abc" (write-to-string '|ABC-abc| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "ABC-abc" (write-to-string '|ABC-abc| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "ABC-abc" (write-to-string '|ABC-abc| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "ABC-abc" (write-to-string '|ABC-abc| :escape nil :case :downcase)))


(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc-abc" (write-to-string '|abc-ABC| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :downcase)
  (string= "abc-ABC" (write-to-string '|abc-ABC| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :preserve)
  (string= "abc-ABC" (write-to-string '|abc-ABC| :escape nil :case :downcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :invert)
  (string= "abc-ABC" (write-to-string '|abc-ABC| :escape nil :case :downcase)))



;; keyword symbol, escaping off
(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc-Abc"
           (write-to-string ':|abc-ABC| :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc-ABC" (write-to-string ':|abc-ABC| :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc-abc" (write-to-string ':|abc-ABC| :escape nil :case :downcase)))


;; non accessible symbol, escaping off
(let ((*readtable* (copy-readtable nil)))
  (when (find-package "TEST-PKG0") (delete-package "TEST-PKG0"))
  (make-package "TEST-PKG0" :use ())
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc" (write-to-string (intern "abc" "TEST-PKG0")
                                  :escape nil :case :capitalize)))

(let ((*readtable* (copy-readtable nil)))
  (when (find-package "TEST-PKG0") (delete-package "TEST-PKG0"))
  (make-package "TEST-PKG0" :use ())
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc" (write-to-string (intern "abc" "TEST-PKG0")
                                  :escape nil :case :upcase)))

(let ((*readtable* (copy-readtable nil)))
  (when (find-package "TEST-PKG0") (delete-package "TEST-PKG0"))
  (make-package "TEST-PKG0" :use ())
  (setf (readtable-case *readtable*) :upcase)
  (string= "abc" (write-to-string (intern "abc" "TEST-PKG0")
                                  :escape nil :case :downcase)))



;; accessible symbol, *print-readably* t
(loop
 named loop0
 with printed-name
 with *readtable* = (copy-readtable nil)
 for readtable-case in '(:upcase :downcase :preserve :invert)
 do (loop
     for *print-case* in '(:upcase :downcase :capitalize)
     do (loop
         for symbol in '(|ZEBRA| |Zebra| |zebra|)
         do (setf (readtable-case *readtable*) readtable-case)
            (setq printed-name (write-to-string symbol :readably t))
         unless (eq symbol (read-from-string printed-name))
           do (format t "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                      symbol printed-name readtable-case *print-case*)
              (return-from loop0 nil)))
 finally (return-from loop0 t))

;; keyword symbol, *print-readably* t
(loop
 named loop0
 with printed-name
 with *readtable* = (copy-readtable nil)
 for readtable-case in '(:upcase :downcase :preserve :invert)
 do (loop
     for *print-case* in '(:upcase :downcase :capitalize)
     do (loop
         for symbol in '(:|ZEBRA| :|Zebra| :|zebra|)
         do (setf (readtable-case *readtable*) readtable-case)
            (setq printed-name (write-to-string symbol :readably t))
         unless (eq symbol (read-from-string printed-name))
           do (format t "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                      symbol printed-name readtable-case *print-case*)
              (return-from loop0 nil)))
 finally (return-from loop0 t))

;; non accessible symbol, *print-readably* t
(progn
  (when (find-package "TEST-PKG0") (delete-package "TEST-PKG0"))
  (make-package "TEST-PKG0" :use ())
  (loop
   named loop0
   with printed-name
   with *readtable* = (copy-readtable nil)
   for readtable-case in '(:upcase :downcase :preserve :invert)
   do (loop
       for *print-case* in '(:upcase :downcase :capitalize)
       do (loop
           for symbol in (mapcar #'(lambda (name) (intern name "TEST-PKG0"))
                                 '("ZEBRA" "Zebra" "zebra"))
           do (setf (readtable-case *readtable*) readtable-case)
              (setq printed-name (write-to-string symbol :readably t))
           unless (eq symbol (read-from-string printed-name))
             do (format t "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                        symbol printed-name readtable-case *print-case*)
             (return-from loop0 nil)))
   finally (return-from loop0 t)))


;; symbols having nongraphic characters in their name
(eq '|	| (read-from-string (write-to-string '|	| :readably t)))
(eq '|
| (read-from-string (write-to-string '|
| :readably t)))

;; symbols having nonalphabetic characters in their name
(eq '| | (read-from-string (write-to-string '| | :readably t)))
(eq '|"| (read-from-string (write-to-string '|"| :readably t)))
(eq '|#| (read-from-string (write-to-string '|#| :readably t)))
(eq '|'| (read-from-string (write-to-string '|'| :readably t)))
(eq '|(| (read-from-string (write-to-string '|(| :readably t)))
(eq '|)| (read-from-string (write-to-string '|)| :readably t)))
(eq '|,| (read-from-string (write-to-string '|,| :readably t)))
(eq '|;| (read-from-string (write-to-string '|;| :readably t)))
(eq '|\\| (read-from-string (write-to-string '|\\| :readably t)))
(= 1 (length (symbol-name (read-from-string (write-to-string '|\\|
							     :readably t)))))
(eq '|`| (read-from-string (write-to-string '|`| :readably t)))
(eq '|\|| (read-from-string (write-to-string '|\|| :readably t)))
(= 1 (length (symbol-name (read-from-string (write-to-string '|\||
							     :readably t)))))
(loop
 for symbol in '(|-!-| |/*/| |$$$| |^^^^^^^^^^^^^|)
 always (loop
         with *readtable* = (copy-readtable nil)
         for table-case in '(:upcase :downcase :preserve :invert)
         do (setf (readtable-case *readtable*) table-case)
         always (loop for *print-case in '(:upcase :downcase :capitalize)
                      always (string= (symbol-name symbol)
                                      (write-to-string symbol :escape nil)))))

;; uninterned symbols
(string= "ABC"
	 (symbol-name (read-from-string (write-to-string (make-symbol "ABC")
							 :readably t
							 :case :upcase))))
(string= "ABC"
	 (symbol-name (read-from-string (write-to-string (make-symbol "ABC")
							 :readably t
							 :case :downcase))))
(string= "ABC"
	 (symbol-name (read-from-string (write-to-string (make-symbol "ABC")
							 :readably t
							 :case :capitalize))))
(string= "G01" (write-to-string (make-symbol "G01") :escape t :gensym nil))
(string= "G01" (write-to-string (make-symbol "G01") :escape nil :gensym nil))
(string= "#:G01" (write-to-string (make-symbol "G01") :escape t :gensym t))
#-CLISP ;Bruno: CLISP prints symbols readably with vertical bars: "#:|G01|"
(string= "#:G01"
         ;; Variable *PRINT-READABLY*
         ;; http://www.lispworks.com/reference/HyperSpec/Body/v_pr_rda.htm
         ;; If the value of some other printer control variable is such
         ;; that these requirements would be violated, the value of that
         ;; other variable is ignored.
         ;; Specifically, if *print-readably* is true, printing proceeds
         ;; as if *print-escape*, *print-array*, and *print-gensym* were
         ;; also true, and as if *print-length*, *print-level*, and
         ;; *print-lines* were false.         
         (write-to-string (make-symbol "G01")
                          :escape nil :gensym nil :readably t))


;; "FACE" as a symbol when *read-base* is 16
(let ((face (let ((*print-base* 16)) (write-to-string 'face :readably t)))
      (*read-base* 16))
  ;; 22.1.3.3 Printing Symbols
  ;; http://www.lispworks.com/reference/HyperSpec/Body/22_acc.htm
  ;; When printing a symbol, the printer inserts enough single escape
  ;; and/or multiple escape characters (backslashes and/or vertical-bars)
  ;; so that if read were called with the same *readtable* and with
  ;; *read-base* bound to the current output base, it would return the same
  ;; symbol (if it is not apparently uninterned) or an uninterned symbol
  ;; with the same print name (otherwise).
  ;; For example, if the value of *print-base* were 16 when printing the
  ;; symbol face, it would have to be printed as \FACE or \Face or |FACE|,
  ;; because the token face would be read as a hexadecimal number (decimal
  ;; value 64206) if the value of *read-base* were 16.
  (eq 'face (read-from-string face)))


(eq '|01| (read-from-string (write-to-string '|01| :readably t)))
(eq '|1| (read-from-string (write-to-string '|1| :readably t)))
(eq '|0123456789| (read-from-string (write-to-string '|0123456789|
                                                     :readably t)))

;; symbols in a package with a mixed case name, *print-readably* t
(progn
  (when (find-package "Test-Pkg0") (delete-package "Test-Pkg0"))
  (make-package "Test-Pkg0" :use ())
  (loop
   named loop0
   with printed-name
   with *readtable* = (copy-readtable nil)
   for readtable-case in '(:upcase :downcase :preserve :invert)
   do (loop
       for *print-case* in '(:upcase :downcase :capitalize)
       do (loop
           for symbol in (mapcar #'(lambda (name) (intern name "Test-Pkg0"))
                                 '("ZEBRA" "Zebra" "zebra"))
           do (setf (readtable-case *readtable*) readtable-case)
              (setq printed-name (write-to-string symbol :readably t))
           unless (eq symbol (read-from-string printed-name))
             do (format t "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                        symbol printed-name readtable-case *print-case*)
             (return-from loop0 nil)))
   finally (return-from loop0 t)))

;; symbols in a package with weird chars in the name, *print-readably* t
(progn
  (when (find-package "Test\|Pkg 0\;") (delete-package "Test\|Pkg 0\;"))
  (make-package "Test\|Pkg 0\;" :use ())
  (loop
   named loop0
   with *readtable* = (copy-readtable nil)
   for readtable-case in '(:upcase :downcase :preserve :invert)
   do (loop
       for *print-case* in '(:upcase :downcase :capitalize)
       do (loop
           for symbol in (mapcar #'(lambda (name) (intern name "Test\|Pkg 0\;"))
                                 '("ZEBRA" "Zebra" "zebra"))
           do (setf (readtable-case *readtable*) readtable-case)
           unless (eq symbol (read-from-string (write-to-string symbol
                                                                :readably t)))
             do (format t "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                        symbol printed-name readtable-case *print-case*)
             (return-from loop0 nil)))
   finally (return-from loop0 t)))


;; weird symbols in a weird package, *print-readably* t
(progn
  (when (find-package "Test\|Pkg 0\;") (delete-package "Test\|Pkg 0\;"))
  (make-package "Test\|Pkg 0\;" :use ())
  (loop
   named loop0
   with *readtable* = (copy-readtable nil)
   for readtable-case in '(:upcase :downcase :preserve :invert)
   do (loop
       for *print-case* in '(:upcase :downcase :capitalize)
       do (loop
           for symbol in (mapcar #'(lambda (name) (intern name "Test\|Pkg 0\;"))
                                 '("Z\\E\"BRA" "Z\;e\|bra" "z\:e bra"))
           do (setf (readtable-case *readtable*) readtable-case)
           unless (eq symbol (read-from-string (write-to-string symbol
                                                                :readably t)))
             do (format t "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                        symbol printed-name readtable-case *print-case*)
             (return-from loop0 nil)))
   finally (return-from loop0 t)))



;; bit-vector
(string= "#*0101" (write-to-string #*0101 :readably t :array t))
(string= "#*01" (write-to-string #*01 :readably t :array t))
(string= "#*0" (write-to-string #*0 :readably t :array t))
(string= "#*1" (write-to-string #*1 :readably t :array t))
(string= "#*" (write-to-string #* :readably t :array t))
(string= "#*10101111000" (write-to-string #*10101111000
					  :readably t :array t))

(string= "#*0101" (write-to-string #*0101 :readably t :array nil))
(string= "#*01" (write-to-string #*01 :readably t :array nil))
(string= "#*0" (write-to-string #*0 :readably t :array nil))
(string= "#*1" (write-to-string #*1 :readably t :array nil))
(string= "#*" (write-to-string #* :readably t :array nil))
(string= "#*10101111000" (write-to-string #*10101111000
					  :readably t :array nil))

(string= "#*0101" (write-to-string #*0101 :array t))
(string= "#*01" (write-to-string #*01 :array t))
(string= "#*0" (write-to-string #*0 :array t))
(string= "#*1" (write-to-string #*1 :array t))
(string= "#*" (write-to-string #* :array t))
(string= "#*10101111000" (write-to-string #*10101111000 :array t))

(zerop (search "#<" (write-to-string #*0101 :array nil)))
(zerop (search "#<" (write-to-string #*01 :array nil)))
(zerop (search "#<" (write-to-string #*0 :array nil)))
(zerop (search "#<" (write-to-string #*1 :array nil)))
(zerop (search "#<" (write-to-string #* :array nil)))
(zerop (search "#<" (write-to-string #*10101111000 :array nil)))
(string= "#*01"
	 (write-to-string (make-array 10
                                      :element-type 'bit
				      :initial-contents '(0 1 0 1 0 1 0 1 0 1)
				      :fill-pointer 2)
			  :readably t :array t))


;; list
(null (read-from-string (write-to-string '())))
(string= (write-to-string '(1) :pretty nil) "(1)")
(string= (write-to-string '(1 2) :pretty nil) "(1 2)")
(string= (write-to-string '(1 2 3) :pretty nil) "(1 2 3)")
(string= (write-to-string '(1 2 3 4) :pretty nil) "(1 2 3 4)")
(string= (write-to-string '(1 . 2) :pretty nil) "(1 . 2)")
(string= (write-to-string '(1 2 . 3) :pretty nil) "(1 2 . 3)")
(string= (write-to-string '(1 2 3 . 4) :pretty nil) "(1 2 3 . 4)")
(let ((list (loop for i from 0 upto 100 collect i)))
  (equal (read-from-string (write-to-string list)) list))

;; list *print-level* *print-length*
(string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 0) "#")
(string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 1)
         "(1 #)")
(string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 2)
         "(1 (2 #))")
(string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 3)
         "(1 (2 (3 #)))")
(string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 4)
         "(1 (2 (3 (4 #))))")
(string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 4)
         "(1 (2 (3 (4 #))))")
(string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 5)
         "(1 (2 (3 (4 (5 #)))))")
(string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 6)
         "(1 (2 (3 (4 (5 (6))))))")
(string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 7)
         "(1 (2 (3 (4 (5 (6))))))")
(string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 100)
         "(1 (2 (3 (4 (5 (6))))))")

(string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 0) "(...)")
(string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 1) "(1 ...)")
(string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 2) "(1 2 ...)")
(string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 3) "(1 2 3 ...)")
(string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 4) "(1 2 3 4 ...)")
(string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 5)
         "(1 2 3 4 5 ...)")
(string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 6)
         "(1 2 3 4 5 6)")
(string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 7)
         "(1 2 3 4 5 6)")
(string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 100)
         "(1 2 3 4 5 6)")

(string= (write-to-string '(1 2 . 3) :pretty nil :length 0) "(...)")
(string= (write-to-string '(1 2 . 3) :pretty nil :length 1) "(1 ...)")
(string= (write-to-string '(1 2 . 3) :pretty nil :length 2) "(1 2 . 3)")
(string= (write-to-string '(1 2 . 3) :pretty nil :length 3) "(1 2 . 3)")

(string= (write-to-string '(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :level 0 :length 0)
         "#")
(string= (write-to-string '(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :level 1 :length 0)
         "(...)")
(string= (write-to-string '(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :level 0 :length 1)
         "#")
(string= (write-to-string '(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :level 1 :length 1)
         "(1 ...)")
(string= (write-to-string '(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :level 2 :length 1)
         "(1 ...)")
(string= (write-to-string '(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :level 2 :length 2)
         "(1 (2 #))")

(string= (write-to-string '((((1))) ((2)) (3) 4)
                          :pretty nil :level 0 :length 0)
         "#")
(string= (write-to-string '((((1))) ((2)) (3) 4)
                          :pretty nil :level 1 :length 0)
         "(...)")
(string= (write-to-string '((((1))) ((2)) (3) 4)
                          :pretty nil :level 1 :length 4)
         "(# # # 4)")
(string= (write-to-string '((((1))) ((2)) (3) 4)
                          :pretty nil :level 2 :length 3)
         "((#) (#) (3) ...)")
(string= (write-to-string '((((1))) ((2)) (3) 4)
                          :pretty nil :level 3 :length 3)
         "(((#)) ((2)) (3) ...)")
(string= (write-to-string '((((1))) ((2)) (3) 4)
                          :pretty nil :level 4 :length 3)
         "((((1))) ((2)) (3) ...)")
(string= (write-to-string '((((1))) ((2)) (3) 4)
                          :pretty nil :level 2 :length 4)
         "((#) (#) (3) 4)")
(string= (write-to-string '((((1))) ((2)) (3) 4)
                          :pretty nil :level 4 :length 4)
         "((((1))) ((2)) (3) 4)")

(string= (write-to-string '((((1))) ((2)) (3) 4 (5) ((6)) (((7))))
                          :pretty nil :level 3 :length 6)
         "(((#)) ((2)) (3) 4 (5) ((6)) ...)")

(string= (write-to-string '((((1 ((2)) (3)))) ((2 (3) 4 5 6)) (3 (4 (5 6))))
                          :pretty nil :level 6 :length 3)
         "((((1 ((2)) (3)))) ((2 (3) 4 ...)) (3 (4 (5 6))))")
(string= (write-to-string '((((1 ((2)) (3)))) ((2 (3) 4 5 6)) (3 (4 (5 6))))
                          :pretty nil :level 2 :length 2)
         "((#) (#) ...)")
(string= (write-to-string '((((1 ((2)) (3)))) ((2 (3) 4 5 6)) (3 (4 (5 6))))
                          :pretty nil :level 3 :length 2)
         "(((#)) ((2 # ...)) ...)")
(string= (write-to-string '(((1)) ((1) 2 ((3)) (((4)))) 3 (4))
                          :pretty nil :level 2 :length 3)
          "((#) (# 2 # ...) 3 ...)")



;; vector
;; 22.1.3.7 Printing Other Vectors
;; http://www.lispworks.com/reference/HyperSpec/Body/22_acg.htm
;;  If *print-array* is true and *print-readably* is false, any vector
;; other than a string or bit vector is printed using general-vector
;; syntax; this means that information about specialized vector
;; representations does not appear. The printed representation of a
;; zero-length vector is #(). The printed representation of a
;; non-zero-length vector begins with #(. Following that, the first
;; element of the vector is printed. If there are any other elements,
;; they are printed in turn, with each such additional element preceded
;; by a space if *print-pretty* is false, or whitespace[1] if
;; *print-pretty* is true. A right-parenthesis after the last element
;; terminates the printed representation of the vector.
(string= (write-to-string '#() :pretty nil :array t) "#()")
(string= (write-to-string '#(1) :pretty nil :array t) "#(1)")
(string= (write-to-string '#(1 2 3) :pretty nil :array t) "#(1 2 3)")
(string= (write-to-string (make-array 10
                                      :initial-contents '(0 1 2 3 4 5 6 7 8 9)
                                      :fill-pointer 3)
                          :pretty nil :array t)
         "#(0 1 2)")
                 
;; vector *print-level* *print-length*
(string= (write-to-string '#(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :array t :level 0) "#")
(string= (write-to-string '#(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :array t :level 1)
         "#(1 #)")
(string= (write-to-string '#(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :array t :level 2)
         "#(1 (2 #))")
(string= (write-to-string '#(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :array t :level 3)
         "#(1 (2 (3 #)))")
(string= (write-to-string '#(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :array t :level 4)
         "#(1 (2 (3 (4 #))))")
(string= (write-to-string '#(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :array t :level 4)
         "#(1 (2 (3 (4 #))))")
(string= (write-to-string '#(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :array t :level 5)
         "#(1 (2 (3 (4 (5 #)))))")
(string= (write-to-string '#(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :array t :level 6)
         "#(1 (2 (3 (4 (5 (6))))))")
(string= (write-to-string '#(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :array t :level 7)
         "#(1 (2 (3 (4 (5 (6))))))")
(string= (write-to-string '#(1 (2 (3 (4 (5 (6))))))
                          :pretty nil :array t :level 100)
         "#(1 (2 (3 (4 (5 (6))))))")

(string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 0)
         "#(...)")
(string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 1)
         "#(1 ...)")
(string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 2)
         "#(1 2 ...)")
(string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 3)
         "#(1 2 3 ...)")
(string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 4)
         "#(1 2 3 4 ...)")
(string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 5)
         "#(1 2 3 4 5 ...)")
(string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 6)
         "#(1 2 3 4 5 6)")
(string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 7)
         "#(1 2 3 4 5 6)")
(string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 100)
         "#(1 2 3 4 5 6)")

(string= (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                          :pretty nil :array t :level 0 :length 0)
         "#")
(string= (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                          :pretty nil :array t :level 1 :length 0)
         "#(...)")
(string= (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                          :pretty nil :array t :level 0 :length 1)
         "#")
(string= (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                          :pretty nil :array t :level 1 :length 1)
         "#(1 ...)")
(string= (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                          :pretty nil :array t :level 2 :length 1)
         "#(1 ...)")
(string= (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                          :pretty nil :array t :level 2 :length 2)
         "#(1 #(2 #))")

(string= (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                          :pretty nil :array t :level 0 :length 0)
         "#")
(string= (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                          :pretty nil :array t :level 1 :length 0)
         "#(...)")
(string= (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                          :pretty nil :array t :level 1 :length 4)
         "#(# # # 4)")
(string= (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                          :pretty nil :array t :level 2 :length 3)
         "#(#(#) #(#) #(3) ...)")
(string= (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                          :pretty nil :array t :level 3 :length 3)
         "#(#(#(#)) #(#(2)) #(3) ...)")
(string= (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                          :pretty nil :array t :level 4 :length 3)
         "#(#(#(#(1))) #(#(2)) #(3) ...)")
(string= (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                          :pretty nil :array t :level 2 :length 4)
         "#(#(#) #(#) #(3) 4)")
(string= (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                          :pretty nil :array t :level 4 :length 4)
         "#(#(#(#(1))) #(#(2)) #(3) 4)")

(string= (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4 #(5) #(#(6)) #(#(#(7))))
                          :pretty nil :array t :level 3 :length 6)
         "#(#(#(#)) #(#(2)) #(3) 4 #(5) #(#(6)) ...)")

(string= (write-to-string '#(#(#(#(1 #(#(2)) #(3))))
                             #(#(2 #(3) 4 5 6))
                             #(3 #(4 #(5 6))))
                          :pretty nil :array t :level 6 :length 3)
         "#(#(#(#(1 #(#(2)) #(3)))) #(#(2 #(3) 4 ...)) #(3 #(4 #(5 6))))")
(string= (write-to-string '#(#(#(#(1 #(#(2)) #(3))))
                             #(#(2 #(3) 4 5 6))
                             #(3 #(4 #(5 6))))
                          :pretty nil :array t :level 2 :length 2)
         "#(#(#) #(#) ...)")
(string= (write-to-string '#(#(#(#(1 #(#(2)) #(3))))
                             #(#(2 #(3) 4 5 6))
                             #(3 #(4 #(5 6))))
                          :pretty nil :array t :level 3 :length 2)
         "#(#(#(#)) #(#(2 # ...)) ...)")
(string= (write-to-string '#(#(#(1)) #(#(1) 2 #(#(3)) #(#(#(4)))) 3 #(4))
                          :pretty nil :array t :level 2 :length 3)
          "#(#(#) #(# 2 # ...) 3 ...)")


;; array
(string= (write-to-string '#0A1 :pretty nil :array t) "#0A1")
(string= (write-to-string '#1A() :pretty nil :array t) "#()")
(string= (write-to-string '#1A(1 2 3) :pretty nil :array t) "#(1 2 3)")
(string= (write-to-string '#2A((1 2 3) (4 5 6)) :pretty nil :array t)
         "#2A((1 2 3) (4 5 6))")
(string= (write-to-string '#3A(((1 a) (2 b) (3 c))
                               ((4 d) (5 e) (6 f))) :pretty nil :array t)
         "#3A(((1 A) (2 B) (3 C)) ((4 D) (5 E) (6 F)))")
(string= (write-to-string (make-array (make-list 20 :initial-element 1)
                                      :initial-element 0)
                          :pretty nil :array t)
         "#20A((((((((((((((((((((0))))))))))))))))))))")

;; array *print-level* *print-length*
;(string= (write-to-string '#0A10 :pretty nil :array t :level 0 :length 0) "#")
(string= (write-to-string '#0A10 :pretty nil :array t :level 1 :length 1)
         "#0A10")
(string= (write-to-string '#2A((0) (1) (2) (3))
                          :pretty nil :array t :level 1 :length 1)
         "#2A(# ...)")
(string= (write-to-string '#2A((0) (1) (2) (3))
                          :pretty nil :array t :level 2 :length 2)
         "#2A((0) (1) ...)")
(string= (write-to-string '#2A((0) (1) (2) (3))
                          :pretty nil :array t :level 2 :length 0)
         "#2A(...)")
(string= (write-to-string '#3A(((0) (1) (2)) ((3) (4) (5)))
                          :pretty nil :array t :level 3 :length 2)
         "#3A(((0) (1) ...) ((3) (4) ...))")
(string= (write-to-string (make-array (make-list 20 :initial-element 1)
                                      :initial-element 0)
                          :pretty nil :array t :level 0 :length 100)
         "#")
(string= (write-to-string (make-array (make-list 20 :initial-element 1)
                                      :initial-element 0)
                          :pretty nil :array t :level 100 :length 0)
         "#20A(...)")
(string= (write-to-string (make-array (make-list 20 :initial-element 1)
                                      :initial-element 0)
                          :pretty nil :array t :level 10 :length 100)
         "#20A((((((((((#))))))))))")
(string= (write-to-string '#2A((0 1 2) (3 4 5) (6 7 8) (9 10 11))
                          :pretty nil :array t :level 2 :length 2)
         "#2A((0 1 ...) (3 4 ...) ...)")
(string= (write-to-string '#2A((0 1 2) (3 4 5) (6 7 8) (9 10 11))
                          :pretty nil :array t :level 1 :length 2)
         "#2A(# # ...)")
(string= (write-to-string '#3A(((0) (1) (2)) ((3) (4) (5))
                               ((6) (7) (8)) ((9) (10) (11)))
                          :pretty nil :array t :level 2 :length 3)
         "#3A((# # #) (# # #) (# # #) ...)")
(string= (write-to-string '#3A(((0) (1) (2)) ((3) (4) (5))
                               ((6) (7) (8)) ((9) (10) (11)))
                          :pretty nil :array t :level 3 :length 4)
         "#3A(((0) (1) (2)) ((3) (4) (5)) ((6) (7) (8)) ((9) (10) (11)))")


;; *print-array*
(string= (write-to-string "abc" :array t :escape nil) "abc")
(string= (write-to-string "abc" :array nil :escape nil) "abc")
(= 2 (mismatch "#<" (write-to-string #() :array nil)))
(= 2 (mismatch "#<" (write-to-string #(1 2 3) :array nil)))
(= 2 (mismatch "#<" (write-to-string #*1010 :array nil)))
(= 2 (mismatch "#<" (write-to-string #2A((0 1 2) (3 4 5)) :array nil)))
(= 2 (mismatch "#<" (write-to-string #3A(((0 1) (2 3)) ((4 5) (6 7)))
                                     :array nil)))
(= 2 (mismatch "#<" (write-to-string #4A((((0) (1)) ((2) (3)))
                                         (((4) (5)) ((6) (7)))
                                         (((8) (9)) ((10) (11))) 
                                         (((12) (13)) ((14) (15))))
                                     :array nil)))



;; label
(let* ((list '#1=(#1# . #1#))
       (x (read-from-string (write-to-string list :circle t))))
  (and (eq x (car x))
       (eq x (cdr x))))

(let* ((list '#1=(a . #1#))
       (x (read-from-string (write-to-string list :circle t))))
  (and (eq (car x) 'a)
       (eq x (cdr x))))

(let* ((list '(a . #1=(b c . #1#)))
       (x (read-from-string (write-to-string list :circle t))))
  (and (eq (first x) 'a)
       (eq (second x) 'b)
       (eq (third x) 'c)
       (eq (fourth x) 'b)
       (eq (cdr x) (nthcdr 3 x))))

(let* ((list '(#1=#:G1041 #1#))
       (x (read-from-string (write-to-string list :circle t))))
  ;; 22.1.3.3.1 Package Prefixes for Symbols
  ;; http://www.lispworks.com/reference/HyperSpec/Body/22_acca.htm
  ;; Because the #: syntax does not intern the following symbol, it is
  ;; necessary to use circular-list syntax if *print-circle* is true and
  ;; the same uninterned symbol appears several times in an expression to
  ;; be printed. For example, the result of
  ;; 
  ;;  (let ((x (make-symbol "FOO"))) (list x x))
  ;; 
  ;; would be printed as (#:foo #:foo) if *print-circle* were false, but as
  ;; (#1=#:foo #1#) if *print-circle* were true.
  (and (= 2 (length x))
       (symbolp (first x))
       (eq (first x) (second x))))

(let* ((list '#1=(a (b #2=(x y z) . #1#) . #2#))
       (x (read-from-string (write-to-string list :circle t))))
  (and (eq (first x) 'a)
       (eq x (cddr (second x)))
       (eq (second (second x)) (cddr x))))

(let* ((list '#1=#(#1# a))
       (x (read-from-string (write-to-string list :circle t))))
  (and (eq x (aref x 0))
       (eq 'a (aref x 1))))

(let* ((list '#1=#(a #1#))
       (x (read-from-string (write-to-string list :circle t))))
  (and (eq (aref x 0) 'a)
       (eq x (aref x 1))))

(let* ((list '#(#1=#:G00 #1#))
       (x (read-from-string (write-to-string list :circle t))))
  (and (eq (aref x 0) (aref x 1))
       (string= (symbol-name (aref x 0)) "G00")
       (null (symbol-package (aref x 0)))))

(let* ((list '#(#(#1=#:G00) #2=#(#1# a) #(#2# #1#)))
       (x (read-from-string (write-to-string list :circle t))))
  (and (= 3 (length x))
       (= 1 (length (aref x 0)))
       (= 2 (length (aref x 1)))
       (= 2 (length (aref x 2)))
       (eq (aref (aref x 0) 0) (aref (aref x 1) 0))
       (eq 'a (aref (aref x 1) 1))
       (eq (aref (aref x 0) 0) (aref (aref x 2) 1))
       (eq (aref x 1) (aref (aref x 2) 0))))

(let* ((array '#1=#0A#1#)
       (x (read-from-string (write-to-string array :array t :circle t))))
  (and (null (array-dimensions array))
       (eq x (aref x))))
(let* ((array '#1=#2A((1 2 3) (4 5 #1#)))
       (x (read-from-string (write-to-string array :array t :circle t))))
  (and (equal (array-dimensions array) '(2 3))
       (= 1 (aref x 0 0))
       (= 2 (aref x 0 1))
       (= 3 (aref x 0 2))
       (= 4 (aref x 1 0))
       (= 5 (aref x 1 1))
       (eq x (aref x 1 2))))
(let* ((array #1=#3A(((1 a) (2 b) (3 #1#)) ((4 d) (5 e) (6 f))))
       (x (read-from-string (write-to-string array :array t :circle t))))
  (and (equal (array-dimensions array) '(2 3 2))
       (= 1 (aref x 0 0 0))
       (eq 'a (aref x 0 0 1))
       (= 2 (aref x 0 1 0))
       (eq 'b (aref x 0 1 1))
       (= 3 (aref x 0 2 0))
       (eq x (aref x 0 2 1))
       (= 4 (aref x 1 0 0))
       (eq 'd (aref x 1 0 1))
       (= 5 (aref x 1 1 0))
       (eq 'e (aref x 1 1 1))
       (= 6 (aref x 1 2 0))
       (eq 'f (aref x 1 2 1))))

(let* ((array #3A(((1 #1=#:G0) (#2=#:G1 b) (3 #1#)) ((4 d) (5 e) (#2# f))))
       (x (read-from-string (write-to-string array :array t :circle t))))
  (and (equal (array-dimensions array) '(2 3 2))
       (= 1 (aref x 0 0 0))
       (eq (aref x 0 0 1) (aref x 0 2 1))
       (null (symbol-package (aref x 0 0 1)))
       (string= "G0" (symbol-name (aref x 0 0 1)))
       (eq (aref x 0 1 0) (aref x 1 2 0))
       (null (symbol-package (aref x 0 1 0)))
       (string= "G1" (symbol-name (aref x 0 1 0)))
       (eq 'b (aref x 0 1 1))
       (= 3 (aref x 0 2 0))
       (= 4 (aref x 1 0 0))
       (eq 'd (aref x 1 0 1))
       (= 5 (aref x 1 1 0))
       (eq 'e (aref x 1 1 1))
       (eq 'f (aref x 1 2 1))))

(let* ((array #1=#3A(((#1# #2=#:G0) (#3=#:G1 #2#) (#3# #1#))
                     ((#1# #2#) (#2# #3#) (#2# #1#))))
       (x (read-from-string (write-to-string array :array t :circle t))))
  (and (equal (array-dimensions array) '(2 3 2))
       (eq x (aref x 0 0 0))
       (null (symbol-package (aref x 0 0 1)))
       (string= (symbol-name (aref x 0 0 1)) "G0")
       (null (symbol-package (aref x 0 1 0)))
       (string= (symbol-name (aref x 0 1 0)) "G1")
       (eq (aref x 0 1 0) (aref x 0 2 0))
       (eq x (aref x 0 2 1))
       (eq x (aref x 1 0 0))
       (eq (aref x 1 0 1) (aref x 0 0 1))
       (eq (aref x 1 1 0) (aref x 0 0 1))
       (eq (aref x 1 1 1) (aref x 0 1 0))
       (eq (aref x 1 2 0) (aref x 0 0 1))
       (eq (aref x 1 2 1) x)))

(let* ((array #4A((((0 #1=#:G00 2) (#1# 4 #2=#:G01))
                   ((#3=#:G02 #2# 8) (9 #4=#:G03 #3#))
                   ((#4# 12 #5=#:G04) (#6=#:G05 #6# #5#)))))
       (x (read-from-string (write-to-string array :array t :circle t))))
  (and (equal (array-dimensions array) '(1 3 2 3))
       (= 0 (aref x 0 0 0 0))
       (null (symbol-package (aref x 0 0 0 1)))
       (string= (symbol-name (aref x 0 0 0 1)) "G00")
       (= 2 (aref x 0 0 0 2))

       (eq (aref x 0 0 1 0) (aref x 0 0 0 1))
       (= 4 (aref x 0 0 1 1))
       (null (symbol-package (aref x 0 0 1 2)))
       (string= (symbol-name (aref x 0 0 1 2)) "G01")

       (null (symbol-package (aref x 0 1 0 0)))
       (string= (symbol-name (aref x 0 1 0 0)) "G02")
       (eq (aref x 0 1 0 1) (aref x 0 0 1 2))
       (= 8 (aref x 0 1 0 2))

       (= 9 (aref x 0 1 1 0))
       (null (symbol-package (aref x 0 1 1 1)))
       (string= (symbol-name (aref x 0 1 1 1)) "G03")
       (eq (aref x 0 1 1 2) (aref x 0 1 0 0))

       (eq (aref x 0 2 0 0) (aref x 0 1 1 1))
       (= 12 (aref x 0 2 0 1))
       (null (symbol-package (aref x 0 2 0 2)))
       (string= (symbol-name (aref x 0 2 0 2)) "G04")

       (null (symbol-package (aref x 0 2 1 0)))
       (string= (symbol-name (aref x 0 2 1 0)) "G05")
       (eq (aref x 0 2 1 1) (aref x 0 2 1 0))
       (eq (aref x 0 2 1 2) (aref x 0 2 0 2))))


(let* ((sequence '#1=(#(0 #2=(#1#) #1# 3) #3=#2A((#1# #2#) (#3# 4))))
       (x (read-from-string (write-to-string sequence :array t :circle t))))
  (and (= 2 (length x))
       (= 4 (length (first x)))
       (= 0 (aref (first x) 0))
       (eq x (first (aref (first x) 1)))
       (eq x (aref (first x) 2))
       (= 3 (aref (first x) 3))
       (equal (array-dimensions (second x)) '(2 2))
       (eq x (aref (second x) 0 0))
       (eq (aref (second x) 0 1) (aref (first x) 1))
       (eq (aref (second x) 1 0) (second x))
       (= 4 (aref (second x) 1 1))))

(let* ((sequence '#1=#(#2=(0 1 . #3=(2)) #(#3# #2# #1#) #3A(((#1# #2# #3#)))))
       (x (read-from-string (write-to-string sequence :array t :circle t))))
  (and (= 3 (length x))
       (= 3 (length (aref x 0)))
       (= 0 (first (aref x 0)))
       (= 1 (second (aref x 0)))
       (= 2 (third (aref x 0)))
       (= 3 (length (aref x 1)))
       (eq (aref (aref x 1) 0) (cddr (aref x 0)))
       (eq (aref (aref x 1) 1) (aref x 0))
       (eq (aref (aref x 1) 2) x)
       (equal (array-dimensions (aref x 2)) '(1 1 3))
       (eq (aref (aref x 2) 0 0 0) x)
       (eq (aref (aref x 2) 0 0 1) (aref x 0))
       (eq (aref (aref x 2) 0 0 2) (cddr (aref x 0)))))

;; *print-level* *print-length* array, vector, list intermingled
(let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 0 :length 10)
           "#"))
(let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 1 :length 10)
           "(# # #)"))
(let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 2 :length 10)
           "((1 2 3) #(4 5 6) #2A(# #))"))
(let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 3 :length 10)
           "((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))"))
(let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 3 :length 1)
           "((1 ...) ...)"))
(let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 3 :length 2)
           "((1 2 ...) #(4 5 ...) ...)"))
(let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 3 :length 3)
           "((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))"))
(let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9 10) (11 12 13 14)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 3 :length 3)
           "((1 2 3) #(4 5 6) #2A((7 8 9 ...) (11 12 13 ...)))"))

(let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 0 :length 10)
           "#"))
(let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 1 :length 10)
           "#(# # #)"))
(let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 2 :length 10)
           "#((1 2 3) #(4 5 6) #2A(# #))"))
(let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 3 :length 10)
           "#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))"))
(let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 3 :length 1)
           "#((1 ...) ...)"))
(let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 3 :length 2)
           "#((1 2 ...) #(4 5 ...) ...)"))
(let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 3 :length 3)
           "#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))"))
(let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9 10) (11 12 13 14)))))
  (string= (write-to-string sequence
                            :pretty nil :array t :level 3 :length 3)
           "#((1 2 3) #(4 5 6) #2A((7 8 9 ...) (11 12 13 ...)))"))

(let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 0 :length 0)
           "#"))
(let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 1 :length 0)
           "#2A(...)"))
(let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 1 :length 1)
           "#2A(# ...)"))
(let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 2 :length 1)
           "#2A((# ...) ...)"))
(let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 2 :length 2)
           "#2A((# #) (# #))"))
(let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 3 :length 1)
           "#2A(((10) ...) ...)"))
(let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 3 :length 2)
           "#2A(((10) #(100)) ((0 1 ...) #2A(# # ...)))"))
(let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 4 :length 2)
           "#2A(((10) #(100)) ((0 1 ...) #2A((3) (4) ...)))"))
(let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 4 :length 3)
           "#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) ...)))"))
(let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 4 :length 5)
           "#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))"))
(let* ((array '#2A(((10) #((100)))
                   ((0 (1) ((2))) #2A((3) ((4)) (((5))) ((6)) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 3 :length 5)
           "#2A(((10) #(#)) ((0 # #) #2A(# # # # #)))"))
(let* ((array '#2A(((10) #((100)))
                   ((0 (1) ((2))) #2A((3) ((4)) (((5))) ((6)) (7))))))
  (string= (write-to-string array
                            :pretty nil :array t :level 4 :length 5)
           "#2A(((10) #((100))) ((0 (1) (#)) #2A((3) (#) (#) (#) (7))))"))
(let* ((array '#2A(((10) #((100)))
                   ((0 (1) ((2))) #2A((3) ((4)) (((5))) ((6)) (7))))))
  (string=
   (write-to-string array :pretty nil :array t :level 5 :length 5)
   "#2A(((10) #((100))) ((0 (1) ((2))) #2A((3) ((4)) ((#)) ((6)) (7))))"))
(let* ((array '#2A(((10) #((100)))
                   ((0 (1) ((2))) #2A((3) ((4)) (((5))) ((6)) (7))))))
  (string=
   (write-to-string array :pretty nil :array t :level 6 :length 4)
   "#2A(((10) #((100))) ((0 (1) ((2))) #2A((3) ((4)) (((5))) ((6)) ...)))"))


;; (string= (write-to-string '#1=(0 #1#) :pretty nil :length 1 :circle t)
;;          "(0 ...)") ;; or "#1=(0 ...)"

;; *print-readably*, *print-level*, and *print-length
(equal (read-from-string
        (write-to-string '(0 1 2) :pretty nil :readably t :level 0 :length 0))
       '(0 1 2))
(equalp (read-from-string
         (write-to-string #(0 1 2) :pretty nil :readably t :level 0 :length 0))
        #(0 1 2))
(equalp (read-from-string
         (write-to-string #2A((0) (1) (2))
                          :pretty nil :readably t :level 0 :length 0))
        #2A((0) (1) (2)))


;; *print-level* *print-length*
;; Variable *PRINT-LEVEL*, *PRINT-LENGTH*
;; http://www.lispworks.com/reference/HyperSpec/Body/v_pr_lev.htm
;; *print-level* and *print-length* affect the printing of an any object
;; printed with a list-like syntax. They do not affect the printing of
;; symbols, strings, and bit vectors.
(string= "LENGTH" (write-to-string 'LENGTH :escape nil :level 0))
(string= "LENGTH" (write-to-string 'LENGTH :escape nil :length 2))
(string= "LENGTH" (write-to-string 'LENGTH :escape nil :level 0 :length 0))
(string= "abcdefg" (write-to-string "abcdefg" :escape nil :level 0))
(string= "abcdefg" (write-to-string "abcdefg" :escape nil :length 2))
(string= "abcdefg" (write-to-string "abcdefg" :escape nil :level 0 :length 0))
(string= "#*0101" (write-to-string #*0101 :array t :level 0))
(string= "#*0101" (write-to-string #*0101 :array t :length 2))
(string= "#*0101" (write-to-string #*0101 :array t :level 0 :length 0))
