;;; Test-only adapter for the two Latin-1 input operations in CL-PPCRE's
;;; upstream harness. This is not a FLEXI-STREAMS implementation. The upstream
;;; library and all test assertions are loaded unchanged.
(defpackage :valtan-ppcre-test-streams
  (:use :cl)
  (:nicknames :flex)
  (:export :octet :make-flexi-stream))
(in-package :valtan-ppcre-test-streams)
(deftype octet () '(unsigned-byte 8))
(defun make-flexi-stream (stream &key external-format)
  (assert (equal external-format '(:latin-1 :eol-style :lf)))
  (make-string-input-stream
   (with-output-to-string (out)
     (loop for byte = (read-byte stream nil nil)
           while byte do (write-char (code-char byte) out)))))
