(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")

(defpackage :hoge
  (:use :cl :valtan.react-utilities))
(in-package :hoge)

(define-react-component <app> ()
  (tag :h1 () "hello world"))

(setup #'<app> "root")

(js:console.log ((ffi:ref "lisp" "uptime")))

