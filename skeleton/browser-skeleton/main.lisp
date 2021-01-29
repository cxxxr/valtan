(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")

(defpackage :<% @var name %>
  (:use :cl :valtan.react-utilities))
(in-package :<% @var name %>)

(define-react-component <app> ()
  (jsx (:h1 () "hello world")))

(setup #'<app> "root")
