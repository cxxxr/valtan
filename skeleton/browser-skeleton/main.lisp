(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")

(defpackage :<% @var name %>
  (:use :cl :valtan.react-utilities))
(in-package :<% @var name %>)

(define-react-component js:-app ()
  (tag :h1 () "hello world"))

(js:react-dom.render
 (js:react.create-element js:-app)
 (js:document.get-element-by-id #j"root"))
