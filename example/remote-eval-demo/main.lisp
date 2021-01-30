;; -*- mode:valtan -*-

(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")

(defpackage :remote-eval-demo
  (:use :cl :valtan.react-utilities))
(in-package :remote-eval-demo)

(define-react-component <app> ()
  (jsx (:h1 () "Hello World")))

(setup '<app> "root" :remote-eval t)
