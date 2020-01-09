(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")

(define-react-component js:-app ()
  (tag :h1 () "hello world"))

(unless (eq (ffi:typeof js:window) #j"undefined")
  (js:react-dom.render
   (js:react.create-element js:-app)
   (js:document.get-element-by-id #j"example")))
