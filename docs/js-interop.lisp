;;; js interop

;; convert between
(ffi:cl->js x)
(ffi:js->cl x)

;; string
#j"foo" ; clの文字列とjsの文字列は同じように扱えない(stringはarray structure)、なので(cl->js string)用のリテラルを用意した
; => "foo"

;; object
(ffi:object #j"foo" 10
            #j"bar" 20
            :foo 30
            :foo-bar 40 ; keywordはkebab caseからcamel-caseのjsの文字列として扱われる
            "hoge" 50)  ; 文字列はjsの文字列に変換される
=> {"foo": 10, "bar": 20, "foo": 30, "fooBar" 40, "hoge" 50}

#j{ #j"foo" 10 "fooBar" 20 }
#j{ :foo 10 :foo-bar 20 }
=> {"foo": 10, "fooBar": 20}

;; js package
js:foo-bar ; jsパッケージはkebab caseからcamel-caseに変換される
=> fooBar

;; array
#j[ 1 2 3 ]
(ffi:array 1 2 3)
=> [1, 2, 3]

(ffi:aget js:array index)
=> array[index]

(ffi:set (ffi:aget js:array index) value)
=> array[index] = value

;; var
(ffi:ref "foo")
=> foo

(ffi:set (ffi:ref "foo") 100)
=> foo = 100

;; get property
(ffi:ref x "innerHTML")
=> intern("X").value.innerHTML

#j:el:innerHTML ; #j:のあとはcase sensitive
=> el.innerHTML

;; set property
(ffi:set (ffi:ref el "innerHTML") #j"string")
(ffi:set #j:el:innerHTML #j"string")
(ffi:set js:inner-h-t-m-l #j"string")
=> el.innerHTML = "string"

;; call function
(js:alert #j"hello")
=> alert("hello")
