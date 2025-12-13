# Valtan FFI (Foreign Function Interface) リファレンス

ValtanのFFIシステムを使用してJavaScriptと相互運用するための完全なリファレンスです。

## 目次

1. [概要](#概要)
2. [値の変換](#値の変換)
3. [オブジェクト操作](#オブジェクト操作)
4. [配列操作](#配列操作)
5. [プロパティアクセス](#プロパティアクセス)
6. [関数呼び出し](#関数呼び出し)
7. [変数操作](#変数操作)
8. [JSパッケージ](#jsパッケージ)
9. [リーダーマクロ](#リーダーマクロ)
10. [実践パターン](#実践パターン)

---

## 概要

ValtanのFFIは、Common LispコードからJavaScriptのオブジェクト、関数、値にアクセスするための機能を提供します。

### 主要なシンボル

| シンボル | 説明 |
|----------|------|
| `ffi:cl->js` | Lisp値をJavaScript値に変換 |
| `ffi:js->cl` | JavaScript値をLisp値に変換 |
| `ffi:object` | JavaScriptオブジェクトを作成 |
| `ffi:array` | JavaScript配列を作成 |
| `ffi:aget` | プロパティ/要素にアクセス |
| `ffi:ref` | JavaScript変数/プロパティを参照 |
| `ffi:set` | 値を設定 |
| `ffi:define` | JavaScript変数を定義 |
| `ffi:define-function` | JavaScript関数を定義 |
| `ffi:typeof` | JavaScript typeof演算子 |
| `ffi:instanceof` | JavaScript instanceof演算子 |
| `ffi:require` | モジュールをインポート |
| `ffi:js-eval` | JavaScript文字列を評価 |

---

## 値の変換

### ffi:cl->js

Lisp値をJavaScript値に変換します。

```lisp
;; 文字列
(ffi:cl->js "hello")  ; => "hello" (JS string)

;; ベクタ
(ffi:cl->js #(1 2 3))  ; => [1, 2, 3] (JS array)

;; その他の値はそのまま渡される
(ffi:cl->js 42)        ; => 42
(ffi:cl->js t)         ; => true
(ffi:cl->js nil)       ; => false (注意: nilはfalseになる)
```

### ffi:js->cl

JavaScript値をLisp値に変換します。

```lisp
;; JavaScript文字列 -> Lisp文字列
(ffi:js->cl js-string)

;; JavaScript配列 -> Lispベクタ
(ffi:js->cl js-array)
```

---

## オブジェクト操作

### ffi:object

JavaScriptオブジェクトを作成します。

```lisp
;; キーワードシンボル（ケバブケース→キャメルケース変換）
(ffi:object :foo-bar "value"
            :on-click handler)
; => {"fooBar": "value", "onClick": handler}

;; 文字列キー
(ffi:object "firstName" "John"
            "lastName" "Doe")
; => {"firstName": "John", "lastName": "Doe"}

;; 混合
(ffi:object :class-name "container"
            "data-id" 123)
; => {"className": "container", "data-id": 123}
```

### リーダーマクロ #j{...}

オブジェクトリテラルの省略記法です。

```lisp
#j{ :foo 10 :bar-baz 20 }
; => {"foo": 10, "barBaz": 20}

#j{ "key" "value" }
; => {"key": "value"}
```

---

## 配列操作

### ffi:array

JavaScript配列を作成します。

```lisp
(ffi:array 1 2 3)
; => [1, 2, 3]

(ffi:array "a" "b" "c")
; => ["a", "b", "c"]

;; 空の配列
(ffi:array)
; => []
```

### リーダーマクロ #j[...]

配列リテラルの省略記法です。

```lisp
#j[ 1 2 3 ]
; => [1, 2, 3]

#j[ "hello" "world" ]
; => ["hello", "world"]
```

### 配列要素へのアクセス

```lisp
;; 読み取り
(ffi:aget array 0)      ; => array[0]
(ffi:aget array index)  ; => array[index]

;; 書き込み
(ffi:set (ffi:aget array 0) value)  ; array[0] = value
```

---

## プロパティアクセス

### ffi:aget

オブジェクトのプロパティにアクセスします。

```lisp
;; 単一プロパティ
(ffi:aget obj "property")
; => obj.property または obj["property"]

;; ネストしたプロパティ
(ffi:aget obj "nested" "deep" "value")
; => obj.nested.deep.value

;; 動的なキー
(let ((key "dynamicKey"))
  (ffi:aget obj key))
```

### ffi:ref

変数またはプロパティを参照します。

```lisp
;; グローバル変数
(ffi:ref "window")
; => window

;; オブジェクトのプロパティ
(ffi:ref obj "property")
; => obj.property

;; ネストしたアクセス
(ffi:ref document "body" "innerHTML")
; => document.body.innerHTML
```

### ffi:set

値を設定します。

```lisp
;; プロパティに設定
(ffi:set (ffi:aget obj "property") value)
; => obj.property = value

(ffi:set (ffi:ref obj "property") value)
; => obj.property = value
```

### リーダーマクロ #j:

プロパティアクセスの省略記法です。

```lisp
;; 単一プロパティ
#j:el:innerHTML
; => el.innerHTML

;; 設定
(ffi:set #j:el:innerHTML #j"<p>Hello</p>")
; => el.innerHTML = "<p>Hello</p>"
```

**注意**: `#j:`の後はケースセンシティブです。

---

## 関数呼び出し

### JSパッケージを使用

`js:`パッケージのシンボルはJavaScript関数として呼び出せます。

```lisp
;; グローバル関数
(js:alert #j"Hello!")
; => alert("Hello!")

;; console.log
(js:console.log "Debug message")
; => console.log("Debug message")

;; document.getElementById
(js:document.get-element-by-id "root")
; => document.getElementById("root")
```

### メソッド呼び出し

オブジェクトのメソッドを呼び出すには`funcall`を使用します。

```lisp
;; メソッドを取得して呼び出す
(funcall (ffi:ref obj "method") arg1 arg2)
; => obj.method(arg1, arg2)

;; 例: addEventListener
(funcall (ffi:ref element "addEventListener")
         "click"
         (lambda (e) (js:console.log "Clicked!")))
```

### ffi:require

ESモジュールをインポートします。

```lisp
;; モジュールをインポート
(ffi:require js:react "react")
(ffi:require js:axios "axios")

;; 使用
(js:react.create-element "div" #j:null "Hello")
(js:axios.get "/api/data")
```

---

## 変数操作

### ffi:define

JavaScript変数を定義します。

```lisp
(ffi:define "myGlobal" 42)
; => var myGlobal = 42;

(ffi:define js:app-config (ffi:object :debug t :version "1.0"))
; => var appConfig = {debug: true, version: "1.0"};
```

### ffi:define-function

JavaScript関数を定義します。

```lisp
(ffi:define-function "greet" (name)
  (js:console.log (format nil "Hello, ~A!" name)))
; => function greet(name) { console.log("Hello, " + name + "!"); }
```

### ffi:var

変数を宣言します（初期化なし）。

```lisp
(ffi:var "myVar")
; => var myVar;
```

---

## JSパッケージ

`js:`パッケージのシンボルは特別に扱われ、ケバブケースからキャメルケースに自動変換されます。

### 変換規則

| Lisp | JavaScript |
|------|------------|
| `js:foo-bar` | `fooBar` |
| `js:console.log` | `console.log` |
| `js:document.get-element-by-id` | `document.getElementById` |
| `js:inner-h-t-m-l` | `innerHTML` |
| `js:x-m-l-http-request` | `XMLHttpRequest` |

### 例

```lisp
;; グローバルオブジェクト
js:window
js:document
js:console

;; 組み込み関数
(js:parse-int "42")          ; => parseInt("42")
(js:parse-float "3.14")      ; => parseFloat("3.14")
(js:set-timeout fn 1000)     ; => setTimeout(fn, 1000)
(js:set-interval fn 500)     ; => setInterval(fn, 500)

;; Date
(js:-date)                   ; => new Date()
(js:date.now)                ; => Date.now()

;; JSON
(js:-j-s-o-n.parse str)      ; => JSON.parse(str)
(js:-j-s-o-n.stringify obj)  ; => JSON.stringify(obj)
```

---

## リーダーマクロ

### #j"..."  - JavaScript文字列

```lisp
#j"Hello, World!"
; => "Hello, World!" (JavaScript string)

;; Lisp文字列とは異なる
"Hello"    ; Lisp string (配列ベース)
#j"Hello"  ; JavaScript string (プリミティブ)
```

### #j[...] - JavaScript配列

```lisp
#j[ 1 2 3 4 5 ]
; => [1, 2, 3, 4, 5]
```

### #j{...} - JavaScriptオブジェクト

```lisp
#j{ :name "John" :age 30 }
; => {"name": "John", "age": 30}
```

### #j:... - 特殊値・プロパティアクセス

```lisp
;; 特殊値
#j:null       ; => null
#j:undefined  ; => undefined
#j:true       ; => true
#j:false      ; => false

;; プロパティアクセス
#j:obj:property
; => obj.property
```

---

## 実践パターン

### DOM操作

```lisp
;; 要素の取得
(let ((el (js:document.get-element-by-id "my-element")))
  ;; テキスト設定
  (ffi:set (ffi:aget el "textContent") "New text")

  ;; スタイル設定
  (ffi:set (ffi:aget el "style" "color") "red")

  ;; クラス操作
  (funcall (ffi:ref el "classList" "add") "active")
  (funcall (ffi:ref el "classList" "remove") "hidden"))
```

### イベントリスナー

```lisp
(let ((button (js:document.get-element-by-id "my-button")))
  (funcall (ffi:ref button "addEventListener")
           "click"
           (lambda (event)
             (funcall (ffi:ref event "preventDefault"))
             (js:console.log "Button clicked!"))))
```

### Fetch API

```lisp
(defun fetch-data (url callback)
  (let ((promise (js:fetch (ffi:cl->js url))))
    (funcall (ffi:ref promise "then")
             (lambda (response)
               (funcall (ffi:ref response "json"))))
    (funcall (ffi:ref promise "then") callback)
    (funcall (ffi:ref promise "catch")
             (lambda (error)
               (js:console.error "Fetch error:" error)))))
```

### ローカルストレージ

```lisp
;; 保存
(funcall (ffi:ref js:local-storage "setItem")
         "key"
         (ffi:cl->js "value"))

;; 取得
(let ((value (funcall (ffi:ref js:local-storage "getItem") "key")))
  (ffi:js->cl value))

;; 削除
(funcall (ffi:ref js:local-storage "removeItem") "key")
```

### タイマー

```lisp
;; setTimeout
(let ((id (js:set-timeout
            (lambda () (js:console.log "Timeout!"))
            1000)))
  ;; キャンセル
  (js:clear-timeout id))

;; setInterval
(let ((id (js:set-interval
            (lambda () (js:console.log "Tick"))
            1000)))
  ;; 停止
  (js:clear-interval id))
```

### Promiseの処理

```lisp
(defun async-operation ()
  (let ((promise (some-async-function)))
    ;; .then
    (setq promise
          (funcall (ffi:ref promise "then")
                   (lambda (result)
                     (js:console.log "Success:" result)
                     result)))
    ;; .catch
    (funcall (ffi:ref promise "catch")
             (lambda (error)
               (js:console.error "Error:" error)))
    ;; .finally
    (funcall (ffi:ref promise "finally")
             (lambda ()
               (js:console.log "Done")))))
```

---

## 注意事項

### 文字列の違い

Lispの文字列とJavaScriptの文字列は内部表現が異なります：

```lisp
"hello"      ; Lisp文字列（配列ベース）
#j"hello"    ; JavaScript文字列（プリミティブ）

;; JavaScript APIに渡す場合は変換が必要
(ffi:cl->js "hello")  ; Lisp文字列をJS文字列に変換
```

### nil と null/undefined

```lisp
nil           ; Lispのnil
#j:null       ; JavaScriptのnull
#j:undefined  ; JavaScriptのundefined

;; 条件分岐での注意
(if (eq value #j:null) ...)
(if (eq value #j:undefined) ...)
```

### this コンテキスト

メソッド呼び出し時に`this`を保持するには注意が必要です：

```lisp
;; 正しい: thisが保持される
(funcall (ffi:ref obj "method") args)

;; 問題あり: thisが失われる可能性
(let ((method (ffi:ref obj "method")))
  (funcall method args))  ; thisはundefinedになる可能性
```

---

## 関連ドキュメント

- [Reactガイド](./react-guide.md)
- [サンプルコード集](./examples.md)
