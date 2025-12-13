# Valtan React Guide

ValtanでReactアプリケーションを開発するための包括的なガイドです。

## 目次

1. [はじめに](#はじめに)
2. [セットアップ](#セットアップ)
3. [コンポーネントの定義](#コンポーネントの定義)
4. [JSX構文](#jsx構文)
5. [State管理](#state管理)
6. [React Hooks](#react-hooks)
7. [イベントハンドリング](#イベントハンドリング)
8. [実践例](#実践例)

---

## はじめに

`valtan.react-utilities`パッケージは、Common LispからReactを使用するためのマクロと関数を提供します。

```lisp
(defpackage :my-app
  (:use :cl :valtan.react-utilities))
(in-package :my-app)
```

### エクスポートされるシンボル

| シンボル | 説明 |
|----------|------|
| `define-react-component` | Reactコンポーネントを定義 |
| `jsx` | JSX風の構文でReact要素を作成 |
| `with-state` | useState Hookをラップ |
| `with-effect` | useEffect Hookをラップ |
| `with-memo` | useMemo Hookをラップ |
| `with-callback` | useCallback Hookをラップ |
| `with-ref` | useRef Hookをラップ |
| `setup` | Reactアプリをマウント |

---

## セットアップ

### プロジェクト構成

```
my-app/
├── my-app.system      # システム定義
├── main.lisp          # メインコード
├── package.json       # npm依存関係
├── vite.config.js     # Vite設定
└── index.html         # HTMLテンプレート
```

### システム定義 (my-app.system)

```lisp
(defsystem "my-app"
  :serial t
  :components ((:file "main"))
  :depends-on ("react-utilities"))
```

### アプリケーションのマウント

```lisp
;; main.lisp
(defpackage :my-app
  (:use :cl :valtan.react-utilities))
(in-package :my-app)

(define-react-component <app> ()
  (jsx (:div ()
         (:h1 () "Hello, Valtan!"))))

;; #rootにマウント
(setup '<app> "root")
```

---

## コンポーネントの定義

### 基本構文

```lisp
(define-react-component <component-name> (prop1 prop2 ...)
  body)
```

### Propsの受け取り

```lisp
;; propsはキャメルケースに自動変換される
(define-react-component <greeting> (user-name message)
  ;; user-name → userName, message → message
  (jsx (:div ()
         (:h1 () (format nil "Hello, ~A!" user-name))
         (:p () message))))

;; 使用時
(jsx (<greeting> (:user-name "Alice" :message "Welcome!")))
```

### デフォルト値付きProps

```lisp
(define-react-component <button> ((label "Click me") (disabled nil))
  (jsx (:button (:disabled disabled)
         label)))
```

---

## JSX構文

### 基本構文

```lisp
(jsx (tag-name (options...) children...))
```

### HTML要素

```lisp
;; キーワードシンボルでHTML要素を指定
(jsx (:div (:class-name "container")
       (:h1 () "Title")
       (:p (:style (ffi:object :color "blue")) "Blue text")))
```

### カスタムコンポーネント

```lisp
;; <name>形式でコンポーネントを参照
(jsx (<my-component> (:prop1 value1 :prop2 value2)
       "Child content"))
```

### オプション (Props)

```lisp
;; オプションはplistで指定
(:class-name "my-class"          ; className
 :on-click handler               ; onClick
 :style (ffi:object :color "red" :fontSize "16px"))
```

### 条件付きレンダリング

```lisp
(define-react-component <conditional> (show-message)
  (jsx (:div ()
         (when show-message
           (jsx (:p () "Message is shown"))))))
```

### リストのレンダリング

```lisp
(define-react-component <list-view> (items)
  (jsx (:ul ()
         (mapcar (lambda (item)
                   (jsx (:li (:key (ffi:aget item "id"))
                          (ffi:aget item "name"))))
                 items))))
```

---

## State管理

### with-state

`with-state`マクロはReactの`useState`をラップします。

```lisp
(with-state ((var setter initial-value) ...)
  body)
```

### 基本例

```lisp
(define-react-component <counter> ()
  (with-state ((count set-count 0))
    (jsx (:div ()
           (:p () (format nil "Count: ~A" count))
           (:button (:on-click (lambda () (set-count (1+ count))))
             "Increment")))))
```

### 複数のState

```lisp
(define-react-component <form> ()
  (with-state ((name set-name "")
               (email set-email "")
               (submitted set-submitted nil))
    (if submitted
        (jsx (:p () (format nil "Thanks, ~A!" name)))
        (jsx (:form (:on-submit (lambda (e)
                                  (funcall (ffi:ref e "preventDefault"))
                                  (set-submitted t)))
               (:input (:value name
                        :on-change (lambda (e)
                                     (set-name (ffi:aget e "target" "value")))))
               (:input (:value email
                        :on-change (lambda (e)
                                     (set-email (ffi:aget e "target" "value")))))
               (:button (:type "submit") "Submit"))))))
```

---

## React Hooks

### with-effect (useEffect)

副作用を実行します。

```lisp
;; 構文
(with-effect ((dependency...))
  body)

;; 毎回レンダリング時に実行
(with-effect (nil)
  (js:console.log "Rendered"))

;; マウント時のみ実行（空の依存配列）
(with-effect (())
  (js:console.log "Mounted"))

;; 依存値が変わった時に実行
(with-effect ((count))
  (js:console.log (format nil "Count changed to ~A" count)))
```

### クリーンアップ

```lisp
(define-react-component <timer> ()
  (with-state ((seconds set-seconds 0))
    (with-effect (())
      (let ((id (js:set-interval
                  (lambda () (set-seconds (1+ seconds)))
                  1000)))
        ;; クリーンアップ関数を返す
        (lambda () (js:clear-interval id))))
    (jsx (:p () (format nil "~A seconds" seconds)))))
```

### with-memo (useMemo)

計算結果をメモ化します。

```lisp
;; 構文
(with-memo ((dependency...))
  expensive-computation)

;; 例: 重い計算をメモ化
(define-react-component <expensive> (items filter-text)
  (let ((filtered (with-memo ((items filter-text))
                    (remove-if-not
                      (lambda (item)
                        (search filter-text (ffi:aget item "name")))
                      items))))
    (jsx (:ul ()
           (mapcar (lambda (item)
                     (jsx (:li (:key (ffi:aget item "id"))
                            (ffi:aget item "name"))))
                   filtered)))))
```

### with-callback (useCallback)

コールバック関数をメモ化します。

```lisp
;; 構文
(with-callback ((dependency...))
  callback-body)

;; 例: 子コンポーネントへの安定したコールバック
(define-react-component <parent> ()
  (with-state ((count set-count 0))
    (let ((handle-click (with-callback ((count))
                          (set-count (1+ count)))))
      (jsx (:div ()
             (:p () (format nil "Count: ~A" count))
             (<child-button> (:on-click handle-click)))))))
```

### with-ref (useRef)

ミュータブルな参照を作成します。

```lisp
;; 構文
(with-ref initial-value)

;; 例: DOM要素への参照
(define-react-component <focus-input> ()
  (let ((input-ref (with-ref #j:null)))
    (jsx (:div ()
           (:input (:ref input-ref :type "text"))
           (:button (:on-click (lambda ()
                                 (funcall (ffi:ref (ffi:aget input-ref "current") "focus"))))
             "Focus Input")))))

;; 例: 前回の値を保持
(define-react-component <previous-value> (value)
  (let ((prev-ref (with-ref value)))
    (with-effect ((value))
      (setf (ffi:aget prev-ref "current") value))
    (jsx (:p () (format nil "Current: ~A, Previous: ~A"
                        value
                        (ffi:aget prev-ref "current"))))))
```

---

## イベントハンドリング

### イベントハンドラの基本

```lisp
(:on-click (lambda (event)
             ;; eventはReactのSyntheticEvent
             (funcall (ffi:ref event "preventDefault"))
             (do-something)))
```

### よく使うイベント

| Lisp | React |
|------|-------|
| `:on-click` | `onClick` |
| `:on-change` | `onChange` |
| `:on-submit` | `onSubmit` |
| `:on-key-down` | `onKeyDown` |
| `:on-mouse-enter` | `onMouseEnter` |

### イベントオブジェクトへのアクセス

```lisp
(lambda (e)
  ;; target要素の値を取得
  (let ((value (ffi:aget e "target" "value")))
    (set-state value)))
```

---

## 実践例

### Todo App

```lisp
(defpackage :todo-app
  (:use :cl :valtan.react-utilities))
(in-package :todo-app)

(define-react-component <todo-item> (todo on-toggle on-delete)
  (jsx (:li (:style (ffi:object
                      :textDecoration (if (ffi:aget todo "done")
                                          "line-through"
                                          "none")))
         (:input (:type "checkbox"
                  :checked (ffi:aget todo "done")
                  :on-change (lambda () (funcall on-toggle))))
         (ffi:aget todo "text")
         (:button (:on-click on-delete) "Delete"))))

(define-react-component <todo-app> ()
  (with-state ((todos set-todos '())
               (input-text set-input-text ""))
    (flet ((add-todo ()
             (when (> (length input-text) 0)
               (set-todos (cons (ffi:object :id (js:date.now)
                                            :text input-text
                                            :done #j:false)
                                todos))
               (set-input-text "")))
           (toggle-todo (id)
             (set-todos (mapcar (lambda (todo)
                                  (if (= (ffi:aget todo "id") id)
                                      (ffi:object :id id
                                                  :text (ffi:aget todo "text")
                                                  :done (not (ffi:aget todo "done")))
                                      todo))
                                todos)))
           (delete-todo (id)
             (set-todos (remove-if (lambda (todo)
                                     (= (ffi:aget todo "id") id))
                                   todos))))
      (jsx (:div ()
             (:h1 () "Todo App")
             (:div ()
               (:input (:value input-text
                        :on-change (lambda (e)
                                     (set-input-text (ffi:aget e "target" "value")))
                        :on-key-down (lambda (e)
                                       (when (string= (ffi:aget e "key") "Enter")
                                         (add-todo)))))
               (:button (:on-click #'add-todo) "Add"))
             (:ul ()
               (mapcar (lambda (todo)
                         (jsx (<todo-item>
                                (:key (ffi:aget todo "id")
                                 :todo todo
                                 :on-toggle (lambda () (toggle-todo (ffi:aget todo "id")))
                                 :on-delete (lambda () (delete-todo (ffi:aget todo "id")))))))
                       todos)))))))

(setup '<todo-app> "root")
```

---

## FFIとの連携

詳細は [FFIリファレンス](./ffi-reference.md) を参照してください。

### よく使うFFIパターン

```lisp
;; JavaScriptオブジェクトの作成
(ffi:object :key1 "value1" :key2 "value2")

;; プロパティアクセス
(ffi:aget obj "property")
(ffi:aget obj "nested" "property")

;; メソッド呼び出し
(funcall (ffi:ref obj "method") arg1 arg2)

;; JavaScript値への変換
(ffi:cl->js lisp-value)

;; null/undefinedリテラル
#j:null
#j:undefined
```

---

## トラブルシューティング

### コンポーネントがundefinedを返す

Reactコンポーネントは常に有効な値を返す必要があります。`define-react-component`は自動的に`nil`を`#j:null`に変換しますが、明示的に`#j:null`を返すこともできます。

### Hooksのルール違反

ReactのHooksルールはValtanでも適用されます：
- Hooksはコンポーネントのトップレベルでのみ呼び出す
- 条件分岐やループの中でHooksを呼び出さない

```lisp
;; NG: 条件の中でHooksを使用
(when condition
  (with-state ((x set-x 0))
    ...))

;; OK: Hooksは常に呼び出し、条件は中で処理
(with-state ((x set-x 0))
  (when condition
    ...))
```

---

## 関連ドキュメント

- [FFIリファレンス](./ffi-reference.md)
- [サンプルコード集](./examples.md)
- [Vite設定ガイド](./vite-setup.md)
