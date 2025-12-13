# Valtan サンプルコード集

実践的なValtanコード例を集めたドキュメントです。

## 目次

1. [基本例](#基本例)
2. [React コンポーネント](#react-コンポーネント)
3. [状態管理パターン](#状態管理パターン)
4. [DOM操作](#dom操作)
5. [非同期処理](#非同期処理)
6. [フォーム処理](#フォーム処理)
7. [アニメーション](#アニメーション)

---

## 基本例

### Hello World

```lisp
(defpackage :hello-world
  (:use :cl :valtan.react-utilities))
(in-package :hello-world)

(define-react-component <app> ()
  (jsx (:div ()
         (:h1 () "Hello, Valtan!")
         (:p () "Common Lisp で React アプリを開発しています"))))

(setup '<app> "root")
```

### カウンター

```lisp
(define-react-component <counter> ()
  (with-state ((count set-count 0))
    (jsx (:div (:class-name "counter")
           (:h2 () (format nil "Count: ~A" count))
           (:div (:class-name "buttons")
             (:button (:on-click (lambda () (set-count (1- count))))
               "-")
             (:button (:on-click (lambda () (set-count 0)))
               "Reset")
             (:button (:on-click (lambda () (set-count (1+ count))))
               "+"))))))
```

---

## React コンポーネント

### Props を受け取るコンポーネント

```lisp
;; Card コンポーネント
(define-react-component <card> (title (subtitle "") children)
  (jsx (:div (:class-name "card")
         (:div (:class-name "card-header")
           (:h3 () title)
           (when (> (length subtitle) 0)
             (jsx (:p (:class-name "subtitle") subtitle))))
         (:div (:class-name "card-body")
           children))))

;; 使用例
(jsx (<card> (:title "Welcome" :subtitle "Getting started")
       (:p () "This is the card content.")
       (:button () "Learn More")))
```

### コンポーネントの合成

```lisp
;; Button コンポーネント
(define-react-component <button> (variant (disabled nil) on-click children)
  (jsx (:button (:class-name (format nil "btn btn-~A" (or variant "primary"))
                 :disabled disabled
                 :on-click on-click)
         children)))

;; Icon コンポーネント
(define-react-component <icon> (name (size 24))
  (jsx (:svg (:width size :height size :class-name (format nil "icon icon-~A" name))
         ;; SVG content here
         )))

;; IconButton - 合成コンポーネント
(define-react-component <icon-button> (icon-name on-click (label ""))
  (jsx (<button> (:on-click on-click)
         (<icon> (:name icon-name))
         (when (> (length label) 0)
           (jsx (:span (:class-name "btn-label") label))))))
```

### 条件付きレンダリング

```lisp
(define-react-component <user-status> (user)
  (let ((logged-in (ffi:aget user "loggedIn")))
    (jsx (:div (:class-name "user-status")
           (if logged-in
               (jsx (:span ()
                      (:img (:src (ffi:aget user "avatar") :class-name "avatar"))
                      (ffi:aget user "name")))
               (jsx (<button> (:on-click #'handle-login)
                      "ログイン")))))))
```

### リストレンダリング

```lisp
(define-react-component <task-list> (tasks on-toggle on-delete)
  (jsx (:ul (:class-name "task-list")
         (mapcar (lambda (task)
                   (let ((id (ffi:aget task "id"))
                         (text (ffi:aget task "text"))
                         (done (ffi:aget task "done")))
                     (jsx (:li (:key id :class-name (if done "done" ""))
                            (:input (:type "checkbox"
                                     :checked done
                                     :on-change (lambda () (funcall on-toggle id))))
                            (:span () text)
                            (:button (:on-click (lambda () (funcall on-delete id)))
                              "Delete")))))
                 tasks))))
```

---

## 状態管理パターン

### 複数の状態を持つコンポーネント

```lisp
(define-react-component <search-filter> ()
  (with-state ((query set-query "")
               (category set-category "all")
               (sort-by set-sort-by "date")
               (results set-results '()))

    ;; 検索実行
    (with-effect ((query category sort-by))
      (when (> (length query) 0)
        ;; API呼び出しなど
        (js:console.log (format nil "Searching: ~A in ~A sort by ~A"
                                query category sort-by))))

    (jsx (:div (:class-name "search-filter")
           ;; 検索入力
           (:input (:type "text"
                    :value query
                    :placeholder "Search..."
                    :on-change (lambda (e)
                                 (set-query (ffi:aget e "target" "value")))))

           ;; カテゴリ選択
           (:select (:value category
                     :on-change (lambda (e)
                                  (set-category (ffi:aget e "target" "value"))))
             (:option (:value "all") "All")
             (:option (:value "active") "Active")
             (:option (:value "completed") "Completed"))

           ;; ソート
           (:select (:value sort-by
                     :on-change (lambda (e)
                                  (set-sort-by (ffi:aget e "target" "value"))))
             (:option (:value "date") "Date")
             (:option (:value "name") "Name")
             (:option (:value "priority") "Priority"))

           ;; 結果表示
           (:div (:class-name "results")
             (if (null results)
                 (jsx (:p () "No results"))
                 (jsx (<result-list> (:items results)))))))))
```

### 状態のリフトアップ

```lisp
;; 親コンポーネント - 状態を管理
(define-react-component <temperature-converter> ()
  (with-state ((celsius set-celsius ""))
    (let ((fahrenheit (if (> (length celsius) 0)
                          (format nil "~,1F" (+ (* (parse-integer celsius) 9/5) 32))
                          "")))
      (jsx (:div ()
             (<temperature-input>
               (:scale "celsius"
                :value celsius
                :on-change set-celsius))
             (<temperature-input>
               (:scale "fahrenheit"
                :value fahrenheit
                :on-change (lambda (f)
                             (when (> (length f) 0)
                               (set-celsius
                                 (format nil "~D"
                                         (round (* (- (parse-integer f) 32) 5/9)))))))))))))

;; 子コンポーネント - 状態を持たない
(define-react-component <temperature-input> (scale value on-change)
  (jsx (:fieldset ()
         (:legend () (format nil "Temperature in ~A"
                             (if (string= scale "celsius") "Celsius" "Fahrenheit")))
         (:input (:value value
                  :on-change (lambda (e)
                               (funcall on-change (ffi:aget e "target" "value"))))))))
```

---

## DOM操作

### refを使ったDOM操作

```lisp
(define-react-component <focus-manager> ()
  (let ((input-ref (with-ref #j:null))
        (container-ref (with-ref #j:null)))

    ;; マウント時にフォーカス
    (with-effect (())
      (when (ffi:aget input-ref "current")
        (funcall (ffi:ref (ffi:aget input-ref "current") "focus"))))

    (jsx (:div (:ref container-ref)
           (:input (:ref input-ref
                    :type "text"
                    :placeholder "Auto-focused input"))
           (:button (:on-click (lambda ()
                                 (funcall (ffi:ref (ffi:aget input-ref "current") "focus"))))
             "Focus Input")
           (:button (:on-click (lambda ()
                                 (let ((rect (funcall (ffi:ref (ffi:aget container-ref "current")
                                                               "getBoundingClientRect"))))
                                   (js:console.log "Container size:"
                                                   (ffi:aget rect "width")
                                                   (ffi:aget rect "height")))))
             "Log Size")))))
```

### スクロール制御

```lisp
(define-react-component <scroll-to-top> ()
  (with-state ((visible set-visible nil))

    ;; スクロールイベントをリッスン
    (with-effect (())
      (let ((handle-scroll (lambda ()
                             (set-visible (> (ffi:aget js:window "scrollY") 300)))))
        (funcall (ffi:ref js:window "addEventListener") "scroll" handle-scroll)
        ;; クリーンアップ
        (lambda ()
          (funcall (ffi:ref js:window "removeEventListener") "scroll" handle-scroll))))

    (when visible
      (jsx (:button (:class-name "scroll-to-top"
                     :on-click (lambda ()
                                 (funcall (ffi:ref js:window "scrollTo")
                                          (ffi:object :top 0 :behavior "smooth"))))
             "Top")))))
```

---

## 非同期処理

### データフェッチ

```lisp
(define-react-component <user-profile> (user-id)
  (with-state ((user set-user #j:null)
               (loading set-loading t)
               (error set-error #j:null))

    (with-effect ((user-id))
      (set-loading t)
      (set-error #j:null)

      (let ((promise (js:fetch (ffi:cl->js (format nil "/api/users/~A" user-id)))))
        ;; レスポンスをJSON化
        (setq promise (funcall (ffi:ref promise "then")
                               (lambda (res)
                                 (if (ffi:aget res "ok")
                                     (funcall (ffi:ref res "json"))
                                     (error "Failed to fetch")))))
        ;; データを設定
        (funcall (ffi:ref promise "then")
                 (lambda (data)
                   (set-user data)
                   (set-loading #j:false)))
        ;; エラーハンドリング
        (funcall (ffi:ref promise "catch")
                 (lambda (err)
                   (set-error err)
                   (set-loading #j:false)))))

    (cond
      (loading
       (jsx (:div (:class-name "loading") "Loading...")))
      (error
       (jsx (:div (:class-name "error")
              (format nil "Error: ~A" error))))
      (user
       (jsx (:div (:class-name "profile")
              (:h2 () (ffi:aget user "name"))
              (:p () (ffi:aget user "email"))))))))
```

### デバウンス検索

```lisp
(define-react-component <debounced-search> ()
  (with-state ((query set-query "")
               (debounced-query set-debounced-query "")
               (results set-results '()))

    ;; デバウンス処理
    (with-effect ((query))
      (let ((timer-id (js:set-timeout
                        (lambda () (set-debounced-query query))
                        300)))
        ;; クリーンアップでタイマーキャンセル
        (lambda () (js:clear-timeout timer-id))))

    ;; デバウンスされた値で検索
    (with-effect ((debounced-query))
      (when (> (length debounced-query) 0)
        (search-api debounced-query
                    (lambda (data) (set-results data)))))

    (jsx (:div ()
           (:input (:value query
                    :on-change (lambda (e)
                                 (set-query (ffi:aget e "target" "value")))
                    :placeholder "Search..."))
           (<search-results> (:results results))))))
```

---

## フォーム処理

### バリデーション付きフォーム

```lisp
(define-react-component <registration-form> ()
  (with-state ((form-data set-form-data (ffi:object :name "" :email "" :password ""))
               (errors set-errors (ffi:object))
               (submitted set-submitted nil))

    (flet ((validate ()
             (let ((new-errors (ffi:object)))
               ;; 名前のバリデーション
               (when (< (length (ffi:aget form-data "name")) 2)
                 (ffi:set (ffi:aget new-errors "name") "Name must be at least 2 characters"))

               ;; メールのバリデーション
               (unless (search "@" (ffi:aget form-data "email"))
                 (ffi:set (ffi:aget new-errors "email") "Invalid email address"))

               ;; パスワードのバリデーション
               (when (< (length (ffi:aget form-data "password")) 8)
                 (ffi:set (ffi:aget new-errors "password") "Password must be at least 8 characters"))

               (set-errors new-errors)
               (= (ffi:aget (js:-object.keys new-errors) "length") 0)))

           (handle-change (field)
             (lambda (e)
               (let ((new-data (js:-object.assign (ffi:object) form-data)))
                 (ffi:set (ffi:aget new-data field) (ffi:aget e "target" "value"))
                 (set-form-data new-data))))

           (handle-submit (e)
             (funcall (ffi:ref e "preventDefault"))
             (when (validate)
               ;; フォーム送信処理
               (js:console.log "Form submitted:" form-data)
               (set-submitted t))))

      (if submitted
          (jsx (:div (:class-name "success")
                 (:h2 () "Registration Complete!")
                 (:p () (format nil "Welcome, ~A!" (ffi:aget form-data "name")))))

          (jsx (:form (:on-submit #'handle-submit)
                 (:div (:class-name "field")
                   (:label () "Name")
                   (:input (:value (ffi:aget form-data "name")
                            :on-change (handle-change "name")))
                   (when (ffi:aget errors "name")
                     (jsx (:span (:class-name "error") (ffi:aget errors "name")))))

                 (:div (:class-name "field")
                   (:label () "Email")
                   (:input (:type "email"
                            :value (ffi:aget form-data "email")
                            :on-change (handle-change "email")))
                   (when (ffi:aget errors "email")
                     (jsx (:span (:class-name "error") (ffi:aget errors "email")))))

                 (:div (:class-name "field")
                   (:label () "Password")
                   (:input (:type "password"
                            :value (ffi:aget form-data "password")
                            :on-change (handle-change "password")))
                   (when (ffi:aget errors "password")
                     (jsx (:span (:class-name "error") (ffi:aget errors "password")))))

                 (:button (:type "submit") "Register")))))))
```

---

## アニメーション

### CSSトランジション

```lisp
(define-react-component <animated-list> (items)
  (jsx (:ul (:class-name "animated-list")
         (mapcar (lambda (item)
                   (jsx (:li (:key (ffi:aget item "id")
                              :class-name "list-item"
                              :style (ffi:object
                                       :animation "fadeIn 0.3s ease-in"
                                       :animationFillMode "forwards"))
                          (ffi:aget item "text"))))
                 items))))
```

### インタラクティブアニメーション

```lisp
(define-react-component <draggable-box> ()
  (with-state ((position set-position (ffi:object :x 0 :y 0))
               (dragging set-dragging nil))

    (let ((box-ref (with-ref #j:null)))

      (with-effect (())
        (let ((handle-mouse-move
                (lambda (e)
                  (when dragging
                    (set-position (ffi:object
                                    :x (ffi:aget e "clientX")
                                    :y (ffi:aget e "clientY"))))))
              (handle-mouse-up
                (lambda () (set-dragging nil))))

          (funcall (ffi:ref js:document "addEventListener") "mousemove" handle-mouse-move)
          (funcall (ffi:ref js:document "addEventListener") "mouseup" handle-mouse-up)

          ;; クリーンアップ
          (lambda ()
            (funcall (ffi:ref js:document "removeEventListener") "mousemove" handle-mouse-move)
            (funcall (ffi:ref js:document "removeEventListener") "mouseup" handle-mouse-up))))

      (jsx (:div (:ref box-ref
                  :class-name (format nil "draggable-box~A" (if dragging " dragging" ""))
                  :style (ffi:object
                           :position "absolute"
                           :left (format nil "~Apx" (ffi:aget position "x"))
                           :top (format nil "~Apx" (ffi:aget position "y"))
                           :cursor (if dragging "grabbing" "grab"))
                  :on-mouse-down (lambda () (set-dragging t)))
             "Drag me!")))))
```

---

## 関連ドキュメント

- [Reactガイド](./react-guide.md)
- [FFIリファレンス](./ffi-reference.md)
