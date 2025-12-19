# Valtan 開発計画

## 目標

**valtanで快適なウェブアプリケーション開発体験を実現する**

---

## フェーズ0: ビルドシステム刷新（最優先）

### TASK-0: Vite + ES Modules移行
- **状態**: 完了
- **方針**: コンパイラをES modules出力に変更し、Viteでビルド

#### TASK-0a: コンパイラをES modules対応に変更
- **状態**: 完了
- **対象ファイル**: `host-src/build.lisp`
- **内容**:
  - [x] `emit-import-lisp`: `var lisp = require('lisp')` → `import * as lisp from 'lisp'`
  - [x] `in-pass2`: `require()` → `import` 文に変更
  - [x] モジュールエクスポートの対応確認

#### TASK-0b: browser-skeletonをVite対応に更新
- **状態**: 完了
- **対象ファイル**: `skeleton/browser-skeleton/`
- **内容**:
  - [x] `vite.config.js` 作成（resolve.aliasでlispカーネルを解決）
  - [x] `package.json` 更新（vite依存追加、scripts変更、React 18）
  - [x] `index.html` 更新（Vite用にscript type="module"）
  - [x] `webpack.config.js` 削除

#### TASK-0c: node-skeletonをVite対応に更新
- **状態**: 完了
- **対象ファイル**: `skeleton/node-skeleton/`
- **内容**:
  - [x] `vite.config.js` 作成（SSR/Node.js用設定）
  - [x] `package.json` 更新
  - [x] `webpack.config.js` 削除

#### TASK-0d: exampleプロジェクトを更新
- **状態**: 完了
- **対象ファイル**: `example/*/`
- **内容**:
  - [x] react-tic-tac-toe: Vite + React 18
  - [x] browser-repl: Vite + React 18
  - [x] remote-eval-demo: Vite + React 18
  - [x] try-using-alexandria: Vite (Node.js)

#### TASK-0e: testsをVite対応に更新
- **状態**: 完了
- **対象ファイル**: `tests/`

**成果**:
- webpack → Vite移行完了
- CommonJS → ES Modules移行完了
- React 16 → React 18依存更新完了

---

## フェーズ1: 基盤整備

### TASK-1: React 18対応
- **状態**: 完了
- **対象ファイル**:
  - `library/react-utilities/react-utilities.lisp`
  - `skeleton/browser-skeleton/package.json`
  - `example/*/package.json`
- **内容**:
  - [x] `ReactDOM.render` → `ReactDOM.createRoot` API移行
  - [x] `setup` 関数の更新
  - [x] package.jsonのReactバージョン更新（18.x）
- **後方互換性**: 不要（完全移行）
- **期待効果**: 最新のReact機能が利用可能に

### TASK-2: React Hooks拡張
- **状態**: 完了
- **対象ファイル**: `library/react-utilities/react-utilities.lisp`
- **内容**:
  - [x] `with-effect` マクロ追加（useEffect対応）
  - [x] `with-callback` マクロ追加（useCallback対応）
  - [x] `with-memo` マクロ追加（useMemo対応）
  - [x] `with-ref` マクロ追加（useRef対応）
- **エクスポート追加**: `:with-effect`, `:with-callback`, `:with-memo`, `:with-ref`
- **期待効果**: 実用的なReactアプリケーション開発が可能に

### ~~TASK-3: webpack 5移行（node-skeleton）~~
- **状態**: 不要（TASK-0でViteに移行済み）

### TASK-4: 開発者向けドキュメント整備
- **状態**: 完了
- **内容**:
  - [x] `docs/react-guide.md` 作成
  - [x] `docs/ffi-reference.md` 作成
  - [x] `docs/examples.md` 作成
- **依存**: TASK-1, TASK-2完了後 ✅

### ~~TASK-5: 例題プロジェクトの更新~~
- **状態**: 完了（TASK-0dに統合）
- **備考**: TASK-0dで全exampleプロジェクトを更新済み

---

## フェーズ2: 短期（コア機能強化）

### ~~TASK-6: シーケンス関数の修正~~
- **状態**: 完了 ✅
- **現状**: 483/483成功 (100%)
- **備考**: 計測時点（2025-12-16）で全テスト通過

### ~~TASK-7: 文字列関数の修正~~
- **状態**: 完了 ✅
- **現状**: 414/414成功 (100%)
- **備考**: 計測時点（2025-12-16）で全テスト通過

### ~~TASK-8: シンボル関数の修正~~
- **状態**: 完了 ✅
- **現状**: 113/113成功 (100%)
- **備考**: 計測時点（2025-12-16）で全テスト通過

### TASK-9: エラーメッセージの改善
- **状態**: 未着手
- **対象ファイル**: `library/valtan-core/compiler/error.lisp`
- **内容**:
  - [ ] エラー種別の定義追加
  - [ ] ソース位置情報の表示改善
  - [ ] スタックトレースの可読性向上

### TASK-10: ソースマップの改善
- **状態**: 未着手
- **対象ファイル**: `host-src/build.lisp:122-132`
- **内容**:
  - [ ] マッピング精度の向上
  - [ ] ブラウザDevToolsでのLispコード表示改善

### TASK-16: 配列関数の改善
- **状態**: 大幅改善 ✅
- **現状**: ビットベクタサポート追加、型チェック改善
- **対象ファイル**: `library/valtan-core/lisp/array.lisp`
- **完了内容**:
  - [x] ビットベクタサポート追加
  - [x] 型チェック改善
- **残作業**:
  - [ ] 多次元配列の完全サポート

### TASK-17: リーダーの改善
- **状態**: 大幅改善 ✅
- **現状**: *READ-BASE*、エスケープ処理実装
- **対象ファイル**: `library/valtan-core/lisp/reader.lisp`
- **完了内容**:
  - [x] `*READ-BASE*`サポート実装
  - [x] 複数エスケープの修正
  - [x] 文字列エスケープの修正
  - [x] `read-from-string`の修正
- **残作業**:
  - [ ] `*read-suppress*`がnilの場合の処理

### TASK-18: ハッシュテーブルの改善
- **状態**: 完了 ✅
- **対象ファイル**: `library/valtan-core/lisp/hash-table.lisp`
- **完了内容**:
  - [x] EQUAL/EQUALPハッシュテーブルサポート
  - [x] SXHASH実装
  - [x] イテレータのnil値返却修正
  - [x] hash-table-testのシンボル正規化
  - [x] ドキュメント追加 (`docs/hashtable-implementation.md`)

### TASK-19: ループマクロの完全化
- **状態**: 完了 ✅
- **対象ファイル**: `library/valtan-core/lisp/loop.lisp`
- **テスト結果**: 849/850 (99.9%)
- **完了内容**:
  - [x] PROGRAM-ERROR検出追加（無効なLOOP組み合わせ）
  - [x] FOR-THEN句対応（tagbody/loop状態機械）
  - [x] エッジケースの検証完了
- **既知の制限**:
  - LOOP-KEY-TEST（Test 13）: ランタイムで作成されたパッケージのシンボルを`eval`経由でLOOPキーワードとして使用できない（valtanのeval実装の制限）

### TASK-20: CI/テスト可視化
- **状態**: 完了 ✅
- **内容**:
  - [x] README.mdにテスト結果表を追加
  - [x] GitHub Actions CIワークフロー作成
  - [x] テスト成功率の閾値チェック（85%）
- **備考**: 2025-12-16完了。リグレッション防止の基盤確立

### TASK-21: data-and-control関数の改善
- **状態**: 大幅改善 ✅
- **対象ファイル**: `library/valtan-core/lisp/`
- **完了内容**:
  - [x] MULTIPLE-VALUE-CALL実装（全値収集）
  - [x] VALUES-LIST実装
  - [x] NTH-VALUE実装
  - [x] ORマクロの複数値保持修正
  - [x] PSETF実装
  - [x] SETQでのsymbol-macro対応
- **残作業**:
  - [ ] 一部エッジケースの修正

### TASK-22: printerの改善
- **状態**: 完了 ✅
- **対象ファイル**: `library/valtan-core/lisp/print.lisp`
- **完了内容**:
  - [x] プリンタ全般の改善（478a24cコミット）
  - [x] readtable-case対応（:upcase, :downcase, :preserve, :invert）
  - [x] print-case対応（:upcase, :downcase, :capitalize）
  - [x] シンボル名エスケープ処理
- **テスト結果**: desirable-printer.lisp 36/36 (100%)

### TASK-23: 数値演算の改善
- **状態**: 完了 ✅
- **完了内容**:
  - [x] 除算演算子の修正
  - [x] FLOORの剰余計算修正（負数対応）
  - [x] Float値の二重ラップ防止
  - [x] Floatラッパー導入（整数と浮動小数点の区別）
  - [x] 型チェック追加

---

## フェーズ3: 中期（開発体験の洗練）

### TASK-11: ホットリロード機能の強化
- **状態**: 未着手
- **依存**: TASK-0（Vite移行済みで実現が容易に）
- **内容**:
  - [ ] HMR（Hot Module Replacement）対応
  - [ ] React状態の保持
  - [ ] ファイル変更時の部分再コンパイル

### TASK-12: TypeScript型定義の自動生成
- **状態**: 未着手
- **内容**:
  - [ ] Lispの型情報からTypeScript定義を生成
  - [ ] FFI呼び出し時の型安全性向上

### TASK-13: CLOS実装の完全化
- **状態**: 未着手
- **対象ファイル**: `library/valtan-core/lisp/clos.lisp`
- **内容**:
  - [ ] defclass, defmethod, defgenericの完全動作確認
  - [ ] オブジェクト指向UIコンポーネント設計のサポート

### ~~TASK-14: ES Modules対応~~
- **状態**: 完了（TASK-0に統合）
- **備考**: TASK-0でES Modules出力に移行済み。Tree Shakingは今後の最適化課題

### TASK-15: テスト自動化とCI/CD
- **状態**: 未着手
- **依存**: TASK-6, TASK-7, TASK-8
- **内容**:
  - [ ] GitHub Actions設定
  - [ ] テスト結果の自動レポート

---

## 依存関係

```
TASK-0 (Vite/ESM) ───┬──> TASK-1 (React 18) ✅
                     ├──> TASK-2 (Hooks) ✅
                     ├──> TASK-4 (ドキュメント) ✅
                     ├──> TASK-5 (例題更新) ✅
                     └──> TASK-11 (ホットリロード)

ANSI CL互換性向上（2025-12-20更新）:
┌──────────────────────────────────────────┐
│ ✅ TASK-6 (sequence) 完了                │
│ ✅ TASK-7 (string) 完了                  │
│ ✅ TASK-8 (symbol) 完了                  │
│ ✅ TASK-18 (hash-table) 完了             │
│ ✅ TASK-19 (loop) 完了                   │
│ ✅ TASK-22 (printer) 完了                │
│ ✅ TASK-23 (数値演算) 完了               │
├──────────────────────────────────────────┤
│ ✅ TASK-16 (array) 大幅改善              │
│ ✅ TASK-17 (reader) 大幅改善             │
│ ✅ TASK-21 (data-control) 大幅改善       │
└──────────────────────────────────────────┘
                     ↓
              TASK-20 (CI) ✅
```

**全体成功率**: 99.3% (4389/4420)

---

## 現在の作業順序

### 完了済み（フェーズ0-1）
1. ~~**TASK-0**: Vite + ES Modules移行~~ ✅
2. ~~**TASK-1**: React 18対応~~ ✅
3. ~~**TASK-2**: React Hooks拡張~~ ✅
4. ~~**TASK-3**: webpack 5移行~~ ✅ (TASK-0に統合)
5. ~~**TASK-4**: ドキュメント整備~~ ✅
6. ~~**TASK-5**: 例題更新~~ ✅ (TASK-0dに統合)
7. ~~**TASK-14**: ES Modules対応~~ ✅ (TASK-0に統合)

### 完了済み（ANSI CL互換性）
8. ~~**TASK-6**: シーケンス関数~~ ✅
9. ~~**TASK-7**: 文字列関数~~ ✅
10. ~~**TASK-8**: シンボル関数~~ ✅
11. ~~**TASK-18**: ハッシュテーブル~~ ✅
12. ~~**TASK-23**: 数値演算~~ ✅

### 完了済み（ANSI CL互換性 - loop）
13. ~~**TASK-19**: ループマクロ~~ ✅ (849/850テスト、99.9%通過)

### 大幅改善済み（ANSI CL互換性）
14. ~~**TASK-16**: 配列関数~~ ✅ (ビットベクタ、型チェック)
15. ~~**TASK-17**: リーダー~~ ✅ (*READ-BASE*、エスケープ)
16. ~~**TASK-21**: data-and-control~~ ✅ (複数値、PSETF)

### 完了済み（インフラ）
17. ~~**TASK-20**: CI/テスト可視化~~ ✅

### 完了済み（printer）
18. ~~**TASK-22**: printer改善~~ ✅ (36/36テスト通過)

### 次に着手（開発体験向上）
19. **TASK-9**: エラーメッセージの改善
20. **TASK-10**: ソースマップの改善

### 開発体験向上
21. **TASK-11**: ホットリロード機能の強化
22. **TASK-12**: TypeScript型定義の自動生成

### 品質・インフラ
23. **TASK-15**: テスト自動化とCI/CD（部分完了 - TASK-20で基盤確立）
24. **TASK-13**: CLOS実装の完全化

---

## 進捗ログ

| 日付 | タスク | 状態 | メモ |
|------|--------|------|------|
| 2025-12-14 | ARCHITECTURE.md | 完了 | アーキテクチャドキュメント生成 |
| 2025-12-14 | PLAN.md | 完了 | 開発計画策定 |
| 2025-12-14 | TASK-0 | 完了 | Vite移行、ES modules対応完了 |
| 2025-12-14 | TASK-1 | 完了 | React 18 createRoot API対応 |
| 2025-12-14 | TASK-2 | 完了 | with-effect/memo/callback/ref追加 |
| 2025-12-14 | TASK-4 | 完了 | react-guide.md, ffi-reference.md, examples.md作成 |
| 2025-12-16 | TASK-20 | 完了 | CI/テスト可視化（GitHub Actions、README更新） |
| 2025-12-16 | 計測更新 | - | 全体成功率88%確認。TASK-6/7/8は既に100%達成 |
| 2025-12-16 | PLAN.md | 更新 | 正確なテスト結果で全面更新、TASK-20〜22追加 |
| 2025-12-17 | defpackage修正 | 完了 | ホストでパッケージ存在時のeval回避 |
| 2025-12-17 | IOTA修正 | 完了 | tagbody/loop状態機械のFOR-THEN句対応 |
| 2025-12-17 | &ENVIRONMENT対応 | 完了 | defmacro, destructuring-bind, define-setf-expander |
| 2025-12-17 | 計測更新 | - | 全体成功率88%→94%（+268テスト通過）|
| 2025-12-18 | TASK-17 | 大幅改善 | *READ-BASE*、エスケープ処理、read-from-string修正 |
| 2025-12-18 | TASK-19 | 大幅改善 | PROGRAM-ERROR検出、無効LOOP組み合わせ対応 |
| 2025-12-18 | TASK-18 | 完了 | hash-table-test正規化 |
| 2025-12-18 | TASK-21 | 大幅改善 | VALUES-LIST、NTH-VALUE、MULTIPLE-VALUE-CALL実装 |
| 2025-12-18 | TASK-21 | 大幅改善 | ORマクロ複数値保持、PSETF実装、SETQのsymbol-macro対応 |
| 2025-12-18 | TASK-23 | 完了 | 除算、FLOOR剰余、型チェック修正 |
| 2025-12-19 | TASK-16 | 大幅改善 | ビットベクタサポート、型チェック改善 |
| 2025-12-19 | TASK-23 | 完了 | Floatラッパー導入、二重ラップ防止 |
| 2025-12-19 | TASK-18 | 完了 | EQUAL/EQUALPハッシュテーブル、SXHASH、ドキュメント追加 |
| 2025-12-19 | EQUALベクタ修正 | 完了 | ANSI CL準拠のベクタ比較 |
| 2025-12-19 | コンパイラ修正 | 完了 | シンボル名のケース保持 |
| 2025-12-19 | 計測更新 | - | 全体成功率94%→99.3%（4389/4420テスト通過）|
| 2025-12-19 | PLAN.md | 更新 | 最新の進捗を反映、TASK-23追加 |
| 2025-12-20 | TASK-22 | 完了 | printerテスト36/36通過（readtable-case、print-case対応確認）|
| 2025-12-20 | TASK-19 | 完了 | loopテスト849/850通過（99.9%）。LOOP-KEY-TESTはeval制限のため既知の制限として記録 |
