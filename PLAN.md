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
- **状態**: 未着手
- **現状**: 158/323成功 (49%) ← 次に着手推奨
- **対象ファイル**: `library/valtan-core/lisp/array.lisp`
- **分析対象**: `tests/sacla-tests/must-array.lisp`
- **内容**:
  - [ ] 失敗テスト165件の原因分析
  - [ ] `make-array`, `aref`, `array-dimensions`等の修正
  - [ ] 多次元配列サポートの確認

### TASK-17: リーダーの改善
- **状態**: 未着手
- **現状**: 172/258成功 (67%)
- **対象ファイル**: `library/valtan-core/lisp/reader.lisp`
- **分析対象**: `tests/sacla-tests/must-reader.lisp`
- **内容**:
  - [ ] 失敗テスト86件の原因分析
  - [ ] リーダーマクロの修正
  - [ ] 特殊構文のパース改善

### TASK-18: ハッシュテーブルの改善
- **状態**: 未着手
- **現状**: 67/96成功 (70%)
- **対象ファイル**: `library/valtan-core/lisp/hash-table.lisp`
- **分析対象**: `tests/sacla-tests/must-hash-table.lisp`
- **内容**:
  - [ ] 失敗テスト29件の原因分析
  - [ ] `gethash`, `remhash`, `maphash`等の修正

### TASK-19: ループマクロの完全化
- **状態**: 未着手
- **現状**: 778/850成功 (92%)
- **対象ファイル**: `library/valtan-core/lisp/loop.lisp`
- **分析対象**: `tests/sacla-tests/must-loop.lisp`
- **内容**:
  - [ ] 残り72件の失敗テスト分析
  - [ ] エッジケースの修正

### TASK-20: CI/テスト可視化
- **状態**: 完了 ✅
- **内容**:
  - [x] README.mdにテスト結果表を追加
  - [x] GitHub Actions CIワークフロー作成
  - [x] テスト成功率の閾値チェック（85%）
- **備考**: 2025-12-16完了。リグレッション防止の基盤確立

### TASK-21: data-and-control関数の改善
- **状態**: 未着手
- **現状**: 162/282成功 (57%)
- **対象ファイル**: `library/valtan-core/lisp/`
- **分析対象**: `tests/sacla-tests/must-data-and-control.lisp`
- **内容**:
  - [ ] 失敗テスト120件の原因分析
  - [ ] 制御構造の修正

### TASK-22: printerの改善
- **状態**: 未着手
- **現状**: 14/36成功 (39%)
- **対象ファイル**: `library/valtan-core/lisp/print.lisp`
- **分析対象**: `tests/sacla-tests/desirable-printer.lisp`
- **内容**:
  - [ ] 失敗テスト22件の原因分析
  - [ ] 出力フォーマットの修正

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

ANSI CL互換性向上（2025-12-16計測）:
┌──────────────────────────────────────────┐
│ ✅ TASK-6 (sequence) 100%                │
│ ✅ TASK-7 (string) 100%                  │
│ ✅ TASK-8 (symbol) 100%                  │
├──────────────────────────────────────────┤
│ TASK-22 (printer) 39% ← 最優先          │
│ TASK-16 (array) 49%                      │
│ TASK-21 (data-control) 57%               │
│ TASK-17 (reader) 67%                     │
│ TASK-18 (hash-table) 70%                 │
│ TASK-19 (loop) 92%                       │
└──────────────────────────────────────────┘
                     ↓
              TASK-20 (CI) ✅
```

**全体成功率**: 88% (3902/4411)

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

### 完了済み（ANSI CL互換性 - 100%達成）
8. ~~**TASK-6**: シーケンス関数~~ ✅ (100%)
9. ~~**TASK-7**: 文字列関数~~ ✅ (100%)
10. ~~**TASK-8**: シンボル関数~~ ✅ (100%)

### 完了済み（インフラ）
11. ~~**TASK-20**: CI/テスト可視化~~ ✅

### 次に着手（ANSI CL互換性向上 - 成功率順）
12. **TASK-19**: ループマクロ完全化 (92%) ← 最も達成しやすい
13. **TASK-18**: ハッシュテーブル改善 (70%)
14. **TASK-17**: リーダー改善 (67%)
15. **TASK-21**: data-and-control改善 (57%)
16. **TASK-16**: 配列関数改善 (49%)
17. **TASK-22**: printer改善 (39%)

### 開発体験向上
18. **TASK-9**: エラーメッセージの改善
19. **TASK-10**: ソースマップの改善
20. **TASK-11**: ホットリロード機能の強化

### 品質・インフラ
21. **TASK-15**: テスト自動化とCI/CD（部分完了 - TASK-20で基盤確立）
22. **TASK-13**: CLOS実装の完全化
23. **TASK-12**: TypeScript型定義の自動生成

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
