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

### TASK-3: webpack 5移行（node-skeleton）
- **状態**: 未着手
- **対象ファイル**: `skeleton/node-skeleton/package.json`
- **内容**:
  - [ ] webpack 4.30.0 → 5.x へ更新
  - [ ] webpack-cli更新
  - [ ] webpack.config.js互換性確認
- **参考**: browser-skeletonは既にwebpack 5.101.3

### TASK-4: 開発者向けドキュメント整備
- **状態**: 完了
- **内容**:
  - [x] `docs/react-guide.md` 作成
  - [x] `docs/ffi-reference.md` 作成
  - [x] `docs/examples.md` 作成
- **依存**: TASK-1, TASK-2完了後 ✅

### TASK-5: 例題プロジェクトの更新
- **状態**: 未着手
- **対象ファイル**: `example/*/package.json`
- **内容**:
  - [ ] 全exampleのpackage.json更新
  - [ ] 動作確認
- **依存**: TASK-1, TASK-3完了後

---

## フェーズ2: 短期（コア機能強化）

### TASK-6: シーケンス関数の修正
- **状態**: 未着手
- **現状**: 6/3859成功 (0.2%)
- **対象ファイル**: `library/valtan-core/lisp/sequence.lisp`
- **分析対象**: `tests/sacla-tests/must-sequence.lisp`
- **内容**:
  - [ ] 失敗テストの原因分析
  - [ ] 主要関数の修正: `map`, `reduce`, `find`, `position`, `remove`, `sort`等
  - [ ] `:test`, `:key`, `:from-end`オプションの動作確認

### TASK-7: 文字列関数の修正
- **状態**: 未着手
- **現状**: 0/414成功 (0%)
- **対象ファイル**: `library/valtan-core/lisp/string.lisp`
- **分析対象**: `tests/sacla-tests/must-string.lisp`
- **内容**:
  - [ ] 失敗テストの原因分析
  - [ ] 文字列比較関数の修正
  - [ ] 文字列操作関数の修正

### TASK-8: シンボル関数の修正
- **状態**: 未着手
- **現状**: 5/196成功 (2.5%)
- **対象ファイル**: `library/valtan-core/lisp/symbol.lisp`
- **分析対象**: `tests/sacla-tests/must-symbol.lisp`
- **内容**:
  - [ ] 失敗テストの原因分析
  - [ ] `symbol-name`, `symbol-package`, `gensym`等の修正

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

---

## フェーズ3: 中期（開発体験の洗練）

### TASK-11: ホットリロード機能の強化
- **状態**: 未着手
- **依存**: TASK-1, TASK-3
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

### TASK-14: ES Modules対応
- **状態**: 未着手
- **依存**: TASK-3
- **内容**:
  - [ ] import/export文の生成
  - [ ] Tree Shakingの有効化

### TASK-15: テスト自動化とCI/CD
- **状態**: 未着手
- **依存**: TASK-6, TASK-7, TASK-8
- **内容**:
  - [ ] GitHub Actions設定
  - [ ] テスト結果の自動レポート

---

## 依存関係

```
TASK-0 (Vite/ESM) ───┬──> TASK-1 (React 18)
                     ├──> TASK-2 (Hooks)
                     └──> TASK-11 (ホットリロード) ※Viteで大幅簡略化

TASK-2 (Hooks) ──────> TASK-4 (ドキュメント)

TASK-1 (React 18) ───┬──> TASK-4 (ドキュメント)
                     └──> TASK-5 (例題更新) ※TASK-0に統合

TASK-6 (sequence) ───┐
TASK-7 (string) ─────┼──> TASK-15 (CI/CD)
TASK-8 (symbol) ─────┘
```

**注**: TASK-3 (webpack 5移行) は TASK-0 (Vite移行) に置き換え

---

## 現在の作業順序

1. ~~**TASK-0**: Vite + ES Modules移行~~ ✅ 完了
2. ~~**TASK-1**: React 18対応 (createRoot API)~~ ✅ 完了
3. ~~**TASK-2**: React Hooks拡張~~ ✅ 完了
4. ~~**TASK-4**: 開発者向けドキュメント整備~~ ✅ 完了
5. **TASK-6**: シーケンス関数の修正 ← 次に着手
6. **TASK-7**: 文字列関数の修正
7. **TASK-8**: シンボル関数の修正

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
