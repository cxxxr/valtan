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

**SACLAテスト成功率（上流ANSIテストとは別指標）**: **98.07%（9,824/10,017ケース）**。全27ファイル完走、未完了0件。2026-11-04実測。[結果](tests/conformance-results.json)。ANSI仕様全体の網羅率ではなく、同梱SACLAでの互換性指標。

**上流ANSIテスト成功率**: **91.45%（20,062/21,937ケース）**。2026-11-04、固定した上流版の全24カテゴリをevalモードで完走。未実行0件、登録名・件数・実行バンドルの独立検査も成功し、90%以上の目標を達成。[全体結果](tests/ansi/full-results.json)。失敗1,875件は記録に保持。

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
| 2026-11-04 | ANSI互換性の90%目標 | 進行中（上流スイート） | SACLAでは全27ファイル10,017ケースを完走し9,802件成功（97.85%）。これは参考指標であり、上流ansi-test全体での90%以上を目標の達成条件とする。未実行・読み込み失敗・未登録を除外して成功率を算出しない |

### 上流 ansi-test の90%目標（進行中）

固定リビジョンの未改変テストを導入し、登録数・読み込み失敗・未実行・タイムアウトを区別して集計する。SBCLでの参照登録数は21,944件。上流ATOMの5件が完走し全成功。CONSカテゴリの記録は1,882件完走、1,882件成功（100%）。汎用代入・集合演算・EVALの定数同一性・ハッシュ表拡張・パッケージ操作・ABSの修正を含む。その後、配列変位・NAME-CHAR・ハッシュ表イテレータ・MACROLET・空のシンボル名の読み込みも修正した。FORMAT/FORMATTERの共通書式処理・数値印字・LOG・シンボルのエスケープ・WRITEのオプションと省略表示も追加し、ランタイム回帰テスト253件を検証。上流cl-test-package.lspとHANDLER-CASE/HANDLER-BIND補助マクロの省略も解消した。ランタイムのスナップショットを固定した全体測定と各カテゴリの再測定を継続中であり、上流スイートの90%達成は未確認。[実行方法](tests/ansi/README.md)、[ATOM実測](tests/ansi/smoke-results.json)。

上流の[旧全体測定](tests/ansi/full-baseline.json)は13,175件成功・5,896件失敗で、未実行と未登録が残るため準拠率は算出しない。その後の[4カテゴリ測定](tests/ansi/core-results.json)は全件完走し、PACKAGES 479/500、CHARACTERS 259/259、HASH-TABLES 147/158、ARRAYS 1,137/1,356。別指標のSACLAは9,802/10,017件成功（97.85%）、全27ファイル完走を確認した。最新版で上流全24カテゴリを再測定中。

[書式の8ファイル](tests/ansi/format-results.json)は参照登録数と一致する604件を完走し全成功。PRINTER全カテゴリも2,446件を登録できるようになり、旧測定で失われていた619件の登録を回復した。後続の全体測定で確認した&WHOLEとSETF関数・展開関数の暗黙ブロック、SETF関数のFUNCTION参照も修正した。標準出力への文字列書き込みで改行時のフラッシュを行い、実行ログの到達を回帰テストで検証した。

分数の正規化と正確な四則演算・比較・読み書き・コンパイル済み定数、複素数演算、浮動小数点の分解とスケーリング、整数論理演算を追加した。十進小数は正確な分数を経由して一度だけ浮動小数点に変換し、負のゼロも保存する。MAPと文字列のINITIAL-CONTENTSにあったリスト先頭からの反復走査を解消し、65,536文字の変換を回帰テストで確認した。これらの数値・走査修正後のSACLA記録は97.85%（9,802/10,017件）で、全27ファイル完走。後続のストリーム・ファイル修正より前の測定であり、上流ANSIテスト全体の達成判定には使わない。

数値カテゴリは中間版で1,444件を実行し922件成功・522件失敗となったが、補助ファイルの読み込みエラーが残るため完了扱いにしない。[中間測定](tests/ansi/numeric-progress.json)。その後、浮動小数点の分解関数とLOGANDのコンパイル処理を修正した。LOGANDはJavaScriptの32ビット演算への置換をやめ、多倍長整数対応のランタイム関数を呼ぶ。

FORMAT・プリンタ改善後の[全24カテゴリ測定](tests/ansi/full-format-progress.json)は15,375件成功・4,316件失敗。PRINTERは2,446件を完走し1,897件成功した。一部カテゴリにタイムアウト・読み込み失敗・登録数差が残るため、全体準拠率は算出しない。このスナップショットは今回の数値処理・線形走査修正より前であり、修正後の全体測定を別途継続している。

文字・バイナリファイルと複合ストリームを実装し、上流STREAMSは初回測定で762/803件成功。FILES 42/87、PATHNAMES 126/215も完走した（[記録](tests/ansi/stream-progress.json)）。この測定後、上流RTに合わせて実行時パッケージをCL-TEST、既定パスを作業用コピーのsandbox/に設定した。独立したSTREAMSワーカーにはFILESカテゴリで作成されるscratch/も用意する。テストの改行なし出力によって結果通知が失われる問題も修正した。上流のテスト本体・期待値・参照登録数は変更していない。

CLOSではENSURE-GENERIC-FUNCTIONによる通常関数の上書きを防止し、呼び出し可能な関数を返すようにした。スロットの型・文書オプション、継承した初期値の優先順位、NIL初期値、インスタンス作成時の既定初期化引数も修正した。アダプターを含む26検証グループが成功し、後続のストリーム修正は関連4グループで再確認した。数値修正時点の[全体測定](tests/ansi/full-numeric-progress.json)は15,126件成功・3,914件失敗だが、プリンターのタイムアウト等により未完了。最新版で上流全24カテゴリとSACLAの回帰測定を継続している。

ビットフィールド操作（BYTE/LDB/MASK-FIELD/DPB/DEPOSIT-FIELD、SETFの評価順序を含む）と任意精度ISQRTを実装した。平方根、三角・双曲線・逆関数、EXP/CIS/PHASE/SIGNUM、非整数EXPTも実装し、実数・複素数・分岐上の値を回帰テストで確認した。上流NUMBERS全1,444件を再測定中。通常のDEFSTRUCTにクラス・スロット情報を持たせ、FIND-CLASS/CLASS-OF/TYPEP/SUBTYPEP/スロットアクセス/メソッド選択を連携させた。既定・NILコンストラクターとCONC-NAME NILも修正し、構造体・型カテゴリを再測定中。総称関数の同一性、ADD-METHOD/REMOVE-METHODの戻り値、引数個数検査も修正した。

実行環境修正後の[ストリーム測定](tests/ansi/stream-results.json)はSTREAMS 781/803、FILES 66/87、PATHNAMES 128/215で全件完走（全体測定から完了カテゴリを抽出）。後続の細かなストリーム・総称関数修正より前の結果。READERの不足4登録はreadtable-case.lspのMACROLET定義が外側のSYMBOL-MACROLETを参照できず読み込みに失敗しているためと判明した。MACROLET展開関数のコンパイル時に外側の字句マクロ環境を引き継ぐ修正により、READTABLE-CASEの全17登録を回復した（16成功・型検査1失敗）。通常のEVALが字句環境を捕捉しないことも回帰テストで確認した。参照登録数は変更していない。

数値関数追加後の[NUMBERS測定](tests/ansi/numbers-results.json)は全1,444件を読み込み失敗・登録不足なく完走し、1,388件成功（96.12%）・56件失敗。総称関数の引数個数・字句マクロ環境の修正より前の結果。[REMOVE-METHODとCOMPUTE-APPLICABLE-METHODS](tests/ansi/method-results.json)は全21件完走・全成功。これらは選択カテゴリ／ファイルの結果であり、上流スイート全体の90%達成とは区別する。既存の29検証グループ（ランタイム203件を含む）も全成功を確認した。

CLOSの共有スロットは定義元の保存場所を継承先と共有し、クラス定義時に初期化するよう修正した。SLOT-MAKUNBOUND、SLOT-MISSING／SLOT-UNBOUND、UNBOUND-SLOT条件、REINITIALIZE-INSTANCEと初期化引数検査を追加し、関連51件が成功。コンパイラでは引数を左から右へ評価した値を保持し、FUNCALLの関数式も引数より先に評価するよう修正した。さらにLETの動的束縛復元とUNWIND-PROTECTの後処理で戻り値が変わる不具合を再現し、戻り値を退避する修正を検証中。

OBJECTSの参照登録数との差1件はDEFGENERIC.30の条件による。上流defgeneric.lspはSTANDARD-GENERIC-FUNCTIONのメタクラスがSTANDARD-CLASSの部分型である場合だけ登録する。SBCLではこのSUBTYPEPが(NIL T)となる一方、現在のValtanでは真となるため追加登録される。参照数862を変更せず、実行結果863との差を記録している。DECLARATIONの8件とともに、後述のテスト名照合で条件付き登録の差として記録する。

後続のストリーム・初期CLOS修正後のSACLA測定も全27ファイル10,017件を完走し、9,813件成功・204件失敗（97.96%）。[記録](tests/conformance-results.json)を更新した。数値の追加関数・構造体クラス・総称関数の引数検査・字句マクロ・評価順序修正より前のスナップショットであり、上流ANSIテスト全体の達成判定には使わない。

[構造体・型の測定](tests/ansi/structure-results.json)はSTRUCTURES 1,181/1,394、TYPES-AND-CLASSES 387/627で全件完走。構造体の前回794成功から387件改善した。総称関数引数検査・字句マクロ・共有スロット・評価順序より前のスナップショット。[マクロとスロット操作](tests/ansi/macro-slot-results.json)はMACROLET 41/53、SYMBOL-MACROLET 11/12、SLOT-MAKUNBOUND 7/8で全件完走。LET／UNWIND-PROTECT戻り値修正より前の値。引数評価順序修正時点の29検証グループ（ランタイム222件を含む）は全成功した。

ストリーム・初期CLOS・実行環境修正時点の[全体測定](tests/ansi/full-stream-clos-progress.json)は16,847件成功・3,638件失敗。PRINTERがFORMAT.X.5、READERがSYNTAX.SHARP-BAR.9でタイムアウトし、補助ファイル失敗と条件付き登録数差も残るため全体成功率は算出しない。この版は後続の数値関数・構造体クラス・マクロ・評価順序修正より前。最新ビルドで全24カテゴリを再測定し、重い数値印字・文字読み込みループを完走できるようカテゴリごとの制限を3,600秒にした。

&OPTIONAL／&KEYの既定値を引数未指定時だけ評価するよう修正した。&RESTは&KEYの既定値より先に束縛し、引数初期化中のエラーでも実際に束縛済みのSPECIAL変数だけを復元する。後続の既定値から局所SPECIAL宣言済みの引数も参照できる。これらを含むコンパイラ回帰43件は成功し、&AUXの逐次初期化・動的束縛の復元も確認した。

LETの動的束縛復元前・UNWIND-PROTECTの後処理前に戻り値を保存する修正は、失敗を再現した2件を含むコンパイラ32件で成功した。複数値と0個の値も保持する。最新ランタイム回帰ケース226件と上流ATOMを含む29検証グループがすべて成功した。ホストSBCL側でも外側のSYMBOL-MACROLET／MACROLETを参照するコンパイル2例が成功した。

構造体カテゴリのロード中CPUプロファイルでは、サンプル14,208のうちTYPEP配下が8,084（重複を除いた関数内集計）。コンパイラ内部のHIR等の構造体述語がTYPEPの長いCASEを通るため、ネイティブ構造体のタグと型名が一致する場合を早期判定する最適化を実施し、型・コンパイラ・構造体の関連回帰テストが成功した。

標準クラス階層（BUILT-IN-CLASS、GENERIC-FUNCTION、METHOD）と文字列・ビットベクタのCLASS-OFを修正した。短形式／組み込みメソッド結合、長形式の:arguments／:generic-function／評価される:order、メソッド引数リストの順序を実装・修正した。EQL特殊化の優先順位と引数位置を含むキャッシュキー、NO-APPLICABLE-METHOD、空の修飾子パターンも修正した。REMFはindicator評価後にplaceの値を読み取る。コンパイラ43・代入7・CLOS28の計78件が成功し、全回帰テストと上流該当ファイルを再測定中。

参照インベントリは未改変の上流ローダーで再生成し、21,944件および全ファイル／カテゴリ件数が従来と同一であることを確認した。ファイル別テスト名を追加し、実行名との照合を導入した。DECLARATION.4〜.11の全8件省略とDEFGENERIC.30の追加だけを上流条件分岐に基づく差として記録し、元の参照数を保持したまま有効期待数を算出する。他の欠落・追加や条件グループの部分欠落は未完了とする。テスト本体・期待値・成功／失敗の判定は変更していない。

引数・メソッド結合・REMF修正時点の全30検証グループが成功した（ランタイム253件、上流ATOM 5件を含む）。[上流の選択測定](tests/ansi/lambda-method-results.json)はLAMBDA 69/69、短形式メソッド結合13/13、REMF 11/11、DECLARATION 3/3（条件付き8件省略を記録）、長形式メソッド結合29/33で全件完走。後者の4失敗はPROGRAM-ERRORにformat-controlを渡さない場合の印字失敗。既定メッセージとメソッド結合:argumentsの&AUX対応を追加し、関連CLOSを再検証する。全体測定はランタイムSHAを固定し別途継続中。

PROGRAM-ERRORの既定メッセージとメソッド結合の&AUX対応は、コア20件・CLOS28件の関連48件が成功した。[長形式の上流33件](tests/ansi/long-method-results.json)も全成功した。進行中の全24カテゴリ測定はこの2修正の直前のスナップショットであり、SHAを固定している。

数値・構造体クラス・字句マクロ・共有スロット・戻り値修正時点のSACLA測定が完了し、9,814/10,017件成功（97.97%）、全27ファイル完走を確認した。記録を更新した。後続の引数初期化・メソッド結合・環境機能の修正より前のスナップショットであり、上流全体の90%判定には使用しない。

環境機能（日時変換・時計・処理系とホスト情報・APROPOS・DESCRIBE・TIME・TRACE・文書情報保存）を実装した。明示した時差では任意精度のグレゴリオ暦計算を用い、既定の現地時刻ではホストDateの範囲と時差情報を用いる。[初回測定](tests/ansi/environment-progress.json)は142/210、[TRACE・文書保存追加後](tests/ansi/environment-trace-results.json)は175/210で全件完走した。後続のSETF関数名対応・DOCUMENTATIONの単一戻り値修正はこの測定に含まない。ソートの:KEY適用箇所、短いベクタの内容消失、SOMEの戻り値、NOTEVERY未実装、PATHNAMEの余分な戻り値も修正し、関連回帰テストで検証した。

SETF関数名のFBOUNDP／FMAKUNBOUND、非インターン名のDEFUN、FDEFINITION差し替え後の代入先呼び出しを修正した。DOCUMENTATIONは単一値を返し、変位ベクタのEQUALPは論理添字を使う。[環境カテゴリ](tests/ansi/environment-results.json)は201/210件で完走。数値SUBTYPEPでは整数・非整数有理数・binary64を別領域として区間の交差／和集合／差集合を扱い、符号付きゼロと浮動小数点の隣接値も区別する。関連修正時点の全33検証グループ（ランタイム289件＋上流ATOM 5件）が成功した。後続のADJUST-ARRAY・ベクタ拡張・変位ビット配列修正は別途検証中。

[数値範囲とソートの選択測定](tests/ansi/numeric-type-results.json)はSORT 34/34、STABLE-SORT 34/34、SUBTYPEP-FLOAT 79/79、SUBTYPEP-REAL 25/25、SUBTYPEP-RATIONAL 18/18、SUBTYPEP-INTEGER 78/78で完走した。同時測定したCLASS-PRECEDENCE-LISTSは0/71で、上流の補助総称関数定義が未定義のLOGICAL-PATHNAMEクラスで途中停止していた。標準クラス階層にこのクラスを追加し再測定中。旧マクロ・共有スロット時点の[全体測定](tests/ansi/full-macro-slot-progress.json)は17,923成功・2,654失敗、STRUCTURESとPRINTERが制限時間に達して未完了である。

ADJUST-ARRAYは元の配列の同一性を保ち、全保存領域と変位情報を更新する。INITIAL-CONTENTS指定時は旧内容を破棄し、未指定時は共通の添字の内容を保持する。フィルポインタの保持／検査、VECTOR-PUSH-EXTENDのextension指定、変位配列に対するBIT演算とVECTOR-POPも修正した。8件の回帰ケースすべてで修正前の失敗を再現し、関連75件が修正後に成功。[ARRAYSの全測定](tests/ansi/array-results.json)は1,355/1,356件成功で完走した。残る1件はUPGRADED-ARRAY-ELEMENT-TYPEのNIL型。

FTYPE宣言の誤った変数扱いを修正し、上流FUNCTIONの24件が成功した。PROGVを動的束縛と復元を行う形で実装した。UNWIND-PROTECTは後処理へ入る時点で複数値を保存し、RETURN-FROM／THROWによる脱出でも保持する。THROWは結果式の全値を捕捉し、CATCHは0個を含めて復元する。関連コンパイラ50・配列8・CLOS29の計87件が成功した。その後、標準クラスの不足（METHOD-COMBINATION、STRING-STREAM等）とSIMPLE-TYPE-ERROR／PRINT-NOT-READABLE条件、ストリームのCLASS-OFを補完した。最新版SHAを固定し、全24カテゴリを8並列・各7,200秒で測定中。

標準クラス補完後の[選択測定](tests/ansi/control-class-results.json)はCLASS-PRECEDENCE-LISTS 71/71、PROGV 18/18、CATCH 15/16で完走した。CATCHの残り1件は一致するタグがないTHROWでCONTROL-ERRORを返す必要がある。最新版の全34検証グループ（ランタイム307件＋上流ATOM 5件）が成功した。全24カテゴリとSACLAは同じ最新版で測定を継続しており、上流全体の90%はまだ未確定。

引数初期化・メソッド結合・環境機能・数値範囲判定までのSACLA測定は9,819/10,017件成功（98.02%）、全27ファイル完走。件数検証スクリプトも成功し、記録を更新した。配列調整・PROGV・非局所脱出の複数値・標準クラス補完より前のスナップショットであり、最新版のSACLAは別途測定中。

WITH-SLOTS、WITH-ACCESSORS、CHANGE-CLASSとUPDATE-INSTANCE-FOR-DIFFERENT-CLASSを実装し、スロットの既定の割り当て種別も修正した。新しい回帰8件と既存CLOS 31件は成功。上流の文字名から生成されるテスト名は、SBCLとValtanの82組を一対一で監査し、大文字・小文字を維持して記録する。修正後の参照生成器は元の21,944件の一覧を完全に再現した。新しい固定バンドルd0cc927bで全24カテゴリ、全ハーネス、SACLAを再検証中。

メソッド結合までの[過去の全体測定](tests/ansi/full-method-combination-progress.json)は19,218件成功・2,719件失敗で全カテゴリの実行を終えたが、READERの登録名監査が不一致のため未完了扱いとし、全体成功率は記録しない。この旧測定を新しい監査で遡って成功扱いにはしていない。

[クラス変更・登録名の選択測定](tests/ansi/class-change-registration-results.json)はWITH-SLOTS 21/21、WITH-ACCESSORS 15/15、CHANGE-CLASS 36/40、UPDATE-INSTANCE-FOR-DIFFERENT-CLASS 8/8で完走した。残る4件はクラスオブジェクトのEQL特殊化に関係する。SET-SYNTAX-FROM-CHARも101件を漏れなく実行し、82組の名前対応を監査できた。失敗101件は失敗のまま保持し、全体の成功率には代用しない。

クラス変更・登録名監査の修正後、全36検証グループが成功した（回帰315件＋上流ATOM 5件、失敗0・スキップ0）。同じ意味の実装でSACLAも再ビルド済み。固定バンドルd0cc927bの上流全24カテゴリと最新SACLAは実行中であり、全体90%は完走結果で確定する。

上流全体用の90%検査を `tests/ansi/check-conformance.mjs` に追加した。全24カテゴリ・全ファイル・登録名・実行件数・失敗記録・測定バンドルを照合し、部分実行や未完了の結果を拒否する。境界値、件数改変、同数のテスト差し替え、条件付き登録、文字名の対応を含む5検査が成功した。手動CIの全体実行にも適用し、単独カテゴリの実行は従来どおり全件成功を要求する。実行中のLispバンドルは変更していない。

配列・PROGV・非局所脱出の複数値・標準クラス補完までのSACLA測定は9,823/10,017件成功（98.06%）、全27ファイル完走・未完了0件で件数検査も成功した。記録を更新した。クラス変更機能などを含む最新版のSACLAと、固定版d0cc927bの上流全24カテゴリは引き続き実行中。配列カテゴリも1,355/1,356件で完走した。

クラス変更機能と既定スロット割り当てを含む最新版のSACLAも9,823/10,017件成功（98.06%）、全27ファイル完走・未完了0件で独立した件数検査を通過した。`tests/conformance-results.json` を現在の実装の結果に更新した。上流全体は21カテゴリが完了し、構造体・プリンタ・リーダの測定を継続中。

全体測定のPRINTER遅延をNode inspectorで調べ、PRINT.INTEGERS.RANDOM.1の判定後に4,217件の失敗値を循環検査付きで整形する二乗時間の処理を確認した。旧2測定のプリンタ子プロセスを停止し、未実行件数と未完了状態を保持した。d0cc927bの結果は `tests/ansi/full-class-change-progress.json` に保存。診断整形だけに長さ10・深さ5の上限を設け、循環検査を無効にした。大きな失敗値・循環値・循環した条件を失敗のまま報告し、後続テストと印字制御の復元を確認する検査が成功した。

併せてディスパッチ引数をREAD-BASEに関係なく十進数で読むよう修正し、#Bの符号・分数の区切りを捨てる旧処理を共通基数リーダーに統合した。追加2回帰は旧実装で失敗し、修正版で2〜36進数の210往復を含め成功した。関連7グループは全成功。固定バンドル17e2da5d293126ee76d15768d985bf64d549c68f94724e0c28d72c8ffd624723で全24カテゴリと全ハーネスを再実行し、SACLAも再ビルド中。

基数リーダーと診断上限の修正後、全42検証グループが成功した（統合37グループ：回帰317件＋上流ATOM 5件など、90%判定ゲート5グループ）。失敗0・スキップ0。固定バンドル17e2da5dの上流全24カテゴリと、同じ実装で再ビルドしたSACLAを実行中。

基数リーダー修正後のSACLAは9,824/10,017件成功（98.07%）、全27ファイル完走・未完了0件。リーダーで1件改善し、独立した件数検査を通過した。測定記録を最新版に更新。上流全体は21カテゴリ完了、構造体・プリンタ・リーダーの3カテゴリを継続中。

上流ANSI全24カテゴリが完走し、20,062/21,937件成功（91.45%）、失敗1,875件、未実行0件となった。固定上流版ebf9c806・固定バンドル17e2da5d、evalモード、8ワーカーで約61分。全カテゴリの登録名監査・条件付き登録・82組の文字名対応・失敗記録・実行バンドルを独立した90%ゲートで照合し成功した。`tests/ansi/full-results.json` に全結果を保存。上流テスト本体と比較条件は変更せず、過去の部分測定やSACLAを合算していない。全42ハーネス検証と最新版SACLA 98.07%も成功しており、全体90%以上の目標を達成した。残る失敗は整形出力、構造体/CLOS、読み取り構文、パス名、システム構築などで、記録から継続対応できる。
