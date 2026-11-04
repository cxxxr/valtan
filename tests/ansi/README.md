# Upstream ANSI tests for Valtan

The unmodified `ansi-test` suite is pinned as the Git submodule
`submodules/ansi-test` at `ebf9c806f383d5dc921c421e0d083e925c64a6cd`
(2026-11-02). Upstream: https://gitlab.common-lisp.net/ansi-test/ansi-test.
The upstream license and individual copyright notices are retained.

## Latest complete measurement

The [complete conformance result](full-conformance-results.json) records
**21,812/21,937 passes (99.43%)** and 125 failures on 2026-11-04, in eval mode.
All 24 categories complete with zero unexecuted tests and no registration gaps.
The independent **99% gate passes**, including verification against the measured
runtime bundle. Upstream test bodies and expected results are unchanged.
Bundle SHA-256: `48cb024c2df9a563c6fa5ccd0dfbf2838a75e51cd79bbc33903d02f4b761667b`.
The run used eight workers and a 7,200-second limit per category and completed
in about 21 minutes. The full compiled-mode suite has not been measured on this
snapshot; selected regression fixtures cover that mode separately.

This is a success rate for the pinned test suite, not a percentage of the ANSI
specification implemented. Remaining failures include 34 object-system tests,
32 condition tests, and 11 printer tests; every failure is retained in the report.
The [selected FORMAT/reader run](reader-format-final-selection.json) also records
two subnormal-float `~E` differences. Selected runs can exercise different random
values after different preceding tests; their counts are not combined with the
full-suite result.

For comparison, the [preceding sequence/control run](full-sequence-control-progress.json)
recorded 21,521 passes (98.10%), and the [first 99% candidate](full-conformance-candidate.json)
recorded 21,790 passes (99.33%). The original 20,062-pass baseline is preserved below.

## Implementation and regression coverage

The changes cover structure layouts and inheritance, reader syntax and readtable
copies, pretty printing and FORMAT, sequence and type operations, generalized
places, dynamic CATCH/THROW, LOOP bindings and accumulation, numeric rounding and
floating-point conditions, CLOS method congruence and dispatch, and compiler
macro environments and EVAL-WHEN. COMPILE-FILE writes an artifact that LOAD can
execute in a fresh process after the source is removed; pathname operations
include logical pathname translation.

The latest harness run passes all 49 groups, including 438 focused runtime
regressions, upstream ATOM tests, cross-process compiled-artifact loading,
registration auditing, and rejection of incomplete or misleading reports.
The 35 FORMAT/printer regressions also pass in both eval and compiled modes.
Compiler semantic regressions, eight workload checks, and two kernel test
groups pass. These checks are separate from the upstream ANSI denominator.

The [latest SACLA result](../sacla-conformance-results.json) records
9,953/10,017 passes (99.36%) and 64 failures, with all 27 files complete.
Its independent coverage check passes; the earlier 9,824-pass baseline is
preserved separately.

The SACLA suite also checks exact symbol spellings under nonstandard readtable
case modes with `:READABLY T`. Its 22 newly failing desirable-printer cases remain
failures in the report. The implementation now preserves symbol identity when
read with the standard readtable, as required by
[`*PRINT-READABLY*`](https://www.lispworks.com/documentation/HyperSpec/Body/v_pr_rda.htm);
the requested spelling can conflict with that requirement or differ from another
valid readable spelling. SACLA diagnostic output enables circular printing so
logging a circular test form cannot prevent later tests from executing.

## Historical PPRINT and FORMAT measurements

The [complete reader/structure run](full-reader-structure-progress.json) records
20,451/21,937 passes (93.23%) and 1,486 failures. All categories and the
independent registration audit complete. This is below the required 99%.

The subsequent [four-category measurement](pprint-string-progress.json), bundle
`3e68c7cd26535709e44090217d6119e987bd4226eaf34aa2c40de35f974412dd`,
records READER 673/673, ARRAYS 1,356/1,356, STRINGS 491/509 and PRINTER
2,191/2,446. The [FORMAT pretty selection](format-pprint-progress.json), bundle
`b02ccdc11a0aecd2e8364abbcd2f77d60345f943ab3d0d7ed345b65b0f831026`,
passes all 151 logical-block, conditional-newline, indentation and tab tests.
These selected results do not establish an overall rate.

The new pretty printer captures logical blocks in a document tree, then chooses
line breaks using section widths, margins and miser mode. It implements prefixes,
indentation, tabs, length/depth abbreviation, dispatch-table copies and priorities,
and shared/circular lists. FORMAT and FORMATTER use these same facilities for
logical blocks and pretty directives. String output escapes quotes and backslashes;
symbol case conversion preserves letters unaffected by the readtable case.
Decimal float input uses one native decimal conversion to avoid double rounding.
Array/string subtyping includes NIL element types, following the
[STRING type definition](https://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm).
The NIL-vector string tests differ from SBCL's treatment of NIL vectors; they
are checked against the specification and the unchanged upstream expectations.

The first full run exposed ten reader regressions from conditionally defined
structures: registering the class at compile time made runtime guards skip the
constructor definitions. Registration now occurs with the definitions at runtime;
a dedicated fixture covers both eval and compiled modes. FLET and LABELS bodies
now have their required implicit named blocks, preserving local declarations.

The [preceding complete full run](full-pprint-progress.json) records
20,987/21,937 passes (95.67%) and 950 failures, with all categories complete
and no registration gaps. It snapshots bundle
`0511b112e31daa1e4e30b8262835b8d3e1e827c6456b8ad44cd1c60a53a91af2`.
All 45 harness groups pass on that bundle, including 362 focused regressions;
the 21 printer regressions in that snapshot also pass in compiled mode.
Later edits require another measured
bundle before any claim about their overall success rate.

The [language progress full run](full-language-progress.json) records
21,128/21,937 passes (96.31%) and 809 failures, with all 24 categories complete
and no registration gaps. Its bundle SHA-256 is
`80326cee8d12eb53b7ba8e4aabd2bb26364ee6e649b9658a9643c152116cc652`.
The independent conformance gate rejects this result: 99% requires at most
219 failures. This snapshot includes compiler metadata isolation, local SETF
functions, named LOOP exits, multiple-value generalized places, compound
condition types and additional pretty-print limits. It predates the subsequent
COMPILE-FILE, LOAD, CATCH and pathname implementation changes.

The [compilation selection](compile-eval-progress.json) completes with
SYSTEM-CONSTRUCTION 74/77 and EVAL-AND-COMPILE 287/318. The
[CATCH/place selection](catch-place-progress.json) passes all 73 tests.
These later selections are not combined into the full-suite rate.

## Historical structure measurements

The [structure layout result](structure-layout-results.json) completes all
1,394 STRUCTURES tests successfully (100%). It fixes all 197 failures from
the recorded full-run baseline without regressing previously passing tests.
This measurement uses bundle SHA-256
`ddc71c7d9faa30728b631f0389694b3021c3b8558b2eeeaab96a2345d419928e`.
It is extracted from the completed full-suite
measurement below; it is not an overall success rate. Invalid typed predicates
and name tags that cannot be represented by the vector element type signal
errors and have regression coverage.

DEFSTRUCT now implements list and vector layouts, specialized vector element
types, `:NAMED`, inherited layouts and initial offsets, included slot overrides,
multiple constructors and BOA argument defaults. Ordinary keyword constructors
use fresh parameter names so special or constant slot names do not become
lambda variables. Structure classes inherit from their included parent for
TYPEP, SUBTYPEP and generic dispatch. Copiers preserve the dynamic structure
type and copy the slot storage while sharing its contents. `:PRINT-OBJECT`
receives two arguments. These semantics follow the
[DEFSTRUCT specification](https://www.lispworks.com/documentation/HyperSpec/Body/m_defstr.htm)
and [BOA lambda lists](https://www.lispworks.com/documentation/HyperSpec/Body/03_df.htm).

The first ten new layout regressions all fail on the preceding reader build;
the twelve current regressions pass on SBCL and the final Valtan build in
both eval and compiled modes. All 44 harness groups pass, including 339 focused
runtime regressions. Upstream sources and expected results are unchanged.

The [full reader/structure run](full-reader-structure-progress.json) snapshots
the follow-up bundle at SHA-256
`ddc71c7d9faa30728b631f0389694b3021c3b8558b2eeeaab96a2345d419928e`,
using eight workers and a 7,200-second limit per category. Check its `complete`
field before interpreting totals. It must pass the independent 99% full-suite
gate before the overall target can be considered achieved.

## Historical reader measurements

The [reader syntax result](reader-syntax-results.json) completes all 673 READER
tests with 672 passes (99.85%) and one failure, `WITH-STANDARD-IO-SYNTAX.23`
(missing `COPY-PPRINT-DISPATCH`). Compared with the recorded full-run baseline,
191 failures are fixed and no previously passing READER test regresses. The
bundle SHA-256 is
`ec13c2fef7f0c41b52b6c0e9ae3fcf8f9c95975028c17ce5d8454a623b5e49c5`.
This is a selected-category result, not a new overall ANSI success rate.

Readtables now retain and independently copy syntax types and dispatch tables.
The reader implements `SET-SYNTAX-FROM-CHAR`, `READ-PRESERVING-WHITESPACE`,
structure literals, bit-vector length extension, delimited-list terminators,
and stream information on reader errors. String escapes remove the escape
character, feature expressions compare symbol identity, commas require a
backquote context, and invalid constituents and zero-denominator ratios signal
reader errors. Symbol printing escapes package markers, numeric names and
embedded escape characters so those names can be read back.

The [related stream/numeric selection](reader-stream-results.json) completes
PEEK-CHAR with 25/28 passes (two improvements) and PARSE-INTEGER with 55/56
passes. Remaining failures concern string-output-stream positions and NIL
element-type strings. PARSE-INTEGER retains standard whitespace independently
of readtable changes, as specified by the
[HyperSpec](https://www.lispworks.com/documentation/HyperSpec/Body/f_parse_.htm).
Readtable copying and recursive whitespace preservation follow
[SET-SYNTAX-FROM-CHAR](https://www.lispworks.com/documentation/HyperSpec/Body/f_set_sy.htm)
and [READ](https://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm).

All 43 harness groups pass on this bundle, including 327 focused regressions.
The ten new reader regressions also pass in compiled mode and on SBCL; the
first nine all failed on the preceding baseline bundle. Upstream test bodies,
expected results and registration auditing are unchanged. The full-run
conformance gate now requires 99% success, without rounding up the measured rate.

## Historical baseline measurement

The [full result](full-results.json) records **20,062/21,937 passes (91.45%)**
and 1,875 failures on 2026-11-04. All 24 categories complete in eval mode,
with no loader errors or unexecuted tests and a complete registration audit. The independent
coverage and former 90% gate passed against the measured bundle, SHA-256
`17e2da5d293126ee76d15768d985bf64d549c68f94724e0c28d72c8ffd624723`.
The run used eight workers and a 7,200-second timeout per category, and finished
in about 61 minutes. This is a result for the pinned test suite; the full compiled
mode suite has not been measured on this snapshot.

All 42 harness groups pass, including 317 focused runtime regressions, the five
original ATOM tests, reporting and registration checks, and five tests of the
full-run gate. Separately, SACLA passes 9,824/10,017 cases (98.07%) with all
27 files complete. Neither fixtures nor SACLA contribute to the ANSI totals.
Pretty printing, parts of structures/CLOS, reader syntax, pathnames and system
construction still have failures; see the full report for each test and reason.

## Build and run

From the repository root, build the Valtan executable using the normal project
setup, then:

```sh
git submodule update --init --recursive
cd tests
npm install
npm run build:ansi
npm run test:ansi:harness
npm run test:ansi -- --list
npm run test:ansi -- --file cons/atom.lsp
npm run test:ansi -- --category cons --timeout 900
npm run test:ansi -- --mode compiled --file cons/atom.lsp
npm run test:ansi -- --jobs 4 --timeout 7200 --output ansi-results.json
npm run test:ansi:check -- ansi-results.json dist-ansi/ansi.js
```

No selector means **all 24 categories** in upstream `gclload2.lsp`, in its
original order. Repeat `--category` or `--file` to select several. Each selection
runs in a fresh Node process with its own temporary copy of the suite. Test bodies
use that copy’s `sandbox/` as `*default-pathname-defaults*` and `CL-TEST` as the
current package, matching upstream `RT:DO-TESTS`. STREAMS workers also create
`sandbox/scratch/`, which the preceding FILES category normally creates. Temporary
work directories are removed when their workers finish. The default timeout is 120 seconds per selection;
large categories need a longer timeout. A timeout kills that process and the
runner continues with the next category. `--jobs` runs independent selections
in parallel (default: 1). The JSON report is atomically updated during execution
and after each selection. The CLI snapshots the runtime bundle at startup and
records its SHA-256, so a rebuild cannot change later workers in the same run.
Select one file for faster diagnosis; its preceding category
auxiliary files are loaded as well. Dependencies on definitions in other test
files may require running the whole category.

`manifest.json` contains 779 ordered loading steps, including auxiliary files.
It is generated by reading the pinned upstream loaders with SBCL under the
`(:valtan)` feature list, without evaluating those
loaders. It is a file inventory, **not a test-case count**. Regenerate from the
repository root with:

```sh
sbcl --script tests/ansi/generate-manifest.lisp > tests/ansi/manifest.json
```

`reference-inventory.json` additionally records **21,944 unique test names**
registered by the original RT loaders under SBCL with `*features* = '(:valtan)`.
The inventory includes names by source file as well as counts. This is a
diagnostic registration reference, not a Valtan result or a passing percentage.
The runner compares executed names against it, so equal counts cannot hide a
missing test replaced by an unrelated one. Two audited upstream conditionals
are recorded separately: `DECLARATION.4`–`.11` are registered only if unknown
declarations produce warnings; `DEFGENERIC.30` is registered only for a particular
generic-function metaclass relationship. The original reference counts remain
unchanged. Only those exact alternatives adjust the effective expected count;
other missing or unexpected names, including a partially missing declaration
group, make the selection incomplete. No registered test is skipped or counted
as passing by this audit. In `reader/set-syntax-from-char.lsp`, 82 test names
are generated from implementation-defined ASCII character names. The audit
pairs SBCL's exact Unicode names with Valtan's corresponding characters, one
for one, with no change to the expected count. Missing characters, case-folded
duplicates, unknown names, and extra registrations still make the result
incomplete. Protocol names use SYMBOL-NAME so distinct mixed-case symbols
remain distinct independently of the implementation's printer.
To reproduce the reference, extract the pinned Git revision into a scratch
directory, then run:

```sh
sbcl --script tests/ansi/generate-reference.lisp /tmp/ansi-copy/ > reference.json
```

The original loaders create compiled files, so the generator rejects a Git
checkout. All upstream test bodies and comparison expectations remain unchanged.

## Adapter and semantics

Valtan does not yet support enough pathname, package, and compiler operations to
boot upstream `init.lsp` and all of RT. `rt-compat.lisp` supplies a small test
registry and the upstream `DEFTEST` record model: name, properties, form, and
expected multiple values. The `equalp-with-case` comparison function is copied
verbatim from pinned upstream `rt.lsp`; the harness test checks that match.
Bodies and expected values in upstream test files are never changed. Notes are
retained as metadata; this adapter does not disable tests by note or apply any
implementation's expected-failure list. `DEFNOTE` declarations do not filter tests.

The adapter loads upstream `cl-test-package.lsp` and the original auxiliary
definitions with Valtan's reader and `EVAL`, including the shadowed
`HANDLER-CASE`/`HANDLER-BIND` warning wrappers.
Helper `COMPILE-AND-LOAD` calls are routed to this loader without a FASL cache.
This is an adaptation of test infrastructure, not a complete implementation of
RT or of Common Lisp file compilation. These adaptations apply only in the
separate ANSI runner bundle.

`eval` mode evaluates each original form and captures **all** returned values.
Valtan's own EVAL compiles forms to JavaScript. `compiled` mode explicitly calls
Valtan's `COMPILE` on a lambda wrapping the form; it never substitutes EVAL for
an unavailable compiler. COMPILE now uses Valtan's JavaScript compiler for lambda expressions and retains
existing function definitions when no replacement definition is supplied.

## Results and current limitations

Exit status is 0 only when every selected test loaded and ran, at least one test
was registered per selection, and all registered tests passed. Status 1 means
failure or incomplete execution; status 2 means invalid invocation or an
upstream checkout that does not match the pin. A dirty upstream checkout is
rejected. New ANSI results are independent of the recorded SACLA percentage.

The JSON report records:

- `pass`, `fail`, `registered`, `expectedRegistered`, and individual failure messages;
- `status`, the current file, and `registrationMismatch`;
- `referenceRegistrationMismatch`, `effectiveExpectedRegistered`, missing and
  unexpected test names, and explanations of audited `conditionalRegistrations`
  and one-to-one `aliasedRegistrations`;
- `unexecuted`: known registered tests with no result;
- `unregisteredTestsUnknown`: a load/read failure may have prevented additional
  tests from being registered, so their number is unknown;
- loader errors, unread files, a pending test at interruption, and timeouts;
- `complete` and `passRate` (the latter is null for any incomplete selection).

Even if every registered test runs, failed auxiliary initialization makes the
selection incomplete. Missing registrations never count as passes. An uncaught
JavaScript exception fails the current test and allows subsequent tests to run;
it never counts as the Lisp condition expected by an error-handling test. The
latest complete result above includes all failures; a successful 99% gate does
not mean every upstream test passes.

## Implementation and measurement history

The following measurements document earlier snapshots and selected files.
They are not combined into the latest complete result above.

Initial ATOM
smoke execution registered five original tests and exposed missing universe
initialization, auxiliary macros, and error behavior. Runtime macro and structure
bootstrap errors, constant lookup, reader suppression, exact large integer
arithmetic, COMPILE/COERCE, macro environments, two-way streams, random-state
copying, pathname literals, and basic long-form method combinations have since
been addressed. Regression fixtures also cover numeric and array type specifiers,
keyword argument processing, generalized assignment, object identity during EVAL,
growing hash tables, package operations and iteration, DEFPACKAGE option order,
absolute values, displaced arrays, local macro environments, FORMAT/FORMATTER,
reader escapes, and printer controls. The baseline harness run covered 317 focused runtime regression cases.
The latest complete full-suite rate is recorded separately above.

The recorded [CONS baseline](cons-baseline.json) completed all 1,882 tests with
1,882 passes (100%). It includes the assignment, set-operation, literal identity,
package, and ABS fixes, and predates the later displaced-array and local macro fixes. This is a category measurement, not the full
suite percentage.

The recorded [ATOM smoke result](smoke-results.json) loads the common helpers
without errors and passes all five tests in `cons/atom.lsp`. Its `selection` is
`selected`; this small smoke result is not the full suite percentage.

CI always builds the adapter and checks its reporting against fixtures, including
multiple values, case sensitivity, errors, and timeouts, plus an actual upstream
ATOM file. This **harness check** is distinct from a conformance pass. The full
ANSI run is available through the SACLA workflow's manual `run_ansi` input;
the full-run gate requires all 24 categories, the complete audited inventory,
and at least 99% success. It verifies that the measured bundle matches the CI
build, recomputes counts and registration audits, and rejects selected or
incomplete reports. Individual category runs still require every test to pass.
The JSON report is uploaded even on failure. Manual runs allow 7,200 seconds
per category; the runner itself continues to return status 1 for any failure,
while the separate full-run gate decides whether the 99% target is met.

The recorded [four-category measurement](core-results.json) completed PACKAGES
(479/500), CHARACTERS (259/259), HASH-TABLES (147/158), and ARRAYS (1,137/1,356).
It precedes the FORMAT work. The [earlier full attempt](full-baseline.json)
recorded 13,175 passes and 5,896 failures, with additional unexecuted and missing
registrations. It is incomplete and deliberately has no full-suite pass rate.

The FORMAT interpreter and FORMATTER share argument tracking and directive
parsing. Basic output, integer radix formatting, conditional and iterative
controls, recursive controls, case conversion, and justification are implemented.
The initial implementation lacked pretty-printer logical blocks and complete
floating-point directives. Later measurements above cover those additions;
the presence of a directive is not a claim that all of its cases pass.

The recorded [FORMAT measurement](format-results.json) completed all 604 tests
in eight unmodified upstream files (A, S, D, conditional, iteration, argument
position, recursive control, and case conversion), with no failures or missing
registrations. Other printer files remain part of the full-suite measurement.

Exact ratios now retain normalized integer numerators and denominators across
arithmetic, comparison, reading, printing, and compiled literals. Complex
arithmetic, floating-point decomposition/scaling, integer Boolean operations,
and floating rounding functions have regression coverage. Decimal input is
converted to floating point only after exact decimal parsing, including negative
zero. Long list-to-string conversion advances list cursors rather than repeatedly
indexing from the head. The subsequent SACLA measurement completed 9,802/10,017 cases successfully
(97.85%), before the stream and file changes. Full upstream measurements were
still pending on that snapshot.

A registration-only audit traced the eight absent EVAL-AND-COMPILE tests to
`eval-and-compile/declaration.lsp`: DECLARATION.4 through .11 are conditional on
warnings for unknown declarations. Valtan currently emits none, so the original
file registers 318 category tests against SBCL's 326. The reference remains
unchanged and the runner initially marked that mismatch incomplete. The eight
conditional signed-zero tests in `types-and-classes/subtypep-float.lsp` motivated
the EQL signed-zero correction; the category now registers all 627 reference
tests. An output-framing bug dropped one result after text without a newline;
protocol records now begin on their own line, with regression coverage.

The [numeric progress report](numeric-progress.json) executed all 1,444 numeric
tests (922 passes, 522 failures), but its auxiliary initialization failures make
it incomplete. It predates the float decomposition functions and the LOGAND
compiler correction, which removes JavaScript's 32-bit integer coercion. This
report is diagnostic evidence, not a full-suite percentage.

The subsequent [full FORMAT-era attempt](full-format-progress.json) recorded
15,375 passes and 4,316 failures across all 24 categories. PRINTER completed
1,897/2,446 tests successfully. Other categories still had timeouts, loader
failures, or registration differences, so this full attempt remains incomplete.
Its snapshot predates the numeric and sequence-conversion changes described
above; a newer full measurement is running.

Native Node file operations now support character and binary streams, positioning,
closing and abort cleanup, probing, renaming, deleting, and directory creation.
Composite streams and sequence I/O have regression coverage. The initial
[stream progress report](stream-progress.json) completed STREAMS (762/803), FILES
(42/87), and PATHNAMES (126/215). That report predates the sandbox and current
package correction above; missing-fixture failures are retained in the report.
Logical pathnames and broader pathname semantics remain incomplete.

The [rational result](rational-results.json) passes all 48 tests in six original
rational/complex component files; the [Boolean result](bit-results.json) passes
all 50 tests in LOGAND, LOGIOR, LOGXOR, and LOGEQV. These are selected-file results.

CLOS fixes preserve ordinary functions when ENSURE-GENERIC-FUNCTION is called,
return callable generic functions, accept slot type/documentation metadata, and
evaluate default initialization forms when instances are created. More specific
slot initialization forms, including NIL, take precedence over inherited forms.
The broader object-system implementation still needs conformance work.

The [full numeric-era attempt](full-numeric-progress.json) recorded 15,126 passes
and 3,914 failures. NUMBERS completed 1,006/1,444 and SEQUENCES 3,193/3,287;
PRINTER timed out early in its floating-point tests, while object-system failures
prevented many later results. The total is incomplete and is not comparable to
a complete-suite percentage. This snapshot predates the stream/CLOS and adapter
corrections; the latest complete full run is recorded above.

The corrected [stream measurement](stream-results.json), extracted from the
subsequent full run, completes STREAMS (781/803), FILES (66/87), and PATHNAMES
(128/215). It includes the sandbox/package/framing corrections, and predates
the latest stream and generic-function argument checks.

Bit-field operations BYTE/LDB/MASK-FIELD/DPB/DEPOSIT-FIELD now use exact integers,
including SETF places and their evaluation order. ISQRT uses integer Newton
iteration without converting through floating point. Square roots, trigonometric
and hyperbolic functions, their inverses, EXP, CIS, PHASE, SIGNUM and noninteger
powers have regression coverage for real and complex inputs. Branch behavior was
checked against the [CLHS numeric functions](https://www.lispworks.com/documentation/HyperSpec/Body/f_asin_.htm);
the latest full NUMBERS category completes with 1,392/1,444 passes.

Ordinary DEFSTRUCT definitions now expose class and slot metadata to FIND-CLASS,
CLASS-OF, TYPEP, SUBTYPEP, SLOT-VALUE and generic method dispatch. Bare constructor
options preserve the default constructor, NIL suppresses it, and a NIL CONC-NAME
uses the slot symbol itself. Structure inclusion and typed layouts remain
incomplete. Generic functions retain their identity through method additions
and removals and check argument counts before dispatch.

The [numeric measurement](numbers-results.json) completes all 1,444 NUMBERS
tests, with 1,388 passes (96.12%) and 56 failures, without loader errors or
missing registrations. It includes bit-field and transcendental functions,
and predates the generic-function arity and lexical macro corrections.
The [method API measurement](method-results.json) completes all 21 tests in
REMOVE-METHOD and COMPUTE-APPLICABLE-METHODS successfully. These are selected
measurements, not a full-suite conformance rate.

Local macro expanders now compile with their enclosing macro and symbol-macro
bindings. Lexical variables and functions shadow macros in the corresponding
namespace, and NIL remains a valid symbol-macro expansion. Ordinary EVAL still
uses a null lexical environment. The [macro progress report](macro-progress.json)
restores all 17 READTABLE-CASE registrations (16 passes); its other selected
files retain failures predating the later declaration and argument-order fixes.

Class-allocated slots now share storage through inheritance and initialize at
class definition time. SLOT-MAKUNBOUND, SLOT-MISSING, SLOT-UNBOUND and
REINITIALIZE-INSTANCE have regression coverage, including method keyword
arguments and preserving unbound slots. Class redefinition and broader
metaobject behavior still need work.

The one additional OBJECTS registration is DEFGENERIC.30: its upstream guard
checks whether the metaclass of STANDARD-GENERIC-FUNCTION is a subclass of
STANDARD-CLASS. SBCL returns false, while Valtan currently returns true. The
reference count remains 862 and the observed count 863; the runner still marks
that mismatch incomplete pending explicit handling of audited conditional
registrations.

The [structure/class measurement](structure-results.json) completes STRUCTURES
with 1,181/1,394 passes and TYPES-AND-CLASSES with 387/627 passes. It includes
ordinary structure class metadata and constructor corrections, and predates
the later generic-function and compiler changes. The [macro/slot measurement](macro-slot-results.json)
completes MACROLET (41/53), SYMBOL-MACROLET (11/12) and SLOT-MAKUNBOUND (7/8).
It predates the LET/UNWIND-PROTECT return-value capture correction.
The subsequent SACLA measurement completes all 10,017 cases with 9,813 passes
(97.96%); its snapshot includes file/stream and initial CLOS fixes but predates
the newer numeric, structure and compiler work.

The [stream/CLOS-era full attempt](full-stream-clos-progress.json) recorded
16,847 passes and 3,638 failures. It remains incomplete: PRINTER timed out at
FORMAT.X.5, READER at SYNTAX.SHARP-BAR.9, and auxiliary/conditional registration
differences remain. It predates the subsequent numeric, structure and compiler
fixes. The latest full run uses a 3,600-second per-category limit to give the
large numeric-printing and character-reading loops more time.

Function calls now capture each argument before evaluating the next one, and
FUNCALL evaluates its function form first. LET saves its result before restoring
dynamic bindings; UNWIND-PROTECT saves the protected value before cleanup, while
retaining the multiple-value result. Focused regressions reproduce these cases,
including zero values and cleanup that changes the referenced variable.

The final harness run after these corrections passes all 29 groups, including
226 focused runtime regression cases and all five original ATOM tests. Full
upstream and SACLA measurements of this snapshot are running separately.

The parameter, method-combination and REMF snapshot passes all 30 harness groups,
including 253 focused runtime cases and the original five ATOM tests.
The [selected upstream measurement](lambda-method-results.json) completes
LAMBDA (69/69), short-form DEFINE-METHOD-COMBINATION (13/13), REMF (11/11),
and DECLARATION (3/3, with the eight conditional omissions recorded). Long-form
DEFINE-METHOD-COMBINATION completes all 33 tests with 29 passes; its four
remaining failures concern printing a PROGRAM-ERROR without a format control.
This snapshot precedes the default error-report fix and method-combination
`&AUX` extraction. These selected results are separate from full-suite measurements.

The subsequent default PROGRAM-ERROR report and method-combination `&AUX`
extraction fixes pass all 48 related core/CLOS regression cases and all
[33 upstream long-form method-combination tests](long-method-results.json).
The full run started before those changes retained its earlier runtime snapshot.

Environment support now includes universal-time conversion, clocks, implementation
and host information, APROPOS, DESCRIBE, TIME, TRACE and documentation storage.
Explicit time zones use exact Gregorian arithmetic even beyond JavaScript Date
limits; the default local zone uses the host Date implementation. The first
[environment measurement](environment-progress.json) passes 142/210; the
[TRACE/documentation snapshot](environment-trace-results.json) passes 175/210.
Both complete the category, and both precede the subsequent SETF function binding
and single-valued DOCUMENTATION corrections. These are selected results, not
a full-suite conformance percentage.

The [environment result after function-binding corrections](environment-results.json)
completes all 210 tests with 201 passes. The same snapshot passes all 33 harness
groups, including 289 runtime regressions and the five original ATOM tests.
It adds exact numeric interval SUBTYPEP for integer, nonintegral rational and
binary64 domains, including intersections, unions, finite float gaps and signed
zero singleton types. This snapshot precedes the ADJUST-ARRAY corrections.

The [numeric interval/sort measurement](numeric-type-results.json) completes
SORT and STABLE-SORT (34/34 each), float subtyping (79/79), real subtyping
(25/25), rational subtyping (18/18) and integer subtyping (78/78). The same
report includes class precedence tests (0/71); their upstream helper stopped
defining methods at the missing LOGICAL-PATHNAME class. That standard class
is added in the following snapshot.

The [macro/shared-slot full attempt](full-macro-slot-progress.json) records
17,923 passes and 2,654 failures. STRUCTURES and PRINTER reached their
3,600-second limits, so this is incomplete and has no full conformance rate.
It predates parameter, method-combination, environment, numeric interval and
array adjustment work.

The [array adjustment measurement](array-results.json) completes all 1,356
ARRAYS tests with 1,355 passes. ADJUST-ARRAY now preserves identity and updates
storage and displacement, retains fill pointers appropriately, and respects
replacement initial contents. Vector extension and displaced bit operations
are covered by eight regressions that all failed on the preceding snapshot.
The remaining upstream failure concerns the upgraded NIL element type.

The latest full harness run passes all 42 groups: 37 integration groups, including
317 focused runtime cases and the five original ATOM tests, plus five tests of
the full-run conformance gate. The [control/class selection](control-class-results.json)
completes PROGV (18/18), class precedence (71/71), and CATCH (15/16). The remaining
CATCH failure is the condition type signaled for an unmatched THROW tag. Standard
stream classes now support generic dispatch; SIMPLE-TYPE-ERROR and
PRINT-NOT-READABLE provide their standard inheritance and readers.

The [class-change full attempt](full-class-change-progress.json) includes WITH-SLOTS, WITH-ACCESSORS,
CHANGE-CLASS and UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, and fixes its runtime
bundle at SHA-256
`d0cc927b087c748ee3cd715ae07aa0fd0c4d83a720c7b634517f9c69bdfc1dcb`
and used eight workers with a 7,200-second limit per category. It finished 23
categories. PRINTER was explicitly terminated after the debugger identified
quadratic circularity scanning while formatting a large failure result; the
report retains its unexecuted cases and incomplete status. It does not establish
the full-suite 90% target.

Failure diagnostics now bind `*PRINT-LENGTH*` to 10 and `*PRINT-LEVEL*` to 5,
and disable readable, pretty and circular printing after outcome comparison.
This bounds collection display and avoids scanning an entire failure graph.
Test bodies, the original comparator, names, counts and failure outcomes are
unchanged. A fixture verifies that large and circular failed values, including
a circular condition datum, remain failures and allow subsequent tests to run;
the printer controls are restored afterwards.

The latest complete full run fixes its bundle at SHA-256
`17e2da5d293126ee76d15768d985bf64d549c68f94724e0c28d72c8ffd624723`.
It also parses dispatch infixes in decimal independently of `*READ-BASE*`
and uses the common radix reader for binary signs and ratios. Two new runtime
regressions fail on the prior snapshot and pass after these fixes, including
210 numeric round trips across bases 2 through 36. All 42 harness groups pass
without failures or skips. The SACLA rerun completes all 27 files with
9,824/10,017 passes (98.07%) and passes its independent gate. The full upstream
ANSI run completes with 20,062 passes, 1,875 failures and no unexecuted tests,
and its independent coverage and 90% gate passes.

The [method-combination snapshot](full-method-combination-progress.json)
finished all 24 categories with 19,218 passes and 2,719 failures. It remains
incomplete because the old reader protocol folded mixed-case test names and
the registration audit did not yet pair implementation-dependent character
names. Its historical result is retained with a null pass rate. The corrected
reference generator reproduces the original 21,944-name inventory exactly;
neither that inventory nor the upstream test bodies were changed.

The [class-change and registration selection](class-change-registration-results.json)
completes WITH-SLOTS (21/21), WITH-ACCESSORS (15/15), CHANGE-CLASS (36/40),
and UPDATE-INSTANCE-FOR-DIFFERENT-CLASS (8/8). The four remaining class-change
failures involve EQL specializers on class objects. SET-SYNTAX-FROM-CHAR
also completes its registration audit with all 101 tests executed and all 101
failures retained; 82 character-name aliases are recorded without changing the
count or outcomes. This selection is not a full-suite conformance percentage.
