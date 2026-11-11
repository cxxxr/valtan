# Quicklisp / ASDF libraries

Valtan can resolve installed ASDF systems, including Quicklisp libraries, from
`:depends-on`. It uses ASDF to order Lisp sources, including transitive system
dependencies, secondary systems, nested modules, `:serial`, `:pathname`, version
constraints and feature conditions. It does not download packages during a build.

## Using a library

Install the sources in the host Lisp first, for example:

```lisp
(ql:quickload '("alexandria" "split-sequence" "cl-json"))
```

Then use a Valtan project definition, `my-app.system`:

```lisp
(defsystem "my-app"
  :depends-on ("alexandria" "split-sequence" "cl-json")
  :compile-mode :runtime
  :target :node
  :components ((:file "main")))
```

`main.lisp` can call the libraries normally:

```lisp
(format t "~S~%" (split-sequence:split-sequence #\, "a,b,c"))
(format t "~A~%" (json:encode-json-to-string (coerce (alexandria:iota 4) 'vector)))
```

Build with `valtan build my-app.system` and bundle the generated entry point as
usual. The existing Node project skeleton provides the bundler configuration.
ASDF source registries and Quicklisp bundles are also usable; Quicklisp need not
be called from the JavaScript application.

## CL-PPCRE

Use the installed upstream CL-PPCRE sources with `:compile-mode :runtime`:

```lisp
(defsystem "regex-app"
  :depends-on ("cl-ppcre")
  :compile-mode :runtime
  :target :node
  :components ((:file "main")))
```

```lisp
(cl-ppcre:split "[,;]+" "東京,大阪;;京都")
;; => ("東京" "大阪" "京都")
(cl-ppcre:regex-replace-all "[0-9]+" "item-42 / item-7" "#")
;; => "item-# / item-#"
```

A runnable example, including scanner reuse and captures, is in
[`example/try-using-cl-ppcre`](../example/try-using-cl-ppcre). Install CL-PPCRE
with `(ql:quickload "cl-ppcre")`, build Valtan, then run `npm install` and
`npm start` in that directory. Runtime mode compiles the embedded Lisp at startup;
reuse `create-scanner` results when applying the same pattern repeatedly.

The compatibility fixes preserve LOOP variable-clause order and parallel `AND`
updates, give closures fresh lexical bindings on each iteration, and implement
`LOAD-TIME-VALUE` in both compilation modes. No CL-PPCRE source changes are needed.

Run its upstream test suites with:

```sh
cd tests
npm run test:cl-ppcre
# An explicit installed source tree is also supported:
npm run test:cl-ppcre -- --source /path/to/cl-ppcre --output /tmp/ppcre.json
```

This runs the original `simple`, `perltestdata`, and optimized character-test
suites, plus the example assertions. The test harness provides only the Latin-1
input operations needed to read the upstream test data. This test-only adapter
predates the FLEXI-STREAMS support described below. The library, test assertions, character ranges,
and upstream skip list are unchanged. The report records source, compiler,
harness and bundle hashes; incomplete or failed runs cannot report success.
The runner allows up to 30 minutes for the upstream tests.

CL-PPCRE 2.1.2 (`cl-ppcre-20250622-git`) passed all 80 simple API tests,
1,624 Perl cases (the upstream suite skips 5 of its 1,629 cases), and all five
optimized-character-test cases over the full character range. These tests cover
matching, captures, backreferences, lookarounds, replacements, splitting, scanner
options and character-class representations. See
[`ppcre-results.json`](../tests/quicklisp/ppcre-results.json) for the recorded run.

## Portable runtime libraries

Use `:compile-mode :runtime` with the ordinary system names:

```lisp
(defsystem "portable-app"
  :depends-on ("babel" "flexi-streams" "uiop")
  :compile-mode :runtime
  :target :node
  :components ((:file "main")))
```

Valtan provides target adapters for `trivial-gray-streams`, `trivial-features`
and UIOP. These definitions are selected only in the target ASDF registry;
the build host keeps its native libraries. Babel and FLEXI-STREAMS are loaded
from the installed upstream sources without patches.

* Gray Streams supports custom character and binary streams, the CL stream
  predicates and lifecycle, buffered sequence methods, positioning, nonblocking
  reads, formatting and composite streams. The implementation package is
  `valtan-gray` (nickname `gray`); portable libraries use `trivial-gray-streams`.
* `trivial-features` detects the actual target's endianness and, on Node, its OS,
  architecture and word size. Browsers advertise endianness without inventing
  OS/CPU features. The target never pretends to be SBCL.
* Babel uses Valtan's Unicode characters and mutable character arrays, including
  supplementary characters. Character array element types resolve `deftype`
  aliases, as required by FLEXI-STREAMS. Octet arrays retain their specialized
  element type so bivalent streams can distinguish binary and character reads.
* UIOP includes its upstream package, utility, version, OS, pathname, filesystem
  and stream modules, plus the subprocess I/O helpers. These cover strings/lists, package reexports, version
  comparison, environment variables, pathname manipulation, file and stream
  copying, temporary/staging files and directory operations. Pure utilities
  work in browsers; OS and filesystem operations require Node. Synchronous
  `RUN-PROGRAM` uses Node's `spawnSync`, preserving upstream stream processors,
  temporary files and `SUBPROCESS-ERROR` handling. Command lists execute directly
  by default; shell strings and explicitly forced shell execution are supported.
  Browsers signal an unsupported-operation error when subprocess execution is
  requested, while retaining the portable stream and quoting helpers.

The UIOP adapter vendors UIOP 3.3.7.4, commit
`29d098f9121c804af0d525f0f64318bc069591db`, with its MIT license and a separate
Valtan backend under `library/portability/uiop`. It does not provide runtime
ASDF, image dumping, Lisp build orchestration, asynchronous `LAUNCH-PROGRAM`, or UIOP
configuration discovery. Add `"uiop"` explicitly to the application's dependencies
before an upstream library that uses it without declaring it, for example
`:depends-on ("uiop" "cl-colors2")`.

Runtime loading compiles the embedded source at startup. Encoding libraries
contain large tables and generated functions, so their startup can take several
minutes; these adapters do not provide a compiled dependency cache.

### McCLIM portability work

McCLIM GUI support is still in development. The target registry also provides
`trivial-garbage` and a Valtan `closer-mop` adapter. The latter supports custom
metaclasses, direct/effective slot definition classes, inheritance finalization,
forward references, slot access protocols and generic-function subclasses with
custom applicable-method selection. It is not a claim of complete MOP coverage.
`FIND-CLASS` also permits a non-symbol probe with `errorp=nil`, matching SBCL's
behavior for the class objects passed by McCLIM's presentation-type queries.

The garbage adapter provides weak pointers, finalizers and EQ/EQL weak hash
tables. Weak tables participate in ordinary CL hash table operations; their
iteration index does not strongly retain the keys. Other weak hash table tests
are unsupported unless the caller explicitly permits a strong-table fallback
with `:weakness-matters nil`. Explicit GC requests are advisory and only call
the host GC when it is exposed.

Run the offline protocol checks with `npm run test:weak` and `npm run test:mop`
from `tests/`. The weak-table checks exercise actual GC in Node, including
value-to-key cycles. The MOP checks exercise custom slot storage, class
redefinition and generic dispatch through both class and argument protocols.

With McCLIM's upstream sources installed, the following diagnoses the real
system and runs dynamic-slot and geometry assertions:

```sh
node tests/mcclim/probe.mjs clim-core/system /tmp/mcclim-system.json tests/mcclim/system-checks.lisp
```

The probe retains its bundle, logs and report. Passing this gate does not prove
that a McCLIM window renders or receives input; GUI verification remains a
separate requirement. The core now supports package-local nicknames and the
WHILE/UNTIL-before-FOR LOOP extension used by McCLIM's triangulation code.
Long-form DEFSETF also supports zero or multiple store variables, as used by
McCLIM's coordinate setters, and literal keyword arguments in its lambda list.
Source evaluation processes MACROLET,
SYMBOL-MACROLET and LOCALLY bodies in order, retaining their local environments
while making earlier definitions visible to later macro expansions.

The target registry includes `net.didierverna.asdf-flv`, with file-local dynamic
bindings around embedded ASDF source components. This permits the upstream
FiveAM test framework to load without loading host ASDF into the target.
`npm run test:fiveam` runs FiveAM 1.4.3's own upstream suite and explicitly checks
that test bodies execute. Its 57 upstream results pass; see the
[recorded run](../tests/quicklisp/fiveam-results.json). The compiler now preserves
closures and other single-valued expressions in MULTIPLE-VALUE-CALL; previously
FiveAM's runner could silently execute no tests.

An experimental Canvas backend lives in `library/mcclim-browser`, with a
counter-frame example in `example/try-using-mcclim`. It runs Lisp in a worker;
shared-memory input and transferred bitmap snapshots allow synchronous McCLIM
event waits while the page continues handling input. The server must provide
`Cross-Origin-Opener-Policy: same-origin` and
`Cross-Origin-Embedder-Policy: require-corp`. This implementation requires
SharedArrayBuffer, OffscreenCanvas and a bitmap renderer.

The example selects McCLIM's upstream `:clim-without-swank` option using
`:target-features (:clim-without-swank)` in its application `.system` definition.
Application target features are added when planning ASDF dependencies and when
reading embedded Lisp sources. This excludes Drei's optional Swank integration;
it does not substitute a dummy Swank server or advertise another Lisp implementation.

`npm run test:mcclim:transport` (from `tests/`) verifies the browser transport
in Chrome, including drawing during a blocked worker wait, incremental pixels,
pointer input and keyboard input. It also checks mirror positioning, nested
clipping, stacking, titles, visibility, minimize/restore, captured title dragging
and recursive destruction. Raising a mirror preserves keyboard focus. These
transport checks [pass in Chrome](../tests/mcclim/window-transport-results.json)
but do **not** load McCLIM.
The expanded transport gate also verifies explicit pointer grabs, outside clicks,
focus restoration and native IME composition. Canvas cannot start a browser IME,
so each mirror has an editable input that forwards committed Unicode characters
to CLIM. Japanese composition, cancellation, supplementary characters and ordinary
keyboard gestures pass without duplicate insertion. All six transport tests pass;
see the [keyboard and pointer report](../tests/mcclim/keyboard-transport-results.json).
These transport results do not establish text-editor gadget compatibility; the
separate real McCLIM interaction checks are described below.
`npm run test:mcclim:gui` builds and tests the actual McCLIM counter frame,
checks the standard button and keyboard handler, and retains screenshots and
diagnostics alongside a JSON report. Set `CHROME_BINARY` if Chrome is installed
elsewhere. A failed or running GUI report is not evidence of GUI compatibility.
The real counter frame passed this GUI gate in Chrome on 2026-11-10: its
600-by-280 canvas rendered text and geometry, the standard push button changed
the count from 0 to 1, and the `+` key changed it to 2 with different rendered
pixels. See the [recorded report](../tests/mcclim/gui-results.json) and
[screen after input](../tests/mcclim/screenshots/after-input.png). The report also
records the equivalence check against the later compiler build, whose emitted
JavaScript differs only in temporary source pathnames.

The backend supports indexed patterns, packed ARGB images, repeated tiles and
transformed patterns. Clipping includes polygons and ellipses, intersections,
unions and complements. Nested Boolean regions use device-space coverage masks
and composite the drawing once, preserving semitransparent ink in overlapping
members. The [40 pixel assertions](../tests/mcclim/canvas-checks.lisp) passed in
the persistent real McCLIM browser worker after reloading the updated backend;
the [report](../tests/mcclim/canvas-live-results.json) records the original bundle
and source reloads. All 40 also pass after a fresh McCLIM startup, without backend
reloads; see the [cold-start report](../tests/mcclim/canvas-cold-results.json).
`npm run test:mcclim:canvas` provides a separate cold-start
gate and retains its source snapshot and evaluation report.

`npm run test:mcclim:windows` checks actual McCLIM frame geometry and lifecycle
against browser state, including title dragging, minimize/restore and subsequent
gadget input. It creates a second frame for programmatic operations, without
claiming simultaneous frame event loops. All nine stages pass after a fresh
startup, including title dragging and restoration through the real event loop;
see the [cold window report](../tests/mcclim/window-cold-results.json).
The window gate now also exercises popup selection/cancellation and viewport
resizing. The browser graft uses identity coordinates and leaves its background
painting to the DOM, allowing region changes without the missing-transformation
error. All 14 stages of the expanded gate pass after a fresh startup; see the
[window/menu report](../tests/mcclim/window-menu-cold-results.json) and
[popup screenshot](../tests/mcclim/screenshots/menu-cold.png). That source snapshot
predates the later native IME, flipping and input-queue changes.
Focused checks in an existing real McCLIM worker reproduce and fix a minimize
bug: the sheet remained enabled while its browser window was hidden, so
`ENABLE-FRAME` could skip restoring it. The adapter now disables the sheet when
minimizing. Restore, subsequent button input, requested geometry, renaming,
visibility and destruction [pass after reloading the port](../tests/mcclim/window-live-results.json).

The browser system also loads `mcclim-portability`, which implements McCLIM's
environment lookup using Valtan's host API. Node reads its process environment;
browsers return NIL for absent variables. The
[focused checks](../tests/mcclim/environment-results.json) cover Unicode and
missing variables on Node and the absent-variable case in real browser McCLIM.
Non-browser extensions can list `"mcclim-portability"` before their upstream
dependencies. This addresses the `GET-ENVIRONMENT-VARIABLE not implemented`
failure while loading the PostScript dependency of `mcclim-bezier/core`.
`npm run test:mcclim:bezier` loads that extension with the portability adapter,
UIOP and the upstream pure-Lisp compression option, and exercises its
polygonalization smoke assertions. This fresh Node run now
[passes](../tests/mcclim/bezier-results.json), including the ordinary polygon and
degenerate tiny-area cases. It does not verify Bezier drawing in the browser.

Spatial `COMPOSE-IN`, `COMPOSE-OUT` and `COMPOSE-OVER` designs now render through
separate Canvas layers. Masks can contain regions or patterns, and composition
can be nested or explicitly transformed. Applying an OUT mask does not erase
previous drawing. Pattern and composite inks stay anchored to the drawing plane
when geometry or text moves. The expanded gate has 69 checks, including text-mask
coverage, pattern phase, alpha and clipping; all pass after reloading the backend
in the existing Chrome worker. See the
[composite-design report](../tests/mcclim/canvas-composite-results.json).
All 69 also pass after a fresh McCLIM startup, without backend reloads; the
[cold-start report](../tests/mcclim/canvas-cold-results.json) records the source
hash, bundle and completed evaluation.

Solid flipping inks exchange their two designs and restore the original pixels
when the same geometry is drawn twice, including antialiased text and circles.
This uses an opaque-pixel XOR operation with binary shape coverage; partially
transparent destination pixels and nonsolid design samples are left unchanged.
The real CLIM gate checks foreground/background inks, explicit colors, tiled
designs and Boolean clipping. Its 12 new checks and all 69 existing Canvas checks
[pass after backend reloads](../tests/mcclim/flipping-live-results.json).
`npm run test:mcclim:flipping` runs all 81 checks from a fresh startup; all 81 now
[pass in that gate](../tests/mcclim/flipping-cold-results.json). That snapshot
predates the subsequent queue-order and editing-key fixes. The separate host/transport gate also
[passes all seven tests](../tests/mcclim/input-flipping-transport-results.json).

`npm run test:mcclim:gadgets` exercises upstream text fields, toggles and sliders
through Chrome input in a nested frame event loop. The browser queue drains
already-dispatched events before polling more input, so a key immediately after
a click cannot overtake the click's focus notification. Keyboard conversion
supplies the characters needed by Backspace, Delete, Tab and Control/Meta
character gestures while preserving named Escape gestures.
After reloading these fixes in an existing worker, native Japanese IME input,
composition cancellation, supplementary characters, cursor motion, deletion,
activation, toggle/slider callbacks, frame exit and subsequent popup Escape
[pass](../tests/mcclim/gadget-live-results.json); see the
[gadget screenshot](../tests/mcclim/screenshots/gadgets-live.png).
The report retains the reproduced failures and the final successful checks.
The earlier seven-stage gadget gate also
[passes after a fresh startup](../tests/mcclim/gadget-cold-results.json).
That snapshot predates the Escape mapping correction and the added batched-focus
and post-exit popup checks; the expanded nine-stage fresh-start gate is not yet
verified. These results do not establish simultaneous frame event loops or full
Drei editor compatibility.

The browser backend reads static PNG files with the upstream pure-Lisp
`png-read` decoder. Install `png-read` alongside McCLIM in the host Quicklisp;
the backend's dependencies then include it in the target bundle. Ordinary
`read-bitmap-file` and `make-pattern-from-bitmap-file` calls accept `:format :png`.
The default `:bitmap` format also selects a registered reader from the filename
suffix, including uppercase `.PNG`, when another backend has not installed its
own fallback. Declare application images as ASDF `:static-file` components to
embed them for browser file access.

PNG samples are converted to CLIM's packed ARGB arrays, with axis conversion,
low-bit grayscale scaling, 16-to-8-bit quantization and alpha preservation.
The adapter also preserves palette indices across Adam7 passes: PNG-READ 0.3.1
otherwise retains only the final pass's indices and fails while expanding tRNS
transparency. This upstream failure reproduces on SBCL. The correction is scoped
to reads through this backend and distinguishes equal palette colors with
different alpha.
Thirty independently encoded images cover the supported static color/depth
combinations, transparent colors, filters and interlacing. Chrome's native decoder
validates their expected pixels. The real McCLIM file APIs, default format
selection, Canvas pixels and malformed-file checks then
[pass 92 groups in the existing worker](../tests/mcclim/png-live-results.json).
`npm run test:mcclim:png` runs the fresh-start gate and removes the original image
files before the Lisp checks, testing ASDF-embedded resources; that gate remains
pending. This does not establish full PNG conformance, APNG playback, image
writing or other bitmap formats.

This is an experimental backend, not full McCLIM compatibility. Arbitrary
nested/nonsolid flipping designs, other bitmap formats, platform window management and the complete
upstream test suite are not yet verified. Startup still compiles the dependencies in the browser worker;
the earlier counter-only end-to-end run took about 22 minutes, including build
and input checks, before adding the PNG dependencies. There is currently no
persistent compiled-library cache in the browser.
`npm run demo:mcclim` builds the example into a temporary directory and serves
it at `http://127.0.0.1:8080/` with the required headers. Set `PORT` to choose
another port. The demo and GUI test snapshot their browser assets and retain
their build logs, so editing the repository does not change a running test.

A [diagnostic run of 17 upstream suites](../tests/mcclim/upstream-subsets-results.json)
recorded 1,617 passing checks out of 1,618 after the compiler and queue corrections.
It used the upstream default frame instead of the active demo frame, with source
reloads recorded in the report. Pathname access still fails in that older browser
bundle, which predates embedded resource support. The separate older run's
[19 algebra results](../tests/mcclim/algebra-results.json) all pass; its compiler
and incomplete test-source loading are recorded separately, so these results
are not added to the newer selection. Thread-dependent and Bezier tests are not
included in that selection. This is not
a passing full-suite report. The `FIND-CLASS` change separately passes all 36
CLOS regression checks and the MOP protocol fixture in fresh target processes.

Two failures from the earlier run now pass focused checks in the existing browser worker:
the presentation inheritance compilation test and `STREAM-READ-GESTURE.SMOKE`
each execute all 16 assertions. `COMPILE` records warning/failure return values
and recovers from compiler errors by returning a function that signals the
original condition when called. The browser backend also handles standalone
input queues without attempting to poll a nonexistent port. Browser port queues
now give already-dispatched events priority over native polling; other ports
and concurrent queues retain upstream processing. Scheduled events, timeouts,
wait predicates and queue ordering are checked separately. Button and keyboard
input still work after loading the queue adapter. The
[focused report](../tests/mcclim/recovery-results.json) records these source
reloads and the separate successful run of all 50 compiler regression groups.
`npm run test:mcclim:queues` provides a fresh-process queue gate with upstream
McCLIM sources; that gate also [passes](../tests/mcclim/queue-results.json).
These focused results do not replace the full-suite diagnostic.

`npm run test:portability` runs Babel's default upstream tests using a small
Stefil assertion/registration adapter, and all nine FLEXI-STREAMS upstream suites.
Babel's ASDF call is used only to locate its test data. The runner preserves
upstream assertions, Unicode ranges and feature-controlled exclusions, records
source and bundle hashes, independently checks Babel registration with the host
reader, and reports coverage separately from test outcomes. Any failure still
causes a nonzero exit status. It requires the
installed Quicklisp sources. Babel allows up to 45 minutes. FLEXI-STREAMS file
comparisons are partitioned across at most 16 Node workers by default; every
original combination and assertion remains covered, with a two-hour limit per
worker. `npm run test:portability -- --jobs 4` reduces concurrency. The report
checks the complete comparison plan against an independent SBCL-generated plan.
You can run either phase with `--suite babel` or `--suite flexi`.
Offline Node/browser UIOP and feature tests are included in `npm run test:quicklisp:fixtures`.
The browser bundle executes in a VM without Node APIs; this is not a real-browser
integration test. The [subprocess checks](../tests/quicklisp/subprocess-results.json)
pass in both environments: Node exercises actual commands, literal and shell
arguments, Unicode input/output, stream processors, redirection, append/replace,
working directories and unsuccessful exits; the browser checks portable helpers
and explicit rejection of subprocess execution. All 50 compiler regression
groups also pass after adding the Node subprocess initialization.
For a diagnostic host baseline using the same installed upstream sources, run
`sbcl --script tests/quicklisp/portability-host-reference.lisp` from the project
root. Host results are not evidence of target compatibility.

Babel's default upstream run executes all **53 registered tests**: **51 pass**
and **2 fail** (`RW-EQUIV.1` and `ENCODER-AND-DECODER-RETURN-VALUES`). The UTF-8,
UTF-16 and UTF-32 round trips over the full Unicode range pass. Both failing
tests also fail with the installed Babel on SBCL; their target failures enter
the upstream KSC_5601 encoder's undefined `HANDLE-ERROR` path for unmapped ASCII.
These failures are retained, so the upstream runner exits unsuccessfully even
when coverage is complete. See the [target report](../tests/quicklisp/portability-results.json)
and [diagnostic host baseline](../tests/quicklisp/portability-host-reference.json).

FLEXI-STREAMS passes all **nine upstream suites**, including **3,672 file
comparisons** (459 scenarios, two copy implementations and four direction pairs).
The string, sequence, error-handling, unread-character, column, external-format,
peek-byte and in-memory-stream suites also pass in full. The recorded run uses
the same compiler build as the Babel and library-matrix reports. See the
[FLEXI-STREAMS report](../tests/quicklisp/flexi-results.json).

## Compilation modes

The default `:compile-mode :ahead-of-time` keeps the existing build behavior.
Project `.system` definitions take precedence, followed by bundled Valtan ports,
then installed ASDF systems. The host compiler has limitations for libraries
whose macros, reader extensions and read-time expressions need definitions from
previously loaded source files.

`:compile-mode :runtime` embeds application and dependency sources in JavaScript
modules. Valtan's own core is compiled ahead of time; the application sources are
read, compiled to JavaScript and executed by Valtan when the bundle starts. This
keeps macro expansion, read-time evaluation, custom readtables and load-time
initialization in the target Lisp. It follows source `LOAD` semantics, rather
than running ASDF's host `COMPILE-OP`/`LOAD-OP` actions. Shared source modules run
once, and a load error aborts initialization. In this mode, installed ASDF sources
are preferred over older bundled `.system` ports. The portability adapters above
are selected in the isolated ASDF registry; explicit project `.system` definitions
still win. Use this mode when compatibility matters more than startup time.

Runtime compilation requires JavaScript dynamic evaluation, so browser policies
that prohibit `eval` will need ahead-of-time compilation. The current smoke tests
run on Node.js; they do not certify browser compatibility. Lisp sources and
active ASDF `:static-file` components are embedded. Static files are read-only
resources available through ordinary CL file streams on Node and browsers,
including binary reads, seeking, `PROBE-FILE`, `TRUENAME` and `DIRECTORY`.
Embedded data takes precedence over a file at the same host pathname. Files not
declared as static components are not automatically included. `*load-pathname*`
and `*load-truename*` retain the original source pathname; this does not promise
that the Lisp source file itself exists at runtime. Module-level FFI imports
and exports should remain in JavaScript files when using runtime compilation.

Runtime bundles also expose read-only ASDF system/component metadata through
`FIND-SYSTEM`, `FIND-COMPONENT`, `SYSTEM-RELATIVE-PATHNAME` and the component
accessors. This lets libraries locate bundled data using their ordinary ASDF
names. It does not implement runtime ASDF build operations or dynamic downloads.
`npm run test:resources` tests text/binary streams, path lookup, feature-selected
resources and write protection after deleting the original data, on Node and
in real Chrome. The CL-DejaVu font library also passes its metadata and font-header
probe in a Chrome Worker without Node filesystem APIs; see the
[recorded run](../tests/quicklisp/dejavu-browser-results.json).
`npm run test:dejavu:browser` reproduces that probe with installed upstream
sources. `npm run test:zpb-ttf:browser` additionally parses the bundled DejaVu Sans
font with upstream ZPB-TTF and checks glyph metrics and contours, including a
composite glyph and a supplementary Unicode character. Its metrics match SBCL
using the same font; see the [browser parser report](../tests/quicklisp/zpb-ttf-browser-results.json).
This parser gate does not establish complete McCLIM font rendering.

## Target and host separation

ASDF definitions are read in an isolated registry with Valtan's target features,
plus the host ASDF version features. Host flags such as `:sbcl` are not advertised
to target libraries. A system previously loaded into SBCL is not proof of target
compatibility, and its host-feature component selection is not reused.

The bridge consumes ASDF's source dependency plan; it does not execute that plan
or load host FASLs into JavaScript. Definition-time code in `.asd` files still
runs on the host, as it does with ASDF. Custom build operations and custom Lisp
source component classes require adapters and produce errors. ASDF `:perform`
methods and compilation hooks are not replayed in the target. Libraries relying
on those hooks, generated sources, runtime ASDF, UIOP modules outside the
subset above, threads,
sockets, a MOP or native FFI may need additional ports. An ANSI test percentage
alone does not establish compatibility with those facilities.

## Recorded sample (2026-11-10)

With Node 24.21.0 and `:compile-mode :runtime`, **23 of 24 selected libraries**
passed their API smoke tests. The recorded run used `--jobs 4 --timeout 1200`.
The passing systems were:

`alexandria`, `split-sequence`, `anaphora`, `cl-ppcre`, `cl-base64`, `st-json`,
`cl-json`, `parse-number`, `iterate`, `cl-utilities`, `babel`, `yason`,
`trivial-gray-streams`, `md5`, `flexi-streams`, `let-plus`, `xsubseq`,
`cl-colors2`, `trivial-indent`, `chipz`, `salza2`, `trivial-features`, `uiop`.

Compared with the earlier 14/22 sample, the seven formerly blocked systems
`babel`, `yason`, `trivial-gray-streams`, `md5`, `flexi-streams`, `cl-colors2`
and `salza2` now pass. The sample also includes the two new adapter checks.
`bordeaux-threads` remains a failure: its system definition rejects Valtan as
an unsupported threading implementation. No threading support is claimed.

These results cover the checked API calls, not every feature in a passing library.
The default ahead-of-time mode separately passed the `split-sequence` and
`cl-base64` smoke cases. The loader checks, embedded-source fixtures and all 50
ANSI harness/regression groups passed after the changes. The upstream ANSI
iteration category completed with 839/843 passing; the four failures concern
counter values in FINALLY and are marked as specification issues by the upstream
suite. Valtan's values agree with SBCL for those cases. The full ANSI and SACLA
suites were not remeasured for this change.

## Verification

Rebuild the `valtan` executable after compiler/loader changes, then:

```sh
cd tests
npm install
npm run test:quicklisp:fixtures
npm run test:quicklisp
# Limit or increase independent compiler/Node workers (default: 2):
npm run test:quicklisp -- --jobs 4
# Allow more runtime startup time on a busy machine (default: 600 seconds):
npm run test:quicklisp -- --timeout 1200
# Select libraries, or compare the existing compilation mode:
npm run test:quicklisp -- split-sequence cl-base64 --output /tmp/selected.json
npm run test:quicklisp -- --ahead-of-time split-sequence --output /tmp/aot.json
```

The fixture tests require no downloaded libraries. They check ASDF ordering,
features, minimum versions, project overrides, cycles, nested modules, source
loading, macro closures, custom readers, source paths and failure propagation.

The library matrix uses installed upstream sources and the documented target
portability adapters without rewriting the installed distribution.
Each library gets a fresh compiler process and a fresh Node.js process. Tests
call real APIs and check results; these are small smoke tests, **not complete
upstream test suites**. Missing libraries, compile errors, bundle errors, runtime
errors and timeouts remain failures. Every selected library must pass for the
command to exit successfully. An interrupted report has `complete: false`.

The JSON report records the compilation mode, selected systems, test and compiler
hashes, bundle hashes, and hashes/counts of used sources grouped by Quicklisp
release directory. Full diagnostic logs are written next to the report in a
`*-logs` directory. A selected run reports only that selection and is not a
measurement of all libraries in Quicklisp. See
[`tests/quicklisp/results.json`](../tests/quicklisp/results.json) for the recorded
matrix, including unresolved failures.
