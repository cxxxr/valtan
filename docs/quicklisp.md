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

Standard generic functions cache accepted keywords by dispatch key, including
EQL specializers, and invalidate that cache when methods or classes change.
An `&rest`-only method no longer disables keyword checking, as specified by
[CLHS 7.6.5](https://www.lispworks.com/documentation/HyperSpec/Body/07_fe.htm).
Initialization generics explicitly declare `&allow-other-keys`.
The [validation record](../tests/performance/keyword-policy-results.json)
contains six new regression cases, 44 existing CLOS/class-change cases, MOP
checks, and live McCLIM input after reloading the definitions. The upstream ANSI
objects category retains its previous 829 successes and 34 failures. A focused
3,000-call comparison measured median times of 794 ms without the cache and
91 ms with it; this is not an end-to-end McCLIM speed measurement.
The same compiler also passes all 19 upstream McCLIM algebra results in a fresh
Node process, retaining the default 100 random trials and upstream assertions.

Internal CLOS slot searches now compare their symbol names directly, avoiding
repeated general sequence and key-function dispatch. They still read the current
storage and slot definitions on every access and use the existing MOP hooks.
The [slot lookup validation](../tests/performance/slot-lookup-results.json) retains
50 passing regressions in both evaluation and compiled modes, the custom MOP
fixture, and the same 829 passing / 34 failing upstream ANSI objects cases.
A focused Node/closer-mop benchmark measured 401 ms versus 107 ms warm medians
for 2,000 slot updates. The complete browser Drei performance suite with this
optimization is still running; that workload's timing is not yet established.

Generic dispatch passes only the required argument classes to the MOP's
`compute-applicable-methods-using-classes` protocol and dispatch caches. Optional,
rest and keyword arguments still reach the methods and EQL-dependent fallback
unchanged. The [argument-class checks](../tests/mcclim/required-classes-results.json)
cover optional/rest arguments, EQL fallback and zero-required-argument generics,
alongside the existing MOP and CLOS regressions.

Symbol calls with backtraces now invoke the current function directly, avoiding
a redundant JavaScript argument forwarding step. [Validation](../tests/performance/call-forwarder-results.json)
covers redefinition, multiple values, profiling and exception backtraces, plus
the compiler semantic regressions. An isolated CLOS benchmark measured warm
medians of 114 ms before and 101 ms after; full McCLIM timing is not established.

`VALUES-LIST` now validates and collects a proper list in one native traversal,
without forwarding through `APPLY`. It preserves zero values, object identity,
and a stable snapshot when the caller later mutates the list; improper and
circular lists still signal type errors. The
[validation record](../tests/performance/values-list-results.json) includes all
131 selected upstream multiple-value and nonlocal-control tests passing in
both evaluation and compiled modes, compiler semantic regressions, and 281
McCLIM drawing/PNG assertions after a retained-worker reload. A six-process
comparison measured warm medians of 5.6 ms before and 2.3 ms after for 30,000
four-value calls. This measures that operation, not total McCLIM startup time.

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
`npm run test:mcclim:windows:threaded` runs two real frame top levels and the port
I/O thread. Its [fresh browser run](../tests/mcclim/window-threaded-results.json)
passes all 17 stages, including independent key input and repainting, each
frame's dynamic application-frame binding, frame exit followed by thread join,
and popup selection/cancellation before and after viewport resizing. The driver
waits for browser pointer-grab notifications before querying Lisp, allowing menu
layout to proceed without repeated test interruptions. The report records the
frozen compiler and bundle hashes; this run includes native package lookup and
predates the subsequent file-compilation MACROLET correction.
The same driver also [passes all 14 ordinary stages](../tests/mcclim/window-ordinary-results.json)
with the MACROLET correction, without the cooperative-thread features.
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
degenerate tiny-area cases. The same API checks also
[pass in a fresh browser](../tests/mcclim/bezier-browser-results.json), including
the real extension dependencies; rerun with `npm run test:mcclim:bezier:browser`.
These API checks do not verify Bezier drawing.

Browser applications can opt into `mcclim-browser/bezier`, which depends on the
real `mcclim-bezier/core` system, including its Render, PostScript and PDF
dependencies. Add `:use-salza2-zlib` to the application's `:target-features`
alongside `:clim-without-swank` to select the portable PDF compression backend.
The optional adapter draws cubic curves and filled areas with Canvas paths.
Unions and differences use separate coverage masks so overlapping translucent
areas paint once, and holes preserve existing drawing. The ordinary browser
backend remains usable without these extension dependencies.
Bezier areas, unions and differences also provide coverage masks for clipping
and spatial inks, including ordinary region intersections and complements.
Curves have zero area as a mask while retaining their stroked drawing behavior.

`npm run test:mcclim:bezier:canvas` starts a fresh browser, runs the existing
86 Canvas checks, and adds 45 Bezier pixel checks for curves, dashes, areas,
opposite-winding unions, differences, opacity, transforms, clipping, patterned
and composite inks, flipping, and moved output-record replay. Another 40 pixel
checks cover Bezier clipping and spatial inks, for 284 checks in total including
the 53 pixmap-copy and 60 text checks below.
The earlier [157-check gate passes in a retained browser](../tests/mcclim/bezier-region-results.json)
after reloading `HOST-CALL` to establish the single JavaScript return value.
Without that boundary, CLOS method combination observed stale multiple values
instead of the empty curve's Canvas surface. The report retains the original
failure and the diagnostic run. The same command also
[passes all 157 checks after a fresh startup](../tests/mcclim/bezier-region-cold-results.json),
without production reloads. That bundle includes the ASDF component type support
and predates the later TAGBODY activation correction.
An earlier 114-check snapshot [passes](../tests/mcclim/bezier-canvas-results.json) in a fresh
browser containing the adapter and FiveAM, before the upstream suite runs,
without reloading production definitions. The first standalone attempt exposed
a fixture error: recording was still enabled when it selected the drawing
medium and replayed. The report preserves that failure and the corrected
fixture's successful evaluation. The corrected standalone command also
[passes all 114 checks after a fresh startup](../tests/mcclim/bezier-canvas-cold-results.json).
That snapshot includes the original 69 Canvas checks and 45 Bezier checks;
it predates the empty-region fix and Bezier region-mask extensions.

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

Stencil masks now use opaque black foreground and background, independently of
the medium's drawing designs, as required for `COMPOSE-IN` and `COMPOSE-OUT`.
Fourteen additional pixel checks cover a translucent foreground, indirect inks
in pattern palettes, nested masks and transformations. The combined command
`npm run test:mcclim:bezier:canvas -- --flipping` ran 183 Canvas, Bezier and
solid-flipping checks at that revision. They [pass after a fresh startup](../tests/mcclim/stencil-results.json).
Reloading the old renderer reproduces an incorrect alpha of 128 instead of 255;
restoring the correction passes all 183 again. This browser snapshot includes
the slot lookup optimization but predates the later required-argument-class and
JavaScript call-forwarding changes; the report records each validation scope.

Solid flipping inks exchange their two designs and restore the original pixels
when the same geometry is drawn twice, including antialiased text and circles.
This uses an opaque-pixel XOR operation with binary shape coverage; partially
transparent destination pixels and nonsolid design samples are left unchanged.
The real CLIM gate checks foreground/background inks, explicit colors, tiled
designs and Boolean clipping. Its 12 new checks and all 69 existing Canvas checks
[pass after backend reloads](../tests/mcclim/flipping-live-results.json).
`npm run test:mcclim:flipping` now runs 265 checks from a fresh startup. The earlier
81-check snapshot [passes in that gate](../tests/mcclim/flipping-cold-results.json). That snapshot
predates the subsequent queue-order and editing-key fixes. The separate host/transport gate also
[passes all seven tests](../tests/mcclim/input-flipping-transport-results.json).

Flipping operations now survive `COMPOSE-IN`, `COMPOSE-OUT`, `COMPOSE-OVER`,
transformed designs, indexed palettes and rectangular tiles. A separate operation
tree preserves access to the destination pixels; stencil evaluation uses the
source designs' opacity. Fifty-four new assertions cover ordering, masks,
pattern mutation and phase, transforms, clipping, and exact restoration after
repeated circle/text drawing. All 237 combined Canvas, Bezier and flipping
assertions [pass in the retained worker](../tests/mcclim/composite-flipping-results.json)
after loading the new host functions and Lisp backend. This is a backend-reload
result on the earlier runtime. A separate [fresh-start run passes all 237 checks](../tests/mcclim/composite-flipping-cold-results.json),
including the required-argument and JavaScript call-forwarding changes. That
snapshot predates PNG writing, the native VALUES-LIST optimization and the later
pixmap-copy fixes. Nonsolid flipping inputs and
fractional flipping coverage remain outside the verified scope.

Pixmap copies now replace pixel values, including transparency, and respect the
destination's clipping region and native coordinates. Copies within one medium
transform both positions before rebinding its transformation, so translated
scroll/copy operations reach the correct destination. Overlapping regions use a
source snapshot. Allocation reports the actual rounded surface dimensions, and
deallocation releases the backing pixels. The [pixmap report](../tests/mcclim/pixmap-results.json)
records 53 new checks and all 290 combined Canvas/Bezier/flipping checks passing
in the retained worker, plus a fresh host-module test of fractional clipping and
Canvas-state independence. The [fresh 290-check snapshot also passes](../tests/mcclim/pixmap-cold-results.json)
without production reloads. It includes PNG writing and the VALUES-LIST
optimization, and predates the multiline-text correction below.

Multiline text bounds now use the same font line advance as drawing, including
blank lines and reversed page directions. They also report the final cursor's
vertical displacement and accept a character argument. Previously, 20px fixed
text advanced 24px when drawn but only 14px when measured, which displaced
bottom- and center-aligned blocks. The [text report](../tests/mcclim/text-results.json)
records the failing bottom-alignment pixel comparison and 60 passing assertions
after loading the correction. These compare complete images against individually
positioned lines, including scaling and rotation, and check substring ranges,
blank lines, explicit styles and all four page directions. All 350 combined
Canvas/Bezier/flipping assertions pass in that retained worker. The combined
command now expects 350 checks (199 without Bezier or flipping). The [fresh
startup run also passes all 350](../tests/mcclim/text-cold-results.json), without
production reloads. That snapshot predates the binding-context changes.
This does not establish full McCLIM compatibility.

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
The expanded nine-stage gate now
[passes after a fresh startup](../tests/mcclim/gadget-cold-results.json), including
the batched click/key sequence and popup Escape after frame exit. The runner
waits for the popup's actual grab and keyboard focus before sending Escape;
mirror creation alone occurs before input readiness. The report retains the
earlier premature-input attempt and identifies the tested source snapshot.
These results do not establish simultaneous frame event loops or full Drei
editor compatibility.

`npm run test:mcclim:drei` exercises the actual Drei-backed `accept` operation
with native Japanese IME input, supplementary characters, cursor movement,
deletion, integer parsing and Tab completion. It also verifies that the nested
input frame closes and the original frame still handles pointer input.
Drei redisplay exposed rectangle sets containing zero-area bands. Their empty
components now contribute no clipping contour instead of causing an unsupported
mask error. The [existing-worker checks](../tests/mcclim/drei-live-results.json)
pass after reloading the two clipping helpers, including all 72 Canvas checks;
the [screenshot](../tests/mcclim/screenshots/drei-live.png) shows the edited text,
accepted integer and completed member. The same four stages also
[pass after a fresh startup](../tests/mcclim/drei-cold-results.json), without
reloading production definitions; see the
[fresh-start screenshot](../tests/mcclim/screenshots/drei-cold.png).
This run includes the empty-region fix and predates the ASDF metadata type changes.
This is a focused input-editing check, not full Drei coverage.

`npm run test:mcclim:drei:upstream` attempts every source in the separate
`drei-mcclim/test` ASDF plan and runs its default suite in the browser.
`npm run test:mcclim:drei:upstream:host` provides the native SBCL baseline.
Both retain skipped and unexecuted tests as nonpassing coverage. The installed
baseline has 615 registration entries for 596 unique test cases: 3,564 checks
pass, 20 checks skip under the default settings, and 90 separately registered
performance cases remain unexecuted. The diagnostics report both registration
counts and per-test result counts so duplicate FiveAM entries are visible.
The [initial browser diagnostic](../tests/mcclim/drei-upstream-before-results.json)
did not finish `FORM-TO-OBJECT-17`: recursive condition resignal exposed a
nonlocal GO activation bug. The compiler correction and its bounded regressions
are recorded in [the TAGBODY checks](../tests/ansi/tagbody-activation-results.json).
The [completed retained-browser diagnostic](../tests/mcclim/drei-upstream-results.json)
then exposed five lambda-list construction failures. `SUBTYPEP` now reasons about
class types inside `AND` and `OR`, preserves uncertainty for unresolved compound
types, and compares compound array element types structurally. Its
[regressions](../tests/ansi/boolean-class-subtype-results.json) pass in both evaluation
and compiled modes, alongside all 390 selected upstream subtype tests.
With the final definition reloaded, the complete default Drei suite returns
3,563 passing checks, zero failures, the same 20 skips and 90 unexecuted cases.
The one-check difference from SBCL is its extra `SB-INT:&MORE` lambda-list keyword;
all other per-test result counts match after accounting for Gray stream package
names. A [fresh-start run](../tests/mcclim/drei-upstream-cold-results.json) now
reproduces all per-test counts without manual runtime, compiler, or Drei reloads:
all 13 sources load, 3,563 checks pass, zero fail, 20 skip, and 90 remain
unexecuted. This snapshot includes the final subtype correction but predates the
CLOS slot lookup optimization. The retained report also includes native-only baselines for the
separate 90-case performance suite (all pass) and explicitly enabled
self-compilation (fails upstream on SBCL with an illegal function call while
re-evaluating a `FORM-TO-OBJECT` method). Those baseline results are native only.
A separate [fresh Valtan browser performance run](../tests/mcclim/drei-performance-cold-results.json)
now completes all 90 unchanged performance cases with 90 passing checks, no
failures and no skips. Its source and result name sets match. The report retains
its overall failure status because 506 other registered cases were not selected.
This snapshot includes the slot lookup optimization and predates the later
resumable compiler/runtime work.

`npm run test:mcclim:drei:performance` selects the complete, unchanged
`BUFFER-PERFORMANCE-TESTS` suite. `npm run test:mcclim:drei:self-compilation`
explicitly enables and selects `SELF-COMPILATION-TEST`; it bundles all 34 files
from Drei's ASDF component tree as read-only static resources at their original
component pathnames, including feature-disabled components that the upstream
test itself visits. Append `:host` to either command for the native baseline.
Both diagnostics still load all 13 test sources and retain other registered
cases as unexecuted coverage, so a passing selected suite alone does not produce
a full-coverage success status. The [native command checks and resource inventory](../tests/mcclim/drei-extra-host-results.json)
record 90 passing performance checks and the existing native self-compilation
failure. The [fresh browser self-compilation run](../tests/mcclim/drei-self-compilation-cold-results.json)
has now completed its one selected case successfully. The unchanged test
re-evaluates Drei and asserts the status of an inner default-suite run; that
inner run's individual result counts are not returned by this report. All 34
source resources match their recorded hashes and remain accessible through
Drei's ASDF component tree after the test. The report preserves its overall
failure status for the 90 performance cases outside the selected suite. This
snapshot predates the CLOS slot lookup optimization and later resumable work.

The older [retained browser performance run](../tests/mcclim/drei-performance-retained-results.json)
also finished all 90 selected cases successfully. Its source/result name set
matches the fresh optimized run above, but its recorded runtime reloads make it
a separate validation context. Both retained workers finished after their
observers expired; their actual terminal page results were collected without
restarting the tests.

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
files before the Lisp checks, testing ASDF-embedded resources. The
[fresh-start gate passes all 92 groups](../tests/mcclim/png-cold-results.json),
with all 30 independently generated variants also passing native Chrome decoding. This does not establish full PNG conformance, APNG playback, image
writing or other bitmap formats.

PNG writing now uses upstream ZPNG through `CLIMI::WRITE-BITMAP-FILE`, with
`:PNG` and default `:BITMAP` output. Indexed and transformed patterns are
collapsed using McCLIM's sampling rules; RGBA bytes preserve transparency and
hidden RGB values. Caller-owned binary streams stay open. Filesystem destinations
use the host's file I/O; browsers can write to binary memory streams.
[Forty-four checks pass after loading the writer into the retained browser](../tests/mcclim/png-write-results.json).
All eight output images also pass independent CRC/zlib checks, exact RGBA
comparison, and Chrome's native image decoder. SBCL passes the same checks plus
seven pathname, readback and file-replacement assertions. The fresh Valtan
browser run [exposed a missing test dependency](../tests/mcclim/png-write-cold-before-results.json):
FLEXI-STREAMS was absent from the test app. Both test commands now declare it
explicitly. The corrected [fresh browser run passes all 44 checks and eight
independent image validations](../tests/mcclim/png-write-cold-results.json), without
production reloads. It includes the VALUES-LIST and pixmap-copy changes and
predates the multiline-text correction. A [fresh Node run also passes all 51
stream and filesystem assertions](../tests/mcclim/png-write-node-results.json),
including pathname output, readback and replacement of an existing file.
`npm run test:mcclim:png:write` runs the browser gate;
`npm run test:mcclim:png:write:node` runs the stream and filesystem checks on Node.
This adds static 8-bit RGBA PNG output, not APNG or other output formats.

This is an experimental backend, not full McCLIM compatibility. Arbitrary
flipping inks used as inputs to another flipping ink, nonsolid flipping designs,
other bitmap formats and platform window management remain outside the verified
scope. The complete main upstream suite now passes on the recorded threaded
snapshot below; the combined Drei suite is a separate, pending gate.
Startup still compiles the dependencies in the browser worker;
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

For a native comparison with the installed McCLIM test system, run
`sbcl --script tests/mcclim/upstream-host.lisp` from the repository root.
The [SBCL baseline](../tests/mcclim/upstream-host-results.json) executes all
193 registered tests and passes 2,213 checks, with no skipped or unexecuted
tests. It loads McCLIM's normal native dependencies, including threading and
extensions. These host results do not establish Valtan compatibility.

The [complete threaded browser run](../tests/mcclim/upstream-threaded-complete-results.json)
passes all **2,245 assertions from 193 registered cases**, with zero failures,
skips or unexecuted cases. All 27 upstream test sources load successfully.
The [native coverage comparison](../tests/mcclim/upstream-threaded-complete-comparison.json)
passes the source hashes, exact test inventory and per-case assertion counts.
The only count differences are `STRESS.9` and `STRESS.0`: Valtan executes 33
checks in each, while SBCL's `FAKE-PROGV-KLUDGE` reader branch executes 17.
This accounts for all 32 additional target assertions; no other count is waived.

The run uses `:mcclim`, `:clim-mp` and `:valtan-resumable` in a fresh browser,
with Bezier and the unchanged threaded dynamic-variable cases included. Its
evaluation took about 4 hours 40 minutes after startup, without source reloads
or an observation deadline extension. The result records compiler SHA-256
`465b09137a7c584c31000f2fa624945e3984c9a6081a9bb61528fff42b42d72a`
(the `097be96` snapshot), the fixture, bundle and browser asset hashes. It
predates the later file-literal fixes and Drei backend source registration;
it is not a new full-suite run of those later changes. Drei self-compilation,
compiled-library restoration and GUI integration remain separately verified
scopes. Reproduce the full suite and comparison from the repository root:

```sh
node tests/mcclim/upstream.mjs --threaded /tmp/mcclim-full.json
node tests/mcclim/compare-upstream.mjs /tmp/mcclim-full.json tests/mcclim/upstream-host-results.json /tmp/mcclim-full-comparison.json
```

`npm run test:mcclim:upstream` attempts every test source in the installed host
ASDF plan in a real browser containing `mcclim-browser/bezier` and FiveAM. It records
source-load failures, skipped checks and unexecuted cases as failures. Its
source snapshots are also ASDF static resources, supplying the files expected
by the upstream pathname tests. Build, source and compiler hashes identify each
run. The ordinary driver allows an hour after startup; the threaded command
allows six hours for evaluation.
The [existing-worker diagnostic](../tests/mcclim/upstream-source-plan-results.json)
attempted all 27 files: 179 tests registered and executed, with 1,640 of 1,642
checks passing. Two pathname checks failed because their inline-loaded source
file was absent from that worker's filesystem. Two source files could not load
because the BT and MCCLIM-BEZIER packages were absent. The cold run with embedded
source resources [finished](../tests/mcclim/upstream-source-plan-cold-results.json):
all 1,642 checks from 179 registered tests pass, including the pathname checks.
The same two source-load errors remain, so the overall diagnostic still fails.
This run used the compiler before the keyword-policy optimization. Its original
driver hit a ten-minute observation timeout; the worker was retained unchanged
and its completed result collected separately. Both the observer failure and
the completed evaluation are preserved in the report. Neither result establishes
full McCLIM compatibility.

The [newer browser run with Bezier](../tests/mcclim/upstream-bezier-results.json)
uses the keyword-policy cache and loads the real Bezier dependencies. All
1,644 checks from 180 registered tests pass; the only remaining source-load
failure is `Tests/dynamic-vars.lisp`, which needs BT. The diagnostic therefore
still reports failure. The same fresh worker first passed the 114 Canvas/Bezier
checks; this preflight is recorded, and no production definitions were reloaded.
This snapshot predates the Bezier region-mask extensions and the empty-region
clipping fix.

The separate [scheduled dynamic-variable run](../tests/mcclim/dynamic-vars-results.json)
now passes all 244 checks from the 13 cases in the unchanged upstream
`Tests/dynamic-vars.lisp`, with no skips or unexecuted cases. It includes the
64-thread metaclass workload and both 4096-variable stress tests in a fresh
Chrome worker. Run `npm run test:mcclim:dynamic-vars` to reproduce it; the source
comes from the installed ASDF test plan, with its hash and harness hashes recorded.
The recorded bundle includes checkpoint batching and synchronous checkpoint
suppression, but predates the final nested-task checkpoint-boundary fix and
`MAKE-LOAD-FORM` support. This isolated result does not replace the older full-suite
report or establish threaded GUI coverage.

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

Runtime bundles also expose descriptive ASDF system/component metadata through
`FIND-SYSTEM`, `FIND-COMPONENT`, `SYSTEM-RELATIVE-PATHNAME` and the component
accessors. This lets libraries locate bundled data using their ordinary ASDF
names. It does not implement runtime ASDF build operations or dynamic downloads.
The metadata preserves the `COMPONENT`, `MODULE`, `SYSTEM` and `SOURCE-FILE`
type hierarchy, including systems being modules. `MODULE-COMPONENTS` supports
recursive source discovery and methods specialized on these types, as used by
Drei. More specific ASDF file classes and runtime build operations are not supplied.
Target adapters may append their own source components so a library rebuilding
itself from the component tree also restores its adapter. This does not change
the host ASDF registry or remove upstream components.
`npm run test:resources` tests text/binary streams, path lookup, feature-selected
resources, component traversal with generic methods, and write protection after
deleting the original data, on Node and
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

Features added by target ASDF definitions and `:target-features` declared by
dependency systems are preserved when compiling and embedding the application.
The [feature propagation checks](../tests/quicklisp/asdf-features-results.json)
cover definition-side additions, dependency declarations, source reader branches
and host isolation. The McCLIM adapter declares `:mcclim`, because the browser
loads lower-level CLIM systems without the root `mcclim.asd` feature announcement.
Older browser runs lacked that feature and omitted upstream `#+mcclim` assertions
inside otherwise registered tests. New upstream checks require the feature and
record the runtime feature list; the native reference now includes every test
name and per-test assertion counts. Earlier aggregate pass counts do not prove
coverage of those omitted assertions.

After a complete McCLIM run, compare it with the native reference using
`node tests/mcclim/compare-upstream.mjs TARGET.json tests/mcclim/upstream-host-results.json COMPARISON.json`.
The comparison requires matching source hashes, exact test names, successful
execution and per-test assertion counts. It records the two known
`FAKE-PROGV-KLUDGE` variants explicitly: 17 native assertions versus 33 target
assertions in each stress case. Other count differences fail the comparison.
`cd tests && npm run test:mcclim:reports` checks that omissions, duplicates,
source changes and unfinished runs cannot pass this comparison; those verifier
fixtures are not evidence that McCLIM itself has passed.

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
`bordeaux-threads` failed in that default-mode sample: its upstream system
definition rejects Valtan as an unsupported threading implementation. The
optional cooperative port described below is separate from this sample.

The runtime now tracks dynamic bindings as movable stacks. Switching an internal
binding context preserves its local values while exposing shared global values;
global changes made in another context survive restoration. Generated special
bindings and target `PROGV` use this mechanism. Initializers are protected by
cleanup as well as the body, fixing a pre-existing `LET*` leak when a later
initializer exits. This is groundwork for resumable execution, not a thread API.
See the [binding-context checks](../tests/performance/binding-context-results.json)
for the verified scope and binding-cost measurement. A [fresh McCLIM browser run](../tests/mcclim/bindings-cold-results.json)
also passes all 350 drawing assertions with the binding-context changes, without
production reloads; that snapshot predates resumable execution.

An optional generator-based compiler path now preserves nested Lisp calls across
suspension. The internal `compiler::*resumable-functions*` flag selects it for
newly compiled functions; ordinary compilation remains the default. Execution
contexts also retain multiple values and backtraces. Tasks expose bounded
checkpoint advancement at function entries and loop backedges, plus explicit
yield/resume and exceptional interruption. Synchronous JavaScript callers can
use these functions when they do not suspend; an attempted suspension raises an
error while running cleanup. [The resumable-execution checks](../tests/performance/resumable-results.json)
record 43 real Lisp assertions passing in Node and a fresh Chrome worker,
including recursion, shared mutable data, special parameters, nonlocal exits,
conditions, cleanup that suspends, function redefinition and multiple values.
The generator path also passes all 104 selected compiler/control regressions in
both evaluation and compiled modes. A [fresh McCLIM browser run](../tests/mcclim/resumable-kernel-cold-results.json)
also passes all 350 drawing checks using the ordinary compilation mode with
this runtime/compiler snapshot. It predates dual compilation and does not test
suspension within McCLIM.

A system can opt into dual compilation with
`:target-features (:valtan-resumable)`. The build compiles the core with both an
ordinary function body and a generator alternative; the target compiler defaults
to the same `:dual` mode for subsequently loaded libraries and `COMPILE` calls.
Ordinary calls use the direct body. Both implementations capture the same lexical
cells, and each source `LOAD-TIME-VALUE` occurrence is initialized only once.
Generator bodies retain direct JavaScript string literal generation, which is
needed to avoid recursive string conversion during bootstrap.

The flag may also be declared by a dependency. The build selects dual compilation
after collecting dependency features, so core functions such as `SLEEP` have
resumable bodies in both ahead-of-time and runtime compilation modes. The
[dependency feature checks](../tests/quicklisp/dependency-feature-mode.test.mjs)
build each mode, delete the input sources, and verify ordinary calls and scheduled
suspension in a fresh process. Ahead-of-time reader features do not imply that
the same flags are present in the application's runtime `*FEATURES*` list.

The [dual compilation checks](../tests/resumable/results.json) cover suspension
through core `APPLY`, `MAPCAR`, `MAP`, `MAPHASH`, `REMOVE-IF`, `PROGV`, generic
function dispatch, `:AROUND` methods and `CALL-NEXT-METHOD`, including cached
dispatch. They also cover shared closures and load-time object identity. All 43
existing suspension checks and 51 dual-core checks pass in Node and a fresh
Chrome worker. Chrome also passes four assertions that suspend through the real
Quicklisp Alexandria `MAPPEND` implementation. The 104 compiler/control
regressions pass in both evaluation and compiled modes under `:dual`. Run
`npm --prefix tests run build:resumable` followed by
`npm --prefix tests run test:resumable` to build and check the dual core.

A [fresh McCLIM dual-compilation run](../tests/mcclim/dual-cold-results.json)
also completes all 350 Canvas, pixmap, text, Bezier and flipping checks.
That frozen `ddc40da` build executes the drawing checks through ordinary calls
and predates the scheduler, Bordeaux Threads and threaded McCLIM integration.
It verifies loading and drawing with dual definitions; it does not verify
threaded application or I/O execution.
The ordinary core remains the default. Dual builds contain more generated code;
no end-to-end startup or scheduling performance improvement is claimed.

Native package enumeration now delegates to resumable callbacks and yields
checkpoints while scanning, including private symbols. `SYSTEM:MAP-FOREACH`
advances the native Map/weak-map iterator from Lisp, allowing callback suspension
while preserving live Map insertion/deletion behavior. It returns NIL after
normal completion. [The native callback checks](../tests/resumable/callback-results.json)
record 23 new Lisp assertions in Node and fresh Chrome, plus visibility,
shadowing, bounded traversal and interrupted cleanup checks in the kernel.

The internal `CooperativeScheduler` now automatically advances tasks in the same
JavaScript heap. It rotates checkpoint/explicit-yield slices, bounds each host
turn by slice count and elapsed time, and parks tasks on promises through
`SYSTEM::%RESUMABLE-AWAIT`. Promise rejection and cancellation are thrown through
the suspended stack, so cleanup can itself await. An obsolete wait cannot resume
a newer cleanup wait. Cancellation does not abort the underlying host promise.
The [scheduler checks](../tests/resumable/scheduler-results.json) verify 64 real
Lisp tasks sharing an array while retaining separate special bindings, two loops
interleaving without explicit yields, multiple values across asynchronous
cleanup, and host event-loop progress. The Lisp checks pass in Node and a fresh
Chrome worker; the kernel suite has 17 passing tests.

Scheduled `CL:EVAL` and `CL:LOAD` can now suspend through evaluated source and
opt-in compiled artifacts. The generated execution entry retains the original
top-level compiler environment; temporaries belong to each invocation, while
load-time values initialize once. Local macro expansion and definition order
survive waits. [The EVAL/LOAD checks](../tests/resumable/eval-results.json) pass
18 assertions in Node and 17 in a fresh Chrome worker, including source and
compiled-stream loading. A separate two-process Node test deletes the source
before loading its compiled artifact, suspends during loading, and resumes to
install the remaining definition. Browser file output remains unavailable, so
browser COMPILE-FILE is not covered by the passing compiled-stream checks.

Target `COMPILE-FILE` externalizes standard and structure objects through
`MAKE-LOAD-FORM`, including `MAKE-LOAD-FORM-SAVING-SLOTS`. A file-wide literal
pool preserves shared objects and aggregate substructure across source forms
and ordinary/scheduled function entries. Creation dependencies are ordered;
circular creation dependencies signal an error, while circular initialization
references are supported. Each `LOAD` owns a fresh pool, retained by the
functions it installs and isolated from nested loads. `EVAL` continues to use
the original live objects. The [two-process tests](../tests/resumable/file-literals.test.mjs)
delete the source before loading, check 25 graph/slot assertions per load,
verify one load-form call per object per compilation, and exercise suspension
in both creation and initialization forms. These compiler tests do not establish
that all McCLIM components can yet be precompiled and reloaded.
Dependency traversal uses an explicit work stack, so large package export lists
do not exhaust the JavaScript stack. The tests also restore an 8192-element list
shared with an object's slot, exercising both aggregate references and load-form
initialization dependencies.

Hash-table literals use the same object graph restoration. Their comparison
test, rehash settings and entries are preserved, including shared keys, circular
values and tables used as EQ keys. Composite keys are initialized before they
are inserted into EQUAL/EQUALP tables. The
[hash-table fixture](../tests/resumable/file-hash-literals.lisp) is compiled and
loaded in separate processes with its source deleted, in ordinary and scheduled
modes, and checks that reloading creates fresh tables. A
[targeted Babel compilation](../tests/mcclim/compiled-hash-table-results.json)
restores 312 prior artifacts, compiles and loads the ISO-2022-JP component that
previously failed on its codespace table, and proceeds to ISO-8859. The complete
fresh build and browser restoration still require their own gates. The browser
API check now also verifies exact ISO-2022-JP bytes and a Japanese text round-trip;
its expected bytes were checked with native Babel and pass in the incremental
browser replay below. The later fresh full-build checks are recorded below.

Nested `DEFCONSTANT` forms evaluate their initializers at execution time, so a
load form can define a constant using a lexical value. `EVAL` evaluates a
constant initializer once; top-level file compilation still recognizes constants
for later forms. The [constant-definition checks](../tests/resumable/defconstant-results.json)
pass 29 ordinary/scheduled assertions and all eight upstream ANSI cases in both
evaluation and compiled modes. The file-literal tests also exercise a nested
constant definition in a `MAKE-LOAD-FORM` creation form after restarting.

Function-valued constants remain symbol references during file compilation.
Their `DEFCONSTANT` initializer recreates the closure at load time, so structure
defaults and functions in dependent files use the same runtime function instead
of trying to externalize a live compiler closure. The two-process file-literal
tests cover lexical capture, `DEFSTRUCT` defaults, cross-file references and
`EQ` identity in ordinary and scheduled execution after deleting both sources.
This fixes the compilation error discovered in Drei's `drei-redisplay.lisp`
(component 272 of the full staging run). A
[targeted replay](../tests/mcclim/compiled-function-constant-results.json) loads
the preceding 271 artifacts and verifies that the corrected compiler compiles
and loads the actual `drei-redisplay.lisp`, then proceeds to `drawing-options.lisp`.
Those prerequisites were compiled before the fix; the fresh full run and browser
restoration checks below provide separate evidence without that older prefix.

Top-level `DEFTYPE` definitions are available during compilation, allowing later
macros in the same file to use their derived types. McCLIM's pointer-tracking
macro exposed this gap by rejecting a valid clause keyword during staging. Nested
definitions still capture their lexical environment and take effect when
executed. The [type-definition checks](../tests/resumable/deftype-results.json)
exercise ordinary and scheduled compilation and restore the definitions from
an artifact after deleting the source. Full McCLIM staging is verified separately.

File compilation also preserves top-level processing through `MACROLET` bodies.
Compile-time and load-time `EVAL-WHEN` bodies retain the surrounding local macro
and symbol-macro bindings. This fixes the missing compile-time presentation type
definitions encountered in McCLIM's `standard-presentations.lisp`. The
[macrolet file checks](../tests/resumable/macrolet-file-results.json) cover 21
ordinary/scheduled assertions, including source-deleted loading and the absence
of compile-time effects for nested forms. The related 91 upstream ANSI cases
pass in both evaluation and compiled modes. The real staging rerun now compiles
and loads `standard-presentations.lisp` and proceeds to its command components.
The later full McCLIM staging results are recorded below.

An independent [browser replay of 206 previously compiled components](../tests/mcclim/compiled-prefix-browser-results.json)
passes geometry and scheduled thread-join checks with embedded source loading
disabled. The loaded source inventory matches the compiled prefix in order,
with zero errors and no remaining tasks. This diagnostic uses the older core
snapshot that preceded the MACROLET fix; it does not cover the remaining
components or a real application window. Full compiled-library restoration and
GUI input still require their separate gates.

A subsequent [247-component replay](../tests/mcclim/compiled-presentations-browser-results.json)
uses artifacts from the corrected MACROLET compiler and restores `REAL`,
`INTEGER` and bounded-integer presentation types in a fresh browser. Geometry,
the application-frame macro and a scheduled thread join also pass, with library
source loading disabled and the loaded inventory matching the manifest in order.
The original producer was interrupted before finishing. For each included
artifact, a following `TARGET-STAGE` marker proves that its compilation and load
returned. This verifies restoration of the presentation definitions; the full
producer and compiled-library API/GUI results are recorded separately below.

A [375-component incremental browser replay](../tests/mcclim/compiled-incremental-browser-results.json)
restores all libraries from compiled artifacts in fresh browser workers. The
loaded component order matches the manifest; geometry, presentation types,
the application-frame macro, a scheduled thread join and exact ISO-2022-JP
encoding/round-trip checks pass with zero errors or remaining tasks. A separate
GUI worker renders the production counter demo, advances it with button and
keyboard input, and produces the expected changed image. Its source-loaded
application entry uses a real loader symbol so generated call-stack dispatch
works; library source fallback remains disabled. These artifacts come from
three compiler snapshots (271 + 41 + 63 components). The independent fresh
compilation of all 375 components is verified separately below.

The [fresh full compilation and browser replay](../tests/mcclim/compiled-fresh-browser-results.json)
now passes: all 375 components are compiled with one compiler snapshot and loaded
in dependency order, producing 12,775 forms without reusing earlier artifacts.
Fresh browser workers restore the libraries with source fallback disabled.
The API checks pass, including exact Japanese ISO-2022-JP encoding and decoding,
with zero errors or remaining scheduled tasks. The GUI loads the production
application entry from source and passes drawing, button and keyboard checks;
see the [initial frame](../tests/mcclim/screenshots/compiled-fresh-initial.png)
and [frame after input](../tests/mcclim/screenshots/compiled-fresh-after-input.png).
Source, artifact, bundle and browser-asset hashes were verified. This establishes
the compiled-library build/replay gate; full upstream test coverage is separate.

After the Drei backend source-registration fix, a [new fresh build and replay](../tests/mcclim/compiled-backend-browser-results.json)
again passes all 375 components, now producing 12,776 forms. The browser API
checks additionally verify the restored local image, the 35-component Drei tree,
the backend component's parent and its embedded source resource. Reading that
resource confirms availability while library source evaluation remains disabled.
The report includes the API driver and assertion hashes, zero errors and zero
remaining scheduled tasks. The separate production-worker GUI passes the same
drawing, button and keyboard checks; both the [initial screen](../tests/mcclim/screenshots/compiled-backend-initial.png)
and [screen after input](../tests/mcclim/screenshots/compiled-backend-after-input.png)
were visually inspected. Source/module, artifact, compiler, kernel, bundle and
browser-asset hashes were independently verified. The current-version full
McCLIM suite and combined Drei suite remain separate execution gates.

The [unchanged upstream calculator](../tests/mcclim/upstream-calculator-results.json)
also runs with those compiled libraries and the production browser worker.
Twelve actual pointer clicks verify `12 + 3 = 15`, continuing with `* 4 = 60`,
AC and CE clearing, and subsequent number entry. The observer records gadget
values without replacing the upstream callbacks or event loop. Both the
[initial calculator](../tests/mcclim/screenshots/calculator-initial.png) and
[screen after input](../tests/mcclim/screenshots/calculator-after-input.png)
were visually checked. This run took about 5 minutes 13 seconds to become ready
and another 6 minutes 21 seconds for the input sequence on the shared test
machine; interactive performance still needs improvement.
Run `node tests/mcclim/calculator.mjs completed-stage-work output-directory`
from the repository root, or use `npm --prefix tests run test:mcclim:calculator -- completed-stage-work output-directory`.
The staged builder accepts `--application source.lisp --output-directory directory`
in GUI mode, keeping the original producer cache and counter results intact.

The staged-compilation harness is available in the repository. From the
repository root, using a new work directory:

```sh
node tests/mcclim/staged-produce.mjs prepare /tmp/valtan-mcclim-staged
node tests/mcclim/staged-produce.mjs produce /tmp/valtan-mcclim-staged
node tests/mcclim/staged-build.mjs /tmp/valtan-mcclim-staged
node tests/mcclim/staged-api.mjs /tmp/valtan-mcclim-staged
node tests/mcclim/staged-build.mjs /tmp/valtan-mcclim-staged gui
node tests/mcclim/staged-gui.mjs /tmp/valtan-mcclim-staged
```

Preparation builds an isolated cache and records source/module hashes. Production
uses Valtan's `COMPILE-FILE` with browser reader features and loads each artifact
before compiling its dependants. The staging loader itself runs from source;
the manifest covers the subsequent dependency and application components.
Replay checks the complete component inventory and artifact hashes, disables
library source loading, and bundles the compiled forms without rewriting the
cache. The API consumer checks the loaded order, geometry, presentation types,
the Drei backend and its source resource, Babel ISO-2022-JP encoding and a
scheduled thread join. The GUI consumer restores compiled libraries and
loads the production demo's application entry from source, then checks drawing,
button and keyboard input. Results stay in the work directory; the availability
of these commands is not evidence that the pending full McCLIM gates have passed.
The [harness smoke checks](../tests/mcclim/staged-harness-results.json) cover
two compiled fixture components restored in a fresh process, malformed-artifact
rejection, missing-component rejection and artifact-hash validation. Full build
preparation identifies 376 embedded modules, including the staging loader, so
the expected compiled-artifact inventory is 375 components.

Internal cooperative locks and condition variables now share the scheduler's
task identity. Locks support recursive ownership, nonblocking/timed acquisition
and FIFO handoff. Condition waits atomically enqueue and release the lock, then
reacquire it before returning or unwinding; cancellation after a grant cannot
leave an abandoned owner. Repeated cancellation during condition reacquisition
preserves the first interruption until protected cleanup can run with its lock.
The [synchronization checks](../tests/resumable/synchronization-results.json)
exercise shared counters, producer/consumer queues, notification/broadcast,
timeouts and cancellation. The internal timeout unit is milliseconds; the
Bordeaux Threads adapter converts its public seconds-based timeouts.

`CL:SLEEP` in scheduled dual-compiled Lisp now parks on a host timer, allowing
other Lisp tasks and browser events to proceed. Zero seconds also yields a host
turn. Cancellation clears the timer and runs Lisp cleanup, which can itself
sleep. Long timers are split at the host timer limit. The
[sleep checks](../tests/resumable/sleep-results.json) cover rational, floating
and zero durations, interleaving, cancellation and unchanged multiple values
from other environment operations. Ordinary synchronous calls retain their
blocking behavior.

The scheduler's `interrupt(handle, function, args)` now runs a callback in the
target task's dynamic context before resuming its suspended computation. Each
interrupted activation retains its own pending wait, so a promise fulfilled or
rejected during a callback is delivered afterward. Callbacks can suspend or be
interrupted themselves. Cancellation unwinds callback cleanup before the original
task's cleanup. Normal callback results are discarded; nonlocal exits retain the
values transferred through `THROW`/`RETURN-FROM`. The
[interrupt checks](../tests/resumable/interrupt-results.json) cover target Lisp
handlers, CATCH/THROW, nonlocal GO, asynchronous cleanup and binding isolation.
This is the internal scheduler interface used by the Bordeaux Threads adapter.

The optional [Bordeaux Threads port](../library/portability/bordeaux-threads/README.md)
now connects both public API versions to the scheduler. It preserves the
upstream 0.9.4 portable implementations and uses Valtan adapters for thread
identity, creation, joining, locks, condition variables, interruption and
termination. The upstream semaphore and timeout implementations use these
operations. Atomic integers use unsigned 64-bit values. Threads share the
JavaScript heap and retain separate dynamic bindings; joining a destroyed
thread waits for its asynchronous cleanup. Initial binding forms retain the
upstream v1/v2 evaluation rules.
The [public API checks](../tests/bordeaux/results.json) pass all 11 groups in
Node and a fresh Chrome worker, including 64-thread shared counters and atomic
updates, condition broadcasts, asynchronous termination cleanup, self/remote
interruption and timeouts. The kernel suite passes 41 tests; ordinary and dual
compiler/runtime regressions also pass. The separate upstream test-suite run
is not included in those passing API counts.

The [upstream suite results](../tests/bordeaux/upstream-results.json), from a
separate frozen build with the atomic optimization and `CHECK-TYPE` fix,
complete with 82 registered cases and 130 passing checks, no skips, failures
or unexecuted registered cases. This includes the unchanged million-increment
and million-decrement test. Upstream reader guards still apply: the v1
`condition-wait-timeout` case excludes Valtan, while the v2 timeout cases run.
These results predate the native condition-delivery changes below and do not
establish full McCLIM compatibility.

The browser port now starts McCLIM's I/O thread when `:clim-mp` is enabled.
Scheduled input processing polls the browser queue without blocking the worker
and sleeps cooperatively between polls. The counter demo runs its application
frame in a Bordeaux thread in that mode. Enable both `:valtan-resumable` and
`:clim-mp` in the system's target features, or run
`npm run test:mcclim:gui:threaded` from `tests` for the integration check.
The [threaded GUI run](../tests/mcclim/gui-threaded-results.json) uses the production
worker and verifies initial drawing, a button click reaching Counter 1, keyboard
input reaching Counter 2, and changed pixels. A separate
[ordinary GUI run](../tests/mcclim/gui-thread-integration-ordinary-results.json)
passes the same checks. Both start fresh workers and record source and bundle
hashes. The threaded snapshot predates the final nested-task checkpoint fix and
file-literal work; the ordinary snapshot includes the file-literal traversal
fix. These counter checks do not establish full threaded Canvas or upstream
suite coverage.

The separate [threaded Canvas run](../tests/mcclim/canvas-threaded-results.json)
passes all 350 drawing assertions, including Bezier regions and flipping designs,
plus five checks for scheduler context, the application frame, the live I/O thread
and a suspending thread join. The test workbench interrupts the application task
to retain its dynamic bindings while evaluating checks that can suspend.
Run `npm run test:mcclim:canvas:threaded` from `tests` to repeat this suite.
The recorded evaluation took 14.6 minutes after startup; its observer deadline
was extended without reloading or changing the checks. The driver now allows
30 minutes for threaded evaluation. This frozen snapshot has the same checkpoint
limitations as the threaded GUI record above and predates the temporary
declaration optimization. It does not establish a passing full upstream suite.
A [later fresh threaded run](../tests/mcclim/canvas-threaded-features-results.json)
also passes all 350 drawing assertions and five thread-entry checks, with
McCLIM's reader features present in all 496 embedded source modules. It includes
the checkpoint, declaration-deduplication and native package-lookup changes,
uses the unchanged checks, and needs no observer extension or source reload.
Its compiler snapshot predates the subsequent MACROLET file-compilation fix;
the complete upstream suite remains a separate gate.

`CHECK-TYPE` now reports the actual rejected value, preserves `STORE-VALUE`
retry behavior and uses a supplied type description. The
[condition checks](../tests/ansi/check-type-results.json) pass all nine upstream
cases and four focused regressions in both evaluated and compiled modes.
The full conditions category completes with 646 passing and 27 existing
failures, resolving five failures without introducing new failure names.

Atomic integer operations now validate their fixed argument types directly
on the successful path, retaining `CHECK-TYPE` for invalid arguments and
`STORE-VALUE` retries. The [atomic checks](../tests/bordeaux/atomic-results.json)
pass 12 public API groups in fresh Node and Chrome builds. In a same-process
diagnostic, 20,000 increments took a median 39.84 seconds before and 1.01
seconds after this change. This measures that update loop, not library startup
or whole McCLIM performance.

Native symbol access, undefined-function and invalid-function errors, and
generated Lisp argument-count errors now signal through resumable calls.
The [native condition checks](../tests/resumable/native-error-results.json)
pass eight simultaneous cases in Node and Chrome: handlers await while their
dynamic bindings and restarts remain active, then run asynchronous cleanup.
The ordinary compiler, dual compiler/runtime, callback and compiled-artifact
regressions also pass. Builtin argument-count errors now also signal through
resumable calls, before native entry. The
[expanded condition checks](../tests/resumable/native-arity-results.json)
pass ten simultaneous cases in Node and Chrome, including builtins with and
without resumable implementations.

Numerical conditions now also reach resumable handlers, including division by
zero, invalid numerical arguments, floating-point overflow and underflow.
Native arithmetic reports conditions at its call boundary, preserving active
Lisp handlers and restarts; ordinary callers retain synchronous delivery.
The [expanded numerical condition checks](../tests/resumable/numeric-error-results.json)
pass all 27 cases in Node and Chrome, including nested complex arithmetic and
a synchronous callback handling its own numerical error. The 48 existing ANSI
numeric regressions pass in both evaluated and compiled modes. Ordinary and
dual compiler/runtime regressions, including source-deleted compiled artifacts,
also pass.

Scheduled `FUNCALL` now enters the resumable target directly, removing two
forwarding generator frames. The
[call benchmark and regressions](../tests/resumable/funcall-results.json)
measure the same 2,000 CLOS slot updates at a median 5.02 seconds before and
4.23 seconds after the change; the simple closure loop changed little.
Ordinary and dual compiler/runtime checks, 27 browser condition cases and
compiled-artifact checks pass. This is a focused Node measurement, not a
whole-McCLIM speedup measurement.

Registered native functions also return their resumable implementation's
iterator directly on valid calls, avoiding another forwarding generator frame.
Invalid argument counts retain resumable error delivery. The
[native forwarding checks](../tests/resumable/native-forwarding-results.json)
include the existing callback, scheduler, synchronization, interrupt, 27 native
condition and source-deleted artifact regressions. In one seven-sample Node
comparison, a 20-level `APPLY` chain propagating loop checkpoints changed from
1.77 to 1.51 seconds; a shallow `APPLY` workload changed from 225 to 236 ms.
These synthetic timings exclude startup and do not establish a McCLIM speedup.
Run `tests/performance/native-apply.mjs` with a dual runtime bundle and report
path to repeat the workloads.

Compiler checkpoints now consume the task's budget locally and yield only when
that budget is exhausted. Older compiled code and native generators can still
yield individual checkpoints. Synchronous facades omit checkpoint yields that
they would immediately consume; explicitly advancing another task inside such a
callback still respects that task's own budget. The
[checkpoint checks](../tests/resumable/checkpoint-results.json) cover 11 focused
cases, matching old/new/mixed task slices, nested contexts, exceptions and direct
iterator consumers, alongside the existing resumable regressions. A source-deleted
artifact from the older runtime also loads and suspends successfully after restart.
Fresh Chrome checks pass 64 interleaved tasks and all 27 native condition cases.
The existing 2,000-update CLOS benchmark changes from 3.89 to 1.12 seconds, and
the closure workload from 701 to 377 ms. These Node measurements exclude startup
and do not establish full McCLIM performance or GUI compatibility.

Temporary declarations use an `EQUAL` hash table to remove duplicate names while
preserving their previous order. This avoids pairwise string comparisons in
large compiled bodies, observed during the scheduled McCLIM Canvas checks.
The [compilation benchmark](../tests/resumable/compile-benchmark-results.json)
compares actual scheduled `COMPILE` calls before and after this change, verifies
both execution entries, and compares their generated JavaScript hashes.
Its timings cover synthetic assignment bodies on a shared machine, not McCLIM
startup or drawing. To repeat it after building the dual runtime, run
`node tests/resumable/compile-benchmark.mjs tests/dist-resumable/resumable.js /tmp/compile-results.json`.

`FIND-PACKAGE` searches the native registry for global names and nicknames after
checking package-local nicknames. It no longer allocates Lisp strings and lists
for every package on each lookup. A ten-second profile during McCLIM startup
found this lookup in 4,975 of 9,488 samples. The
[lookup checks and benchmark](../tests/resumable/package-lookup-results.json)
preserve local nickname precedence, renamed/deleted packages, package objects,
and string designators in ordinary and scheduled calls. With 128 additional
packages, 200 name lookups averaged 1.58 seconds before and 0.79 milliseconds
after in ordinary calls; scheduled calls averaged 29.06 seconds before and
9.06 milliseconds after. These are two-sample lookup measurements on a shared
machine, not whole-application timings. The complete ANSI package category
retains its previous 491 passes and nine known failures in both modes.
To repeat the measurement after building the dual runtime, run
`node tests/resumable/package-lookup-benchmark.mjs tests/dist-resumable/resumable.js /tmp/lookup-results.json`.

Application drivers still need integration with the scheduler: blocking calls
must originate in scheduled Lisp, and merely enabling `:valtan-resumable` does
not schedule a synchronous application top-level. Other blocking host operations and remaining JavaScript callback
boundaries can still prevent suspension; native error delivery is not yet
generally resumable. Load-time
initializers remain synchronous. The McCLIM Canvas driver
accepts `--resumable` to check loading and ordinary drawing calls against a dual
build; it does not by itself test suspension inside McCLIM operations.

These results cover the checked API calls, not every feature in a passing library.
The default ahead-of-time mode separately passed the `split-sequence` and
`cl-base64` smoke cases. The loader checks, embedded-source fixtures and all 50
ANSI harness/regression groups passed after the changes. The upstream ANSI
iteration category completed with 839/843 passing; the four failures concern
counter values in FINALLY and are marked as specification issues by the upstream
suite. Valtan's values agree with SBCL for those cases. The full ANSI and SACLA
suites were not remeasured for this change.

Single-pair `PSETQ` now expands to `SETQ` followed by `NIL`, avoiding saved
multiple-value lists and setter closures in single-variable `DO` steps. Multiple
assignments retain `PSETF`'s place-subform capture semantics. The
[regression results](../tests/resumable/psetq-results.json) include 106 upstream
`PSETQ`, `PSETF`, `DO` and `GETF` cases passing in each of evaluation and compiled
modes, plus ten ordinary and ten scheduled assignment checks. Run the latter
with `npm run build:resumable && npm run test:resumable:psetq` from `tests`.
An ordinary `GETF` scan over 64 property pairs in the dual runtime dropped from
43 to 1.4 microseconds for a final-key hit, and from 47 to 1.4 microseconds for a
miss. These local timings exclude startup and do not measure McCLIM as a whole.

A [fresh image-enabled Drei run](../tests/mcclim/drei-image-upstream-before-results.json)
executes all 19 previously image-skipped cases: 101 of their 102 assertions pass.
The remaining failure exposes a `LOOP ... ON` issue: after exhausting a list,
`FINALLY` sees the last cons instead of the terminal atom. Drei's `REST-FORMS`
therefore treats `LIST` as an operand in the unfinished input `(list `.

Simple `ON` variables now hold the iterator directly, including its initial value
and terminal atom; destructuring variables still describe the last visited cons.
[The fix checks](../tests/resumable/loop-on-results.json) pass 13 cases each in
ordinary and scheduled execution and repair the focused case in a retained real
Drei parser. The complete ANSI iteration category remains 839/843 in both modes,
with exactly the same four arithmetic `FINALLY` failures as the recorded baseline.
The [fresh browser rerun](../tests/mcclim/drei-image-upstream-results.json)
loads the same 13 unchanged upstream test sources and records 3,665 passed
assertions, zero failed assertions and one default self-compilation skip.
`WITH-CODE-INSIGHT` now passes all 12 assertions, and all 19 previously
image-skipped cases run successfully. The report retains its aggregate `failed`
status because 90 separately selected performance cases remain unexecuted and
self-compilation is skipped in this default-suite invocation. The separate
performance and self-compilation reports above used earlier runtime versions;
these runs do not establish complete current-version or threaded GUI coverage.
`npm run test:mcclim:drei:all` combines the default suite and all 90 performance
cases in one fresh browser, enables self-compilation, and bundles its unchanged
source resources. Missing, skipped or failed cases still prevent a passing
report. It allows 12 hours for evaluation; `upstream.mjs --threaded --drei-all`
allows 24 hours.

The [completed combined run](../tests/mcclim/drei-all-outer-results.json) reports
3,611 passing outer assertions, including all 90 performance checks and the
self-compilation assertion. Its [coverage comparison fails](../tests/mcclim/drei-all-outer-comparison.json):
14 registered cases contribute no outer results, leaving 145 expected target
assertions unaccounted for. FiveAM's recursive run inside self-compilation updates
test statuses; later outer cases can therefore run only inside that recursive
invocation. The outer report also cannot expose skipped checks inside it.
This result does not pass the full Drei gate.

The runner now records every completed test invocation, including nested runs.
Per-test coverage uses each case's last invocation, while failures and skips from
**all** invocations prevent success. The verifier requires every default-suite
case to appear in the nested execution records and checks their counts against
the reported coverage. [Regression checks](../tests/mcclim/fiveam-execution-results.json)
pass on SBCL, ordinary Valtan and scheduled browser execution, including the actual
JSON report and hidden failure/skip cases. Reproduce them with
`cd tests && npm run test:mcclim:executions`. These are synthetic observer tests;
the complete Drei run with this collector and image-backend reload validation
remains pending.
`npm run test:mcclim:drei:all:host` uses upstream's native Swank image backend
without starting a server. Its [native reference](../tests/mcclim/drei-all-host-results.json)
executes all 596 cases with zero skips, but records 11 failures: one upstream
self-compilation reader failure and ten Swank completion-protocol failures.
These native failures remain in the report and are not Valtan results.
For a completed combined target run, compare coverage from the repository root:

```sh
node tests/mcclim/compare-drei.mjs target.json tests/mcclim/drei-all-host-results.json comparison.json
```

The comparison requires successful target execution, all 596 unique tests,
615 registration entries, matching test/resource hashes and per-test assertion
counts. Native failures remain visible and never excuse a target failure.
The [coverage metadata](../tests/mcclim/drei-coverage-metadata.json) records two
implementation differences: 13 Gray protocol test names use `SB-GRAY` symbols
on SBCL and `VALTAN-GRAY` symbols on Valtan; the lambda-list-keyword test makes
one additional check on SBCL because of `SB-INT:&MORE`. Only these named
differences are accepted. The verifier tests include actual incomplete browser
output, hidden nested outcomes and synthetic complete fixtures; they do not establish that the pending
combined target run has passed.

Run `npm run build:resumable && npm run test:resumable:loop-on` from `tests` to
repeat the focused checks. The iterator behavior follows the
[CLHS description of `ON`](https://www.lispworks.com/documentation/HyperSpec/Body/06_abac.htm).

The optional `mcclim-portability/image-support` system provides symbol completion,
operator argument descriptions and CLOS initialization argument descriptions for
editor integration. Completion respects package exports, internal access,
package-local nicknames and escaped names; hyphen-separated abbreviations such
as `m-v-b` match `multiple-value-bind`. It does not intern completion requests.
Class descriptions include inherited slots and applicable initialization methods,
without evaluating slot or default-initarg forms. Argument descriptions use
retained compiler metadata and editor descriptions for standard body forms;
unknown native function signatures remain unavailable.

[The helper checks](../tests/mcclim/image-support-results.json) pass 33 assertions
each in fresh Node and Chrome runtimes. Run them from the repository root with:

```sh
node tests/mcclim/probe.mjs mcclim-portability/image-support /tmp/image-support-node.json tests/mcclim/image-support-checks.lisp
node tests/mcclim/probe.mjs mcclim-portability/image-support /tmp/image-support-browser.json tests/mcclim/image-support-checks.lisp --browser
```

These are helper API checks; they do not establish Drei adapter or GUI coverage.

`mcclim-browser` also loads `mcclim-portability/drei`. With `:clim-without-swank`,
Drei uses a Valtan local image for argument lists, symbol completion, class
initialization arguments and compilation. Explicitly selected image objects and
Swank configurations retain their upstream selection behavior. String
compilation processes forms in order through Valtan's compiler and returns
warning/error notes; file compilation optionally loads the generated artifact.
[Adapter checks](../tests/mcclim/drei-image-results.json) pass 22 assertions in a
fresh real Chrome McCLIM workbench and 11 filesystem assertions in fresh Node.
The browser run predates the PSETQ optimization; the Node run includes it.

Reloading upstream `lisp-syntax.lisp` replaces `DEFAULT-IMAGE` with its no-backend
fallback, `T`, as reproduced in the [reload regression](../tests/mcclim/drei-image-reload-before-results.json).
The Valtan adapter now declares its source as a static resource and appends a
`valtan-image` component after all 34 original Drei components. Drei's own
component-based self-compilation can therefore restore the image backend last.
Registration is idempotent and retains the original components and their order.
The full-suite driver verifies the 34-plus-backend tree and the selected image
before and after the suite; the original 34 hashes are still compared with SBCL,
and the adapter's hash is recorded separately.

The [focused reload checks](../tests/mcclim/drei-image-reload-results.json) pass
in Node and Chrome. `cd tests && npm run test:mcclim:drei:reload` re-evaluates the unchanged upstream
syntax source, reloads the backend through its component pathname, repeats that
sequence, then runs the 22 adapter assertions in Node and Chrome. This focused
check is separate from Drei's parser-based self-compilation of every source.

From the repository root, reproduce the browser method checks with the workbench
and inspect the `drei-image` response for `(:DREI-IMAGE-CHECKS 22)`:

```sh
node tests/mcclim/workbench.mjs <<'JSON'
{"op":"eval","id":"drei-image","file":"tests/mcclim/drei-image-checks.lisp","timeout":300000}
{"op":"close"}
JSON
node tests/mcclim/probe.mjs mcclim-portability/drei /tmp/drei-image-file-node.json tests/mcclim/drei-image-file-checks.lisp clim-without-swank
```

These checks cover adapter methods, not the complete upstream suite or GUI input.
Browser pathname output remains unavailable. Compiler notes have messages and
severity, but no source locations; fuzzy completion and definition lookup still
use upstream fallback methods.

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
