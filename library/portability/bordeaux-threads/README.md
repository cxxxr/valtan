# Bordeaux Threads on Valtan

This port uses the unchanged portable parts of Bordeaux Threads 0.9.4 with
Valtan's cooperative scheduler. `upstream/source.json` records their source
and SHA-256 hashes; their MIT license is in `upstream/LICENSE`. The native
adapters and atomic integers are implemented outside that directory.

Add `"bordeaux-threads"` to a system's dependencies and enable
`:target-features (:valtan-resumable)`. Valtan's target ASDF registry selects
this port only for that feature. The host Lisp's ASDF registry is unaffected.

Both `bt` and `bt2` run threads in the same JavaScript heap. They support
thread identity and names, creation, multiple-value joins, separate dynamic
bindings, locks and recursive locks, condition variables, semaphores,
interrupts, termination and timeouts. BT2 atomic integers use unsigned 64-bit
arithmetic on both Node and browsers. Their updates contain no scheduling
checkpoint. Scheduling is cooperative and does not provide parallel CPU
execution.

Calls that wait must run inside a `lisp.CooperativeScheduler` task. An ordinary
startup call has a stable main-thread identity and can create threads, but it
cannot synchronously join a live thread or wait on a contended lock. The
application must arrange its asynchronous entry; enabling the feature alone
does not turn its synchronous top-level call into a scheduled task. Blocking
foreign calls still block the worker.

New threads use global special-variable values unless explicitly rebound.
The upstream APIs retain their different binding rules: v1 evaluates initial
binding forms in the new thread; v2 evaluates them in the creator. Destroying
a thread marks it dead immediately, then unwinds its Lisp stack. Joining waits
for cleanup, including cleanup that suspends. Self-interruption completes
inline before the caller continues.

The tests in `tests/bordeaux/` exercise the public API through scheduled Lisp
functions. Run from the repository root:

```sh
npm --prefix tests run build:bordeaux
npm --prefix tests run test:bordeaux
npm --prefix tests run build:bordeaux:upstream
npm --prefix tests run test:bordeaux:upstream
```

The upstream runner loads both unmodified test suites and explicitly registers
the three unchanged atomic test bodies excluded by upstream's implementation
feature guard. The concurrency test retains its million increments and million
decrements. Other upstream reader conditionals remain in effect; for example,
the v1 `condition-wait-timeout` case's implementation list does not include
Valtan, while the corresponding v2 tests use capability checks. The runner
reports registered cases, results, skipped/failed cases and cases without a
result. It records and cleans up surviving test threads after both suites
finish. Passing these suites does not establish full McCLIM compatibility.
