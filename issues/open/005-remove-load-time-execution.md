---
id: 005
title: Remove load-time execution (tests, asserts, width/TERM detection, trace)
severity: medium
commands: [kwic, check-surface, columnify, cookie-loop, cookie, box, clean-paths]
labels: [epic, framework, cleanup]
status: open
---

# Remove load-time execution (tests, asserts, width/TERM detection, trace)

**Severity:** medium  **Commands:** `kwic`, `check-surface`, `columnify`, `cookie-loop`, `cookie`, `box`, `clean-paths`  **Labels:** epic, framework, cleanup

## Problem

The README explicitly says the new single-image model **must avoid load-time
execution** (top-level forms that run when the command is loaded/compiled into
the dispatcher image). Several files still do it, which slows image build and
risks aborting the whole build if a form errors.

## Sites

- `kwic.lisp:53` — `(test/wl-lessp)` runs the test suite at load time. An assert
  failure would abort loading the dispatcher.
- `check-surface.lisp:166-173` — top-level `(progn (assert ...))` test block runs
  at load time; also stray debug `(defun m () (main '("-e" ".")))` at ~583.
- `columnify.lisp:41` — `*width*` is a `defvar` computed at load time (shells out
  to `stty`), freezing terminal width at build time. Move detection into `main`.
- `cookie-loop.lisp` / `cookie.lisp` — several `defparameter`s read `getenv
  "TERM"` at load time; evaluate at run time instead.
- `box.lisp:58-114` — top-level `setf logical-pathname-translations` / `load` /
  `use-package` / template-file parsing run (and crash) at load (see issue 010).
- `clean-paths.lisp:539` — `(trace clean-name)` left in at top level.

## Fix

Move runtime-dependent computation into `main` (or a lazily-initialized
accessor). Guard or delete test/`assert` blocks (gate behind a feature, or move
to a real test system). Remove leftover `(trace ...)` and debug `defun`s.

## Progress (verified against a real build)

A full build (quicklisp + informatimago libs, SBCL 2.5.2) was run and every
command exercised.  Findings:

- **`columnify` — FIXED (commit).** Its load-time `*width*` was not merely a
  smell: it called `stty` with `:input :terminal`, which this uiop rejects,
  and the error **aborted loading `columnify.fasl`**, so `COMMAND.COLUMNIFY::MAIN`
  was never defined and `columnify` crashed at runtime with "MAIN is undefined".
  Replaced with a guarded run-time `TERMINAL-WIDTH` function.  This is concrete
  proof of the README warning ("a load-time form that errors aborts the build").
- The other listed sites (`kwic` test, `check-surface` assert, `cookie`/`cookie-loop`
  TERM, `clean-paths` trace) did **not** abort loading in this environment — the
  build loads all 64 commands with 0 failures.  They remain smells to clean up
  but are not currently build-breaking here; re-verify on the target platform.

- **`svn-locate-revision` — FIXED (commit).** A new instance not previously
  listed: `svn-locate-revision.lisp:43` did `(defparameter *verbose* t)` at top
  level.  Because the file was `(in-package "SCRIPT")`, this mutated the **shared**
  framework special `SCRIPT:*PROGRAM-VERSION*`'s sibling `*VERBOSE*` to T at build
  time, so the saved image had verbose output turned on for **every** command
  (confirmed: `*verbose*` was T in the core; commands printed `;`-trace lines
  with no `-v`).  Removed the load-time form (and the redundant `(in-package
  "SCRIPT")`, and fixed its won't-dispatch `:main`); `*verbose*` is NIL again.
  This is the second concrete instance (after `columnify`) of load-time forms
  causing real, cross-command misbehaviour — strong support for this epic.

## Acceptance criteria

- Loading the dispatcher image performs no I/O, no subprocess calls, no
  `trace`, and runs no test asserts.
