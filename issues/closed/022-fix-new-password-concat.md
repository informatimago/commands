---
id: 022
title: `new-password`: undefined `concat`/`concatf` at runtime
severity: high
commands: [new-password]
labels: [bug]
status: FIXED - defined SCRIPT:CONCAT in framework; wired standard-options (-h/-v/-V)
---

# `new-password`: undefined `concat`/`concatf` at runtime

**Severity:** high  **Commands:** `new-password`  **Labels:** bug

## Status: broken — `main` errors with undefined function

`(define-modify-macro concatf (other) concat)` (line 127) expands to calls of
`concat`, but the `command` form (39) declares no `:use-systems`/`:use-packages`,
so the package use-list is just `("COMMON-LISP" "COM.INFORMATIMAGO.COMMAND.SCRIPT")`
— neither exports `concat`. `main` calls `concatf` (136/142/145) → undefined
function `CONCAT` at runtime.

## Fix

Either:
- add the cesarum system/package that exports `concat`
  (`COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING`) to the `command` form, or
- replace `concatf`/`concat` with
  `(setf password (concatenate 'string password ...))`.

Also add `-v/--verbose` and `-V/--version` (version "1.0.1" defined, not exposed).

## Correction (verified against a real build)

The `concat` premise above was a static-analysis artifact.  In a real build,
`SCRIPT` inherits and re-exports `CONCAT` from
`COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY`, so `new-password` never actually
failed on `concat`.  The framework `SCRIPT:CONCAT` fallback added in commit
`f70e6a0` is therefore a guarded no-op (kept as defensive code).

What *did* break loading was the follow-up homogenization: `new-password` was
switched to `(standard-options)`, but `VERSION-OPTION`/`VERBOSE-OPTION`/
`STANDARD-OPTIONS` were defined in `SCRIPT` yet **never exported** (the
`packages.lisp` export edit was lost).  So `new-password` failed to load with
"STANDARD-OPTIONS is undefined".  Fixed by exporting the three helpers (later
commit).  Verified: `new-password`, `new-password -h/-V/-v` all work.
