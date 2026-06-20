---
id: 022
title: `new-password`: undefined `concat`/`concatf` at runtime
severity: high
commands: [new-password]
labels: [bug]
status: open
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
