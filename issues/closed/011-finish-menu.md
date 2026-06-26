---
id: 011
title: Finish `menu`: stub main, real code commented out
severity: medium
commands: [menu]
labels: [incomplete]
status: closed
---

## Resolution (implemented + verified)

The commented `#| |#` block only parsed args into items — it never actually
displayed a menu or printed a choice, so it was revived **and completed**:

- `display-menu` prints the numbered items + a `Choice?` prompt on
  *error-output*; `read-choice` reads a 1-based number from *standard-input*
  (retrying on invalid input, NIL on EOF); `main` prints the chosen item on
  *standard-output*. This stdout/stderr split lets the choice be captured:
  `choice=$(menu a b c)`.
- Fixed the `(string= (aref arg 0) #\-)` bug → `(char= (char arg 0) #\-)`, and
  guarded empty-string args with `(plusp (length arg))`.
- Added a `(command :version "0.0.2" :documentation …)` form (replacing the
  load-time `defparameter *program-version*`) and the standard `-h/-v/-V` via
  `(options …)`/`parse-options`.

Verified: `-V` prints 0.0.2; `printf '2\n' | menu apple banana cherry` prints
`banana` on stdout with the menu on stderr; no args → usage, exit 64; invalid
choice then EOF → exit 66. Build: 0 failures.

# Finish `menu`: stub main, real code commented out

**Severity:** medium  **Commands:** `menu`  **Labels:** incomplete

## Status: incomplete-stub

Live `main` (lines 5-8) prints "Not implemented yet." and exits `ex-usage`. The
real implementation sits in a commented `#| ... |#` block (lines 10-41).

## Intended behavior

Display a selectable menu of items passed as arguments and print the choice.

## Work to finish

- Revive the `#| |#` body.
- Bug in the revived code: line ~30 `(string= (aref arg 0) #\-)` compares a
  **character** with `string=` → type error; use `char=`. Guard empty-string args.
- Add a `(command ...)` form (002); `*program-version*` is already "0.0.2".
- Add `-h/-v/-V` (001) — the commented code already had ad-hoc `-h`.
