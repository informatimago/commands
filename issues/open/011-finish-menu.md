---
id: 011
title: Finish `menu`: stub main, real code commented out
severity: medium
commands: [menu]
labels: [incomplete]
status: open
---

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
