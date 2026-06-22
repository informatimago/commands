---
id: 010
title: Finish `box`: stub main + crashes at load time
severity: high
commands: [box]
labels: [incomplete, bug]
status: open
---

# Finish `box`: stub main + crashes at load time

**Severity:** high  **Commands:** `box`  **Labels:** incomplete, bug

## Status: incomplete-stub (and cannot even load cleanly)

`main` (lines 3-7) is `(format t "Not implemented yet.")`. Worse, the rest of the
file has **top-level legacy forms that execute and crash at load**:

- 58-63: `(setf (logical-pathname-translations "PACKAGES") ...)`.
- 65: `(load "PACKAGES:COM;INFORMATIMAGO;...PACKAGE.LISP")` — path won't exist.
- 67-71: `(package:load-package ...)` / `(use-package ...)` — old API, undefined.
- 74-114: `*templates*` built at load time by opening `*box-path*` with
  `:if-does-not-exist :error`.
- 171: `(mapcar (function second) *templates)` — note **`*templates`** missing the
  trailing `*` → unbound symbol; also wrong shape.

## Intended behavior

Draw decorative box-art frames/heads/feet around text; templates are stored as
`;; DATA` comments in the file itself.

## Work to finish

- Delete/neutralize the legacy top-level `setf`/`load`/`use-package` forms (issue 005).
- Port template parsing to the cesarum picture/string systems; load templates
  lazily inside `main`, not at load time.
- Fix the `*templates` typo (171).
- Implement `main`: select a frame and wrap stdin/args.
- Add `(command ...)` form (002) and `-h/-v/-V` (001).
