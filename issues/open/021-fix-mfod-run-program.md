---
id: 021
title: `mfod`: redundant (in-package "SCRIPT"); add -V
severity: low
labels: [cleanup, cli]
status: in-progress - added (version-option); in-package cleanup deferred
commands: [mfod]
---

# `mfod`: redundant (in-package "SCRIPT"); add -V

**Severity:** low  **Commands:** `mfod`  **Labels:** cleanup, cli

## CORRECTION (2026-06): the original "clobbers SCRIPT:RUN-PROGRAM" claim was wrong

On closer inspection the contamination described in the first version of this
issue does **not** happen:

- `builder.lisp:161-170` (`compile-and-load-command`) binds `*package*` to the
  command's own package (`COMMAND.MFOD`) for both `compile-file` and `load`.
- The `command` macro additionally emits `(in-package "COMMAND.MFOD")`.
- The `command` form carries `:shadow ("RUN-PROGRAM")`, so `COMMAND.MFOD` has its
  own `RUN-PROGRAM` symbol.

Therefore `(defun run-program ...)` at line 46 defines
**`COMMAND.MFOD::RUN-PROGRAM`**, not `SCRIPT:RUN-PROGRAM`. There is no
cross-command contamination. Likewise `(defvar *verbose* ...)` and
`(defparameter *program-version* ...)` after the command form are in
`COMMAND.MFOD` (which inherits the symbols from SCRIPT) — they re-default the
shared specials, which is redundant but not harmful at run time (and the global
`*program-version*` concern is tracked separately in issue 006).

## What is actually true

- `(in-package "SCRIPT")` at line 40 is **redundant**: the builder already loads
  the file with `*package*` = `COMMAND.MFOD`, which `:use`s SCRIPT, so `command`
  resolves without it. It is only *risky* in the general anti-pattern sense (if a
  `defun` were placed before the `command` form it would land in SCRIPT) — but
  mfod defines nothing before the form, so today it is harmless. See epic 003.
- mfod had no `-V/--version` even though it defines `*program-version*` "1.0.2".

## Done

- Added `(version-option)` to the `mfod` options list (commit). `mfod -V` now
  works (subject to issue 006 re: the global version value).

## Deferred (needs a build to verify)

- Removing the redundant `(in-package "SCRIPT")` — safe per the analysis above
  but should be confirmed by an actual `make` build; folded into epic 003.
- Dropping the redundant `(defvar *verbose* nil)` (line 72).
