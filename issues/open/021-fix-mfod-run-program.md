---
id: 021
title: `mfod`: clobbers SCRIPT:RUN-PROGRAM and *verbose*
severity: high
commands: [mfod]
labels: [bug]
status: open
---

# `mfod`: clobbers SCRIPT:RUN-PROGRAM and *verbose*

**Severity:** high  **Commands:** `mfod`  **Labels:** bug

## Status: broken (cross-command contamination)

`mfod.lisp:40` does `(in-package "SCRIPT")` before the `command` form, so its
helpers are defined **in the SCRIPT package**:

- line 46 `(defun run-program ...)` **redefines `SCRIPT:RUN-PROGRAM`** globally —
  the `:shadow ("RUN-PROGRAM")` on the command form was meant to keep it local,
  but the early `in-package` defeats it. Other commands now get mfod's version.
- line 72 `(defvar *verbose* nil)` and line 88 `(defparameter *program-version*)`
  overwrite the framework specials.

## Fix

- Remove `(in-package "SCRIPT")`; put `(command :name "mfod" :shadow ("RUN-PROGRAM")
  ...)` first so helpers land in `COMMAND.MFOD` and the shadow works as intended.
- Drop the redundant `*verbose*`/`*program-version*` (re)definitions.
- Add a `-V/--version` option (mfod defines `*program-version*` "1.0.2" but never
  exposes it) — see 001.

This is the concrete instance behind epic 003; fix it explicitly and verify
`SCRIPT:RUN-PROGRAM` is intact for other commands.
