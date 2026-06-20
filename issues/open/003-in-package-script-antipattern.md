---
id: 003
title: Remove (in-package "SCRIPT") package-pollution anti-pattern
severity: high
commands: [mfod, cookie-diff, dedup, fetch-pop, surveille-web-pages, svn-locate-revision, clar]
labels: [epic, framework, bug]
status: open
---

# Remove (in-package "SCRIPT") package-pollution anti-pattern

**Severity:** high  **Commands:** `mfod`, `cookie-diff`, `dedup`, `fetch-pop`, `surveille-web-pages`, `svn-locate-revision`, `clar`  **Labels:** epic, framework, bug

## Problem

Several files do `(in-package "SCRIPT")` **before** their `(command ...)` form.
Because the `command` macro itself re-`in-package`s into `COMMAND.<NAME>`, any
`defun`/`defvar` placed *before* the macro lands in the **`SCRIPT` package** and
can clobber framework symbols shared by every command.

Worst case — `mfod.lisp:40`: `(in-package "SCRIPT")` then `(defun run-program ...)`
at line 46 **redefines `SCRIPT:RUN-PROGRAM`** globally (the `:shadow ("RUN-PROGRAM")`
on the `command` form was meant to shadow it locally, but the early `in-package`
defeats that). `(defvar *verbose* ...)` at line 72 and `*program-version*` at 88
likewise overwrite the framework specials.

## Affected files

- `mfod.lisp` — redefines `SCRIPT:RUN-PROGRAM`, `*verbose*`, `*program-version*`.
- `cookie-diff.lisp:3`
- `dedup.lisp:2`
- `fetch-pop.lisp:57`
- `surveille-web-pages.lisp:53` — also defines pipe-stream stubs into SCRIPT.
- `svn-locate-revision.lisp:34`
- `clar.lisp:41` (`(in-package "SCRIPT")` — less harmful but same smell)

Related: `fpm.lisp:300` re-`(defvar *verbose*)` and `split-merge.lisp:43`,
`shell.lisp:41` re-declare `*program-version*`/`*verbose*` inside their own
(correct) packages — redundant but benign; clean up while here.

## Fix

1. Remove the stray `(in-package "SCRIPT")`.
2. Put the `(command ...)` form first; let the macro own the package.
3. Pass `:shadow`, `:use-systems`, `:use-packages` to the `command` form rather
   than hand-rolling packages.
4. Drop redundant re-declarations of `*verbose*`/`*program-version*`; use the
   framework specials.

## Acceptance criteria

- No command file mutates the `SCRIPT` package's own functions/specials.
- `mfod` no longer redefines `SCRIPT:RUN-PROGRAM` (verify other commands still
  get the framework version).
