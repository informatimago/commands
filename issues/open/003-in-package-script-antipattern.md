---
id: 003
title: Remove (in-package "SCRIPT") package-pollution anti-pattern
severity: high
commands: [mfod, cookie-diff, dedup, surveille-web-pages, svn-locate-revision, clar]
labels: [epic, framework, bug]
status: open
---

# Remove (in-package "SCRIPT") package-pollution anti-pattern

**Severity:** high  **Commands:** `mfod`, `cookie-diff`, `dedup`, `surveille-web-pages`, `svn-locate-revision`, `clar`  **Labels:** epic, framework, bug

## Problem

Several files do `(in-package "SCRIPT")` **before** their `(command ...)` form.

Two facts bound how bad this is:

- `builder.lisp` (`compile-and-load-command`, lines 161-170) already binds
  `*package*` to the command's own package (`COMMAND.<NAME>`, which `:use`s
  SCRIPT) for both compile and load. So the leading `(in-package "SCRIPT")` is
  **redundant** — `command` would resolve without it.
- The `command` macro emits `(in-package "COMMAND.<NAME>")`, so everything
  *after* the form lands in the command package regardless.

The real hazard is therefore narrow: any `defun`/`defvar` placed **between** the
`(in-package "SCRIPT")` and the `(command ...)` form lands in the shared SCRIPT
package and can clobber framework symbols. A file that puts the `command` form
first (nothing before it) is merely redundant, not harmful.

**NOTE — earlier mis-analysis corrected:** `mfod` was previously cited here as
redefining `SCRIPT:RUN-PROGRAM`. It does not: `mfod` defines nothing before its
`command` form, and `:shadow ("RUN-PROGRAM")` makes its `run-program` local to
`COMMAND.MFOD`. See issue 021. Each listed file must be checked individually for
forms that actually precede the `command` form.

## Affected files

- `mfod.lisp` — redefines `SCRIPT:RUN-PROGRAM`, `*verbose*`, `*program-version*`.
- `cookie-diff.lisp:3`
- `dedup.lisp:2`
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
