---
id: 002
title: Add missing (command ...) forms to bare commands
severity: medium
commands: []
labels: [epic, framework]
status: open
---

# Add missing (command ...) forms to bare commands

**Severity:** medium  **Commands:** —  **Labels:** epic, framework

## Problem

~38 command files have **no `(command ...)` form**, so they are registered only
by filename with default metadata: no `:documentation`, no `:use-systems`, and
they rely implicitly on the generator's default package-use-list (which happens
to include `SCRIPT`) for `ex-ok`, `*program-name*`, `getenv`, `run-program` to
resolve. This is fragile and provides no help/option metadata.

## Affected files (no `(command ...)` form)

`add-cookie`, `add-paths`, `batch-emerge`, `bin-to-c-array`, `box`, `buzzword`,
`capitalize`, `clean-bd-archive`, `clean-name`, `columnify`, `commands`,
`cookie`, `cookie-loop`, `departement`, `diss`, `downcase`,
`edit-comments-of-ogg`, `entropy`, `euronews`, `extend-identifiers`,
`get-directory`, `hexbin`, `insulte`, `llen`, `lrev`, `memo`, `menu`,
`news-to-mbox`, `nls`, `programmer`, `pseudo-pop`, `random`, `revlines`,
`rotate`, `sleep-schedule`, `split-dir`, `text`, `when`.

## Fix

Add a minimal form to each, e.g.:

```lisp
(command :name "downcase"
         :documentation "Print each argument string-downcased, one per line.")
```

with `:use-systems` listing any non-core systems the file actually uses
(e.g. `:cl-ppcre`, `:md5`, `:com.informatimago.common-lisp`). Do **not** add a
preceding `(in-package "SCRIPT")` (see issue 003). Pairs naturally with the
option homogenization in issue 001.

## Acceptance criteria

- Every dispatchable file begins with a `(command ...)` form declaring at least
  `:name` and `:documentation`.
- `register-command-file` picks up the documentation (visible in `-h`).
