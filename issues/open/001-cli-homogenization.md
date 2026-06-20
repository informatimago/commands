---
id: 001
title: Homogenize CLI options (-h/--help, -v/--verbose, -V/--version)
severity: high
commands: []
labels: [epic, cli, enhancement]
status: open
---

# Homogenize CLI options (-h/--help, -v/--verbose, -V/--version)

**Severity:** high  **Commands:** —  **Labels:** epic, cli, enhancement

## Problem

Option handling is wildly inconsistent across the 72 commands. Of the standard
trio `-h|--help`, `-v|--verbose`, `-V|--version`:

- **All three** present today: only `radio`, `fpm` (broken, see 020),
  `split-merge` (buggy `-l`/`-r`, see 026).
- **`-h` only / ad-hoc**: `when`, `rotate`, `lc`, `llen`, `memo`, `clean-name`,
  `cookie`, `bin-to-c-array`, `check-surface`, `edit-comments-of-ogg`,
  `pic-resize`, `split-dir`, `commands`, `mfod`, `new-password`, `one-of`,
  `religion`, `ansi-test`, `shell`.
- **None at all** (bare `main`): `add-paths`, `buzzword`, `capitalize`,
  `columnify`, `departement`, `diss`, `downcase`, `entropy`, `extend-identifiers`,
  `lrev`, `nls`, `programmer`, `pseudo-pop`, `random`, `revlines`,
  `sleep-schedule`, `text`, `kwic`, `group-files`, `pjb-diff`, `hexbin`,
  `get-directory`, `news-to-mbox`, and others.

Several commands define `*program-version*` but never expose `--version`
(`mfod` 1.0.2, `new-password` 1.0.1, `menu` 0.0.2, `rotate`, `clar`, `euronews`,
`edit-comments-of-ogg`).

## Proposed standard

Every command must, at minimum, accept:

```
-h | --help       Print usage/help (framework help-option).
-v | --verbose    Set SCRIPT:*VERBOSE* to T.
-V | --version    Print "<program-name> version <*program-version*>" and exit 0.
```

## Recommended implementation — shared helpers in `script.lisp`

Add two helpers next to `help-option` (script.lisp ~942) and export them from
`packages.lisp`:

```lisp
(defun version-option ()
  (option ("version" "-V" "--version") ()
    "Print the version of this command and exit."
    (format t "~A version ~A~%" *program-name* *program-version*)
    (exit ex-ok)))

(defun verbose-option ()
  (option ("verbose" "-v" "--verbose") ()
    "Produce verbose output on *error-output*."
    (setf *verbose* t)))
```

Then each command's option list becomes:

```lisp
(options "foo" (list (help-option) (version-option) (verbose-option)
                     ... command-specific options ...))
```

## Acceptance criteria

- `script.lisp` exports `version-option` and `verbose-option`.
- Every dispatchable command registers `(help-option)`, `(version-option)`,
  `(verbose-option)`.
- `foo -V` prints the version and exits 0 (note: `ansi-test -V` currently exits
  with `ex-usage` instead of 0 — fix as part of this).
- `foo -h` lists all three plus command-specific options.
- A short test script invokes `<cmd> -V` / `<cmd> -h` for every command and
  checks exit code 0.

## Notes

This epic depends on nothing and unblocks most other cleanups. Commands that use
ad-hoc `(member "-h" arguments)` should migrate to `parse-options`; the few pure
filters (`downcase`, `lrev`, `revlines`, `columnify`) can keep reading stdin but
must still recognize `-h/-v/-V` before doing so.
