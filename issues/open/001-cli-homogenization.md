---
id: 001
title: Homogenize CLI options (-h/--help, -v/--verbose, -V/--version)
severity: high
commands: []
labels: [epic, cli, enhancement]
status: in-progress - framework done (006 fixed, help-option exits); 34/64 built commands now expose -h/-v/-V; remaining are mostly commands with their own fix issue, plus a handful of ad-hoc-parser migrations
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

## Progress (build-verified)

Framework is complete:
- `version-option`/`verbose-option`/`standard-options` added and exported; `006`
  fixed so `-V` reports the dispatched command's own version.
- `help-option` now prints help and **exits `ex-ok`** (was: printed and fell
  through, which would hang any stdin filter after `--help`). The three
  programmatic help callers (`split-merge`, `mfod`, `radio`) switched to
  `print-command-help` so they keep their own exit codes.

**34 / 64 built commands** now expose the full `-h/-v/-V` trio (was 11):
ansi-test, dedup, fpm, mfod, new-password, one-of, radio, religion,
remove-duplicate-files, split-merge (pre-existing/earlier), plus the homogenized
batch: buzzword, diss, entropy, programmer, nls, capitalize, downcase,
departement, insulte, random, lrev, revlines, add-cookie, add-paths,
batch-emerge, extend-identifiers, get-directory, group-files, merge, pjb-diff,
pseudo-pop, sleep-schedule, macosx-port-uninstall-recursively.

Pattern used: add `(options "<name>" (standard-options))` and call
`parse-options` at the top of `main` (collecting positional operands via the
undefined-argument handler for commands that take arguments). Also fixed
`merge`, which was un-dispatchable (single-colon `:main` to a non-exported
`MAIN`).

### Remaining (still missing the trio)

- **Have their own fix issue — homogenize there:** cddb-to-tag (030),
  check-surface (031), clean-bd-archive (013), cookie-diff (023),
  edit-comments-of-ogg (044), hexbin (043), html-make-image-index (040), lc
  (041), menu (011), pic-resize (033), rstuml (034), split-dir (035),
  substitute (036), surveille-host (037), surveille-web-pages (014),
  svn-locate-revision (015), text (042).
- **Ad-hoc `-h` parsers to migrate (no dedicated issue):** bin-to-c-array,
  clean-name, llen, when, commands, cookie, cookie-loop, cookie-merge.
- **Misc (no dedicated issue):** clar, rotate (manual `-V` in `main`), columnify,
  kwic (filters), memo (own option DSL), script-test.
