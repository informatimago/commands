---
id: 000
title: Code review summary & remediation plan
severity: info
commands: []
labels: [epic, documentation]
status: open
---

# Code review summary & remediation plan

**Severity:** info  **Commands:** —  **Labels:** epic, documentation

This directory tracks the findings of a full code review of the 72 Common Lisp
command sources under `sources/commands/`, together with a plan to (a) finish
the incomplete commands, (b) fix the bugs in the others, and (c) homogenize the
command-line option handling.

## How a command should look (the framework)

The framework lives in `sources/script.lisp` / `sources/packages.lisp`
(package `SCRIPT`). A well-formed command:

1. Begins with a `(command :name "foo" :use-systems (...) :documentation "...")`
   form. The macro derives the package **`COMMAND.FOO`** and re-`in-package`s
   into it, so *do not* put `(in-package "SCRIPT")` before it.
2. Defines `main` (or names it via `:main "COMMAND.FOO:MAIN"`).
3. Registers options with `(options "foo" (list (help-option) ...))`.
4. Drives parsing from `main` with `(parse-options *command* arguments #'default)`.

## Severity buckets

| Bucket | Meaning | Count |
|---|---|---|
| incomplete-stub | `main` is `(error "Not implemented yet.")`; real code missing or commented | 6 |
| broken | a concrete bug that errors at runtime / corrupts output / mis-dispatches | ~10 |
| partial | works but has unimplemented branches, portability breakage, or risky logic | ~20 |
| cleanup | dead code, data typos, missing `(command ...)` form, option homogenization | many |

## Incomplete commands (must be finished)

- `box` — see issue 010
- `menu` — see issue 011
- `clean-bd-archive` — see issue 013
- `surveille-web-pages` — see issue 014
- `svn-locate-revision` — see issue 015
- ~~`cpcd`~~ — CANCELLED (issue 012, command removed: nobody uses CDs)

## Cancelled commands (removed from the tree)

`cpcd`, `fetch-pop`, `news-to-mbox`, `euronews` were removed by the maintainer
(obsolete: CD ripping / POP / NNTP / dead streams). Their issues are in
`issues/closed/`.

## Cross-cutting epics

- **001** — CLI homogenization: `-h|--help`, `-v|--verbose`, `-V|--version` everywhere.
- **002** — Add missing `(command ...)` forms to bare commands.
- **003** — Remove the `(in-package "SCRIPT")` anti-pattern (package pollution).
- **004** — Portability: replace clisp-only idioms and `uiop:run-program` misuse.
- **005** — Remove load-time execution (test suites, asserts, width detection).

## Per-command bug issues

020–029 cover the broken commands; 030–044 the partial ones; 050 rolls up the
minor cleanups and data fixes.

## Suggested order of work

Note: I've added some NOTES in the issues, and marked some of them
CANCELLED (the command should be git rm'ed).

1. Land the framework helpers from **001** (`version-option`, `verbose-option`)
   and the portability shims from **004** (`run-program`/pipe streams) first —
   most other fixes depend on them.
2. Fix the **broken** commands (020–029): these error on every run.
3. Finish the **stubs** (010–015).
4. Work through the **partial** commands (030–044).
5. Sweep the **cleanup** items (002, 003, 005, 050) and apply 001 to every
   command.
