---
id: 013
title: Finish `clean-bd-archive`: stub main + gutted md5 helper
severity: medium
commands: [clean-bd-archive]
labels: [incomplete, data-loss-risk]
status: open
---

# Finish `clean-bd-archive`: stub main + gutted md5 helper

**Severity:** medium  **Commands:** `clean-bd-archive`  **Labels:** incomplete, data-loss-risk

## Status: incomplete-stub

`main` (lines 77-80) is `(error "not implemented yet")`. Even if it called
`clean-bd-archive`, dedup is non-functional: `md5sum-files` (45-47) is stubbed to
`(make-hash-table)` (always empty → nothing ever matches/deletes). The real
md5 code is commented out (9-43) and clisp-only (`EXT:MAKE-PIPE-INPUT-STREAM`).
`*just-print-p*` (7) is defined but never set.

## Intended behavior

Within each subdirectory of a "bd-archive" tree, delete duplicate files keyed by
md5sum.

## Work to finish

- Implement `md5sum-files` portably (the `md5` system is already a dependency, or
  shell out to `md5sum` via `uiop:run-program ... :output :string`).
- Implement `main`: parse a directory arg, `-n/--dry-run` → `*just-print-p*`, `-h`.
- **Destructive**: require `--dry-run` default OFF only with confirmation; default
  to dry-run or print what would be deleted (cf. issues 016/025).
- Add `(command ...)` (002) and `-h/-v/-V` (001).
