---
id: 013
title: Finish `clean-bd-archive`: stub main + gutted md5 helper
severity: medium
commands: [clean-bd-archive]
labels: [incomplete, data-loss-risk]
status: closed
---

## Resolution (implemented + verified)

Rewritten on top of the shared Trash framework (the same one used by
dedup/remove-duplicate-files), which also resolves the data-loss-risk:

- `md5sum-files` (the stub returning an empty hash table) and the clisp-only
  commented code are gone. `files-md5-groups` computes real md5 groups via
  `md5sum(1)` (args, no shell; matched back by position so spaces in names are
  safe). `bd-archive-duplicate-groups` walks each immediate subdirectory of
  BASE (`uiop:subdirectories`) and groups duplicates within it.
- `main` parses a BASE operand and the standard trash options
  (`trash-disposal-options`: `--dry-run`, `--trash DIR`, `--delete`,
  `--empty-trash`, plus `-h/-v/-V`), then drives `dispose-duplicates-command`.
  **Disposal now defaults to moving duplicates to the Trash** instead of
  `delete-file`, so the command is no longer a silent data-loss hazard;
  `--delete` opts into real deletion and `--dry-run` previews.
- Replaced the load-time `(defparameter *program-version*)` with
  `:version "1.0.2"` on a `(command …)` form; added `:documentation`.

Verified on a synthetic bd-archive (subdirs with 3- and 2-file duplicate
groups + a unique file): `--dry-run` lists exactly the non-kept duplicates and
changes nothing; default run keeps the first of each group (and uniques) and
moves the rest to `$XDG_DATA_HOME/Trash/files`; `--delete` removes them;
no-args prints help and exits 64. Build: 0 failures.

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
