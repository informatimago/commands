---
id: 025
title: `remove-duplicate-files`: eql hash-table never matches dups (silent, destructive)
severity: high
commands: [remove-duplicate-files]
labels: [bug, data-loss-risk]
status: FIXED - rewritten on shared Trash framework; verified end-to-end
---

# `remove-duplicate-files`: eql hash-table never matches dups (silent, destructive)

**Severity:** high  **Commands:** `remove-duplicate-files`  **Labels:** bug, data-loss-risk

## Status: broken/partial — destructive yet likely a no-op

- `(make-hash-table)` (line 19) uses the default **`eql`** test, but keys come
  from `(read sums ...)` over md5sum output. Freshly-read strings/symbols are
  never `eql`-equal, so **duplicates are typically never detected** — or are
  mis-grouped. Use `:test 'equal` and key on the md5 **string** (don't `read`).
- md5sum output is `hash<2 spaces>name`; the code `read`s the hash then
  `read-line`s + `string-trim " "` the name — brittle for names with spaces.
- Destructive `delete-file` (38) with no `--dry-run`, no `-h`, arbitrary keep order.
- line 9 `(error "Usage: ...")` instead of a clean usage exit.

## Fix

`:test 'equal` + string keys + proper 2-space split; add `-n/--dry-run`
(default safe), `-h`, confirmation; add `-v/-V`. Shares root with issue 013.

## Resolution (verified against a real build)

Rewritten as a thin client of the shared Trash framework introduced for `dedup`
(see issue 024):

- **Duplicate detection fixed**: `md5sum` is now invoked with the explicit list
  of files as arguments (no shell glob), and the output lines are matched back to
  the known pathnames *by position*, so the md5 string is read reliably and
  grouped in an `equal` hash table.  Duplicates are actually detected now, and
  file names containing spaces are handled correctly.
- **Safe by default**: duplicates are moved to the Trash (shared
  `dispose-of-duplicates`); `--dry-run`, `--trash DIR`, `--delete`,
  `--empty-trash`, `-v/-V` and a Trash-aware `--help` all come from the shared
  `trash-disposal-options`.
- Directory arguments are collected through `parse-options`; with no directory
  (and not `--empty-trash`) it prints usage and exits `EX-USAGE` instead of
  `(error "Usage: ...")`.
- Keep order is now deterministic: the first file (in directory order) of each
  duplicate group is kept.

Verified: on a tree with duplicate files (including a name with a space), the
extra copies are detected and moved to the Trash while one copy is kept;
`--dry-run` and `--delete` behave correctly.
