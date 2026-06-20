---
id: 025
title: `remove-duplicate-files`: eql hash-table never matches dups (silent, destructive)
severity: high
commands: [remove-duplicate-files]
labels: [bug, data-loss-risk]
status: open
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
