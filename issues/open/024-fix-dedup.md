---
id: 024
title: `dedup`: wrong :main package (won't dispatch) + unguarded delete
severity: high
commands: [dedup]
labels: [bug, data-loss-risk]
status: open
---

# `dedup`: wrong :main package (won't dispatch) + unguarded delete

**Severity:** high  **Commands:** `dedup`  **Labels:** bug, data-loss-risk

## Status: broken

- `:main "COM.INFORMATIMAGO.COMMAND.DEDUP:MAIN"` (line 9) names a package the
  framework never creates — `command-package-name` yields **`COMMAND.DEDUP`**. At
  dispatch, `read-from-string` of the main symbol fails → the command won't run.
  Use `"COMMAND.DEDUP:MAIN"` or drop `:main`.
- `(in-package "SCRIPT")` at line 2 (issue 003).
- **Destructive with no guard**: line 29 `(mapc 'delete-file (cdr vs))` deletes
  files with no `--dry-run`, no confirmation, no `-h`.
- line 18-20: lines lacking the `"  "` (double-space) separator make `p` nil →
  `(+ 2 nil)` errors. Handle malformed input.
- `push` ordering means the **last**-seen file is kept, not the first — confirm
  intent.

## Fix

Correct `:main`; remove `in-package`; add `-n/--dry-run` (default safe), `-h`,
confirmation; handle malformed lines; add `-v/-V`.
