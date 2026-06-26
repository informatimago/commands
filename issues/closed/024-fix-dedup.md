---
id: 024
title: `dedup`: wrong :main package (won't dispatch) + unguarded delete
severity: high
commands: [dedup]
labels: [bug, data-loss-risk]
status: FIXED - redesigned around shared Trash framework; verified end-to-end
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

By default, move the files to be deleted to a Trash directory (with
care to avoid overriding homonym files in Trash.  The Trash directory
may be system defined (eg. on macOS), or may be passed in an option
--trash $dir or have a default value (XDG based). --help should tell
where the Trash is.
There should be an option --empty-trash for non-system trashes.

This command help should explicit clearly what it does.  Is it not a
duplicate of remove-duplicate-files? If they're doing 90% the same
thing, perhaps it's better to merge them and use options of the
variants?

## Resolution (verified against a real build)

Decision (confirmed with maintainer): keep `dedup` and `remove-duplicate-files`
as two separate commands, but factor the shared 90% into one framework helper.

Done:

- New Trash framework in `script.lisp` (exported from `packages.lisp`):
  `default-trash-directory` (macOS `~/.Trash`, else `$XDG_DATA_HOME/Trash/files/`),
  `trash-file` (collision-safe rename, cross-filesystem copy+delete fallback),
  `empty-trash` (refuses the system Trash), `dispose-of-duplicates`, and the
  shared CLI `trash-disposal-options` + `dispose-duplicates-command`.
- `dedup` rewritten as a thin client: correct package (dropped the bad `:main`
  and `(in-package "SCRIPT")`); reads `KEY<2 spaces>PATH` groups from stdin
  keeping the **first** seen; **malformed lines are ignored** (no more
  `(+ 2 nil)` crash); disposes via the shared driver.
- **Safe by default**: duplicates are moved to the Trash, not deleted.
  `--dry-run` lists only; `--trash DIR` overrides; `--delete` deletes
  permanently; `--empty-trash` empties a non-system Trash; `--help` prints the
  resolved Trash directory; `-v/-V` added.

Verified end-to-end: default trash-move (keeps first), Trash homonym collision
(`b.txt` -> `b.txt.1`), `--dry-run`, `--delete`, `--empty-trash`, and malformed
input all behave correctly.  See issue 025 for the `remove-duplicate-files` half.
