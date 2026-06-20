---
id: 030
title: `cddb-to-tag`: no option parsing; --dry-run/--help unwired
severity: medium
commands: [cddb-to-tag]
labels: [enhancement, partial]
status: open
---

# `cddb-to-tag`: no option parsing; --dry-run/--help unwired

**Severity:** medium  **Commands:** `cddb-to-tag`  **Labels:** enhancement, partial

Has a proper `(command ...)` form but **no `(options ...)`, `help-option`, or
`parse-options`** — `main` just iterates args as directories. `*dry-run*` (54)
exists but nothing sets it; header BUGS (21) asks for `--help`/`--dry-run`.

- Add `(options ...)` with `help-option`, `--dry-run`, `-v`, `-V`; wire
  `parse-options` into `main`.
- Dead legacy: `rename-eric-satie-files` under `#-(and)` contains
  `(error "Not implemented yet.")` (595); `set-flac-metadata`/`verbose-rename-file`
  (570-578) are `(warn "Not implemented yet.")` but unused — drop or implement.
- Only cd-info 0.83 parsed (0.82 unsupported) — implement if needed.


NOTE: I have a collection of flac files from audio CD. They would need to be identified and renamed, and tagged nicely.
However, I wonder if using an algorithmic tool is the way to proceed. Perhaps it's a task for an AI?

Let's defer work on this command for now.
