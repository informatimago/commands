---
id: 043
title: `hexbin`: hardcoded output file + fragile `read`-based hex parsing
severity: low
commands: [hexbin]
labels: [bug, partial]
status: open
---

# `hexbin`: hardcoded output file + fragile `read`-based hex parsing

**Severity:** low  **Commands:** `hexbin`  **Labels:** bug, partial

- Output filename hardcoded to `"binary"` (12), clobbering any existing file with
  `:if-exists :supersede`. Take the output (and input) path as arguments.
- Parses bytes via `read` with `*read-base* 16` (6/9) — any non-hex token is read
  as a symbol and `vector-push-extend`ed, then fails `write-sequence` against an
  `(unsigned-byte 8)` array; values >255 aren't range-checked. Parse tokens and
  range-check 0-255.
- Add a `(command ...)` form (002) and `-h/-v/-V` (001).
