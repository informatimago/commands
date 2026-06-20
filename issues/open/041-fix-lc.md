---
id: 041
title: `lc`: stream line-terminator conversion is a no-op except on CCL
severity: medium
commands: [lc]
labels: [bug, portability]
status: open
---

# `lc`: stream line-terminator conversion is a no-op except on CCL

**Severity:** medium  **Commands:** `lc`  **Labels:** bug, portability

- `process-stream` (27-37) only `(warn ...)` on non-ccl and ignores `encoding`, so
  stream-mode line-terminator conversion does nothing except on CCL; clisp builds
  an encoding object but the stream path ignores it.
- Implement real re-encoding for sbcl/ccl (e.g. flexi-streams, or set
  `:external-format` on the streams).
- usage uses `*load-pathname*` (52) — nil at run time; use `*program-name*`.
- Returns numeric `1` (36/58) instead of `ex-*`. Add `--help`/`-v`/`-V` (only `-h`
  today).
