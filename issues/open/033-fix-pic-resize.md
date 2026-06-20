---
id: 033
title: `pic-resize`: unbound `run-mode` in GIMP batch + broken `=` option test
severity: medium
commands: [pic-resize]
labels: [bug, partial]
status: open
---

# `pic-resize`: unbound `run-mode` in GIMP batch + broken `=` option test

**Severity:** medium  **Commands:** `pic-resize`  **Labels:** bug, partial

- The emitted GIMP Scheme references `run-mode` (110, 124-126) which is **never
  bound** → the batch errors. Bind it to `RUN-NONINTERACTIVE`.
- line 86 `(char= (character "=") (char arg 0))` tests for a leading `=`, but the
  option marker is `-` (handled separately at 84). The `=`-vs-`-` logic is
  confused/broken; also `(char arg 0)` crashes on an empty `arg`.
- line 42 `(defparameter *program-name* "resize")` hardcodes the name, overriding
  the dispatcher's argv-derived `*program-name*` — remove.
- `(exit 1)` (92) instead of `ex-usage`. Add `-v/-V` (001); route messages through
  `perror`/`pmessage`.
