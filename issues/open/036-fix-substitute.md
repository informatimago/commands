---
id: 036
title: `substitute`: `check` prints but never aborts; replacement accumulates
severity: medium
commands: [substitute]
labels: [bug, partial]
status: open
---

# `substitute`: `check` prints but never aborts; replacement accumulates

**Severity:** medium  **Commands:** `substitute`  **Labels:** bug, partial

- The `check` macro (89-100) only **prints** errors; on bad/missing args `main`
  falls through (112) and calls `with-open-file`/`regexp-compile` with `nil`
  filenames → a second uglier error. Make `check` `(return-from main ex-usage)`.
- usage prints `(or *load-pathname* ...)` (99) — `*load-pathname*` is nil at run
  time; use `*program-name*`.
- `main` (120) reassigns `substitution` to `substitute-expression`'s return value
  every line, so the replacement string keeps growing across lines — likely
  unintended; reset per line/match.
- Add `-h/-v/-V` (001).
