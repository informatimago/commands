---
id: 012
title: Finish `cpcd`: stub main, clisp-only impl commented out
severity: medium
commands: [cpcd]
labels: [incomplete, portability]
status: open
---

# Finish `cpcd`: stub main, clisp-only impl commented out

**Severity:** medium  **Commands:** `cpcd`  **Labels:** incomplete, portability

## Status: incomplete-stub

`main` (line 5) is `(error "Not implemented yet.")`; the whole implementation is
in a commented `#| ... |#` block (lines 9-124).

## Intended behavior

Copy/rip an audio CD: query disc, rip to WAV with cdparanoia, compress to FLAC,
eject; batch over multiple discs. Options `-b/-f/-o`.

## Work to finish

- Port to portable CL: the commented code is clisp-only (`linux:fork` 76,
  `ext:cd`/`ext:exit` 67/80). Use `uiop:run-program`/`uiop:launch-program` and
  `uiop:with-current-directory`.
- The commented `(command :options (list ...))` uses an `:options` keyword the
  current `command` macro does not accept — register options via a separate
  `(options ...)` form instead (same root cause as `shell`, issue 027).
- Add `-h/-v/-V` (001).
