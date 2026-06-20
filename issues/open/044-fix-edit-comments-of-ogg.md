---
id: 044
title: `edit-comments-of-ogg`: redefines split-string, run-program misuse, fragile char literals
severity: medium
commands: [edit-comments-of-ogg]
labels: [bug, partial]
status: open
---

# `edit-comments-of-ogg`: redefines split-string, run-program misuse, fragile char literals

**Severity:** medium  **Commands:** `edit-comments-of-ogg`  **Labels:** bug, partial

- Redefines framework `split-string`/`string-replace`/`copy-stream`/
  `stream-to-string-list` (82+) with different signatures → conflicts/redefinition
  warnings with the cesarum versions used via SCRIPT.
- `uiop:run-program` misuse: `:output :stream :wait nil` (267-269, 420-423) and
  `commit-comments` `:output :string :wait nil` (428) — contradictory; use
  `:wait t` + `:output :string/:lines`, or read `process-info-output` (epic 004b).
- `(character '\n)`, `(character '\p)`, `'\1`..`'\9` (302-413) are clisp reader
  quirks; replace with `#\n` etc.
- `(read-line nil "q")` (297) passes `"q"` as eof-error-p (truthy) — logic bug.
- `last-*` vars (159-169) used before being set on the first file.
- Add `-v/-V` (only ad-hoc `-h` today); expose `*program-version*`.
