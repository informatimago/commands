---
id: 034
title: `rstuml`: debug prints corrupt output + inverted wget caching
severity: medium
commands: [rstuml]
labels: [bug, partial]
status: open
---

# `rstuml`: debug prints corrupt output + inverted wget caching

**Severity:** medium  **Commands:** `rstuml`  **Labels:** bug, partial

- Stray `(print ...) (finish-output)` at lines 45 and 85 write to
  `*standard-output*`, **corrupting the generated RST** on the stdin path. Remove.
- `get-resource` (46): `(or (uiop:run-program ... :output nil) (progn ...))` —
  `uiop:run-program` returns `nil` on success, so the `or` always falls through;
  the success/caching logic is inverted and wget failure isn't handled (no
  `:ignore-error-status`, so a failed wget aborts). Rework.
- `process-uml` regex requires a trailing options group, so a `.. UML kind/name`
  line with no options is silently dropped — make options optional.
- Dead first `*base-url*` (17, overridden at 18). Add `(command ...)` doc and
  `-h/-v/-V`.
