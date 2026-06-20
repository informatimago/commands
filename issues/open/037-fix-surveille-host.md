---
id: 037
title: `surveille-host`: broken ping exit check + never notifies real transitions
severity: medium
commands: [surveille-host]
labels: [bug, portability]
status: open
---

# `surveille-host`: broken ping exit check + never notifies real transitions

**Severity:** medium  **Commands:** `surveille-host`  **Labels:** bug, portability

- `get-current-state` (110): `(= 0 (uiop:run-program "ping ..." ...))` —
  `uiop:run-program` raises on non-zero exit and doesn't return the code, so
  offline detection is broken. Use `:ignore-error-status t` and check the exit
  code (epic 004c).
- `notificate-state-change` (118-138) opens sendmail with `:input :stream :wait
  nil` and writes to the **return value** — must use `uiop:process-info-input`
  and `wait-process` (epic 004b).
- `check` (142-147) only notifies when the previous state was `:unknown`, so real
  on-line↔off-line transitions after the first never notify (and it spams on the
  first observation). Fix to notify on every genuine transition.
- Hardcoded host/email lists (179-183) — make configurable; add `-h/-v/-V`.
