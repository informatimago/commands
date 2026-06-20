---
id: 042
title: `text`: binary filter is clisp-only, broken/no-op on sbcl
severity: medium
commands: [text]
labels: [bug, portability]
status: open
---

# `text`: binary filter is clisp-only, broken/no-op on sbcl

**Severity:** medium  **Commands:** `text`  **Labels:** bug, portability

- The octet filter reads `(unsigned-byte 8)` from `*standard-input*`, but the
  `setf stream-element-type` to binary is `#+clisp` only (40-42). On sbcl stdin is
  a character stream, so `read-sequence` into an octet buffer errors or
  misbehaves — the command is clisp-only.
- Replace the `#+clisp` hack with a portable binary-stdin approach (uiop, or
  read from fd 0 in binary). Add a `(command ...)` form (002) and `-h/-v/-V`.
