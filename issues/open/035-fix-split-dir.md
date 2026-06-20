---
id: 035
title: `split-dir`: `uiop:run-program :arguments` invalid on sbcl + wrong default link mode
severity: medium
commands: [split-dir]
labels: [bug, portability]
status: open
---

# `split-dir`: `uiop:run-program :arguments` invalid on sbcl + wrong default link mode

**Severity:** medium  **Commands:** `split-dir`  **Labels:** bug, portability

- `hardlink`/`symlink`/`move` (121-124) use `(uiop:run-program "ln" :arguments
  (...))` — **`:arguments` is not a uiop keyword** (clisp idiom) → errors on sbcl.
  Use `(uiop:run-program (list "ln" "-f" src dst))`. (Epic 004a.)
- `du` call (55-59) passes pathnames where strings are expected; coerce to
  namestrings.
- line 133 default `collate` is `symlink`, but the docstring/purpose says
  hard-links — `-H` is needed for the advertised behavior. Fix the default.
- line 141 `(char= #\- (char arg 0))` crashes on empty-string arg.
- Add a `(command ...)` form (002) and `-v/-V` (001).
