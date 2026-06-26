---
id: 004
title: Portability: replace clisp-only idioms & uiop:run-program misuse
severity: high
commands: [split-dir, surveille-host, surveille-web-pages, edit-comments-of-ogg, shell, text]
labels: [epic, portability, bug]
status: open
---

# Portability: replace clisp-only idioms & uiop:run-program misuse

**Severity:** high  **Commands:** `split-dir`, `surveille-host`, `surveille-web-pages`, `edit-comments-of-ogg`, `shell`, `text`  **Labels:** epic, portability, bug

## Problem

Many commands use clisp-specific idioms or misuse `uiop:run-program`, so they
fail on the sbcl build that the README says is now the host.

### 4a. `uiop:run-program :arguments` (invalid keyword on uiop)
`split-dir.lisp` `hardlink`/`symlink`/`move` (lines ~121-124) call
`(uiop:run-program "ln" :arguments (...))`. `uiop:run-program` has no
`:arguments` keyword (that is the clisp `ext:run-program` API) → errors on sbcl.
Fix: `(uiop:run-program (list "ln" "-f" src dst))`.

### 4b. `:input/:output :stream :wait nil` treated as 3 streams
The clisp `ext:run-program` returns separate streams; `uiop:run-program` with
`:input :stream`/`:output :stream` returns a **process-info object**, and you
must read `uiop:process-info-input/-output`. Misused in:
- `surveille-host.lisp` `notificate-state-change` (sendmail) ~118-138
- `surveille-web-pages.lisp` `data-mime-type` ~89-98 (`(multiple-value-bind (io in out) ...)`)
- `edit-comments-of-ogg.lisp` ~267-269, 420-432 (`:output :stream/:string :wait nil` contradiction)

### 4c. `(= 0 (uiop:run-program ...))` for exit status
`surveille-host.lisp:110` pings with `(= 0 (uiop:run-program "ping ..."))`.
`uiop:run-program` raises on non-zero exit and does not return the code. Use
`:ignore-error-status t` and check the third value / `:exit-code`.

### 4d. Unimplemented portable pipe streams
`surveille-web-pages.lisp` and `svn-locate-revision.lisp` both define
`make-pipe-input-stream`/`make-pipe-output-stream` as `(error "Not implemented
yet.")` (see issues 014/015). Provide a portable implementation in `script.lisp`
on top of `uiop:launch-program` + `process-info-input/-output`, or rewrite call
sites to `uiop:run-program ... :output :string`.

### 4e. clisp-only packages
- `shell.lisp` `run-cine-server` uses `socket:` (clisp) — guard with `#+clisp`
  or reimplement with `usocket` (already a dependency).
- `text.lisp` binary mode is `#+clisp` only (`setf stream-element-type`), so the
  filter is a broken no-op on sbcl.
- `batch-emerge.lisp` errors on non-ccl/sbcl by design; verify.
- commented `linux:fork`, `ext:cd`, `ext:exit` in `cpcd`/`radio`/`shell` history.

## Fix / acceptance criteria

- Centralize subprocess plumbing on `uiop:run-program` / `uiop:launch-program`.
  Consider extending `SCRIPT:run-program` (script.lisp:1035) and adding a
  `SCRIPT:run-program/string` and pipe-stream helpers so commands don't re-roll
  process handling.
- Each affected command runs correctly on sbcl (no `:arguments`, no
  return-value-as-stream, exit codes checked).
