---
id: 010
title: Finish `box`: stub main + crashes at load time
severity: high
commands: [box]
labels: [incomplete, bug]
status: closed
---

# Finish `box`: stub main + crashes at load time

**Severity:** high  **Commands:** `box`  **Labels:** incomplete, bug

## Status: incomplete-stub (and cannot even load cleanly)

`main` (lines 3-7) is `(format t "Not implemented yet.")`. Worse, the rest of the
file has **top-level legacy forms that execute and crash at load**:

- 58-63: `(setf (logical-pathname-translations "PACKAGES") ...)`.
- 65: `(load "PACKAGES:COM;INFORMATIMAGO;...PACKAGE.LISP")` — path won't exist.
- 67-71: `(package:load-package ...)` / `(use-package ...)` — old API, undefined.
- 74-114: `*templates*` built at load time by opening `*box-path*` with
  `:if-does-not-exist :error`.
- 171: `(mapcar (function second) *templates)` — note **`*templates`** missing the
  trailing `*` → unbound symbol; also wrong shape.

## Intended behavior

Draw decorative box-art frames/heads/feet around text; templates are stored as
`;; DATA` comments in the file itself.

## Work to finish

- Delete/neutralize the legacy top-level `setf`/`load`/`use-package` forms (issue 005).
- Port template parsing to the cesarum picture/string systems; load templates
  lazily inside `main`, not at load time.
- Fix the `*templates` typo (171).
- Implement `main`: select a frame and wrap stdin/args.
- Add `(command ...)` form (002) and `-h/-v/-V` (001).

## Resolution (2026-06-22)

`box.lisp` rewritten as a proper, self-contained command and added to
`*all-commands*` in `builder.lisp` (so it is built and a `box` symlink is
generated).

- **Legacy crashes removed.** The top-level `logical-pathname-translations`
  `setf`, the `load "PACKAGES:..."`, the `package:load-package`/`use-package`
  forms and the load-time `*templates*` file read are all gone (issues 005/003).
  Nothing executes at load time except the `command` form and `defparameter`s.
- **Templates embedded + parsed lazily.** The historical `;; DATA` template
  definition (HEADS / FEET / FRAMES) is now carried verbatim in the
  `*box-data*` string and parsed once on first use by `parse-box-data`
  (memoised in `*box-sections*`).  The command no longer reads its own source
  file, so it works from a saved core.  The `*templates`-vs-`*templates*` typo
  is moot (that code path is gone).
- **`main` implemented.**  Reads lines from stdin and frames them:
  - default: a plain ASCII `+--+` box sized to the widest line (`-w` width,
    `-p` pad), matching the classic shell version;
  - `-f/--frame NAME`: a named decorative frame (box, bevel, tape, roll, hand,
    directory) — the `M` line repeats once per text line, the rest of the
    picture is kept as decoration;
  - `-H/--head NAME` / `-F/--feet NAME`: ASCII-art heads/feet above/below;
  - `-l/--list`: list the available frames, heads and feet.
- **Trio (001).** `-h/--help`, `-v/--verbose`, `-V/--version` via
  `standard-options`; `:version "1.0.0"`; `:documentation` on the `command`
  form (002).  Unknown frame/head/feet names exit `ex-usage`; a non-numeric
  `-w`/`-p` exits `ex-software` without drawing.

Build verified: 65 commands load with 0 failures; functionally tested the
default box, sizing, `-f bevel`/`-f tape`, `-H cow`, `-l`, the trio, and the
error/exit-code paths against a saved core.
