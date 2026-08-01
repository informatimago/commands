# AGENTS.md

Conventions for working in this repository — a collection of personal
Unix commands written in Common Lisp, compiled together into a single
`commands` executable (each command is a symlink to it; see the
`README`).

## Command versioning

Every command declares its version in its `(command … :version "M.m.d")`
form (in `sources/commands/<name>.lisp`).  The framework reports it
through the standard `-V`/`--version` option, so it must be kept
accurate.

The version follows a `Major.minor.patch` scheme (`M.m.d`), and **must
be increased every time the command's source is touched**:

- **`d` (patch)** — increment for *any* modification: a bug fix, a
  refactor, a documentation or comment change, an internal change —
  anything that touches the source at all.
- **`m` (minor)** — increment (and reset `d` to `0`) when the user
  interface changes in a backward-compatible way: a new option, or a
  changed option.
- **`M` (major)** — increment (and reset `m` and `d` to `0`) for a
  backward-incompatible user-interface change, or a big new feature.

In short: every source edit bumps at least the patch digit; a new or
changed option bumps the minor; an incompatible change or a major new
feature bumps the major.
