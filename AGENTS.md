
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

# commands — agent directives

All the commands are compiled into a **single Lisp image** (`bin/commands`),
dispatched on the invocation name (argv[0]).  Long-form knowledge, build
anatomy and pitfalls: [doc/project-knowledge.md](doc/project-knowledge.md).

## Adding or changing a command

- A new command goes in **three places**, or it does not exist:
  `sources/commands/<name>.lisp`, `ALL_PROGRAMS` in `Makefile` (alphabetical),
  `*all-commands*` in `builder.lisp`.
- The `(command :use-systems …)` form is the **first form of the file**
  (only comments before it).  Anything defined before it lands in the shared
  `SCRIPT` package and can clobber the framework (issue 003); and the builder
  reads up to that form with `*read-eval*` NIL, so it must precede any `#.`.
- Declare **every external system in `:use-systems`** — that is what gets it
  preloaded before the commands are compiled; a system that first loads inside
  a command's `compile-file` leaks its load-time warnings into that command's
  compilation unit and fails the build.
- **No load-time execution** (issue 005): a top-level form runs at build time,
  inside the image shared by all commands.  No I/O, subprocesses, `trace`,
  asserts, or environment sniffing in initforms; compute at run time in `main`.
- Options: `(options "<name>" (option …) … (standard-options)
  (bash-completion-options))`; `main` takes the argument list, calls
  `(parse-options *command* arguments)`, returns an `ex-*` code.
  `-h`/`--help` and `-V`/`--version` must exit 0 (issue 001); the version goes
  in the `command` form's `:version` slot.
- The build treats **any compile warning as a failure**; `make` must end with
  zero failures.

## Housekeeping

- Issues live in `issues/open/NNN-slug.md` (front-matter: id, title, severity,
  commands, labels, status) and move to `issues/closed/` when resolved.

