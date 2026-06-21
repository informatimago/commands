---
id: 006
title: Framework: *program-version* is global, so -V reports the wrong version
severity: medium
labels: [epic, framework, bug, cli]
status: closed
commands: []
---

# Framework: *program-version* is global, so -V reports the wrong version

**Severity:** medium  **Commands:** framework  **Labels:** epic, framework, bug, cli

## Problem (found while implementing issue 001)

`SCRIPT:*PROGRAM-VERSION*` is a single global special (script.lisp ~197,
default `"0.0"`). Commands set their version with a **load-time**
`(defparameter *program-version* "x.y.z")` (e.g. `mfod` 1.0.2, `ansi-test`
1.0.2, `one-of` 0.1.2, `clar`, `religion`, `split-merge`, `new-password`).

In the single-image dispatch model, every command's file is loaded into the
same image, so each `defparameter` overwrites the one global. After the build,
`*program-version*` holds whatever the **last-loaded** command set it to.

`dispatch-command` (script.lisp ~727) rebinds `*command*`, `*program-name*`,
`*program-path*`, `*arguments*` for the dispatched command — but **not**
`*program-version*`. So `<cmd> -V` (whether via the new `version-option` from
issue 001 or the pre-existing manual `-V` options) prints the last-loaded
command's version, not the dispatched command's. Commands that never set it
print `"0.0"`.

## Proposed fix

Make the version a per-command slot, bound at dispatch:

1. Add a `version` slot to the `command` class and a `:version` keyword to the
   `command` macro / `register-command` (default `"0.0"`).
2. In `dispatch-command`, bind `*program-version*` to the command's version:
   ```lisp
   (com.informatimago.command.script:*program-version*
     (or (command-version command) "0.0"))
   ```
3. Migrate command files from `(defparameter *program-version* "x.y.z")` to
   `:version "x.y.z"` on their `command` form.

Once done, `version-option` (issue 001) prints the correct per-command version
with no further change.

## Notes

This is pre-existing behavior — the manual `-V` options in `radio`, `split-merge`,
`one-of`, `religion`, `ansi-test`, etc. all share it. The new `version-option`
helper does not make anything worse; it just inherits the same latent bug, which
this issue fixes at the root.

## Confirmed empirically (real build)

Built the dispatcher and ran several `-V` options:

- `new-password -V` prints `0.1.0`, but `new-password.lisp` declares
  `*program-version*` `"1.0.1"`.
- `ansi-test -V` prints `0.1.0`, but `ansi-test.lisp` declares `"1.0.2"`.

The printed value is neither command's declared version — it is the global left
over from whichever command's `defparameter` ran last during the build, exactly
as predicted.  `*program-name*` is correct (dispatch binds it); `*program-version*`
is not.  Confirms the per-command `:version` slot is the right fix.

## Resolution (implemented + build-verified)

Implemented exactly the proposed fix in `sources/script.lisp`:

1. Added a `version` slot to the `command` class (`:initarg :version`,
   `:initform "0.0"`, accessor `command-version`).
2. Added a `:version` keyword to the `command` macro and to `register-command`
   (defaulting to `"0.0"`); documented it in the macro docstring, noting it
   replaces the load-time `(defparameter *program-version* …)` anti-pattern.
3. `dispatch-command` now binds
   `*program-version*` ← `(or (command-version command) "0.0")` alongside the
   other per-command specials.

Migrated every **registered** command that used a load-time
`(defparameter *program-version* …)` to `:version "x.y.z"` on its `command`
form, and removed the load-time writers: `fpm` (0.0.3), `ansi-test` (1.0.2, two
copies — also dropped its load-time `*program-name*` writers), `split-merge`
(0.1.0), `new-password` (1.0.1), `one-of` (0.1.2), `clar` (0.1.2),
`clean-paths` (1.0.2), `mfod` (1.0.2), `radio` (0.1.2).

Not migrated, by design:
- `shell` (027) — deferred per maintainer.
- `menu`, `rotate`, `edit-comments-of-ogg`, `clean-bd-archive` — stubs with **no
  `command` form**, so nothing to attach `:version` to. Their load-time
  `defparameter` is now harmless because dispatch rebinds the special per
  command. (`rotate`'s `"0.0.0"` is what the image's global default happens to
  hold after the build — and it no longer matters.)
- `clean-paths` is migrated in source but is currently commented out of
  `*all-commands*` in `builder.lisp` (issue 032), so it isn't in the build yet.

### Verified against a real build

Rebuilt the dispatcher core (0 failures) and ran `-V` on every built command
that has a version option:

```
fpm          -> fpm version 0.0.3            (was 0.1.0)   OK
ansi-test    -> ansi-test 1.0.2              (was 0.1.0)   OK
split-merge  -> split-merge version 0.1.0                  OK
new-password -> new-password version 1.0.1   (was 0.1.0)   OK
one-of       -> one-of 0.1.2                               OK
clar         -> clar version 0.1.2                         OK
mfod         -> mfod version 1.0.2                         OK
radio        -> radio 0.1.2                                OK
```

Each now reports its **own** declared version (both the manual `format …
*program-version*` options and the new `version-option` helper, so the fix is
style-independent). Also confirmed the shared global is no longer poisoned:
`*verbose*` is `NIL` and the leftover `*program-version*` global is irrelevant
since dispatch rebinds it.
