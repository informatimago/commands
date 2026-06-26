---
id: 020
title: `fpm`: errors on every run + duplicate *verbose*
severity: critical
commands: [fpm]
labels: [bug]
status: FIXED - removed fatal error line 948 + duplicate *verbose* defvar
---

# `fpm`: errors on every run + duplicate *verbose*

**Severity:** critical  **Commands:** `fpm`  **Labels:** bug

## Status: broken — every invocation errors

`fpm` is otherwise the model framework command (full `-h/-v/-V`, apt/macport/
portage backends), but:

- **`main` line 948: `(error "Not implemented yet.")` runs unconditionally** right
  after `(parse-options ...)` at 947. So even when options dispatch fine, every
  run signals an error; the `ex-usage` on 949 is dead code. **Remove line 948**
  and return the value from `parse-options`.
- `*verbose*` re-`(defvar)`'d at line 300 (the file is `(in-package "SCRIPT")`),
  duplicating the framework special — delete it (issue 003).

## Also (lower priority, track here)

- "Not implemented yet" backend methods: macport `pm-list-packages` (527),
  macport `pm-find-package-with-info` (538), portage `pm-list-packages :required`
  (772); `rpm`/`cygwin` classes effectively unimplemented.
- `unimplemented-pms` subclasses `package` not `package-manager` (795).
- Hardcoded `/home/pjb/bin/fpm-portage-functions.sh` (751).

## Acceptance

`fpm -h`, `fpm -V`, and a real subcommand (e.g. `fpm version`) all succeed on a
supported distro.
