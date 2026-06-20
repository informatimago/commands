---
id: 023
title: `cookie-diff`: type error writing hash-table + does no diff
severity: high
commands: [cookie-diff]
labels: [bug]
status: open
---

# `cookie-diff`: type error writing hash-table + does no diff

**Severity:** high  **Commands:** `cookie-diff`  **Labels:** bug

## Status: broken / misnamed

- `write-cookies` (35-44) `dolist`s over its `cookies` arg, but `main` (50) passes
  a **hash-table** (from `merge-cookies`) → runtime type error. (Contrast
  `cookie-merge` which correctly `maphash`es.)
- Despite the name, `main` performs a **merge** identical to `cookie-merge` and
  hardcodes output to `/tmp/cookies` (50) — there is **no diff logic at all**.
- `(in-package "SCRIPT")` at line 3 before the `command` form (issue 003); no
  `:main`.

## Fix

Decide the intent:
- If a diff is wanted, implement it (compare two cookie sets, report
  added/removed).
- If not, delete this file in favor of `cookie-merge`.
Either way fix `write-cookies` to `maphash` (or pass a list), remove the stray
`in-package`, and add `-h/-v/-V`.
