---
id: 029
title: `religion`: `~9D` on strings crashes `-l`; duplicate `#\F` place code
severity: medium
commands: [religion]
labels: [bug, data]
status: in-progress - FIXED ~9D->~9A, place dup F->H, removed *debug* t, added (verbose-option); TODO: fix -C copy-pasted docstring
---

# `religion`: `~9D` on strings crashes `-l`; duplicate `#\F` place code

**Severity:** medium  **Commands:** `religion`  **Labels:** bug, data

## Status: partial — `-l` crashes, data typo

- The `-l` header `format`s (lines 764-766) use `~9D` on **string** literals
  (`"Code"`, `"---------"`) → `~D` on a string signals an error in strict CL, so
  `religion -l` errors. Use `~9A`/`~A`.
- Duplicate enum key: line 254 `("F" "In a graveyard or mausoleum.")` and 255
  `("F" "In a bathtub or jacuzzi.")` — the second should be `"G"`. `deftype place`
  collapses the two `#\F`s, so generation/lookup of that place is wrong. (Same
  typo in the commented reference block ~461.)
- line 807 `main` sets `*debug* t`, which makes `parse-options` bypass its clean
  error handler (so bad options dump a raw backtrace). Gate behind a real
  `--debug` flag (same as `one-of`, issue 050).
- line 778 `-C/--copyright` has a copy-pasted "version" docstring; add `-v` (001).
