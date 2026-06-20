---
id: 050
title: Minor cleanups: dead code, data typos, debug flags
severity: low
commands: []
labels: [cleanup, data]
status: open
---

# Minor cleanups: dead code, data typos, debug flags

**Severity:** low  **Commands:** —  **Labels:** cleanup, data

A roundup of small, low-risk fixes found during review. Each is a checkbox.

- [ ] `one-of` line 124: `main` hardcodes `*debug* t`, defeating `parse-options`'
      clean error handling. Remove or gate behind `--debug` (cf. `religion`, 029).
- [ ] `one-of` `shuffle-list` (117) has a wrong termination test and is dead code
      (`main` uses `shuffle`) — delete.
- [ ] `departement` line 121: the Paris row `("75" "Paris" "Île-de-France" nil
      nil)` is field-shifted vs the `(numero nom préfecture sous-préfectures
      région)` schema. Fix the row; tighten the loose `search`-over-all-fields
      lookup (166-173).
- [ ] `programmer`: `*gn*` (32-44) is defined but unused (`main` only uses
      `*sentences*`); the `"TODO"` at line 42 is **data, not a stub**. Use `*gn*`
      or delete it.
- [ ] `capitalize`: `split-string-if`/`split-string` (7-28) are dead; the header
      documents dash-replacement behavior `main` doesn't implement. Remove dead
      code or implement the documented behavior.
- [ ] `diss`: local `split-string`/`string-justify-left` (3/31) duplicate and may
      conflict with the cesarum versions exported via SCRIPT — use the library
      ones or shadow explicitly.
- [ ] `pseudo-pop` & `cookie-loop`: `(read-line)` with no EOF args crashes on
      client/stdin EOF — use `(read-line stream nil nil)` and exit cleanly.
- [ ] `macosx-port-uninstall-recursively`: dead `make-pathname*` (5-39) and a
      `(catch 'done ...)` (70) with no matching `throw`; risky self-recursion at
      end of `port`. Clean up; add an OS guard.
- [ ] `group-files` (52-54) and `nls`/`get-directory`: fragile fixed-offset name
      parsing — make robust; `get-directory` `destructuring-bind` (80) errors on 0
      args (no `-h`).
- [ ] `merge`: reconcile the hand-written `defpackage` (40-43) vs the `command`
      macro's generated package — pass `:shadow`/`:use-packages` to `command`
      instead.
- [ ] `pjb-diff`: dead `options` struct + `:shadow ("OPTIONS")` (9-12); no arity
      check in `main` (56). Wire the struct into `load-file` (real
      `--remove-spaces`/`--ignore-case`) or delete it.
- [ ] `add-cookie`/`lc`/`substitute`: usage text built from `*load-pathname*`
      (nil at run time) — use `*program-name*`.
- [ ] `bin-to-c-array`: unused `*element-type*`/`*size-type*` params (7-8); help
      branch returns `nil` instead of `ex-ok` (126).
- [ ] `rotate`: empty stdin crashes `slurp` (`reduce #'max` over empty list) — add
      `:initial-value 0`.
- [ ] `script-test`: diagnostic demo, not a real command — keep it out of the
      production dispatch table; drop the `(print ...)` in `environment`.
