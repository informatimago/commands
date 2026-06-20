---
id: 031
title: `check-surface`: load-time asserts, --read unimplemented, inverted phase flags
severity: medium
commands: [check-surface]
labels: [bug, partial]
status: open
---

# `check-surface`: load-time asserts, --read unimplemented, inverted phase flags

**Severity:** medium  **Commands:** `check-surface`  **Labels:** bug, partial

- Top-level `(progn (assert ...))` test block (166-173) runs at load time
  (issue 005); stray `(defun m () (main '("-e" ".")))` (~583) would crash if called.
- `--read` (564) is `(error "not implemented yet")` — implement or remove.
- Option semantics inverted: `--only-write` sets `phase :read` (559) and
  `--check` sets `phase :write` (562) — names contradict effect. Fix the mapping.
- `check-surface` (498-513) has three `#-(and)` disabled alternative test calls —
  clean up.
- Add `help-option`/`-v`/`-V` (001); migrate to `parse-options`.
