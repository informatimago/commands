---
id: 038
title: `news-to-mbox`: debug output corrupts mbox; dead arg parser; weekday typo
severity: medium
commands: [news-to-mbox]
labels: [bug, partial]
status: open
---

# `news-to-mbox`: debug output corrupts mbox; dead arg parser; weekday typo

**Severity:** medium  **Commands:** `news-to-mbox`  **Labels:** bug, partial

- `news-to-mbox` (264) `princ`s From/Subject/Date to `*standard-output*`
  (270-277) **and** writes the mbox to the same stream (`main`, 302) → interleaved,
  corrupted mbox. Send the diagnostics to `*error-output*`/`*trace-output*` (under
  `-v`), or drop them.
- `parse-arguments` (247-262) is dead (never called from `main`) and its
  job-struct calls are commented out; wire it in or remove. `main` ignores args.
- Hardcoded `"user@example.com"` and current time instead of parsing `From:`/
  `Date:` headers (TODOs 230/232).
- Weekday array (233) `#("Mon" "Tue" "Wed" "Thi" "Fri" "Sat" "Sun")` — "Thi"
  should be "Thu"; verify the `decode-universal-time` dow mapping (0=Monday).
- Add a `(command ...)` form (002) and `-h/-v/-V` (001).
