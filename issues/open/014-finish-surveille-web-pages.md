---
id: 014
title: Finish `surveille-web-pages`: unimplemented pipe streams + package pollution
severity: medium
commands: [surveille-web-pages]
labels: [incomplete, portability]
status: open
---

# Finish `surveille-web-pages`: unimplemented pipe streams + package pollution

**Severity:** medium  **Commands:** `surveille-web-pages`  **Labels:** incomplete, portability

## Status: incomplete-stub

`make-pipe-input-stream` and `make-pipe-output-stream` (lines 56-64) are both
`(error "Not implemented yet.")`. Every real path (`get-resource` 183/190,
`send-notice` 202) calls them, so the command can only error.

Also `(in-package "SCRIPT")` at line 53 (before the `command` form) defines these
stubs **into the SCRIPT package** (issue 003), and `data-mime-type` (89-98)
misuses `uiop:run-program :input/:output :stream` as 3 streams (issue 004b).

## Intended behavior

For each task in `~/SURVEILLE-WEB.DATA`, fetch pages, compare MD5 checksums, and
email a MIME notice when content changed.

## Work to finish

- Implement the pipe-stream helpers portably (issue 004d) **or** rewrite
  `get-resource`/`send-notice`/`data-mime-type` to `uiop:run-program`/
  `launch-program` with `process-info-*` streams.
- Remove `(in-package "SCRIPT")`; move `command` first (003).
- Replace the ad-hoc `-v` member check with framework `-h/-v/-V` (001).
