---
id: 015
title: Finish `svn-locate-revision`: pipe-stream stubs + wrong :main package
severity: medium
commands: [svn-locate-revision]
labels: [incomplete, portability]
status: closed
---

## Resolution (implemented + verified)

- Implemented `make-pipe-input-stream` portably **locally** in the command's own
  package (`uiop:run-program … :output :string` served from a string stream).
  A framework-level version was attempted but collides with
  `surveille-web-pages`'s `(in-package "SCRIPT")` pollution (issue 014/003), so
  it stays local for now; fold into the framework when 014 removes that
  pollution. `make-pipe-output-stream` was unused here and dropped.
- `:main`, the stray `(in-package "SCRIPT")`, and the load-time
  `(defparameter *verbose* t)` were already fixed earlier (issue 005 work).
- Added `-u/--url` (default `"."`) plus the standard `-h/-v/-V` via
  `(options …)`/`parse-options`; revision operands collected through the
  undefined-argument handler. `locate-revision` now uses `*url*` instead of the
  hardcoded `"."`. Added `:version "1.0.0"` and a `:documentation`.

Verified against a freshly created svn repo + working copy: `-V` prints
`1.0.0`; `-h` lists the options and documentation; `svn-locate-revision -u <wc>
1` runs the real svn/XML path (no more "Not implemented yet") and exits 0 with
no mergeinfo. Build: 0 failures.

# Finish `svn-locate-revision`: pipe-stream stubs + wrong :main package

**Severity:** medium  **Commands:** `svn-locate-revision`  **Labels:** incomplete, portability

## Status: incomplete-stub

`make-pipe-input-stream`/`make-pipe-output-stream` (lines 46-56) are
`(error "Not implemented yet.")`; every svn call (`candidate-branches` 61,
`svn-info` 91, `svn-revision` 104) routes through them, so the command errors
immediately.

Additional bugs:
- `:main "SVN-LOCATE-REVISION:MAIN"` (line 41) names a package the framework
  never creates — the macro derives **`COMMAND.SVN-LOCATE-REVISION`**, so
  `dispatch-command`'s `read-from-string` of the main symbol fails. Use
  `"COMMAND.SVN-LOCATE-REVISION:MAIN"` or drop `:main`.
- `(in-package "SCRIPT")` at line 34 (issue 003).
- `(defparameter *verbose* t)` (43) defaults verbose ON and shadows the framework.
- `(locate-revision "." revision)` (170) hardcodes the working-copy URL `"."`.

## Work to finish

- Implement pipe streams portably (004d) or use `uiop:run-program :output :string`.
- Fix `:main`; remove the stray `in-package`.
- Add a repo-URL option; add `-h/-v/-V` (001).
