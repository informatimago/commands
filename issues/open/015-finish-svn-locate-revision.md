---
id: 015
title: Finish `svn-locate-revision`: pipe-stream stubs + wrong :main package
severity: medium
commands: [svn-locate-revision]
labels: [incomplete, portability]
status: open
---

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
