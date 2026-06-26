---
id: 014
title: Finish `surveille-web-pages`: unimplemented pipe streams + package pollution
severity: medium
commands: [surveille-web-pages]
labels: [incomplete, portability]
status: closed
---

## Resolution (implemented + verified)

Rewrote the I/O on `uiop:run-program`, removing the pipe-stream stubs entirely
(so there is nothing left to "implement" and no SCRIPT collision):

- Deleted the `make-pipe-input-stream`/`make-pipe-output-stream` stubs and the
  `(in-package "SCRIPT")` pollution; the `command` form is now first, with
  `:version "1.0.1"` and `:documentation`, `:use-systems (:babel :md5)`.
- `get-resource`: `:page` runs `lynx -dump` (`:output :string`, split to lines);
  `:data` runs `wget -q -O -` capturing raw bytes through an ISO-8859-1 round
  trip (octet <-> char) so binary data is exact.
- `send-notice`: builds the whole MIME message with `with-output-to-string`,
  then pipes it to `sendmail` via `uiop:run-program :input (string stream)` —
  no output pipe needed.
- `data-mime-type`: replaced the bogus 3-stream `run-program` with a single
  `file -ib -` call fed the bytes via stdin (ISO-8859-1) — returns the type.
- `main`: framework `-h/-v/-V` via `(options …)`/`parse-options` (replacing the
  ad-hoc `(member "-v" …)`), uses `*verbose*`, and prints a friendly message +
  `ex-noinput` when `~/SURVEILLE-WEB.DATA` is absent (instead of a backtrace).

Bonus pre-existing bugs fixed while finishing it:
- `compute-checksum` `:page` called `babel:octets-to-string` on a **string**
  (type error, always crashed) → `babel:string-to-octets`.
- the boundary `hostname -f` call used `:output :string :wait nil` (contradictory)
  → dropped `:wait nil`.

Verified against a local HTTP server: `:data` fetch preserves 29 binary bytes
incl. NULs; `:page` fetch renders via lynx; `data-mime-type` returns
`application/pdf…` for PDF bytes; `write-base64-sequence` output is byte-for-byte
identical to coreutils `base64`; a full run with a matching stored checksum
reports "0 changes" and exits 0, and after changing the page reports "1 change"
and assembles+invokes sendmail (only the absent MTA stops delivery here). `-V`
prints 1.0.1; `-h` lists the trio; missing data file → exit 66. Build: 0 failures.

NOTE: a portable `make-pipe-input-stream` could now be promoted into the
framework (script.lisp) and shared with `svn-locate-revision` (which carries a
local copy), since this command no longer defines a conflicting one.

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
