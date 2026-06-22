---
id: 051
title: Port `period-cookie` (bash) to Common Lisp
severity: low
commands: [period-cookie, cookie, box]
labels: [port, incomplete, enhancement]
status: open
---

# Port `period-cookie` (bash) to Common Lisp

**Severity:** low  **Commands:** `period-cookie` (new), reuses `cookie`/`box`  **Labels:** port, incomplete, enhancement

## Status: not in this repo

`period-cookie` currently lives only as a bash script in `~/bin/period-cookie`
(versioned in the `~/bin` repo). It is the last cookie-related command not yet
ported into this Common Lisp command set (cf. `cookie`, `cookie-loop`,
`cookie-diff`, `cookie-merge`, `add-cookie`). It should become a `period-cookie`
command here, built into the multi-call `commands` binary like the rest.

## Intended behavior

Print a "fortune of the moment" boxed under the shell prompt, **rate-limited** so
it only appears once per period. It is invoked from the interactive PS1 (in the
`~/rc` bash prompt) as `$(period-cookie)`.

- **Rate limiting:** keep a timestamp in `~/.period-cookie`; only emit a new
  cookie when `now - last > PERIOD` (default 10 minutes). Update the timestamp
  when emitting.
- **Source:** the bash version shells out to `fortune`/`cookie`. The port should
  instead reuse this project's own `cookie` command / cookie-file reader rather
  than exec'ing an external `fortune`.
- **Two render styles**, selected by the `COLOR_PROMPT` env var:
  - plain: a `+----…` top border, body lines prefixed with `|  `, a `+----…`
    bottom border, then a trailing blank line;
  - color: each body line printed as an ANSI background-filled bar
    (`black_back`+`cyan`, width `COLUMNS-3`), framed by blank lines.
- **Options:** `-i|--immediate` (skip the rate-limit, always print) and
  `-h|--help`. Per issue 001, also wire up the framework `-h/-v/-V`.

## CRITICAL: write to the controlling terminal, not stdout

Because the command is called from PS1 via `$(period-cookie)`, **command
substitution captures stdout and strips all trailing newlines**. If the box is
written to stdout, its bottom border ends up glued directly onto the prompt
(`+----12:26[user@host …]$`). The bash version avoids this by writing the box to
`/dev/tty` as a side effect, so `$()` captures nothing and the box's trailing
blank line survives. See `~/bin/period-cookie` history (commit
`period-cookie: print box to /dev/tty …`) for the exact bug.

The Lisp port must do the same: render to the controlling terminal
(`/dev/tty`, falling back to stdout only when `/dev/tty` is not writable), **not**
to `*standard-output*`. This is the single most important behavioural constraint
— a naive `(format t …)` port will silently reintroduce the prompt-glue bug.

## Work to do

- Add `period-cookie` to the command set (`command` form first, per issues 002/003).
- Reuse the existing `cookie` reader for the fortune text; reuse `box` (issue 010)
  / the cesarum picture system for the plain frame instead of hand-rolling the
  `+---`/`|  ` strings.
- Implement the timestamp-based rate limit (`~/.period-cookie`, default 10 min).
- Open and write the rendered box to the controlling terminal device; fall back
  to stdout only when the tty is unavailable.
- Implement `-i/--immediate`, plus framework `-h/-v/-V` (issue 001).
- Once shipped, update `~/rc` (the PS1 in `bash/legacy/monolith.bash`) and the
  `~/bin/period-cookie` symlink to point at the new command.
