---
id: 027
title: `shell`: options never register (`:options` invalid) + clisp-only socket server
severity: high
commands: [shell]
labels: [bug, portability]
status: open
---

NOTE: this command would require more research.  The idea is to
provide several fun shell environments as can be seen in various
movies showing fictionnal computer systems.  It's basically a game,
simulating the shells seen in these movies. Usually, the shells
portrayed are rather irrealist, providing high level, more or less
natural language commands, sometimes with some AI interpretation,
sometimes more stereotyped interactions.  The commands would be
totally inneffective (only simulated within the shell program, no side
effect on the host system).  The "code" collected so far in the
shell.lisp contains the examples of interactions commands and
responses seen in the corresponding movie (and some comments are as
important as defined data).  If research gives more clues, syntax,
language, etc for a given movie shell, it may be integrated,
otherwise, best guesses and "logical" extensions can be provided.

Some movies depict actual systems (eg. linux, programs such as nmap,
emacs, etc), but with some special or specific commands (eg. a command
to pilot an electric grid).  We could simulate bash or some other
'existing shell, and provide emulations of such commands.  Again, no
real side effects, it's just a game, but if we have a console log
saying the grid is down for the West side of the city, so be it!  :-)

So, we may research and make a inventory of movies showing computer
interactions, find what level of depiction we have (just a command, a
longer interaction, whether we can identify the command system
(perhaps some old system was used, so we may have documentation of the
command language in bitsavers.org etc) or infer its logic, and whether
we can implement a shell that can be interesing for the user).

Some data is source code, often only partial because only partially
visible on screen, but it has often been possible to use google to
find the actual sources matching the code shown.


# `shell`: options never register (`:options` invalid) + clisp-only socket server

**Severity:** high  **Commands:** `shell`  **Labels:** bug, portability

## Status: broken / partial

- line 243: the `(command ... :options (list* ...))` form passes an **`:options`
  keyword the `command` macro does not accept** (valid keys: `:name :use-systems
  :use-packages :shadow :main :documentation :bash-completion-hook`). The options
  (`-V`, `-h`, `-ls`, `-r`, `-s`, `-l`) are therefore **never registered**. Move
  them to a separate `(options "shell" (list* ...))` form (as `split-merge` does).
- `run-cine-server` (226-239) uses the clisp `socket:` package → `--listen` is
  clisp-only and won't load on sbcl. Reimplement with `usocket` (already a
  dependency) or `#+clisp`-guard the option (issue 004e). Also `(socket:socket-server
  7767)` hardcodes the port, ignoring the `port` option arg.
- line 219 `(setf sp (elt *shells* 4)) ;; TEST` forces the Matrix shell, defeating
  `--random` — remove.
- Add `-v/--verbose` (001).

