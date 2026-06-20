---
id: 027
title: `shell`: options never register (`:options` invalid) + clisp-only socket server
severity: high
commands: [shell]
labels: [bug, portability]
status: open
---

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
