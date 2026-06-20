---
id: 032
title: `clean-paths`: macOS-only structs, undefined stat accessors, (trace) left in
severity: medium
commands: [clean-paths]
labels: [bug, portability]
status: open
---

# `clean-paths`: macOS-only structs, undefined stat accessors, (trace) left in

**Severity:** medium  **Commands:** `clean-paths`  **Labels:** bug, portability

- CFFI `struct stat`/`dirent` use **Darwin/BSD layout** (143-218); on Linux/glibc
  the structs are wrong → garbage/crash. Provide Linux definitions guarded by OS
  features, or use `osicat`/`sb-posix`.
- lines 337-341 call `stat-atime`/`stat-mtime`/`stat-ctime`, but the `defcstruct`
  defines `atimespec`/`mtimespec`/`ctimespec` — **those accessors are undefined**.
  Read the `*timespec` `tv_sec`.
- `(trace clean-name)` (539) is a top-level form left in — remove (issue 005).
- `directory-files` (417) calls `pmessage` on every entry — leftover debug spam.
- `s-isfifo` macro (99) expands to undefined `s-iffifo` (typo for `s-ififo`).
- The `#+64-bit-target`/`#-64-bit-target` branches (104-140) are identical.
- `main` (652-711) doesn't use `parse-options`; field 9 and symlink handling
  documented "not implemented". Add `-v/-V`; migrate to `parse-options`.
