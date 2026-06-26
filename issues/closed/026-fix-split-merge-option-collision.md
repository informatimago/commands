---
id: 026
title: `split-merge`: `-l` used for both --left and --right
severity: medium
commands: [split-merge]
labels: [bug, cli]
status: FIXED - right option rekeyed -r/--right
---

# `split-merge`: `-l` used for both --left and --right

**Severity:** medium  **Commands:** `split-merge`  **Labels:** bug, cli

## Status: broken option

`split-merge` is a clean framework command with `-h/-v/-V`, but:

- line 163 `("left" "-l" "--left")` and line 167 `("right" "-l" "--right")` both
  bind **`-l`**. `add-option` overwrites the `-l` key to mean *right*, so the
  short flag for left is unreachable and `-l` sets `*right-path*`. Copy-paste bug
  — change the right option to `("right" "-r" "--right")`.

Minor: drop the redundant local `*verbose*`/`*program-version*` redefinitions
(lines 39/43) in favor of the framework specials (issue 003).
