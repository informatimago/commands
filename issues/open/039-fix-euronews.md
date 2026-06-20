---
id: 039
title: `euronews`: player disabled + returns after first selection; dead data source
severity: low
commands: [euronews]
labels: [bug, partial]
status: open
---

# `euronews`: player disabled + returns after first selection; dead data source

**Severity:** low  **Commands:** `euronews`  **Labels:** bug, partial

- line 171 the actual player call is commented out via `#-(and)` (RealPlayer8),
  and line 170 prints "Please update euronews with a new player!" — so it never
  plays anything. Wire up a current player (mpv/vlc).
- line 176 `(return-from main 1) ; TODO` returns after the first valid selection,
  breaking the menu loop — remove.
- Scrapes `euronews.net/create_html.php` (81), a long-dead endpoint — update or
  retire the command.
- `:output :stream :wait nil` misuse (79-85, epic 004b). Usage lists `po` but the
  language table has no Portuguese. Add `-h/-v/-V` (001).
