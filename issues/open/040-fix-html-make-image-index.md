---
id: 040
title: `html-make-image-index`: off-by-one prev/next loop yields NIL.html links
severity: medium
commands: [html-make-image-index]
labels: [bug, partial]
status: open
---

# `html-make-image-index`: off-by-one prev/next loop yields NIL.html links

**Severity:** medium  **Commands:** `html-make-image-index`  **Labels:** bug, partial

- The `do*` loop (51-56) is mis-seeded: it skips the first image via `(cddr
  args)`, never sets `previous` correctly, and doesn't clamp `previous`/`next`
  being `nil` — so `(format ... "~A.html" nil)` emits **`NIL.html`** links and the
  prev/next chain is broken.
- Fix: seed so the first page has `previous=nil` (omit left link), pages chain
  correctly, and the last page omits the right link.
- Verify `html:pcdata` accepts a format control string + arg (25).
- Add `-h/-v/-V` (001).
