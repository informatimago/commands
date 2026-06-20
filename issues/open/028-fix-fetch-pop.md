---
id: 028
title: `fetch-pop`: broken auth checks, always-delete, swapped host/port
severity: high
commands: [fetch-pop]
labels: [bug, data-loss-risk]
status: open
---

# `fetch-pop`: broken auth checks, always-delete, swapped host/port

**Severity:** high  **Commands:** `fetch-pop`  **Labels:** bug, data-loss-risk

## Status: broken

- `pop-log-in` checks `(positive-response-p headline)` (the stale greeting line)
  at lines 243, 248, 250 instead of the freshly-read `answer` after USER/PASS —
  so login success/failure is keyed off the wrong response. Test `answer`.
- line 380 `(fetch-pop (parse-arguments arguments) 'delete-messages)` passes a
  truthy symbol as `delete-messages-p`, so it **always deletes messages from the
  server** with no opt-out. Make deletion an explicit flag.
- `socket-connect` (61) forwards `(port server)` to `usocket:socket-connect`,
  whose signature is `(host port ...)` — arguments are swapped, so it connects to
  host = port-number. Fix the order.
- No `help-option`/`parse-options`: `-h` hits `parse-arguments`' `otherwise` →
  `(error "Invalid option -h")`. Add `-h/-v/-V` (001).
