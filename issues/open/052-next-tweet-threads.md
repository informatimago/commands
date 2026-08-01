# 052 — next-tweet: implement thread reading (--enter / --leave)

## Status

open

## Description

`sources/commands/next-tweet-readme.org` documents a thread reading
context: `next-tweet --enter` switches to the thread of the last
displayed tweet (the self-replies of its author, in chronological
order, fetched with the recent search API), and `next-tweet --leave`
returns to the timeline, which is preserved meanwhile.  The state
section also mentions `thread-queue.lisp` and the tracking of the
last displayed tweet and reading context in `state.lisp`.

The current `sources/commands/next-tweet.lisp` only implements
`--help`, `--count`, `--peek` and `--status`; it does not track the
last displayed tweet, has no reading context, and rejects `--enter`
and `--leave` as invalid arguments.

## Work

- Track the last displayed tweet in `state.lisp` (plain call and
  `--peek`).
- Add a `:context` (timeline or thread) to the state; make the plain
  call, `--peek` and `--count` operate on the current context.
- `--enter`: fetch the self-replies of the author of the last
  displayed tweet (conversation_id recent search, 7 day window),
  fill `thread-queue.lisp`, print the thread length (0 and stay on
  the timeline when there is none).
- `--leave`: drop the thread queue, return to the timeline, print
  its remaining count.
- At the end of a thread, print 0 (and a reminder on stderr) until
  `--leave`.
- Update the `--help` output and keep the readme in sync.
