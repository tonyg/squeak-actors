---
title: Promises
pagegroup: User Manual
pageorder: 2000
---

 - Promises ([Promises/A+](https://promisesaplus.com/)) for asynchronous RPC

Mention `ActorPromise` and its methods `#wait` and
`#waitFor:ifTimedOut:` (details of the latter in the page on
[time](time.html)).

Mention how Erlang would have given special treatment to
`BrokenPromise` when `exitReason` is `nil`, but that we don't do that
here. I believe Erlang's default is not quite right: when I link to
something, I want to know when it exits, whether the exit is
considered "normal" or not. Special-casing "normal" exits by ignoring
them by default is not the right thing.

`bindActor` and `ActorPromiseContinuation`
