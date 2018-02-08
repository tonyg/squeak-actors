---
title: Time and Timers
pagegroup: User Manual
pageorder: 2000
---

### Sending requests after a delay

If an actor wishes to send itself a message immediately, it can of
course send it via `Actor me`.

```smalltalk
Actor me log: 'Here''s a message'.
```

To send a message to oneself at some point in the future, use Squeak's
built-in `future` mechanism with `Actor me`. For example, this will
cause the current actor's behavior to be sent the `log: 'Tick!'`
message in one second (1,000 milliseconds):

```smalltalk
(Actor me future: 1000) log: 'Tick!'.
```

Note that this causes the `log: 'Tick!'` message to be *enqueued*
after 1,000 ms have elapsed - it may be received and processed
arbitrarily far in the future from that moment.

### Waiting for a reply with a timeout

Promises resulting from invoking a [request object](requests.html) are
in fact instances of `ActorPromise`, which augments the built-in
Squeak `Promise` with a new method, `ActorPromise >> #waitFor:ifTimedOut:`.

```smalltalk
anActorProxy someSlowRequest waitFor: 1000 ifTimedOut: [ aSentinelValue ]
```

Sending `someSlowRequest` to an `ActorProxy` starts the request
process as usual, and the designated actor's behavior object starts
executing its `#someSlowRequest` method. Meanwhile, the calling actor
receives an `ActorPromise` instance.

The `ActorPromise` is immediately sent `#waitFor:ifTimedOut:` with a
timeout of 1,000ms and a timeout handler block that returns a sentinel value.

The call to `ActorPromise >> #waitFor:ifTimedOut:` behaves differently
depending on what happens. Its result will be

 - the result of `someSlowRequest`, if that method returns a value
   before the timeout fires;

 - a `BrokenPromise`, if the called actor terminates abnormally before
   the timeout fires; or

 - the result of the timeout handler block, if the timeout fires
   before any of the other two possibilities come to pass.

The timeout handler block can take any action, including signalling an
error, returning nil, returning a sentinel value, or returning any
other useful value.

The special case of a timeout handler block returning nil can be
written `[]`:

```smalltalk
anActorProxy someSlowRequest waitFor: 1000 ifTimedOut: []
```
