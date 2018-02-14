---
title: Time and Timers
pagegroup: User Manual
pageorder: 2000
---

### Sending requests after a delay

If an actor wishes to send itself a message immediately, it can use
`Actor me`.

```smalltalk
Actor me log: 'Here''s a message'.
```

The result is a [`Promise`](promises.html) of an eventual reply.

To send a message at some point in the future, use Squeak's built-in
`future` mechanism. For example, this will cause the current actor's
behavior to be sent the `log: 'Tick!'` message in one second (1,000
milliseconds):

```smalltalk
(Actor me future: 1000) log: 'Tick!'.
```

Again, a `Promise` of an eventual reply is the result.

This causes the message to be *enqueued* after 1,000 ms have elapsed -
it may not be received and processed until later.

The same technique works for sending delayed messages to other actors,
too. Here, we send `doSomething` via the `ActorProxy` `p` after a
second has gone by:

```smalltalk
(p future: 1000) doSomething
```

The same technique works for asynchronous, synchronous or blocking
[transient proxies](proxies.html#transient-proxies) for an actor:

```smalltalk
(p async future: 1000) doSomething "Later sends an asynchronous request to p"
(p sync future: 1000) doSomething "Later sends a synchronous request to p"
"NB. the `sync` option is just like '(p future: 1000) doSomething'"
(p blocking future: 1000) doSomething "Later sends a blocking request to p"
```

Confusingly, in all three cases, a `Promise` is returned. The promise
is resolved in different ways depending on whether `async`, `sync`
(the default), or `blocking` is used:

 - For `async`, the promise is resolved with `nil` as soon as the
   asynchronous request is sent on to `p`'s actor.

 - For `sync`, the promise (call it "promise A") is linked to the
   promise resulting from the synchronous call ("promise B"). Once
   promise B, in turn, settles, promise A will take on its state,
   either resolved or rejected.

 - For `blocking`, *the UI process will block* until `p`'s actor
   replies. Once this happens, the promise will be resolved with the
   reply value.

The reason the UI process is involved is that Squeak's
delayed-execution mechanism itself always works via the UI process.

{:. class="warning"}
Because the delayed message send happens on the UI process, and
promise resolution also happens on the UI process, execution of
resolution/rejection handlers also happens on the UI process. Make
sure to use `#bindActor` if a resolution handler needs to execute in a
particular actor's context. See
[here](promises.html#where-and-when-do-handlers-run) for more
information.

### Waiting for a reply with a timeout

Promises resulting from invoking a [request object](requests.html) are
in fact instances of `ActorPromise`, which augments the built-in
Squeak `Promise` with a new method, `ActorPromise >> #waitFor:ifTimedOut:`.

```smalltalk
anActorProxy someSlowRequest waitFor: 1000 ifTimedOut: [ "..." ]
```

Sending `someSlowRequest` to an `ActorProxy` starts the request
process as usual, and the `#someSlowRequest` method starts running in
the other actor. Meanwhile, the calling actor receives an
`ActorPromise`, which is immediately sent `#waitFor:ifTimedOut:` with
a timeout of 1,000ms and a timeout handler block.

The call to `ActorPromise >> #waitFor:ifTimedOut:` behaves differently
depending on what happens. Its result will be

 - the result of `someSlowRequest`, if that method returns a value
   before the timeout fires;

 - a `BrokenPromise`, if the called actor terminates abnormally before
   the timeout fires; or

 - the result of the timeout handler block, if the timeout fires
   before any of the other two possibilities come to pass.

The timeout handler block can take any action, including signalling an
error, returning nil, or returning any other useful value.

The special case of a timeout handler block returning nil can be
written `[]`:

```smalltalk
anActorProxy someSlowRequest waitFor: 1000 ifTimedOut: []
```
