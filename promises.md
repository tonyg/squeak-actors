---
title: Promises
pagegroup: User Manual
pageorder: 2000
---

Squeak includes an implementation of *promises*, and this library uses
them for [RPC](https://en.wikipedia.org/wiki/Remote_procedure_call)
between actors.

Promises are values representing a "future value or error". Each
promise holds two collections of callbacks: one set to invoke if the
promise "resolves" to a value; and another to invoke if the promise is
"rejected" with an error.

They are useful for representing eventual completions of asynchronous
tasks such as RPC.

Squeak's promises follow the [Promises/A+](https://promisesaplus.com/)
specification as far as is possible within Squeak's unique context.

More information on promises generally:

 - [Wikipedia on Futures and Promises](https://en.wikipedia.org/wiki/Futures_and_promises)
 - [Mozilla Developer Network on JavaScript Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises)
 - [Promises/A+ specification](https://promisesaplus.com/)

### Promises from interactions with Actors

Every non-asynchronous [`ActorRequest`](requests.html) includes a
`Promise` object (specifically, an [`ActorPromise`](#actorpromise)).
The promise is used by the callee to reply to the caller with either a
value or an error, and by the caller to wait for and retrieve the
value or error.

If `p` is a `Promise` object, then

 - if `p isResolved`, then `p wait` (or, equivalently, `p value`) yields its value.
 - if `p isRejected`, then `p errorValue` is its error value; otherwise `p errorValue` is `nil`.
 - if `p` is neither resolved nor rejected, then `p` is *pending*.

### Waiting for a promise to be resolved or rejected

Use `Promise >> #wait`, `Promise >> #waitTimeoutMSecs:`, and
`ActorPromise >> #waitFor:ifTimedOut:` to block until a promise is
either resolved or rejected.

 - `p wait` returns `p`'s value as soon as `p` is resolved, or signals
   the result of `p errorValue` if `p` is rejected.

 - `p waitTimeoutMSecs: ms` waits for at most `ms` milliseconds for
   `p` to become resolved or rejected. It returns `true` if `p` is
   resolved, and `false` if `p` is rejected or the timeout expires.

 - `p waitFor: ms ifTimedOut: aBlock` waits for at most `ms`
   milliseconds for `p` to become resolved or rejected. It returns `p`'s
   value if `p` is resolved, signals `p errorValue` if `p` is
   rejected, or returns `aBlock value` if the timeout expires.

### Adding a continuation to a Promise

Use the operator `>>=` to attach a *continuation* to a `Promise`.

```smalltalk
(some operationYieldingAPromise) >>= [:v | v + 1]
```

In this example, a continuation block `[:v | v + 1]` is attached to
the promise returned by `#operationYieldingAPromise`—call it "promise
A"—and a *new* promise, "promise B" is returned.

{: .warning}
Make sure to use `bindActor` in conjunction with `>>=` if your
continuation is supposed to run "inside" your actor.
[See below.](#where-and-when-do-handlers-run)

When promise A is resolved with a value, the continuation block is
invoked, and its result is used to resolve promise B.

If promise A is rejected, promise B is also rejected (with the error
value from promise A).

The `>>=` operator takes advantage of Smalltalk's left-associativity
of binary operators, allowing "stacking" of continuations:

```smalltalk
(some operationYieldingAPromise)
  >>= [:v | v + 1]
  >>= [:v | v * 99]
```

{: .implementation-note}
The `>>=` operator is punningly, though inaccurately, named after the
monadic bind operator seen in some functional programming languages.
It's not quite a bind operator for the same reason that `then` isn't a
bind operator in [Promises/A+](https://promisesaplus.com/). It's an
odd, reflective hybrid of bind and `fmap`.

### Where and when do handlers run?

By default, callbacks registered with a `Promise` run in the process
that is resolving or rejecting the promise.

In an actor system, this is often not the right thing.

#### The problem

For example, consider the following method on a behavior object:

{: .incorrect}
```smalltalk
doSomethingWith: anActorProxy
    (anActorProxy compute)
      >>= [:result | self pvtHandleResult: result]
```

When the promise returned by `compute` resolves, the continuation
block will execute.

However, the private method of the *calling* actor will run as part of
the *called* actor's process! If the calling actor—that is, the one
implementing `doSomethingWith:`—happens to be running at the same
time, we have a race condition.

#### The solution

Use `BlockClosure >> #bindActor` to transform an 0-ary or 1-ary block
into an equivalent object that ensures that the block will run as part
of the actor that called `bindActor`.

A correct version of the incorrect example above:

{: .correct}
```smalltalk
doSomethingWith: anActorProxy
    (anActorProxy doSomething)
      >>= [:result | self pvtHandleResult: result] bindActor
```

{: .implementation-note}
Could we design a general mechanism that avoids this kind of problem?
What about storing an optional `Process` along with each resolver and
rejecter in a `Promise` instance? If a `Process` slot is non-`nil`,
we'd have some method like `executePromiseCallback: aBlock with:
aValue` on various kinds of `Process` that automatically did the right
thing. If a `Process` slot is `nil`, the callback would be executed on
the process that happened to be running at the time.

### ActorPromise

Every time a [proxy](proxies.html) kicks off a "synchronous" or
"blocking" RPC [request](requests.html), it constructs a promise to
receive the result of the RPC.

It specifically uses instances of `ActorPromise`, which inherits from
the `Promise` class included with Squeak.

`ActorPromise` augments the default `Promise` waiting behavior with
special treatment of `ActorProcess`es, which must continue to receive
system-level messages while they wait.

It relies on the fact that just such a system-level message is the
means by which each request is answered. This ensures that the waiting
`ActorProcess` is woken up when the promise becomes fulfilled.
