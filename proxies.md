---
title: Proxies
pagegroup: User Manual
pageorder: 2000
---

Instances of `ActorProxy` override `doesNotUnderstand:`, `perform:`
and `perform:withArguments:` in order to provide a convenient way of
sending [requests](requests.html) to `Actor`s.

Methods that go through `doesNotUnderstand:` etc. return
[`ActorPromise`](promises.html) instances, which can be used to
[pick up replies](promises.html#promises-from-interactions-with-actors).

### Switching between an Actor and its ActorProxy

Every `Actor` has exactly one `ActorProxy`, and every `ActorProxy` is
associated with exactly one `Actor`.

{: .implementation-note}
Strictly speaking, `ActorProcess`es also have proxies: see the comment
on `ActorProcess >> #proxy`.

Both `Actor` and `ActorProxy` have methods `actor` and `proxy`.
Calling `actor` always returns the `Actor` of a matched pair; calling
`proxy` always returns the `ActorProxy`.

```smalltalk
           anActor actor == anActor.
     anActor proxy actor == anActor.
      anActorProxy proxy == anActorProxy.
anActorProxy actor proxy == anActorProxy.
```

### ActorProxy and dependents

Each proxy implements `addDependent:` and `removeDependent:` by
asynchronously forwarding them to its backing actor. Similarly, each
proxy forwards calls to `update:` and `update:with:` to its backing
actor.

### Transient proxies

`ActorProxy` instances are intended to be long lived—at least as long
lived as their backing actor.

For this reason, `ActorProxy` inherits from `Object`, including all
the behavior of `Object`. This causes quite a bit of clutter in the
interface of `ActorProxy`!

For example, consider calling `windowTitle` on an `ActorProxy`.

```smalltalk
myProxy windowTitle.
```

The intention here is for the request to be forwarded on to the
backing actor's behavior object. Unfortunately, `Object` implements
`windowTitle`. The request will thus be answered locally at the proxy,
without ever being sent on to its actor.

To avoid this problem, proxy objects allow creation of *transient*
proxy objects which have a much smaller interface and can therefore
easily forward more requests than an ordinary proxy can:

```smalltalk
myProxy async windowTitle.
myProxy sync windowTitle.
myProxy blocking windowTitle.
```

The three variants, `async`, `sync`, and `blocking` are discussed
[below](#interaction-patterns).

{: .warning}
Think twice before storing a `TransientActorProxy` instance in a
variable!

The class `TransientActorProxy` inherits from `ProtoObject`, not
`Object`. While the interface of `ProtoObject` is small, it is *so*
small that transient proxies do not play nicely with the inspector,
the explorer and other tools in the development environment. This is
why they should be considered "transient" or temporary.

### Interaction patterns

![TransientActorProxy hierarchy](<img/TransientActorProxy hierarchy.png>){: .floatright .inline}
The three methods `async`, `sync` and `blocking` on an `ActorProxy`
instance each return instances of a distinct subclass of
`TransientActorProxy`.

Each allows a distinct *interaction pattern*:

 - `AsyncTransientActorProxy` offers asynchronous (one-way) messaging;
 - `SyncTransientActorProxy` offers request/reply/error messaging,
   using Promises to mediate between caller and callee; and
 - `BlockingTransientActorProxy` is like `SyncTransientActorProxy`,
   but hides the promises away, waiting for a reply at the time the
   call is made.

#### Asynchronous calls

```smalltalk
someProxy async notifyOfSomeEvent: details
```

The result of `ActorProxy >> #async` is an `AsyncTransientActorProxy`.
Messages sent this way do not allocate any promises, and always return
`nil` immediately.

#### Synchronous RPC

```smalltalk
(someProxy sync doSomethingWith: anArgument)
  >>= [:result | self handleResult: result] bindActor.
self whileWeWaitDoSomethingElse.
```

The result of `ActorProxy >> #sync` is a `SyncTransientActorProxy`.
Like messages sent to a plain old `ActorProxy`, messages sent this way
allocate and immediately return `ActorPromise` instances.

These promises are eventually resolved or rejected according to the
code executing at the remote actor.

Use `>>=` and `bindActor` to attach a continuation to a returned
promise. See [here](promises.html#adding-a-continuation-to-a-promise)
for details of `>>=`, and
[here](promises.html#where-and-when-do-handlers-run) for information
on why `bindActor` is so important.

#### Blocking RPC

```smalltalk
result := someProxy blocking doSomethingWith: anArgument.
self handleResult: result.
self nowDoSomethingElse.
```

The result of `ActorProxy >> #blocking` is a
`BlockingTransientActorProxy`. Messages sent this way internally
allocate an `ActorPromise`, but do not expose it: instead, they
immediately wait for the promise to be resolved or rejected. These
messages *eventually* either return a value or signal `BrokenPromise`.

{: .implementation-note}

There are circumstances where a `blocking` call will not return, such
as when the callee ["detaches"](behaviors.html#suspending-the-caller),
but never replies to, the request it receives.
