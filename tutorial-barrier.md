---
title: Barrier
pagegroup: Tutorials
pageorder: 3020
---

```smalltalk
ActorBehavior subclass: #BarrierActor
    instanceVariableNames: 'waiters'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Actors-Demos'
```

Class `BarrierActor` is a simple [`ActorBehavior`](behaviors.html)
that implements a *barrier*.

Clients may call `#wait`, in which case they will not receive a reply
until some other client calls `#releaseWaiters` or `#releaseWaiters:`.

Any value supplied to `#releaseWaiters:` is used as the resolved value
of waiting clients' promises; `#releaseWaiters` supplies `nil` as this
value.

The instance variable `waiters` holds an `IdentitySet` of
[`ActorRequests`](requests.html), the waiting continuations.

#### Initialization

```smalltalk
initialize
    super initialize.
    waiters := IdentitySet new.
```

#### Waiting for a value

```smalltalk
wait
    waiters add: Actor caller.
```

Retrieving the current [request](requests.html) by invoking `Actor
class >> #caller`
[suspends the remote caller](behaviors.html#suspending-the-caller) of
the actor until either `resolveWith:` or `rejectWith:` is called on
the resulting `ActorRequest` object.

#### Releasing the current set of waiters

```smalltalk
releaseWaiters: anObject
    waiters do: [ :c | c resolveWith: anObject ].
    waiters := IdentitySet new.
```

Each of the waiting requests is released, with `anObject` as the reply
value given to the suspended caller via the [promise](promises.html)
associated with the request.

```smalltalk
releaseWaiters
    self releaseWaiters: nil
```
