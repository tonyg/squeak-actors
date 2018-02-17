---
sidebar_heading: Counter
title: 'Writing a “Counter” actor'
pagegroup: Tutorials
pageorder: 3010
---

Our first example is a simple counter.

### Defining our behavior class

```smalltalk
ActorBehavior subclass: #CounterActor
    instanceVariableNames: 'count'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'ActorExamples'
```

We define an `initialize` method...

```smalltalk
initialize
    super initialize.
    count := 0.
```

... and `count` accessor, just as if this behavior object were a
normal Smalltalk object.

```smalltalk
count
    ^ count.
```

### Constructing an instance

In a workspace, evaluate

```smalltalk
a := CounterActor spawn. "an ActorProxy for an Actor (10138) on nil"
a count. "a Promise"
```

The result of the second expression is a [promise](promises.html) of
an eventual answer.

We can wait for the promise to resolve with `wait`.

```
a count wait. "0"
```

Alternatively, we can use the [`blocking`](proxies.html#blocking-rpc)
convenience method to arrange for `wait` to be called for us:

```
a blocking count. "0"
```

### Counting

Add the following two methods to `CounterActor`.

```smalltalk
add: aNumber
    count := count + aNumber.
    ^ count
```
```smalltalk
increment
    ^ self add: 1
```

Now, in the workspace,

```smalltalk
a blocking add: 123. "123"
a blocking add: 123. "246"
a blocking add: 123. "369"
```

We can also send a one-way, fire-and-forget asynchronous request to the actor:

```smalltalk
a async add: 123. "nil"
```

And we can increment its count by one:

```smalltalk
a increment wait. "493"
```

Notice that `increment` is implemented in terms of `self add:`. In a
behavior object, `self` refers to the object itself, *not* the current
actor.
[Use `Actor me` to refer to the current actor.](processes.html#implementing-a-behavior)

### Tracing activity

Evaluate

```smalltalk
a actor tracer: ActorEventStreamTracer forTranscript.
```

Open a Transcript window.

Now, when we interact with the actor, we will see
[information about which messages are sent where](tracing.html) in
both the Transcript and on Squeak's standard error stream.

For example, evaluating

```smalltalk
a async increment.
(a add: 123) wait.
a blocking count.
```

might yield the following trace in the Transcript:

```
2018-02-17 19:54:17 (82053) traceEnqueuedMessage: an asynchronous ActorRequest from 54210 (increment)
2018-02-17 19:54:17 (82053) traceEnqueuedMessage: a synchronous ActorRequest from 54210 (add: 123)
2018-02-17 19:54:17 (82053) traceDeliveredMessage: an asynchronous ActorRequest from 54210 (increment)
2018-02-17 19:54:17 (82053) traceHandleRequest: an asynchronous ActorRequest from 54210 (increment)
2018-02-17 19:54:17 (82053) traceDeliveredMessage: a synchronous ActorRequest from 54210 (add: 123)
2018-02-17 19:54:17 (82053) traceHandleRequest: a synchronous ActorRequest from 54210 (add: 123)
2018-02-17 19:54:17 (82053) traceEnqueuedMessage: a synchronous ActorRequest from 54210 (count)
2018-02-17 19:54:17 (82053) traceDeliveredMessage: a synchronous ActorRequest from 54210 (count)
2018-02-17 19:54:17 (82053) traceHandleRequest: a synchronous ActorRequest from 54210 (count)
```

Each actor can have its own [`ActorEventTracer`](tracing.html), and if
it spawns other actors, they inherit its tracer. If no special tracer
is assigned to an actor, it uses the system-wide default.

### Terminating an actor

Our variable `a` holds an instance of [`ActorProxy`](proxies.html)
referring to an underlying [`Actor`](processes.html), which in turn
holds a reference to an instance of `CounterActor`.

If we want to terminate our actor, we must call `terminate` or `kill`
on the `Actor`.

```smalltalk
a actor kill. "an Actor (82053) on a CounterActor (terminated)"
```

Since we earlier configured a tracer, the event is logged to the
Transcript:

```
2018-02-17 19:57:44 (82053) traceActorStopped: Error: Killed
```

And now our actor has become inert. Making requests of it will yield
`BrokenPromise` exceptions:

```smalltalk
p := a count.
p isKindOf: Promise. "true"
p isResolved. "false"
p isRejected. "true"
p error. "an ActorTerminated for an Actor (82053) on a CounterActor (terminated) with Error: Killed"
```

Even the rejected call to `count` generates a Transcript message:

```
2018-02-17 20:00:32 (54210) traceRejectedMessage: a synchronous ActorRequest from 54210 (count)
```
