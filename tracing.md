---
title: Tracing and debugging
pagegroup: User Manual
pageorder: 2000
---

Every [`ActorProcess`](processes.html) may have a *tracer* object
associated with it.

Tracers capture interesting events corresponding to important parts of
the Actor-style programming model.

Any time an actor is started, it inherits its tracer from its parent.
Actors without an associated tracer object of their own use the
systemwide tracer object, `ActorProcess defaultTracer`.

### Tracer objects

Tracer objects are called as part of exception handling; message
sending, delivery, and interpretation; RPC requesting, replying, and
failure; link and monitor registration and activation; and at major
points in an actor's lifecycle. They are also called as part of the
[logging facility](behaviors.html#class-actorbehavior) available in
`ActorBehavior` instances.

#### ActorEventTracer: the default tracer

The default tracer object is an instance of `ActorEventTracer`, which
does nothing with any of the trace events it is sent except for:

 - logging requests, which are sent to the Transcript, and
 - exceptions, which are printed to the Transcript and the standard
   error stream, and which may optionally trigger the opening of a
   debugger.

#### Enabling the system debugger

The default for `ActorEventTracer` instances is *not* to debug
exceptions; they are only logged. Use `ActorEventTracer >>
#debugExceptions:` to enable debugging:

```smalltalk
ActorProcess defaultTracer debugExceptions: true.
```

#### ActorEventStreamTracer

Instances of `ActorEventStreamTracer` inherit from `ActorEventTracer`.

They respond to trace events by logging them to a configurable
collection of streams, often including the Transcript or a
standard-error output stream.

 - `ActorEventStreamTracer forStderr` constructs an instance sending
   output to the standard error (only).
 - `ActorEventStreamTracer forTranscript` constructs an instance
   sending output to both the standard error and the Transcript.

For example, trace events can be logged to the standard error stream,
system wide, using

```smalltalk
ActorEventStreamTracer forStderr beDefault.
```

### Trace events

Tracer objects are called with the following messages. Implicit in
each is the identity of the currently-active actor.

In the following descriptions,

 - `aReason` is usually an `Exception`, an `ActorTerminated`, or `nil`;
 - `aUserMessage` is usually an `ActorRequest`; and
 - `anActorish` is either an `ActorProcess` or an `ActorProxy`.

#### Logging

```smalltalk
traceLogAll: anOrderedCollection.
```

Triggered in order to print each item in `anOrderedCollection` to the
log output.

#### Actor lifecycle

```smalltalk
traceActorCreated: anActorProcess.
traceActorStarted.
traceNewBehavior: anObject.
traceException: anException.
traceActorStopped: aReason.
```

The first event, `traceActorCreated:`, describes the creation of an
actor from the perspective of the *spawning* actor, and describes the
newly-*spawned* actor.

The remainder describe events in the lifecycle of an actor from that
actor's own perspective.

#### Links and monitors

```smalltalk
traceLinkAddedTo: anActorProcess.
traceLinkRemovedTo: anActorProcess.
traceLinkedPeer: anActorProcess terminatedWith: aReason.
```

```smalltalk
traceMonitorAddedTo: anActorProcess reference: anObject.
traceMonitorRemovedTo: anActorProcess reference: anObject.
tracePeer: anActorProcess monitor: anObject terminatedWith: aReason.
```

These two groups of events capture the addition, removal, and
activation of links and monitors, respectively.

#### User-level messages

```smalltalk
traceEnqueuedMessage: aUserMessage.
traceDeliveredMessage: aUserMessage.
traceReceiveNextTimeout.
traceRejectedMessage: aUserMessage.
```

The first event describes the moment when `aUserMessage` is enqueued
for later consumption by the current actor. The second describes the
moment it is *dequeued*, just prior to interpretation.

The third event, `traceReceiveNextTimeout`, is emitted when a call to
`ActorProcess >> #receiveNextTimeout:` times out.

The fourth event, `traceRejectedMessage:`, is emitted when an actor
dies with pending, unhandled user messages in its queue.

#### Request handling

```smalltalk
traceHandleRequest: anActorRequest.
traceRequest: anActorRequest redirectedTo: anActorish message: aMessage.
traceRequest: anActorRequest rejectedWith: anObject.
traceRequest: anActorRequest resolvedWith: anObject.
traceRequest: anActorRequest sentTo: anActorProcess.
```

These events relate to [`ActorRequest` instances](requests.html).

The first is emitted at the moment an actor begins to interpret a
received request.

The second is emitted if a request is redirected elsewhere as the
result of a use of [`#redirectTo:`](requests.html#sending-requests).

The third and fourth capture rejection (error) and resolution (reply)
of a request, respectively; and the fifth is emitted just before a new
request is enqueued for a recipient.

### Interactive debugging utilities

The utility `ActorProcess class >> #loggingActorNamed:` spawns and
returns an actor (proxy) which simply logs every user-level message it
receives to its tracer. Such an actor can be useful during interactive
debugging and exploration of a system.
