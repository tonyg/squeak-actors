---
title: Links and Monitors
pagegroup: User Manual
pageorder: 2000
---

Erlang pioneered two important additions to the Actor model: *links*
and *monitors*.

Links and monitors offer a mechanism for failure-signaling and error
propagation that works well in a concurrent system, unlike the
stack-based approach of normal exception-handling.

They allow an actor to keep track of the lifecycle of another actor.
That is, an actor receives a message when a linked or monitored actor
terminates.

[Exit reasons](error-handling.html#exit-reason) propagate through
links and monitors. Each message describing the termination of a
linked or monitored actor includes the actor's exit reason value.

More information on links and monitors in Erlang:

 - [The Erlang/OTP documentation on processes](https://www.erlang.org/doc/system/ref_man_processes.html)
   discusses links and monitors
 - [Learn You Some Erlang](https://learnyousomeerlang.com/errors-and-processes) on errors and processes

## Links

A *link* is a **bidirectional, symmetric** relationship between two
actors. If one actor links itself to another, the other becomes linked
to the one.

When an actor terminates, a link activation message is sent along each
link connecting it to a peer. The message carries the identity of the
terminated actor and its exit reason.

### Adding a link

Links may be established at boot time, by using `#spawnLink`,
`#bootLinkProxy:`, `#boot:link:`, or any of the
[other constructors](processes.html#actor-and-actorprocess-constructors)
mentioning linking, or at any time thereafter, by calling
`ActorProcess >> #link`.

A link may be established to a terminated process: this will cause a
link activation message to be enqueued immediately.

### Removing a link

Links can be removed by calling `ActorProcess >> #unlink`. Be warned
that a link activation message may be enqueued but not yet processed
at the time `unlink` is called! Actors must be prepared to handle this
situation.

### Idempotency

Adding a link and removing a link are both idempotent actions.

### Handling link activation

A link activation report message will **by default terminate the
receiving actor**. This gives an easy method for making an entire
group of actors "live and die together".[^fate-sharing]

  [^fate-sharing]: This idea is also known as *fate sharing*. See
    David Clark's paper,

    > *"The Design Philosophy of the DARPA Internet Protocols"*, David
    D. Clark, ACM SIGCOMM Computer Communication Review, 1988.
    [ACM metadata.](https://dl.acm.org/doi/10.1145/52324.52336)
    [PDF.](http://ccr.sigcomm.org/archive/1995/jan95/ccr-9501-clark.pdf)

The exception to the rule is for `Actor` instances that have behavior
objects that respond to `#linkedPeer:terminatedWith:`. In this case,
that method is called with the identity of the newly-terminated peer
and its [exit reason](error-handling.html#exit-reason), and the actor
is not automatically terminated. The actor is still free to call
`#terminate` or `#terminateWith:` when appropriate.

{: .implementation-note}
Note that Erlang treats "normal" exits specially. If an Erlang process
exits with `normal` exit reason, *none of its links are activated*,
and it exits without terminating linked peers. My
[experience with Erlang programming](https://leastfixedpoint.com/tonyg/kcbbs/projects/rabbitmq.html)
Has led me to believe that Erlang's default is perhaps not quite
right: when I link to something, I want there to be a consequence when
it exits, no matter whether the exit is considered "normal" or not.
Therefore, in this library, I have chosen *not* to special-case
"normal" exits; they are treated like any other exit, and always cause
link activation.

## Monitors

A *monitor* is a **unidirectional, asymmetric** relationship from a
monitored actor to a monitoring actor.

Each monitor includes a *reference* object that can be used by a
monitoring actor to tell monitors apart. A single actor can monitor a
particular peer any number of times, so long as each monitor is
created with a different reference object.

When an actor terminates, a monitor activation message is sent for
each monitor installed on the actor. The message carries the identity
of the terminated actor and its exit reason.

### Adding a monitor

Monitors are installed with

```smalltalk
anActorProcess monitor: aReference.
```

The calling actor will be informed when the receiver,
`anActorProcess`, terminates.

{: .warning}
Actors must use unique reference objects. Monitors are stored
internally in a `Dictionary` keyed by reference object. If a
particular reference object is passed to `monitor:` by two different
actors, the second call will overwrite the results of the first call.

A monitor may be installed on a terminated process: this will cause a
monitor activation message to be enqueued immediately.

### Removing a monitor

The `ActorProcess >> #unmonitor:` method removes a previously-added
monitor, keyed by the reference object supplied as the argument. Be
warned that a monitor activation message may be enqueued but not yet
processed at the time `unmonitor:` is called! Actors must be prepared
to handle this situation.

### Idempotency

Adding a link and removing a link are both idempotent actions, modulo
the caveat about unique reference objects above.

### Handling monitor activation

A monitor activation message will by default invoke the
`#peer:monitor:terminatedWith:` of the actor's behavior object, if
any, and if it responds to that selector, and will otherwise do
nothing.

In particular, while a link will by default terminate the receiving
actor, a monitor will *never* automatically terminate the receiving
actor.

The arguments to `#peer:monitor:terminatedWith:` are the identity of
the newly-terminated peer, the reference object associated with the
activated monitor, and the terminated peer's
[exit reason](error-handling.html#exit-reason).
