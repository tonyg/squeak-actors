---
title: Error handling
pagegroup: User Manual
pageorder: 2000
---

During its execution, if an `ActorProcess` or any method of an
`Actor`'s behavior object signals an uncaught exception, the actor is
terminated permanently.

By default, the system debugger is not invoked when such a crash
occurs. Instead, the stack trace of the exception is logged to the
standard error and to the Transcript. All of this can be
[configured](tracing.html#enabling-the-system-debugger).

When an actor terminates, its
[links and monitors](links-and-monitors.html) are triggered. This
happens for both normal and abnormal termination.

### Exit reason

Every `ActorProcess` has an `exitReason` instance variable (accessible
via the `#exitReason` message) that is set when the actor is
terminated. It is set when the actor terminates normally, as well as
when it terminates because of an uncaught exception.

The exit reason can take on many different values:

 - `nil` indicates "normal" termination.
 - An `Exception` indicates an uncaught exception.
 - An instance of `ActorTerminated` indicates termination caused by a
   [linked](links-and-monitors.html#links) peer's termination.
 - Any other value can be supplied when explicitly terminating an actor.

An actor can be terminated in three different ways:

 - `Process >> #terminate` sets `exitReason` to be nil.
 - `ActorProcess >> #terminateWith:` sets `exitReason` to the argument
   of the message.
 - `ActorProcess >> #kill` simulates an uncaught a generic `Error`
   exception in the actor.

### ActorTerminated

Instances of `ActorTerminated` represent a chain of actor terminations
propagating through links. An actor that signals an uncaught exception
will be terminated with the exception as its exit reason; actors
linked to that will be terminated with an `ActorTerminated` as the
exit reason, with its `actor` field the original signalling actor and
its `exitReason` field the original signalled exception; and actors
linked to *those* will be terminated with an exit reason that adds
another `ActorTerminated` to the chain; and so on.

As exit reasons propagate across [links](links-and-monitors.html), the
use of `ActorTerminated` rather than just the exception value alone
allows the program or programmer to identify the actor that originally
crashed.
