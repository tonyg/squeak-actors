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
 - An instance of `ActorFailure` indicates an uncaught exception.
 - Any other value can be supplied when explicitly terminating an actor.

An actor can be terminated in three different ways:

 - `Process >> #terminate` sets `exitReason` to be nil.
 - `ActorProcess >> #terminateWith:` sets `exitReason` to the argument
   of the message.
 - `ActorProcess >> #kill` simulates an uncaught a generic `Error`
   exception in the actor.

### ActorFailure

Instances of `ActorFailure` represent the cause of a chain of actor
terminations propagating through links. An actor that signals an
uncaught exception will be terminated with an `ActorFailure` as the
exit reason, where the `actor` field is the original signalling actor,
and the `exception` field is the original signalled exception.

As exit reasons propagate across
[links and monitors](links-and-monitors.html), the use of
`ActorFailure` rather than just the exception value alone allows the
program or programmer to identify the actor that originally crashed.
