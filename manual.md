---
title: User Manual
pagegroup: User Manual
---

This library implements the Actor model for Smalltalk, drawing heavily
on the design of [Erlang](https://www.erlang.org/) and its standard
library, [OTP][].

As such, it brings Smalltalk into the *Process-based* family of
Actor-style languages[^DeKoster2016].

#### Processes and Behaviors

Each Actor is a Smalltalk [Process](processes.html) with an ordinary
Smalltalk object as its [Behavior](behaviors.html).

#### Messages, Proxies, RPC, Promises, Timeouts and error handling

Actors send each other messages via [Proxies](proxies.html). Messages
may be [synchronous](synchronous-calls.html) (RPC) or
[asynchronous](asynchronous-calls.html) (one-way). Synchronous calls
can be made to block, or to return a [Promise](promises.html) of a
future reply. [Exception handling](error-handling.html) and
[timeouts and timers](time.html) are fully integrated.

#### Links and Supervision

Borrowing from [Erlang/OTP][OTP], actors may be
[linked](links-and-monitors.html) to each other. Failures (uncaught
exceptions or other crashes) propagate along these links. This gives a
robust approach to error handling in a concurrent setting.

Links (and the related [monitors](links-and-monitors.html#monitors))
are the foundation for [Supervisors](supervision.html), another
Erlang/OTP idea. A Supervisor manages the lifecycle of a collection of
actors, starting and stopping them and restarting them according to a
configurable policy when they fail.

#### Socket and GUI support

Erlang-inspired
[TCP/IP server and client socket support](sockets.html) is included.
Some support for [Morphic-based](morphic.html) GUI programming with
Actors is also provided.

#### Tracing and debugging

A [tracing](tracing.html) facility is built in to the message-passing
mechanism. An "event tracer" receives fine-grained notifications of
important steps in the routing, queueing and delivery of requests,
replies and exception messages. It also receives notifications of
actor lifecycle events.

  [^DeKoster2016]: The following paper is an excellent survey of the
    different families of the Actor model:

    > *"43 Years of Actors: a Taxonomy of Actor Models and Their Key
    Properties"* Joeri De Koster, Tom Van Cutsem, and Wolfgang De
    Meuter Proc. AGERE Workshop, 2016.
    [ACM metadata.](https://dl.acm.org/citation.cfm?id=3001890)
    [Tech Report version (PDF).](http://soft.vub.ac.be/Publications/2016/vub-soft-tr-16-11.pdf)

    In addition, I have written an informal article on the
    [history of the Actor model](https://eighty-twenty.org/2016/10/18/actors-hopl).

  [OTP]: http://erlang.org/doc/
