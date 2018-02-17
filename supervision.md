---
title: Supervision
pagegroup: User Manual
pageorder: 2000
---

In addition to [links and monitors](links-and-monitors.html), Erlang
pioneered an *application* of links called "supervisors".

 - [The Erlang/OTP documentation on supervisors](http://erlang.org/doc/man/supervisor.html)

{:. class="warning"}
Support for supervision is experimental.

A supervisor is an actor which holds *specifications* describing the
construction and maintenance of other, supervised ("child"), actors.

When a supervised actor terminates, the supervisor restarts it,
following the specification associated with it when it was added to
the supervisor.

Erlang has a rich suite of supervisor behaviors; so far, this library
includes only a very simple `Supervisor` class that offers the ability
to restart child actors after they terminate, so long as either they
terminate *normally* (with `nil` exit reason) or they do not terminate
*abnormally* more frequently than a configured limit.

### Example

We will create a `Supervisor` that supervises a `DemoSocketTerminal`.

```smalltalk
s := Supervisor spawn.
s instantiateSpec: [ | a |
	a := DemoSocketTerminal spawn.
	a async open.
	a ].
```

Executing these commands will lead to a `DemoSocketTerminal` opening
on the screen.

Closing the window leads to a replacement being created after the
window has closed. The supervisor is re-evaluating the "specification"
block supplied to it each time the supervisee terminates.

```smalltalk
s intensity: 2 period: 5 seconds.
```

This configures the supervisor to terminate *itself* if its
supervisees exceed a restart rate of two restarts within a five-second
window.

Closing the `DemoSocketTerminal` counts as a normal termination; we
will have to get creative to simulate an abnormal termination.

Executing the following a few times in rapid succession will do the
trick:

```smalltalk
s blocking children do: [:a | a kill ].
```

The result is that, after a few restarts of the "failed" actor, the
supervisor itself terminates with a `MaxRestartIntensityExceeded`
exception.
