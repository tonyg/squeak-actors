---
title: Tutorials
pagegroup: Tutorials
---

The easiest way to learn the library is to try it out. There's no
substitute for experimentation in a live image!

The code snippets in these pages cannot be as interactive as the real
thing. It can be difficult to get a feel for the system without
hands-on experience. [Install the system](installation.html) to try it
out.

{: .implementation-note}
If you prefer to load the completed tutorial classes, rather than
building them yourself while following along with each tutorial, you
can load the `ActorExamples` package from SqueakSource:  
`(Installer squeaksource project: 'Actors') install: 'ActorExamples'`

### Examples and demos

Besides the tutorial examples below, the library includes a handful of
demos and larger examples in the `Actors-Demos` package.

### Tutorials

#### Basics

The [Counter](tutorial-counter.html) tutorial goes over the basics,
with a simple stateful actor.

The [Ping-pong](tutorial-ping-pong.html) tutorial introduces more
complex multi-party interaction.

#### Scheduling and inter-actor continuations

The [Barrier](tutorial-barrier.html) tutorial introduces techniques
for managing an Actor's incoming requests. These techniques allow
suspension of active requests and replying to requests in a different
order than they were received in.

#### Plain Old Smalltalk Objects

The [Collections-as-behavior](tutorial-collection.html) tutorial
covers use of existing Smalltalk objects as Actor behaviors,
discussing the benefits, drawbacks and pitfalls of the idea.

#### Client and Server TCP/IP Sockets

The [TCP/IP Echo Server](tutorial-echo-server.html) tutorial covers
programming with Actors representing TCP/IP connection sockets and
listening TCP/IP server sockets.
