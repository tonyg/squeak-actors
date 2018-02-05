---
sidebar_heading: Overview
title: Actors for Squeak Smalltalk
pageorder: 0000
---

![Smalltalk Balloon (by Bert Freudenberg)](img/balloon-only.svg){: width="72pt" class="floatleft inline"}
The Smalltalk-80 concurrency model is, at heart, threads with mutable
shared state plus locks, semaphores, mutexes and so on.

{: class="clear"}
This library adds an Erlang-style Actor model to Squeak.

The following code snippet creates an actor, and invokes one of its
methods via synchronous ([Promise](promises.html)-based) RPC:

{: class="center"}
```smalltalk
h := HelloWorldActor spawn.
(h greet: 'Actor world') wait. "produces 'Hello, Actor world!'"
```

{% include nextstep.html prefix='Get started: ' url='/installation.html' %}

{% include nextstep.html prefix='Dive in: ' url='/tutorial-counter.html' %}

### Quickstart

{% include quickstart.md %}

### Features

 - Erlang-inspired model, including
    - [links and Monitors](links-and-monitors.html)
    - a [tracing facility](tracing.html)
    - Actor-based [socket support](sockets.html)
 - Smalltalk-inspired extensions and conveniences
    - [Actor behaviors](behaviors.html) are plain old objects
    - Synchronous and asynchronous RPC to actors, using [promises](promises.html)

{% include nextstep.html prefix='Read more: ' url='/design.html' %}
