---
sidebar_heading: Counter
title: 'Writing a “Counter” actor'
pagegroup: Tutorials
pageorder: 3010
---

```smalltalk
ActorBehavior subclass: #HelloWorldActor
              instanceVariableNames: ''
              classVariableNames: ''
              poolDictionaries: ''
              category: 'Actors-Demos'

greet: name
    ^ 'Hello, ', name, '!'

h := HelloWorldActor spawn.
(h greet: 'Actor world') wait. "produces 'Hello, Actor world!'"
```

Mention the use of the tracer early on.
