---
sidebar_heading: Counter
title: 'Writing a “Counter” actor'
pagegroup: Tutorials
pageorder: 2999
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
