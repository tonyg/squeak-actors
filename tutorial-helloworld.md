---
title: Hello World
pagegroup: Tutorials
pageorder: 3000
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
