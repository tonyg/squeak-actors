---
title: Ping-pong
pagegroup: Tutorials
pageorder: 3011
---

### Synchronous version

```smalltalk
ActorBehavior subclass: #PingPong1
              instanceVariableNames: ''
              classVariableNames: ''
              poolDictionaries: ''
              category: 'Actors-Demos'
```

```smalltalk
ping: peer times: count
    count = 0 ifTrue: [^self].
    self logAll: 'Synchronously pinging ', peer printString.
    peer yourself >>= [ self ping: peer times: count - 1 ].
```

Kick it off with

```smalltalk
actor1 := PingPong1 spawn.
actor2 := Actor for: Object new.
actor1 ping: actor2 times: 5.
actor1 actor kill.
actor2 actor kill.
```

### Asynchronous version

```smalltalk
ActorBehavior subclass: #PingPong2
              instanceVariableNames: ''
              classVariableNames: ''
              poolDictionaries: ''
              category: 'Actors-Demos'
```

```smalltalk
ping: peer times: count
    count = 0 ifTrue: [^self].
    self logAll: 'Asynchronously pinging ', peer printString.
    peer async ping: Actor me times: count - 1.
```

Kick it off with

```smalltalk
actor1 := PingPong2 spawn.
actor2 := PingPong2 spawn.
actor1 ping: actor2 times: 10.
actor1 actor kill.
actor2 actor kill.
```
