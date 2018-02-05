---
title: Behaviors
pagegroup: User Manual
pageorder: 2000
---

For instances of `ActorBehavior`, you can use `spawn`:

```smalltalk
MyActorBehavior spawn
```

is a convenient abbreviation for

```smalltalk
Actor bootProxy: [ MyActorBehavior new ]
```

