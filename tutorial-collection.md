---
title: Collections as behavior
pagegroup: Tutorials
pageorder: 3000
---

```smalltalk
a := Actor bootProxy: [ OrderedCollection new ].
a add: 3.
a add: 4.
a sum.
a sum wait.
a size. "Uh oh!"
a sync size wait.
(a sync at: 1) wait

a removeFirst wait. "OK, twice, but then: Uh oh! Killed the actor!"
```

```smalltalk
d := Actor bootProxy: [ Dictionary new ].
d async at: 1 put: 2.
d async at: 3 put: 4.
(d at: 11 ifAbsent: [ Actor callerError: 'No such key' ]) wait
(d removeKey: 1 ifAbsent: [ Actor callerError: 'No such key' ]) wait
```
