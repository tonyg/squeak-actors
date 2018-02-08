---
title: Collections as behavior
pagegroup: Tutorials
pageorder: 3030
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

(FROM PROCESSES.MD:)

```smalltalk
d := Actor bootProxy: [ Dictionary new ].
a async at: 1 put: 2.
a blocking removeKey: 1. "This works fine..."
a blocking removeKey: 99. "... but this kills the whole actor."
```

If, instead, we use `removeKey:ifAbsent:`, we can avoid killing the
whole actor, but at the cost of having the `ifAbsent:` block execute
*in the wrong context*. That is, if it runs, it will run in a context
where `self` is the `Dictionary`, and `Actor current` and `Actor me`
denote the actor that the client knows as `d`. The tutorial on
[collections as behavior](tutorial-collection.html) covers this topic
in more detail.
