---
title: Design
pagegroup: User Manual
pageorder: 2000
---

Weaknesses:
 - hack to change returning self to returning self proxy
 - no translation of blocks, hence custom control flow will be weird.
   Perhaps something (inefficient?) can be done about that
 - no isolation!!! this is really important - you still end up having
   to worry about concurrent access to data structures. Witness: the
   "copy" call in the chat room code

```smalltalk
b := Actor bootProxy: [ true ].
(b ifTrue: [ 1 ] ifFalse: [2]) wait
" ^ Wow, this doesn't work. Invokes mustBeBoolean. This is leaking some of the optimization-enabling assumptions the VM makes."
```

(See also usage of blocks in `OrderedCollection` and `Dictionary` examples <http:tutorial-collection.html>)

Interesting to also look at and think about:

    Installer squeakmap update; install: 'Actalk'

<http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.41.2445&rep=rep1&type=pdf>

Apparent differences (though I have only a shallow understanding):

 - we use `Process` subclassing
 - we use `Promise`s in a pervasive convention for RPC to `Actor`s
 - we allow any object to be the behavior of an `Actor`
 - we offer links and monitors
