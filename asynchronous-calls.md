---
title: Asynchronous calls
pagegroup: User Manual
pageorder: 2000
---

```smalltalk
asynchronouslyDelegateComputationTo: anActorProxy using: aNumber
	^ (anActorProxy divideOneBy: aNumber)
		>>= [:tmp | anActorProxy addOneTo: tmp]
```
