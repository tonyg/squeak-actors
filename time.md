---
title: Time and Timers
pagegroup: User Manual
pageorder: 2000
---

```smalltalk
(Actor me future: 1000) log: 'Tick!'.
```

```smalltalk
testTimeout
	| a p |
	a := SimpleWorkerActor spawn.
	[
		p := a work: [ (Delay forMilliseconds: 100) wait. 'finished' ].
		p waitFor: 10 ifTimedOut: [^ self].
		self fail: 'Expected a timeout'
	] ensure: [a actor terminate]
```
