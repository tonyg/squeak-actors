---
title: Behaviors
pagegroup: User Manual
pageorder: 2000
---

Every instance of `Actor` has a *behavior* object associated with it.

Every
[user-level message](processes.html#system-level-and-user-level-messages)
sent to an `Actor` must be a [request](requests.html) carrying a
`Message` instance. The message is sent to the actor's behavior
object, and the reply is relayed to the calling actor.

Methods taking blocks or `ActorProxy` values often demand special
treatment; see the section on the
[weaknesses of the library's design](processes.html#weaknesses-of-the-design)
for more information.

### Class ActorBehavior

Any Smalltalk object can serve as a behavior object, but inheriting
from `ActorBehavior` offers a number of convenient features:

 - `ActorBehavior class >> #spawn` is a convenient abbreviation for
   `Actor class >> #bootProxy:`.

 - `ActorBehavior >> #log:` and `#logAll:` produce log message events
   using the [tracing mechanism](tracing.html). This allows log
   messages to be recorded in the correct order with respect to
   surrounding events when tracing is active. (By default, these
   messages go to the `Transcript`; see the section on
   [tracing](tracing.html) for details.)

 - `ActorBehavior >> #changed`, `#changed:` and `#changed:with:`
   ensure that dependents of the behavior are `update`d in the UI
   process, rather than directly in the actor's own process. This is
   important for robustness: firstly, if any of the `update` methods
   signals an exception, it is signalled in the UI process, rather
   than killing the actor; and secondly, an exception from one
   `update` method will not prevent the other dependents of the
   `changed` object from running.
