---
title: Behaviors
pagegroup: User Manual
pageorder: 2000
---

Every instance of
[`Actor`](processes.html#actor-concurrent-smalltalk-objects) has a
*behavior* object associated with it.

### Receiving and processing messages

Every
[user-level message](processes.html#system-level-and-user-level-messages)
sent to an `Actor` must be a [request](requests.html) carrying a
`Message` instance. The message is sent to the actor's behavior
object, and the reply is relayed to the calling actor.

Methods taking blocks or `ActorProxy` values often demand special
treatment; see the section on the
[weaknesses of the library's design](processes.html#weaknesses-of-the-design)
for more information.

### Ways of responding to messages

#### Simple replies
Methods on a behavior object can simply return their result, like any
other method on any other object, and it will be relayed to the
caller.

#### Replying with a promise
They may also choose to return a [promise](promises.html) of an
eventual answer. The caller will get their reply when the promise
resolves or is rejected.

#### Suspending the caller
Finally, they may *suspend* the decision about how to reply to the
caller.

Calling the method `Actor class >> #caller` retrieves *and detaches*
the [request](requests.html) object that the actor is working on right
now. The actor can then store the request in a variable,
[making its reply](requests.html#sending-replies-or-notifying-of-failures)
using `#resolveWith:` or `#rejectWith:` later. Calling `Actor class >>
#caller` a second time will return `nil`, since the request was
detached on the first call.

The actor only automatically replies to a request if `Actor class >>
#caller` has not been called. Making a call to `Actor class >>
#caller` signals that the request will be taken care of manually.

The [Barrier tutorial](tutorial-barrier.html) shows an example of this
technique.

### Class ActorBehavior

Any Smalltalk object can serve as a behavior object, but inheriting
from `ActorBehavior` offers a number of convenient features:

 - `ActorBehavior class >> #spawn` is a convenient abbreviation for
   `Actor class >> #bootProxy:`. See
   [ways of constructing an actor](processes.html#actor-and-actorprocess-constructors).

 - `ActorBehavior >> #log:` and `#logAll:` produce log message events
   using the [tracing mechanism](tracing.html). This allows log
   messages to be recorded in the correct order with respect to
   surrounding events when tracing is active. (By default, these
   messages go to the `Transcript`; see the section on
   [tracing](tracing.html) for details.)

 - `ActorBehavior >> #changed`, `#changed:` and `#changed:with:`
   ensure that dependents of the behavior are `update`d in the UI
   process, rather than directly in the actor's own process. This is
   important not only because Morphic dependents often rely on
   executing in the UI process, but also for robustness. If any of the
   `update` methods signals an exception, the offending dependent is
   simply removed, rather than killing the actor. An exception from
   one `update` method therefore will not prevent the other dependents
   of the `changed` object from running.
