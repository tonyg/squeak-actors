---
title: Requests
pagegroup: User Manual
pageorder: 2000
---

While any object can be sent as a user-level message between Actors,
the convention is that instances of `ActorRequest` are the only kind
of user-level message exchanged.

An `ActorRequest` (a "request") is a triple of a `Message` destined
for a remote behavior (the "message"), a `Process` interested in the
reply (the "sender"), and a `Promise` by which the reply is to be
delivered to the `Process` (the "promise").

The sender is not always an `ActorProcess`. An ordinary `Process` can
send `ActorRequests` and can wait for eventual replies or exceptions.

If the sender is `nil`, the request is asynchronous and no-one cares
about the reply to the eventual evaluation of the message.

A request is not intrinsically targeted at any actor in particular; it
does not store any information about the identity of its target.

Requests can be sent to an actor with `ActorRequest >> #sendTo:` or
the `#redirectTo:` family of methods. There are also convenience
methods `Actor class >> #sendRequest:to:` and `sendAsyncRequest:to:`,
and of course any instance of `ActorProxy` builds and sends
`ActorRequest` instances.

Ultimately, when a response is ready for a request, it is transmitted
by invoking `ActorRequest >> #resolveWith:` or `#rejectWith:`, as
appropriate. Compare these to similar and similarly-named methods on
`Promise`.

In order to ensure that every request receives an answer, a request
sometimes stores the identity of an `Actor` (in its `worker` instance
variable) that has taken responsibility for the request, so that it
can send a default answer if no-one else supplies anything first. The
request object takes care to signal the `worker` whenever someone
supplies a reply to it, so that the `worker` knows it no longer needs
to bother with the request.
