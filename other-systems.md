---
title: Other actor systems
pagegroup: User Manual
pageorder: 2000
---

There do not seem to be very many other implementations of the actor
model for Squeak.

### Actalk

The
[Actalk system](http://map.squeak.org/package/19ae332d-1111-4131-8b56-1fd7e9d2b35f)
dates back to 1989, and was subsequently ported to Squeak. A version
that can be loaded in to Squeak is still available on SqueakMap:

```smalltalk
Installer squeakmap update; install: 'Actalk'
```

The system is described in a paper:

> "Actalk: a Testbed for Classifying and Designing Actor Languages in
> the Smalltalk-80 Environment", Jean-Pierre Briot, Proc. ECOOP, 1989.
> [Full ECOOP'89 conference proceedings.](http://www.lirmm.fr/~ducour/Doc-objets/ECOOP/tocs/tec89.htm)
> [PDF (scan of proceedings).](http://www.lirmm.fr/~ducour/Doc-objets/ECOOP/papers/ec89/ec890109.pdf)
> [PDF (alternate version).](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.41.2445&rep=rep1&type=pdf)

While I only have a shallow understanding of Actalk, it seems that
differences between Actalk and this system include:

 - our `Actor` class subclasses `Process`
 - we use `Promise`s in a pervasive convention for RPC to `Actor`s
 - we allow any object to be the behavior of an `Actor`
 - we offer links and monitors

### Squeak-E

Research into [object capability systems](http://erights.org/) has led
to a number of implementations of languages and libraries inspired by
Mark Miller's E language:

> "Robust Composition: Towards a Unified Approach to Access Control
> and Concurrency Control", Mark Samuel Miller, PhD. dissertation,
> Johns Hopkins University, 2006.
> [Dissertation resource page.](http://www.erights.org/talks/thesis/)
> [PDF.](http://www.erights.org/talks/thesis/markm-thesis.pdf)

In particular,
[Squeak-E](http://www.erights.org/history/squeak-e.html) is "a project
putting E concepts into Squeak".

The design and goals of E and Squeak-E go far beyond the ambition of
this library. In particular, Squeak-E aims to support a concept called
[refraction](http://lists.squeakfoundation.org/pipermail/squeak-e/2003-February/000027.html)
which would not only allow secure actor-style interaction among
parties that do not necessarily trust one another, but also secure
reflection, management, monitoring, debugging and administration of
such a system. Because our library subclasses `Process` but does not
otherwise change the VM or any other aspect of the image, it cannot
provide such features.

Another major difference is in the *vat* concept of E. A vat is
analogous to an actor in our system, but is like a small segment of a
running image, containing *multiple* distinct and individually
addressable objects. Where in our system an `ActorProxy` denotes an
actor, in Squeak-E (and E), a *reference* denotes an *object* within a
vat, rather than denoting an entire vat. This allows for a more
flexible, fine-grained style of programming with concurrent objects.

Finally, E has special syntactic and language support for working with
promises of various kinds, while Squeak and our library implement
`Promise`s as an ordinary objects in the system.

The Squeak-E implementation was not integrated with the core of
Squeak, and does not seem to be available any more. I would welcome
correction on this point! If anyone can point me to an installable
Squeak-E, [please get in touch](mailto:{{ site.contact_email }}).
