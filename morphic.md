---
title: Morphic GUI
pagegroup: User Manual
pageorder: 2000
---

*See also: [GUI utilities for tracing Actor interactions](tracing.html).*

TODO update link immediately above to correct subsection

It is *extremely* easy to lock up the user interface. The interrupt
key (`Alt-.`) is helpful when this happens. However, don't rely on it:
from time to time you will simply have to kill and restart your VM.
**Save often.**

Morphic really isn't set up for this. (`PluggableTextMorph >>
appendEntry`, for example, assumes that the entry is to be found at
`model contents`!)

Similarly, there's the back-and-forth dance between `changed:` and
`update:` that one must remain painfully aware of.

The main problem is that morphs and model objects must run in the UI
process, because some of the interplay between morphs and their models
is synchronous.
