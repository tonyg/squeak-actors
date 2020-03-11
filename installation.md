---
title: Installation
pageorder: 1000
---

To install the package, you will need a recent Squeak. I have been
using the 6.0-alpha trunk series: that is, the bleeding edge.

### Quickstart

{% include quickstart.md %}

{% include nextstep.html prefix='Now learn the basics: ' url='/tutorial-counter.html' %}

---

### Detailed instructions

#### Download Squeak

Download
[a recent 6.0-alpha trunk image and VM](http://files.squeak.org/6.0alpha/Squeak6.0alpha-17606-32bit/)
for your platform.

#### Update Squeak

Update your image. Execute the following in a workspace:

```smalltalk
MCMcmUpdater updateFromServer
```

Alternatively, click the Squeak icon in the top left of the window,
and choose "Update Squeak":

![Squeak update menu item](<img/squeak-update.png>)

As of this writing, my updated image is at update number {{ site.squeak_updatenumber }}. This
includes all the `Kernel` changes necessary for supporting the
`Actors` package.

#### Install the Actors package

Execute the following in a workspace

```smalltalk
(Installer squeaksource project: 'Actors') install: 'ConfigurationOfActors'
```

Generally, the `ConfigurationOfActors` package is the latest release
of the package. After it has been loaded, you can optionally update to
the latest development version with

```smalltalk
"Optional step: update to the latest development version."
(Installer squeaksource project: 'Actors') install: 'Actors'
```

{% include nextstep.html prefix='Optional next step: ' url='/testing.html' %}

