# Cuis

[Cuis](http://www.cuis-smalltalk.org) is a free Smalltalk-80 environment originally derived from [Squeak](http://www.squeak.org) with a specific set of goals: being simple and powerful. It is also portable to any platform, fast and efficient. This means it is a great tool for running on any hardware, ranging from supercomputers to tablets and smart phones, and everything in between, including regular PCs.

Cuis is

* Easy to understand and extend
* Small
* Clean

Like Squeak, Pharo and other Squeak variants, Cuis is also:
* Open Source
* Multiplatform

Like other Smalltalk-80 environments (including Squeak, Pharo and others), Cuis is also:
* A complete development environment written in itself
* A pure Object Oriented language

Cuis is different from other Squeak variants in that it takes an active attitude towards system complexity.

As systems grow older, they usually become more complex. New features are added as layers on top of whatever is below, sometimes without really understanding it, and almost always without modifying it. Complexity grows without control. At some point, the system can't evolve anymore and becomes "legacy code".

The only way to avoid this is by understanding the complete system, and reengineering the whole system all the time. Keeping it simple and consistent.

This is important. Complexity puts a limit to the level of understanding of the system a person might reach, and therefore limits the things that can be done with it. Dan Ingalls says all this in ["Design Principles Behind Smalltalk"](http://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html). Even if you have already done so, please go and read it again!

This presentation by Rich Hickey, ["Simple made Easy"](http://www.infoq.com/presentations/Simple-Made-Easy) is also an excellent reflection on these issues.

Cuis is continuously evolving towards simplicity. Each release is better (i.e. simpler) than the previous one. At the same time, features are enhanced, and any reported bugs fixed. Cuis includes recent enhancements from Squeak, but only those that meet Cuis objectives: stuff whose complexity outweighs its value is not included.

If you want to know how Cuis has evolved in the past, check the [Release Notes](http://www.cuis-smalltalk.org/CuisReleaseNotes.html).

### Getting Started ###
If you are on Windows, follow the instructions in [Getting started using GUI](Documentation/GettingStarted-UsingGUI.md) .

If you are on Linux, follow the instructions in [Getting started using commandline](Documentation/GettingStarted-UsingCommandline.md) .

If you are on Mac OS X, you might follow any of the above. If you are comfortable with Terminal, [Getting started using commandline](Documentation/GettingStarted-UsingCommandline.md) is probably best.

Alternatively, you might also follow [Getting started: Alternative method using Git and commandline](Documentation/GettingStarted-AlternativeMethodUsingGitAndCommandline.md) .

### Towards Morphic 3
In Cuis work is on the way to restructure and upgrade the Morphic classes. For more see [Notes on Morphic](Documentation/NotesOnMorphic.md).

### Contributing to Cuis ###
Cuis is maintained on https://github.com/Cuis-Smalltalk.

Please read [Code Management in Cuis](Documentation/CodeManagementInCuis.md), about developing packages for Cuis, and [Using Git and GitHub to host and manage Cuis code](Documentation/CuisAndGitHub.md). While Cuis should work equally well with any file-based DVCS, we encourage the use of Git and GitHub.

To contribute code, please use an image with all included packages already loaded, and include new versions of affected packages. This will ensure we don't break them while we evolve Cuis.
Here is a Smalltalk script to load all packages currently included:
```
Feature require: 'Core-Packages'
```

Cuis is distributed subject to the MIT License. See the LICENSE file. Any contribution submitted for incorporation into or for distribution with Cuis shall be presumed subject to the same license.
