Cuis
----

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

Cuis is continuously evolving towards simplicity. Each release is better (i.e. simpler) than the previous one. At the same time, features are enhanced, and any bugs fixed. Cuis includes recent enhancements from Squeak, but only those that meet Cuis objectives: stuff the complexity of which outweighs its utility is not included.

If you want to know how Cuis evolved please check the [Release Notes](http://www.cuis-smalltalk.org/CuisReleaseNotes.html).

### Getting Started ###
To get started with Cuis you need to do the following:

1. Download Cuis by clicking on [Download ZIP](https://github.com/bpieber/Cuis-Smalltalk-Dev/archive/master.zip).

2. Extract the downloaded ZIP archive.

3. Get a VM for your platform. You can run Cuis with Squeak's official Virtual Machines from http://www.squeakvm.org or the faster Cog VMs from [Eliot Miranda's site](http://www.mirandabanda.org/files/Cog/VM/). (Please use the non-MT variants of Eliot's latest release.)

4. Start Cuis by dragging the Cuis 4.2 image onto the VM.

As an alternative you can use the following in a shell if you have Git installed and configured:
```
mkdir CuisDevelopment
cd CuisDevelopment
git clone https://github.com/bpieber/Cuis-Smalltalk-Dev.git
Cuis-Smalltalk-Dev/bin/newImage.sh MyProject
```
The last line creates a new image with a current Cog VM in a folder named MyProject. Start Cuis by dragging MyProject.image onto the Cog VM. This method has the advantage to set you up to easily contribute code back to Cuis packages.

Disclaimer: It should work on OS X and Unix. However, I only tested it on OS X. Please test on other platforms and let me know the results. Feel free to send pull requests!

### Contributing to Cuis ###
Cuis is maintained on https://github.com/Cuis-Smalltalk.

Please read [Code Management in Cuis 4](http://www.cuis-smalltalk.org/CodeManagementInCuis4.html), about developing packages for Cuis, and [Using Git and GitHub to host and manage Cuis code](http://www.cuis-smalltalk.org/CuisAndGitHub.html). While Cuis should work equally well with any file-based DVCS, we encourage the use of Git and GitHub.

To contribute code, please use an image with all included packages already loaded, and include new versions of affected packages. This will ensure we don't break them while we evolve Cuis.
Here is a Smalltalk script to load all packages currently included:
```
#('Graphics-Files-Additional' 'Network-Kernel' 'SignalProcessing' 'Tests' 'Theme-Themes' 'FFITests' 'JSON')
    do: [:each | Feature require: each]
```

Cuis is distributed subject to the MIT License. See the LICENSE file. Any contribution submitted for incorporation into or for distribution with Cuis shall be presumed subject to the same license.
