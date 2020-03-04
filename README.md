[![Build Status](https://travis-ci.org/Cuis-Smalltalk/Cuis-Smalltalk-Dev.svg?branch=master)](https://travis-ci.org/Cuis-Smalltalk/Cuis-Smalltalk-Dev)

# Table of Contents

#### [About Cuis Smalltalk](#about-cuis)
#### [The Philosophy behind Cuis Smalltalk](#the-philosophy-behind-cuis)
#### [About the name Cuis Smalltalk](#about-the-name-cuis)
#### [Running Cuis Smalltalk](#setting-up-cuis-in-your-machine)
#### [Learning Cuis Smalltalk](#learning-about-cuis-smalltalk)
#### [Contributing to the project](#contributing-to-cuis)

## About Cuis
[(back to ToC)](#table-of-contents)

Cuis is an Open Source, multiplatform [Smalltalk-80](https://en.wikipedia.org/wiki/Smalltalk) system.

Cuis is
* Small
* Clean
* Appropriable

Additionally, Cuis is:
* Open Source
* Multiplatform

Like other Smalltalk systems, Cuis is also:
* A complete development environment written in itself
* A pure, dynamic Object Oriented language

Cuis assumes very little on the underlying platform, and this lets it run out-of-the-box on Windows, MacOS, Linux, ChromeOS and WebBrowsers. Cuis shares the [OpenSmalltalk Virtual Machine](http://www.opensmalltalk.org) with Squeak, Pharo and Newspeak.


## The Philosophy behind Cuis
[(back to ToC)](#table-of-contents)

What sets Cuis apart from the other members of the Squeak family is the focus on Smalltalk-80 and an active attitude towards system complexity:

Unbound complexity growth, together with development strategies focused only in the short term, are the worst long term enemies of all software systems. As systems grow older, they usually become more complex. New features are added as layers on top of whatever is below, sometimes without really understanding it, and almost always without modifying it. Complexity and size grow without control. Evolution slows down. Understanding the system becomes harder every day. Bugs are harder to fix. Codebases become huge for no clear reason. At some point, the system can't evolve anymore and becomes "legacy code".

Complexity puts a limit to the level of understanding of the system a person might reach, and therefore limits the things that can be done with it. Dan Ingalls says all this in ["Design Principles Behind Smalltalk"](http://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html). Even if you have already done so, please go and read it again!

This presentation by Rich Hickey, ["Simple made Easy"](http://www.infoq.com/presentations/Simple-Made-Easy) is also an excellent reflection on these values.

We follow a set of ideas that started with Jean Piaget's [Constructivism](https://en.wikipedia.org/wiki/Constructivism_(philosophy_of_education)), and were further developed in Seymour Papert's [Mathland](https://en.wikipedia.org/wiki/Experiential_learning). These lead to Alan Kay's Learning Research Group's [Personal Computer for Children of All Ages](http://www.vpri.org/pdf/hc_pers_comp_for_children.pdf), [Personal Dynamic Media](http://www.vpri.org/pdf/m1977001_dynamedia.pdf), i.e. the [Dynabook](http://www.vpri.org/pdf/hc_what_Is_a_dynabook.pdf) and to [Smalltalk-80](https://en.wikipedia.org/wiki/Smalltalk). To us, a Smalltalk system is a Dynabook. A place to experiment and learn, and a medium to express and register the knlowledge we acquire. We understand software development as the activity of learning and documenting knowledge, for us and others to use, and also to be run on a computer. The fact that the computer run is useful, is a consequence of the knowldege being sound and relevant. (Just making it work is _not_ the important part!)

Cuis Smalltalk is our attempt at this challenge. Furthermore, we believe we are doing something else that no other Smalltalk, commercial or open source, does. We attempt to give the true Smalltalk-80 experience, and keep Smalltalk-80 not as legacy software historic significance, but as a live, evolving system. We feel we are the keepers of the Smalltalk-80 heritage, and enablers of the Dynabook experience.

Cuis is continuously evolving towards simplicity. Each release is better (i.e. simpler) than the previous one. At the same time, features are enhanced, and any reported bugs fixed. We also adopt recent enhancements from Squeak and share our work with the wider Squeak and Smalltalk community.

## About the name Cuis
[(back to ToC)](#table-of-contents)

Cuis is the common name of a [small animal](https://en.wikipedia.org/wiki/Southern_mountain_cavy) that lives in Argentina's countryside. Cuis Smalltalk was originally forked from Squeak Smalltalk and many of us are also active in the Squeak community. So, picking the onomatopoeia of the voice of a mouse for the name makes sense. As the project was started in Buenos Aires, 'Cuis' (essentially 'Squeak' in Rioplatense Spanish) was the obvious choice.

## Setting up Cuis in your machine
[(back to ToC)](#table-of-contents)

The quickest way to try Cuis (on Windows or MacOS) is by following [Getting started using Mac Finder or Windows Explorer](Documentation/GettingStarted-NoCommandLine.md) .

If you are on a Linux system, or have git available and are comfortable with it, follow [Setting up and starting Cuis Smalltalk](Documentation/GettingStarted.md) .

## Learning about Cuis Smalltalk
[(back to ToC)](#table-of-contents)

If you are learning Smalltalk, the Cuis community can help you. Check the ["Learning Cuis Smalltalk"](https://github.com/Cuis-Smalltalk/Learning-Cuis "Learning Cuis Smalltalk") repository. It includes several great tutorials. Also, the TerseGuide.pck.st package (in the /Packages folder in this repo) is useful both as a guide and a reference.

Additionally, there are many tutorials and references for Smalltalk in the web. They apply quite well to Cuis, especially those written originally for Smalltalk-80 or Squeak. These books ["Smalltalk-80 the language and its implementation"](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf) and ["Inside Smalltalk volume I"](http://stephane.ducasse.free.fr/FreeBooks/InsideST/InsideSmalltalk.pdf) are great introductory texts, and they are also the reference for the language and basic class library. Both are freely available.

The user interface enables you to access most of the code and conduct Smalltalk experiments on your own. You can review its features at ["Quick Tour of the UI"](https://github.com/Cuis-Smalltalk/Learning-Cuis/blob/master/Quick-UI-Tour.md). 

Visit the ["Cuis Smalltalk YouTube Channel"](https://www.youtube.com/playlist?list=PLbevs6Mp0MMMaR5gSYzJQXQ56OplFSCJk) 

## Contributing to Cuis
[(back to ToC)](#table-of-contents)

Cuis is maintained on https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev. The main meeting point for Cuis users and developers is the mail list https://lists.cuis.st/mailman/listinfo/cuis-dev. You can broswse the archives for a glimpse of our discussions. An older archive with pre-April-2019 messages can be found at http://cuis-smalltalk.org/pipermail/cuis-dev_cuis-smalltalk.org/.

For the recommended way of handling your own projects based on Cuis, please read [Code Management in Cuis](Documentation/CodeManagementInCuis.md), about developing packages for Cuis, and [Using Git and GitHub to host and manage Cuis code](Documentation/CuisAndGitHub.md). While Cuis should work equally well with any file-based DVCS, we encourage the use of Git and GitHub.

To contribute code to the base image, use the tools includede in Cuis, such as the ChangeSorter and the ChangeList to prepare ChangeSets and save them to file. Send the files as attachments to a message describing them to our mail list, so we all can review and discuss. We prefer this over git pull requests for changes to the base image. For changes to existing packages, or contribution of new packages, pull requests are OK.

Please use an image with all relevant packages already loaded, using updated versions, especially, of any affected packages. This will ensure we don't break them while we evolve Cuis.

Any contribution must be under the MIT license.

Here is a Smalltalk script to load all packages currently included in this repo:
```
Feature require: 'CorePackages'
```

If you have already cloned all the repos in the https://github.com/Cuis-Smalltalk organization (maybe running clonePackageRepos.sh), this will load all of them (except for OMeta, that is a bit special).
```
Feature require: 'AllPackages'
```

Cuis is distributed subject to the MIT License. See the LICENSE file. Any contribution submitted for incorporation into or for distribution with Cuis shall be presumed subject to the same license.
