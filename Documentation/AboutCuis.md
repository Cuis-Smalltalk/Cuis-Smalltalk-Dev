# Table of Contents

#### [About Cuis Smalltalk](#about-cuis)
#### [The Philosophy behind Cuis Smalltalk](#the-philosophy-behind-cuis)
#### [About the name Cuis Smalltalk](#about-the-name-cuis)
#### [The Cuis Smalltalk project and community](#the-cuis-project-and-community)
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

What sets Cuis apart from other Smalltalk systems is the focus on the original values of the Smalltalk project at Xerox PARC, and an active attitude towards system complexity:

Unbound complexity growth, together with development strategies focused only in the short term, are the worst long term enemies of all software systems. As systems grow older, they usually become more complex. New features are added as layers on top of whatever is below, sometimes without really understanding it, and almost always without modifying it. Complexity and size grow without control. Evolution slows down. Understanding the system becomes harder every day. Bugs are harder to fix. Codebases become huge for no clear reason. At some point, the system can't evolve anymore and becomes "legacy code".

Complexity puts a limit to the level of understanding of the system a person might reach, and therefore limits the things that can be done with it. Dan Ingalls says all this in ["Design Principles Behind Smalltalk"](http://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html). Even if you have already done so, please go and read it again!

This presentation by Rich Hickey, ["Simple made Easy"](http://www.infoq.com/presentations/Simple-Made-Easy) is also an excellent reflection on these values.

We follow a set of ideas that started with Jean Piaget's [Constructivism](https://en.wikipedia.org/wiki/Constructivism_(philosophy_of_education)), and were further developed in Seymour Papert's [Mathland](https://en.wikipedia.org/wiki/Experiential_learning). These lead to Alan Kay's Learning Research Group's [Personal Computer for Children of All Ages](http://www.vpri.org/pdf/hc_pers_comp_for_children.pdf), [Personal Dynamic Media](http://www.vpri.org/pdf/m1977001_dynamedia.pdf), i.e. the [Dynabook](http://www.vpri.org/pdf/hc_what_Is_a_dynabook.pdf) and to [Smalltalk-80](https://en.wikipedia.org/wiki/Smalltalk). To us, a Smalltalk system is a Dynabook. A place to experiment and learn, and a medium to express and register the knlowledge we acquire. We understand software development as the activity of learning and documenting knowledge, for us and others to use, and also to be run on a computer. The fact that the computer run is useful, is a consequence of the knowledge being sound and relevant. (Just making it work is _not_ the important part!)

Cuis Smalltalk is our attempt at this challenge. Furthermore, we believe we are doing something else that no other Smalltalk, commercial or open source, does. We attempt to give a true Smalltalk-80 experience, and keep Smalltalk-80 not just as legacy software historic significance, but as a live, evolving system. We feel we are the keepers of this Smalltalk heritage, and enablers of the Dynabook experience.

As Cuis evolves, we keep on these values. Every update, be it a bug fix or a feature enhancement, is reviewed carefully to avoid adding unneeded complexity to the system. Every opportunity to remove unneeded complexity is followed. As we go, features are enhanced, and any reported bugs fixed. We also engage in discussion and share code and knowledge with the wider Smalltalk community.

## About the name Cuis
[(back to ToC)](#table-of-contents)

Cuis is the common name of a [small animal](https://en.wikipedia.org/wiki/Southern_mountain_cavy) that lives in Argentina's countryside. Cuis Smalltalk was originally forked from Squeak Smalltalk, so picking the onomatopoeia of the voice of a mouse for the name makes sense. As the project was started in Buenos Aires, 'Cuis' (essentially 'Squeak' in Rioplatense Spanish) was the obvious choice.

## The Cuis project and community
[(back to ToC)](#table-of-contents)

Cuis is maintained on our [Main GitHub repo](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev). All source code, issue tracker and documentation is held there.

This is our main [Project Website](https://www.cuis.st). It includes a general overview of the project. You can find the schedule for our future onlime monthly meetings, and watch video recordings for past ones.

Cuis has an active community of developers and users. Our main meeting point is the [mailing list](https://lists.cuis.st/mailman/listinfo/cuis-dev). You can browse the archives for a glimpse of our discussions. Pre-April-2019 archives are found [here](http://cuis-smalltalk.org/pipermail/cuis-dev_cuis-smalltalk.org/) and [here](http://jvuletich.org/mailman/listinfo/cuis_jvuletich.org). You are welcome here. If you use Cuis or are curious about our work, subscribe to the mail list to ask questions and tell us about your own projects and ideas.

## Learning about Cuis Smalltalk
[(back to ToC)](#table-of-contents)

If you are learning Smalltalk, the Cuis community can help you. Check the ["Learning Cuis Smalltalk"](https://github.com/Cuis-Smalltalk/Learning-Cuis "Learning Cuis Smalltalk") repository. It includes several great tutorials. Also, the TerseGuide.pck.st package (in the /Packages folder in this repo) is useful both as a guide and a reference.

Additionally, there are many tutorials and references for Smalltalk in the web. They apply quite well to Cuis. These books ["Smalltalk-80 the language and its implementation"](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf) and ["Inside Smalltalk volume I"](http://stephane.ducasse.free.fr/FreeBooks/InsideST/InsideSmalltalk.pdf) are great introductory texts, and they are also the reference for the language and basic class library. Both are freely available.

The user interface enables you to access most of the code and conduct Smalltalk experiments on your own. You can review its features at ["Quick Tour of the UI"](https://github.com/Cuis-Smalltalk/Learning-Cuis/blob/master/Quick-UI-Tour.md). 

Visit the ["Cuis Smalltalk YouTube Channel"](https://www.youtube.com/playlist?list=PLbevs6Mp0MMMaR5gSYzJQXQ56OplFSCJk) 

## Contributing to Cuis
[(back to ToC)](#table-of-contents)

Cuis is maintained on our [Main GitHub repo](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev). All source code, issue tracker and documentation is held there.

For the recommended way of handling your own projects based on Cuis, please read [Code Management in Cuis](Documentation/CodeManagementInCuis.md), about developing packages for Cuis, and [Using Git and GitHub to host and manage Cuis code](Documentation/CuisAndGitHub.md). While Cuis should work equally well with any file-based DVCS, we encourage the use of Git and GitHub.

To contribute code to the base image, use the tools includede in Cuis, such as the ChangeSorter and the ChangeList to prepare ChangeSets and save them to file. Send the files as attachments to a message describing them to our mail list, so we all can review and discuss. We prefer this over git pull requests for changes to the base image. For changes to existing packages, or contribution of new packages, pull requests are OK.

Please use an image with all relevant packages already loaded, using updated versions, especially, of any affected packages. This will ensure we don't break them while we evolve Cuis.

For some ideas on how you can help Cuis, see [Helping Cuis](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/wiki/Helping-Cuis). You can contribute your own too.

Cuis is distributed subject to the MIT License. See the LICENSE file. Any contribution must also be under the MIT license.
 