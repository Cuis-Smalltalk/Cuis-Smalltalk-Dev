# Setting up and starting Cuis using a Windows or MacOS (no command line required) #

What follows are instructions for setting up Cuis on Windows or Mac OS X without using the command line. If you are familiar with Git, please see [Getting started using Git Bash](GettingStarted.md). This document is intended for beginners, and tries to require as little previous knowledge as possible.

If you want to contribute back to the community, you might subscribe to the Cuis mail list at http://cuis-smalltalk.org/mailman/listinfo/cuis-dev_cuis-smalltalk.org , and email your code there.

The Cuis image specified is the most current one. The VM specified is the latest one too.

## On Windows ##
* Create a new folder in your machine using Windows Explorer
* download [`master.zip`](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip)
* extract the contents of the zip file to your folder
* download [`squeak.cog.spur_win64x64_201807260206.zip`](https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201807260206/squeak.cog.spur_win64x64_201807260206.zip)
* extract the contents of the zip file to your folder
* drop the Cuis5.0-3564-32.image over the Squeak.exe file

## On Mac OS X ##
* Create a new folder in your machine using Mac Finder
* download [`master.zip`](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip)
* extract the contents of the zip file to your folder
* download [`squeak.cog.spur_macos64x64_201807260206.dmg`](https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201807260206/squeak.cog.spur_macos64x64_201807260206.dmg)
* double click on the dmg file
* drop the Cuis5.0-3564.image over the Squeak.app file
* open System Preferences and go to Security Privacy. You will see that Squeak.app was not allowed to be installed and you have to click Allow.
* drop the Cuis5.0-3564.image over the Squeak.app file. On subsequent runs, drop the Cuis5.0-3564.image over the Squeak.app file

## Notes ##
* If you can't find Cuis5.0-3564.image, then this document is outdated. Use the Cuis image with the latest update number available.
* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the the Squeak Cog Spur VM for your platform with the latest Date and Time available from https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/latest or http://opensmalltalk.org/
