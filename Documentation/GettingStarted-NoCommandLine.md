# Quick setup for MacOS and Windows #

What follows are instructions for setting up Cuis on Windows or Mac OS X without using Git or the command line. If you are comfortable with Git, please see [Getting started using Git Bash](GettingStarted.md). This document describes the simplest way to run Cuis on your computer.

## On Windows ##
* Create a new folder in your machine using Windows Explorer
* download [`master.zip`](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip), saving it to that folder.
* extract the contents of the zip file right there ("extract here")
* download [`squeak.cog.spur_win64x64.zip`](https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/202112201228/squeak.cog.spur_win64x64.zip), saving it to yourFolder\Cuis-Smalltalk-Dev-master\ (the folder that was just created while extracting the first zip file).
* extract the contents of the zip file right there ("extract here")
* drop the Cuis6.0-5069.image over the Squeak.exe file. Alternatively, double click on the Squeak.exe file, and when prompted to select an image file, select Cuis6.0-5069.image.
* If you get a message like "This publisher could not be verified. Are you sure you want to run this software?", then untag "Always ask before opening this file" (if present) and click [Run].
* If you get a message like "Windows protected your PC", then click on "More info", and click [Run anyway].

## On Mac OS X ##
* Create a new folder in your machine using Mac Finder
* download [`master.zip`](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip) to your folder
* extract the contents of the zip file (double click on it)
* download [`squeak.cog.spur_macos64x64.dmg`](https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/202112201228/squeak.cog.spur_macos64x64.dmg), saving it to your folder
* double click on the dmg file
* Drag Squeak to your folder
* [control]+click on Squeak. [control]+Open in the menu.
* If you get a message like '"Squeak" can not be opened because the developer can not be verified', click [Open].
* Click [Cancel] to close the file dialog
* drop the Cuis6.0-5069.image over the Squeak.app file
* If you get "Squeak is an app downloaded from the Internet. Are you sure you want to open it?", click on [Open]

## Troubleshooting ##
* If you can't find Cuis6.0-5069.image, then this document is outdated. Use the Cuis image with the latest update number available.
* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the latest available Squeak Cog Spur VM for your platform from https://github.com/OpenSmalltalk/opensmalltalk-vm/releases
* If you can't get Cuis to run on your system after trying the above instructions, send mail to the Cuis-Dev mail list. Please give enough detail of your system, what you tried, and any error messages you got.
