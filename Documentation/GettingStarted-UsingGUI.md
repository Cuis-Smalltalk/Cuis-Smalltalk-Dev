## Setting up and starting Cuis using a Windows or MacOS (no command line required)

What follows are instructions for setting up Cuis on Windows or Mac OS X without using the command line. If you are familiar with Git, please see [Getting started using Git Bash](GettingStarted-UsingGitAndCommandline.md) . This document is intended for beginners, and tries to require as little previous knowledge as possible.

If you want to contribute back to the community, you might subscribe to the Cuis mail list at http://jvuletich.org/mailman/listinfo/cuis_jvuletich.org , and email your code there.

The Cuis image specified is the most current one. The VM specified is the latest one too.

### On Windows ###
* Crea a new folder in your machine using Windows Explorer
* donwload https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
* extract the contents of the zip file to your folder
* download http://www.mirandabanda.org/files/Cog/VM/VM.r3732/cogwin-16.21.3732.zip
* extract the contents of the zip file to your folder
* drop the Cuis4.2-2829.image over the Squeak.exe file

### On Mac OS X ###
* Crea a new folder in your machine using Mac Finder
* donwload https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
* extract the contents of the zip file to your folder
* download http://www.mirandabanda.org/files/Cog/VM/VM.r3732/Cog.app-16.21.3732.tgz
* extract the contents of the zip file to your folder
* drop the Cuis4.2-2829.image over the Squeak.app file
* [ctrl] click on Cog application. Open. "Cog is from an unidentified developer"; dialog. Click on [Open] Supply required credentials. (This is required only on the first run). Select Cuis4.2-2829.image. (If this step is a problem, you might try the command line variant, described in another document.)
* On subsequent runs, drop the Cuis4.2-2829.image over the Squeak.app file
