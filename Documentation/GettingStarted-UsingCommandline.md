## Setting up and starting Cuis using a command line

What follows are instructions for setting up Cuis (32 bits Spur flavor) on Linux or Mac OS X using the command line. If you are familiar with Git, please see [Getting started using Git Bash](GettingStarted-UsingGitAndCommandline.md). If you are on Windows or Mac, and prefer not using the command line, you might follow [Getting started using Mac Finder or Windows Explorer](GettingStarted-UsingGUI.md).

If you want to contribute back to the community, you might subscribe to the Cuis mail list at http://cuis-smalltalk.org/mailman/listinfo/cuis-dev_cuis-smalltalk.org , and email your code there.

The Cuis image specified is the most current one. The VM specified is the latest one too.

### On Mac OS X ###
```
$ mkdir MyProject
$ cd MyProject
$ curl -L -o master.zip  https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
$ unzip master.zip
$ curl -L -o CogSpur.tgz https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201608171728/cog_macos32x86_squeak.cog.spur_201608171728.tar.gz
$ tar -zxvf CogSpur.tgz
$ CogSpur/Contents/MacOS/Squeak Cuis-Smalltalk-Dev-master/Cuis5.0-3007-spur.image
```

### On Linux ###
On Linux we recommend using 64 bits Cuis if possible (i.e. if on x86-64 hardware). See [Setting up and starting Cuis 64 bits using a command line](GettingStarted-UsingCommandline-Linux64.md). For Linux on other platforms, see [Getting started using Git Bash](GettingStarted-UsingGitAndCommandline.md).

### Notes ###
* If you can't find Cuis5.0-3007-spur.image, then this document is outdated. Use the Cuis spur image with the latest update number available.
* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the the Squeak Cog Spur VM for your platform with the latest Date and Time available.
