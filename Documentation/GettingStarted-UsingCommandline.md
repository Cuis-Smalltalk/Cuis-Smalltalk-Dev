## Setting up and starting Cuis using a command line

What follows are instructions for setting up Cuis on Linux or Mac OS X using the command line. If you are familiar with Git, please see [Getting started using Git Bash](GettingStarted-UsingGitAndCommandline.md) . If you are on Windows or Mac, and prefer not using the command line, you might follow [Getting started using Mac Finder or Windows Explorer](GettingStarted-UsingGUI.md).

If you want to contribute back to the community, you might subscribe to the Cuis mail list at http://http://cuis-smalltalk.org/mailman/listinfo/cuis-dev_cuis-smalltalk.org , and email your code there.

The Cuis image specified is the most current one. The VM specified is the latest one too.

### On Linux ###
```
$ mkdir MyProject
$ cd MyProject
$ wget https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
$ unzip master.zip
$ wget -O cogspur.tgz https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201608171728/cog_linux32x86_squeak.cog.spur_201608171728.tar.gz
$ tar -zxvf cogspur.tgz
$ mv ./sqcogspurlinuxht ./cogspur
```
If this is the first time you run Cuis Smalltalk on this system, add 32 bit libraries.
(This was tested on Debian 8)
```
sudo dpkg --add-architecture i386
sudo apt-get update
sudo apt-get install libc6-i386
# This (libc6-i386) was needed to make ldd work on 32 bit programs and dynamic libraries
# So, we can do stuff like       $ ldd cogspur/lib/squeak/*/squeak       to find about missing libraries
sudo apt-get install libuuid1:i386
sudo apt-get install libX11-6:i386
sudo apt-get install libXext6:i386
sudo apt-get install libsm6:i386
# Next 3 are only needed if you want to play with OpenCL (you also need Catalyst driver for AMD GPU, or similar for Intel/Nvidia)
sudo apt-get install mesa-common-dev:i386
sudo apt-get install libgl1-mesa-dev:i386
cp /usr/lib/i386-linux-gnu/libOpenCL.so.1 libOpenCL.so
```
```
$ cogspur/squeak Cuis-Smalltalk-Dev-master/Cuis5.0-3007-spur.image
```

### On Mac OS X ###
```
$ mkdir MyProject
$ cd MyProject
$ curl -L -o master.zip  https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
$ unzip master.zip
$ curl -o CogSpur.tgz https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201608171728/cog_macos32x86_squeak.cog.spur_201608171728.tar.gz
$ tar -zxvf CogSpur.tgz
$ CogSpur/Contents/MacOS/Squeak Cuis-Smalltalk-Dev-master/Cuis5.0-3007-spur.image
```

### Notes ###
* If you can't find Cuis5.0-3007-spur.image, then this document is outdated. Use the Cuis spur image with the latest update number available.
* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the the Squeak Cog Spur VM for your platform with the latest Date and Time available.
