## Setting up and starting Cuis using a command line

What follows are instructions for setting up Cuis on Linux or Mac OS X using the command line. If you are familiar with Git, you can use it, but that is not described here. If you are on Windows, there are instructions for using the Windows file explorer in another document. This document is intended for beginners, and tries to require as little previous knowledge as possible.

### On Linux ###
```
$ mkdir MyProject
$ cd MyProject
$ wget https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
$ unzip master.zip
$ wget -O coglinux.tgz www.mirandabanda.org/files/Cog/VM/VM.r3386/coglinux-15.25.3390.tgz
$ tar -zxvf coglinux.tgz
```
If this is the first time you run Cuis Smalltalk on this system, add 32 bit libraries:
```
sudo dpkg --add-architecture i386
sudo apt-get update
sudo aptitude install ia32-libs
```
```
$ coglinux/squeak Cuis-Smalltalk-Dev-master/Cuis4.2-2400.image
```

### On Mac OS X ###
```
$ mkdir MyProject
$ cd MyProject
$ curl -L -o master.zip  https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
$ unzip master.zip
$ curl -o Cog.app.tgz www.mirandabanda.org/files/Cog/VM/VM.r3386/Cog.app-15.25.3390.tgz
$ tar -zxvf Cog.app.tgz
$ Cog.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev-master/Cuis4.2-2400.image
```