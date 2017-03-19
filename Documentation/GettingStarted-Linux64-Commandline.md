## Setting up and starting Cuis 64 bits using a command line

These are instructions for setting up Cuis in the 64 bits Spur flavor, on Linux on x86-64 computers using the command line. If you are familiar with Git, please see [Getting started using Git Bash](GettingStarted-Linux64-Commandline.md). If you are not on x86-64 or still prefer a 32 bit VM, see [Setting up and starting Cuis using a command line](GettingStarted-Commandline.md).

If you want to contribute back to the community, you might subscribe to the Cuis mail list at http://cuis-smalltalk.org/mailman/listinfo/cuis-dev_cuis-smalltalk.org , and email your code there.

The Cuis image specified is the most current one. The VM specified is the latest one too.

### Create project folder and add Cuis Smalltalk ###
```
mkdir MyProject
cd MyProject
~/MyProject# wget https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
~/MyProject# unzip master.zip
```

### Get an appropriate VM ###
```
~/MyProject# rm -r cogspur64
~/MyProject# wget -O cogspur64.tgz https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_linux64x64_squeak.cog.spur_201703051406.tar.gz
~/MyProject# tar -zxvf cogspur64.tgz
~/MyProject# mv ./sqcogspur64linuxht ./cogspur64
```
If you get this error message (you won't get it if you run Cuis as admin or sudo):
```
pthread_setschedparam failed: Operation not permitted
Read e.g. https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/tag/r3732#linux
```
Then you need to do this (just one time):
```
~/MyProject# sudo cp Cuis-Smalltalk-Dev/squeak.conf /etc/security/limits.d/squeak.conf
```
Log out and log back in, or reboot the machine.

Next is only needed if you want to play with OpenCL (you also need Catalyst driver for AMD GPU, or similar for Intel/Nvidia)
```
~/MyProject# cp /usr/lib/libOpenCL.so.1 libOpenCL.so
```

### Starting Cuis Smalltalk ###
```
~/MyProject# cogspur64/squeak Cuis-Smalltalk-Dev-master/Cuis5.0-3068-spur-64.image
```

### Notes ###

* If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 68021).", (68021 or some other reasonable number) it means you image and VM are mismatched. For example, one of them is Spur and the other is pre-Spur, or one of them is 32 bits and the other is 64 bits.

* If you can't find Cuis5.0-3068-spur-64.image, then this document is outdated. Use the Cuis spur image with the latest update number available.

* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the the Squeak Cog Spur VM for your platform with the latest Date and Time available.
