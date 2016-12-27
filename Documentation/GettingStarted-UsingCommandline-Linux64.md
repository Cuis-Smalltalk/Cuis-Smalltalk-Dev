## Setting up and starting Cuis using a command line

These are instructions for setting up Cuis in the 64 bits flavor, on Linux using the command line. If you are familiar with Git, please see [Getting started using Git Bash](GettingStarted-UsingGitAndCommandline-Linux64.md).

If you want to contribute back to the community, you might subscribe to the Cuis mail list at http://cuis-smalltalk.org/mailman/listinfo/cuis-dev_cuis-smalltalk.org , and email your code there.

The Cuis image specified is the most current one. The VM specified is the latest one too.

### On Linux ###
```
mkdir MyProject
cd MyProject
~/MyProject# wget https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
~/MyProject# unzip master.zip
~/MyProject# wget -O cogspur.tgz https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201608171728/cog_linux32x86_squeak.cog.spur_201608171728.tar.gz
~/MyProject# tar -zxvf cogspur.tgz
~/MyProject# mv ./products/cogspurlinuxht ./cogspur
# Next is only needed if you want to play with OpenCL (you also need Catalyst driver for AMD GPU, or similar for Intel/Nvidia)
~/MyProject# cp /usr/lib/libOpenCL.so.1 libOpenCL.so
```
```
~/MyProject# cogspur/squeak Cuis-Smalltalk-Dev-master/Cuis5.0-3007-spur.image
```

### Notes ###
* If you can't find Cuis5.0-3007-spur.image, then this document is outdated. Use the Cuis spur image with the latest update number available.
* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the the Squeak Cog Spur VM for your platform with the latest Date and Time available.
