## Setting up and starting Cuis using Git and the command line

These are instructions for setting up Cuis in the 64 bits flavor, on Linux using Git Bash. This method has the advantage to set you up to easily contribute code back to Cuis packages, using Git pull requests. If you are on Windows or Mac, and prefer not using the command line, you might follow [Getting started using Mac Finder or Windows Explorer](GettingStarted-UsingGUI.md).

If you want to contribute back to the community, you might subscribe to the Cuis mail list at http://cuis-smalltalk.org/mailman/listinfo/cuis-dev_cuis-smalltalk.org , and email your code there.

The Cuis image specified is the most current one. The VM specified is the latest one too.

### Create project folder and add Cuis Smalltalk ###
```
mkdir MyProject
cd MyProject
~/MyProject# git clone https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev.git
```
### Get an appropriate VM ###
For Linux 
```
~/MyProject# wget -O cogspur.tgz https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201608171728/cog_linux32x86_squeak.cog.spur_201608171728.tar.gz
~/MyProject# tar -zxvf cogspur.tgz
~/MyProject# mv ./products/cogspurlinuxht ./cogspur
# Next is only needed if you want to play with OpenCL (you also need Catalyst driver for AMD GPU, or similar for Intel/Nvidia)
~/MyProject# cp /usr/lib/libOpenCL.so.1 libOpenCL.so
```

### Starting Cuis Smalltalk ###
Linux:
```
$  cogspur/squeak Cuis-Smalltalk-Dev/Cuis5.0-3007-spur.image
```

If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 68021).", (68021 or some other reasonable number) it means you image and VM are mismatched. For example, one of them is Spur and the other is pre-Spur, or one of them is 32 bits and the other is 64 bits.

If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 1007290890).", (1007290890 or some other absurd number) it means your git installation is breaking the files. It is usually best to configure git not to do any conversion on files.

### For ArchLinux and other not based on Debian or Ubuntu ###

The instructions above are known to work on Debian and Ubuntu. They might work on other distributions, but you might need to find the correct incatation for your distro. If you do, please email the details to the mail list.

For ArchLinux, as an alternative you can get a VM from:
https://www.archlinux.org/packages/?q=squeak-vm
These VMs are not compatible with Spur images. Use the supplied non-Spur image.

### For Chromebooks ###

You can use Linux as a chroot in ChromeOS using Crouton to run a Cuis image.

First you need to follow the directions for installing Crouton at
	https://github.com/dnschneid/crouton

Then follow the Linux directions above -- except for choosing the VM to use.

The reason for this is that JIT (Just In Time) compiling is disabled by ChromeOS.  This means that Cog VMs will not work.

Remember to use the Spur Cuis image when using Spur VMs and the non-Spur image when using non-Spur VMs.

For Intel CPUs:
```
$ wget -O linuxVMx86.sh http://squeakvm.org/unix/release/Squeak-4.10.2.2614-linux_i386.sh
$ ./linuxVMx86.sh
```

For ARM CPUs:
```
$ wget -O stklinuxARM.tgz www.mirandabanda.org/files/Cog/VM/2015/VM.r3386/stklinuxARM-15.24.3386.tgz
$ tar -zxvf stklinuxARM.tgz
```

### For Rasberry Pi Linux ###
```
$ wget -O linuxVM_ARM.sh http://squeakvm.org/unix/release/Squeak-4.10.2.2765-linux_armv6l.sh
$ ./linuxVM_ARM.sh
```

### Notes ###
* If you can't find Cuis5.0-3007-spur.image, then this document is outdated. Use the Cuis spur image with the latest update number available.
* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the the Squeak Cog Spur VM for your platform with the latest Date and Time available.