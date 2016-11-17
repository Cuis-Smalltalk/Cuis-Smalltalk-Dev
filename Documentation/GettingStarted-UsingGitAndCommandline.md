## Setting up and starting Cuis using Git and the command line

Instructions for setting up Cuis, for Linux Bash, MacOSX command line, or Git Bash on Windows. This method has the advantage to set you up to easily contribute code back to Cuis packages, using Git pull requests. If you are on Windows or Mac, and prefer not using the command line, you might follow [Getting started using Mac Finder or Windows Explorer](GettingStarted-UsingGUI.md).

If you want to contribute back to the community, you might subscribe to the Cuis mail list at http://http://cuis-smalltalk.org/mailman/listinfo/cuis-dev_cuis-smalltalk.org , and email your code there.

The Cuis image specified is the most current one. The VM specified is the latest one too.

### Create project folder and add Cuis Smalltalk ###
```
mkdir MyProject
cd MyProject
git clone https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev.git
```
### Get an appropriate VM ###
For Windows Git Bash:
```
$ curl -o cogspur.zip https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_win32x86_squeak.cog.spur_201611161032.zip
$ unzip cogspur.zip
```
For MacOSX:
```
$ curl -o CogSpur.tgz https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_macos32x86_squeak.cog.spur_201611161032.tar.gz
$ tar -zxvf CogSpur.tgz
```
For Linux (except ArchLinux, Chromebooks see below):
```
~/MyProject# wget -O cogspur.tgz https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_linux32x86_squeak.cog.spur_201611161032.tar.gz
~/MyProject# tar -zxvf cogspur.tgz
~/MyProject# mv ./products/sqcogspurlinuxht ./cogspur
~/MyProject# rmdir ./products
```
On Linux, if this is the first time you run Cuis Smalltalk on this system, add 32 bit libraries.
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
# Next 3 are only needed for experimenting with OpenCL
sudo apt-get install mesa-common-dev:i386
sudo apt-get install libgl1-mesa-dev:i386
~/MyProject# cp /usr/lib/i386-linux-gnu/libOpenCL.so.1 libOpenCL.so
```
For ArchLinux: Get a VM from:
https://www.archlinux.org/packages/?q=squeak-vm
These VMs are not compatible with Spur images. Use the supplied non-Spur image.

### Starting Cuis Smalltalk ###
Windows Git Bash:
```
$  cogspur/squeak.exe Cuis-Smalltalk-Dev/Cuis5.0-2974-spur.image
```
MacOSX:
```
$ CogSpur/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis5.0-2974-spur.image
```
Linux:
```
$  cogspur/squeak Cuis-Smalltalk-Dev/Cuis5.0-2974-spur.image
```

If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 1007290890).", it means your git installation is breaking the files. It is usually best to configure git not to do any conversion on files.

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
* If you can't find Cuis5.0-2974-spur.image, then this document is outdated. Use the Cuis spur image with the latest update number available.
* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the the Squeak Cog Spur VM for your platform with the latest Date and Time available.