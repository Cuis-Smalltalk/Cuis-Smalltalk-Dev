## Setting up and starting Cuis using Git and the command line

Instructions for setting up Cuis, for Linux Bash, MacOSX command line, or Git Bash on Windows. This method has the advantage to set you up to easily contribute code back to Cuis packages, using Git pull requests.

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
$ curl -o cogwin.zip http://www.mirandabanda.org/files/Cog/VM/latest/cogwin-16.18.3692.zip
$ unzip cogwin.zip
```
For MacOSX:
```
$ curl -o Cog.app.tgz http://www.mirandabanda.org/files/Cog/VM/latest/Cog.app-16.18.3692.tgz
$ tar -zxvf Cog.app.tgz
```
For Linux (except ArchLinux, Chromebooks see below):
```
$ wget -O coglinuxht.tgz http://www.mirandabanda.org/files/Cog/VM/latest/coglinuxht-16.18.3692.tgz
$ tar -zxvf coglinuxht.tgz
```
On Linux, if this is the first time you run Cuis Smalltalk on this system, add 32 bit libraries.
(This was tested on Debian 8)
```
sudo dpkg --add-architecture i386
sudo apt-get update
sudo apt-get install libc6-i386
# This (libc6-i386) was needed to make ldd work on 32 bit programs and dynamic libraries
# So, we can do stuff like       $ ldd coglinuxht/lib/squeak/4.5-3370/squeak       to find about missing libraries
sudo apt-get install libuuid1:i386
sudo apt-get install libX11-6:i386
sudo apt-get install libXext6:i386
sudo apt-get install libsm6:i386
# Next 3 are only needed for experimenting with OpenCL
sudo apt-get install mesa-common-dev:i386
sudo apt-get install libgl1-mesa-dev:i386
cp /usr/lib/i386-linux-gnu/libOpenCL.so.1 libOpenCL.so
```
For ArchLinux: Get a VM from:
https://www.archlinux.org/packages/?q=squeak-vm

### Starting Cuis Smalltalk ###
Windows Git Bash:
```
$  cogwin/squeak.exe Cuis-Smalltalk-Dev/Cuis4.2-2778.image
```
MacOSX:
```
$ Cog.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis4.2-2778.image
```
Linux:
```
$  coglinuxht/squeak Cuis-Smalltalk-Dev/Cuis4.2-2778.image
```

If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 1007290890).", it means your git installation is breaking the files. It is usually best to configure git not to do any conversion on files.

### For Chromebooks ###

You can use Linux as a chroot in ChromeOS using Crouton to run a Cuis image.

First you need to follow the directions for installing Crouton at
	https://github.com/dnschneid/crouton

Then follow the Linux directions above -- except for choosing the VM to use.

The reason for this is that JIT (Just In Time) compiling is disabled by ChromeOS.  This means that Cog VMs will not work.

Note that there is no Cuise image in Spur format yet, so avoid Spur VMs for now.

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
