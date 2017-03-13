## Setting up and starting Cuis using Git and the command line

Instructions for setting up Cuis using Git, for Linux Bash, MacOSX command line, or Git Bash on Windows. This method has the advantage to set you up to easily contribute code back to Cuis packages, using Git pull requests. If you are on Windows or Mac, and prefer not using the command line, you might follow [Getting started using Mac Finder or Windows Explorer](GettingStarted-NoCommandline.md).

If you want to contribute back to the community, you might subscribe to the Cuis mail list at http://cuis-smalltalk.org/mailman/listinfo/cuis-dev_cuis-smalltalk.org , and email your code there.

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
$ curl -L -o cogspur.zip https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201608171728/cog_win32x86_squeak.cog.spur_201608171728.zip
$ unzip cogspur.zip
```
For MacOSX:
```
$ curl -L -o CogSpur.tgz https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201608171728/cog_macos32x86_squeak.cog.spur_201608171728.tar.gz
$ tar -zxvf CogSpur.tgz
```
For Linux on x86 hardware
- For x86-64 hardware systems, we recommend [Setting up and starting Cuis 64 bits using a command line](GettingStarted-Linux64-Commandline.md)
- Alternative instructions for ArchLinux below
- Specific instructions for Chromebooks and Raspberry Pi below
```
~/MyProject# rm -r cogspur
~/MyProject# wget -O cogspur.tgz https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201608171728/cog_linux32x86_squeak.cog.spur_201608171728.tar.gz
~/MyProject# tar -zxvf cogspur.tgz
~/MyProject# mv ./products/cogspurlinuxht ./cogspur
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
sudo apt-get install libx11-6:i386
sudo apt-get install libxext6:i386
sudo apt-get install libsm6:i386
# Next are only needed if you want to play with OpenCL (you also need Catalyst driver for AMD GPU, or similar for Intel/Nvidia)
sudo apt-get install mesa-common-dev:i386
sudo apt-get install libgl1-mesa-dev:i386
# If you are not using AMD Catalyst driver, you might also need:
sudo apt-get install ocl-icd-libopencl1:i386
cp /usr/lib/i386-linux-gnu/libOpenCL.so.1 libOpenCL-32bit.so
```
If you get this error message (you won't get it if you run Cuis as admin or sudo):
```
pthread_setschedparam failed: Operation not permitted
Read e.g. https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/tag/r3732#linux
```
Then you need to:
```
~/MyProject# sudo cp Cuis-Smalltalk-Dev/squeak.conf /etc/security/limits.d/squeak.conf
```
Log out and log back in, or reboot the machine.

### Starting Cuis Smalltalk ###
Windows Git Bash:
```
$  cogspur/squeak.exe Cuis-Smalltalk-Dev/Cuis5.0-3064-spur.image
```
MacOSX:
```
$ CogSpur/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis5.0-3064-spur.image
```
Linux:
```
$  cogspur/squeak Cuis-Smalltalk-Dev/Cuis5.0-3064-spur.image
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
* If you can't find Cuis5.0-3064-spur.image, then this document is outdated. Use the Cuis spur image with the latest update number available.
* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the the Squeak Cog Spur VM for your platform with the latest Date and Time available.
* If you want to use a Spur64 VM, use the latest from www.opensmalltalk.org (it is still under heavy development and might be less stable than 32 bit VMs)