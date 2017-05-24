# Setting up and starting Cuis Smalltalk #

Cuis is available in 64 bits and 32 bits formats. It also available in 32 bits V3 (pre Spur) format. All three of them share the full source code and have essentially the same behavior, providing full portability for your code betweem them. These instructions are for the recommended format for each system, although most systems support all of them.

These instructions assume you have the Git code versioning system installed. This has the advantage to set you up to easily contribute code back to Cuis packages, using Git pull requests. If you prefer not to use Git, see details at the end of this document. If you are on Windows or Mac, and prefer not using the command line, you might follow [Getting started using Mac Finder or Windows Explorer](GettingStarted-NoCommandLine.md).

If you want to contribute back to the community, you can do pull requests to this repo and/or alternatively subscribe to the Cuis development mail list at http://cuis-smalltalk.org/mailman/listinfo/cuis-dev_cuis-smalltalk.org , and email your code there.

## For all systems: Create project folder and add Cuis Smalltalk ##
```
mkdir MyProject
cd MyProject
git clone https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev.git
```

## For 64 bits Linux running on AMD/Intel x64 hardware ##

### Get and set up an appropriate VM ###
```
~/MyProject# rm -r cogspur64
~/MyProject# wget -O cogspur64.tgz https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_linux64x64_squeak.cog.spur_201705131804.tar.gz
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

### Starting Cuis Smalltalk ###
```
~/MyProject# cogspur64/squeak Cuis-Smalltalk-Dev/Cuis5.0-3081-spur-64.image
```

## For MacOSX ##

### Get and set up an appropriate VM ###
```
$ curl -L -o CogSpur64.tgz https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_macos64x64_squeak.cog.spur_201705131804.tar.gz
$ tar -zxvf CogSpur64.tgz
```

### Starting Cuis Smalltalk ###

```
$ ./Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev-master/Cuis5.0-3081-spur-64.image
```

## For Windows Git Bash ##

Currently we recommend using 32 bits Cuis, both on 64 bits and 32 bits Windows systems, as the 64 bits Windows VM is still under development.

### Get and set up an appropriate VM ###
```
$ rm -r cogspur
$ curl -L -o cogspur.zip https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_win32x86_squeak.cog.spur_201705131804.zip
$ unzip cogspur.zip -d cogspur
$ mv ./cogspur/build/vm/*.* ./cogspur
```

### Starting Cuis Smalltalk ###
```
$ cogspur/squeak.exe Cuis-Smalltalk-Dev/Cuis5.0-3081-spur.image
```

## For Chromebooks ##

You can use Linux as a chroot in ChromeOS using Crouton to run a Cuis image.

First you need to follow the directions for installing Crouton at
	https://github.com/dnschneid/crouton

Then follow the Linux directions above -- except for choosing the VM to use.

The reason for this is that JIT (Just In Time) compiling is disabled by ChromeOS.  This means that Cog VMs will not work.

Remember to use the Spur Cuis image when using Spur VMs and the non-Spur image when using non-Spur VMs.

### For Intel CPUs ###
```
$ wget -O linuxVMx86.sh http://squeakvm.org/unix/release/Squeak-4.10.2.2614-linux_i386.sh
$ ./linuxVMx86.sh
```

### For ARM CPUs ###
```
$ wget -O stklinuxARM.tgz www.mirandabanda.org/files/Cog/VM/2015/VM.r3386/stklinuxARM-15.24.3386.tgz
$ tar -zxvf stklinuxARM.tgz
```

## For Rasberry Pi Linux ##
```
$ wget -O linuxVM_ARM.sh http://squeakvm.org/unix/release/Squeak-4.10.2.2765-linux_armv6l.sh
$ ./linuxVM_ARM.sh
```

## Notes ##
* If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 68021).", (68021 or some other reasonable number) it means you image and VM are mismatched. For example, one of them is Spur and the other is pre-Spur, or one of them is 32 bits and the other is 64 bits.
* If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 1007290890).", (1007290890 or some other absurd number) it means your git installation is breaking the files. It is usually best to configure git not to do any conversion on files.
* If you can't find Cuis5.0-3081-spur.image, then this document is outdated. Use the Cuis spur image with the latest update number available.
* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the the Squeak Cog Spur VM for your platform with the latest Date and Time available.
* To get the contents of this repository without using Git, you can do
```
wget https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
```
or
```
curl -L -o master.zip  https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
```
and extract the contents of the zip file.