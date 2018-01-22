# Setting up and starting Cuis Smalltalk #

Cuis Smalltalk is available in 64 bits and 32 bits variants. It also available in the 32 bits V3 (pre Spur) image format. All three of them share the full source code and have essentially the same behavior, providing full portability for your code between them. These instructions are for the recommended format for each system, although most systems support all of them.

In addition, the [SqueakJS](https://squeak.js.org) JavaScript Squeak VM that can run in a web browser can run our -32 and our -v3 images.

These instructions assume you have the Git code versioning system installed. This has the advantage to set you up to easily contribute code back to Cuis packages, using Git pull requests. If you prefer not to use Git, see details at the end of this document. If you are on Windows or Mac, and prefer not using the command line, you might follow [Getting started using Mac Finder or Windows Explorer](GettingStarted-NoCommandLine.md).

If you want to contribute back to the community, you can do pull requests to this repo and/or alternatively subscribe to the Cuis development mail list at http://cuis-smalltalk.org/mailman/listinfo/cuis-dev_cuis-smalltalk.org , and email your code there.

## For all systems: Create project folder and add Cuis Smalltalk ##
```
mkdir MyProject
cd MyProject
git clone https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev.git
```

## For 64 bits Linux running on AMD/Intel x64 hardware ##

Note: For 32 bits Linux on Intel/AMD, use ```*_linux32x86_*``` vm and the ```*-32.image``` Cuis image.

### Get and set up an appropriate VM ###
```
cd MyProject
rm -r cogspur
wget -O cogspur.tgz https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_linux64x64_squeak.cog.spur_201712121955.tar.gz
tar -zxvf cogspur.tgz
mv ./sqcogspur64linuxht ./cogspur
```

### Starting Cuis Smalltalk ###
```
cogspur/squeak Cuis-Smalltalk-Dev/Cuis5.0-3257.image
```
If you get this error message (you won't get it if you run Cuis as root or sudo):
```
pthread_setschedparam failed: Operation not permitted
...
https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/tag/r3732#linux
```
Then you need to do this (just one time):
```
sudo cp Cuis-Smalltalk-Dev/squeak.conf /etc/security/limits.d/squeak.conf
```
Log out and log back in, or reboot the machine.

## For MacOSX ##

### Get and set up an appropriate VM ###
```
cd MyProject
curl -L -o CogSpur.tgz https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_macos64x64_squeak.cog.spur_201708312323.tar.gz
tar -zxvf CogSpur.tgz
```

### Starting Cuis Smalltalk ###

```
./Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev-master/Cuis5.0-3257.image
```

## For 64 bits Windows (Git Bash) ##

Note: For 32 bits Windows, use ```*_win32x86_*``` vm and the ```*-32.image``` Cuis image.

### Get and set up an appropriate VM ###
```
cd MyProject
rm -r cogspur
curl -k -L -o cogspur.zip https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_win64x64_squeak.cog.spur_201709050420.zip
unzip cogspur.zip -d cogspur
mv ./cogspur/build/vm/*.* ./cogspur
```

### Starting Cuis Smalltalk ###
```
cogspur/squeak.exe Cuis-Smalltalk-Dev/Cuis5.0-3257.image
```

## For Raspberry Pi Raspian ##

### Get and set up an appropriate VM ###
```
cd MyProject
rm -r cogspur
wget -O cogspur.tgz https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_linux32ARMv6_squeak.cog.spur_201708312323.tar.gz
tar -zxvf cogspur.tgz
mv ./sqcogspurlinuxhtRPi ./cogspur
```

### Starting Cuis Smalltalk ###
```
cogspur/squeak Cuis-Smalltalk-Dev/Cuis5.0-3257-32.image
```

## For Chromebooks ##

You can use Linux as a chroot in ChromeOS using Crouton to run a Cuis image.

First you need to follow the directions for installing Crouton at
	https://github.com/dnschneid/crouton

Note: JIT (Just In Time) compiling is disabled by ChromeOS. This means that *_squeak.cog.* VMs will not work.

### For Intel CPUs: Get and set up an appropriate VM ###
```
cd MyProject
rm -r cogspur
wget -O cogspur.tgz https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_linux32x86_squeak.cog.spur_201708312323.tar.gz
tar -zxvf cogspur.tgz
mv ./sqcogspurlinuxht ./cogspur
```
### For ARM CPUs: Get and set up an appropriate VM ###
```
cd MyProject
rm -r stkspur
wget -O stkspur.tgz https://bintray.com/opensmalltalk/vm/download_file?file_path=cog_linux32ARMv6_squeak.stack.spur_201708312323.tar.gz
tar -zxvf stkspur.tgz
mv ./sqstkspurlinuxhtRPi ./stkspur
```

### Starting Cuis Smalltalk ###
```
cogspur/squeak Cuis-Smalltalk-Dev/Cuis5.0-3257-32.image
stkspur/squeak Cuis-Smalltalk-Dev/Cuis5.0-3257-32.image
```

## Notes ##
* If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 68021).", (68021 or some other reasonable number) it means you image and VM are mismatched. For example, one of them is Spur and the other is pre-Spur, or one of them is 32 bits and the other is 64 bits.
* If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 1007290890).", (1007290890 or some other absurd number) it means your git installation is breaking the files. It is usually best to configure git not to do any conversion on files.
* If you can't find Cuis5.0-3257-32.image, then this document is outdated. Use the Cuis spur image with the latest update number available.
* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the the Squeak Cog Spur VM for your platform with the latest Date and Time available from http://opensmalltalk.org/
* To get the contents of this repository without using Git, you can do
```
wget https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
```
or
```
curl -L -o master.zip  https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/archive/master.zip
```
and extract the contents of the zip file.
