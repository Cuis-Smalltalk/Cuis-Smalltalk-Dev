## Setting up and starting Cuis using Git and the command line

Instructions for setting up Cuis, for Linux Bash, MacOSX command line, or Git Bash on Windows. This method has the advantage to set you up to easily contribute code back to Cuis packages, using Git pull requests.

### Create project folder and add Cuis Smalltalk ###
```
mkdir MyProject
cd MyProject
git clone https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev.git
```
### Get an appropriate VM ###
For Windows Git Bash:
```
$ curl -o cogwin.zip www.mirandabanda.org/files/Cog/VM/VM.r3370/cogwin-15.22.3370.zip
$ unzip cogwin.zip
```
For MacOSX:
```
$ curl -o Cog.app.tgz www.mirandabanda.org/files/Cog/VM/VM.r3370/Cog.app-15.22.3370.tgz
$ tar -zxvf Cog.app.tgz
```
For Linux (except ArchLinux, see below):
```
$ wget -O coglinuxht.tgz www.mirandabanda.org/files/Cog/VM/VM.r3370/coglinuxht-15.22.3370.tgz
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
$  cogwin/squeak.exe Cuis-Smalltalk-Dev/Cuis4.2-2568.image
```
MacOSX:
```
$ Cog.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis4.2-2568.image
```
Linux:
```
$  coglinuxht/squeak Cuis-Smalltalk-Dev/Cuis4.2-2568.image
```

If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 1007290890).", it means your git installation is breaking the files. It is usually best to configure git not to do any conversion on files.