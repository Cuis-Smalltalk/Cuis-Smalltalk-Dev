## Setting up and starting Cuis on Unix (Linux or Mac OS X) using Git and the command line

What follows are alternative instructions for setting up Cuis on Linux or Mac OS X using Git and the command line. If you are on Windows, there are instructions for using the Windows file explorer in another document. This document is intended for people comfortable with Git and the command line, desiring a convenient setup allowing for several Cuis directories for different projects.

```
mkdir MyProject
cd MyProject
git clone https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev.git
Cuis-Smalltalk-Dev/bin/newImage.sh MyProject
```
The last line creates a new image  named MyProject with a current Cog VM. Start Cuis by dragging MyProject.image onto the Cog VM.

To start the new Cuis image execute the following on OS X:
```
Cog.app/Contents/MacOS/Squeak MyProject.image
```
or on Linux:
```
coglinux/bin/squeak MyProject.image
```
This method has the advantage to set you up to easily contribute code back to Cuis packages.
If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 1007290890).", it means your git installation is breaking the files. It is usually best to configure git not to do any conversion on files.