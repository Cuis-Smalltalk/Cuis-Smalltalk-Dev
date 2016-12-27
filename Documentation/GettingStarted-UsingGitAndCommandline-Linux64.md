## Setting up and starting Cuis 64 bits using Git and the command line

These are instructions for setting up Cuis in the 64 bits Spur flavor, on Linux on x86-64 computers using Git Bash. This method has the advantage to set you up to easily contribute code back to Cuis packages, using Git pull requests.

If you want to contribute back to the community, you might subscribe to the Cuis mail list at http://cuis-smalltalk.org/mailman/listinfo/cuis-dev_cuis-smalltalk.org , and email your code there.

The Cuis image specified is the most current one. The VM specified is the latest one too.

### Create project folder and add Cuis Smalltalk ###
```
mkdir MyProject
cd MyProject
~/MyProject# git clone https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev.git

```

### Get an appropriate VM ###
```
~/MyProject# wget -O cogspur.tgz https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201608171728/cog_linux32x86_squeak.cog.spur_201608171728.tar.gz
~/MyProject# tar -zxvf cogspur.tgz
~/MyProject# mv ./products/cogspurlinuxht ./cogspur
```
Next is only needed if you want to play with OpenCL (you also need Catalyst driver for AMD GPU, or similar for Intel/Nvidia)
```
~/MyProject# cp /usr/lib/libOpenCL.so.1 libOpenCL.so
```

### Starting Cuis Smalltalk ###
```
~/MyProject# cogspur/squeak Cuis-Smalltalk-Dev/Cuis5.0-3007-spur.image
```

### Notes ###

* If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 68021).", (68021 or some other reasonable number) it means you image and VM are mismatched. For example, one of them is Spur and the other is pre-Spur, or one of them is 32 bits and the other is 64 bits.

* If when starting the image you get error messages like "This interpreter (vers. 6505) cannot read image file (vers. 1007290890).", (1007290890 or some other absurd number) it means your git installation is breaking the files. It is usually best to configure git not to do any conversion on files.

* If you can't find Cuis5.0-3007-spur.image, then this document is outdated. Use the Cuis spur image with the latest update number available.

* If you can't find the Squeak Cog Spur VM specified, then this document is outdated. Use the the Squeak Cog Spur VM for your platform with the latest Date and Time available.
