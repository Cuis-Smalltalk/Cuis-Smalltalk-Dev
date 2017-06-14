## Specific instructions for running a 32Bit Cuis and VM in a 64Bit X64 Linux system
On Linux 64 bit, if this is the first time you run Cuis Smalltalk 32 bit on this system, add 32 bit libraries. This was tested on Debian 8.

```
sudo dpkg --add-architecture i386
sudo apt-get update
sudo apt-get install libc6-i386
# This (libc6-i386) was needed to make ldd work on 32 bit programs and dynamic libraries
# So, we can do stuff like       $ ldd cogspur32/lib/squeak/*/squeak       to find about missing libraries
sudo apt-get install libuuid1:i386
sudo apt-get install libx11-6:i386
sudo apt-get install libxext6:i386
sudo apt-get install libsm6:i386
# Next are only needed if you want to play with OpenCL (you also need Catalyst driver for AMD GPU, or similar for Intel/Nvidia)
sudo apt-get install mesa-common-dev:i386
sudo apt-get install libgl1-mesa-dev:i386
# If you are not using AMD Catalyst driver, you might also need:
sudo apt-get install ocl-icd-libopencl1:i386
# To enable OpenCL support, you need to add the OpenCL driver (the "ICD loader") to the VM plugins folder. For example, For AMD Catalyst driver (AMD GPUs)
cp /usr/lib/i386-linux-gnu/libOpenCL.so.1 cogspur32/lib/squeak/5.0-201706141439/libOpenCL.so.1
```

Note: MacOS and Windows can run 32Bit Cuis without any additional setup.