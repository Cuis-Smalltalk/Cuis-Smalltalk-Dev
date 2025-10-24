## How to build the CuisVM.app multiplatform VM bundle

This procedure bundles together the VMs for various platforms (MacOSx64, MacOSARM, Win64x64, Win64ARM, LinuxX64). The result is the VM we include with Cuis. It was tested on all these platforms.

1) Follow HowToBuildMacUnifiedVM.md

2) Create subfolder Multiplatform

3) Copy Mac unified VM there. Rename it as CuisVM.app. Show package contents

4) Inside Cuis.app / Contents create folders Windows-x86_64, Windows-arm64, Linux-x86_64 and Linux-arm64

5) Fill them with the contents of 
squeak.cog.spur_win64x64_????????????.zip, squeak.cog.spur_win64ARMv8_????????????, squeak.cog.spur_linux64x64_????????????.zip, squeak.cog.spur_linux64ARMv8_????????????.zip

6) In each of the Windows folders, do:
chmod +x *

The result is a CuisVM.app that by doing rightClick / Quick Look is about 32.1Mb
