## How to create a Mac Unified VM

This procedure builds a VM that includes both the Apple Silicon and Intel binaries and runs natively on both flavors of Mac hardware. This Mac Unified VM is the base of the MultiPlatform Cuis VM Bundle.

1) Build or download Mac VM builds from https://github.com/OpenSmalltalk/opensmalltalk-vm/actions or from https://github.com/OpenSmalltalk/opensmalltalk-vm/releases . Use squeak.cog.spur_macos64x64_202412090037.zip, squeak.cog.spur_macos64ARMv8_202412090037.zip or similarly named files. Or your pick, if you know the differences. 

2) Create folder UnifyMacVM. cd to it.

3) Create a folder named after the date of the VM builds, like 24-12-08 (yes. you're likely to repeat all this in the future). cd to it.

4) Add unify.sh there.

5) Create subfolder ARM, containing ARM Squeak.app (Unzip. Mount *.dmg. Extract *.app.)

6) Create subfolder Intel, containing Intel Squeak.app (Unzip. Mount *.dmg. Extract *.app.)

7) Create folder Unified

8) Run ./unify.sh

Done. Unified/Squeak.app is our Unified VM. By doing rightClick / Quick Look is about 7.2Mb
(Thanks Cristián Pérez for this recipe)

