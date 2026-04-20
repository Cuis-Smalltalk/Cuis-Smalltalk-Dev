## How to create a Mac Unified VM

This procedure builds a Mac VM that includes both the Apple Silicon and Intel binaries and runs natively on both flavors of Mac hardware. This Mac Unified VM is the base of the MultiPlatform Cuis VM Bundle.

1) Build or download Mac VM builds from https://github.com/OpenSmalltalk/opensmalltalk-vm/actions or from https://github.com/OpenSmalltalk/opensmalltalk-vm/releases . Use squeak.cog.spur_macos64x64.dmg and squeak.cog.spur_macos64ARMv8.dmg. 

2) Create folder CuisVMBundle. cd to it.

3) Create a folder named after the date of the VM builds, like 2026-04-20 (yes. you're likely to repeat all this in the future). cd to it.

4) Add unify.sh there.

5) Create subfolder ARM, containing ARM Squeak.app (Mount dmg. Drag Squeak.app to ARM folder)

6) Create subfolder Intel, containing Intel Squeak.app (Mount dmg. Drag Squeak.app to Intel folder)

7) Create folder Unified

8) Run ./unify.sh
If you get a message such as "You have not agreed to the Xcode and Apple SDKs license...." you may need to `sudo xcodebuild -license`, then "agree" or whatever Apple comes up next.

Done. Unified/Squeak.app is our Unified VM. Its Contents folder is about 7.2Mb.
(Thanks Cristián Pérez for this recipe)

