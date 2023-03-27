
### Build your own VM ###

If you are comfortable using Linux tools (gcc,make,ld), you can
build the OpenSmalltalk VM yourself.

This may be helpful to obtain a VM for a platform which is supported,
but not part of the official release builds.  For example, Linux on Aarch64/arm64 or RiscV64.

The process is basically:
```git clone --depth 1 https://github.com/OpenSmalltalk/opensmalltalk-vm
cd opensmalltalk-vm/building/linux64ARMv8  [ pick os+cpu ]
  [ read HowToBuild to get required libraries ]
cd squeak.cog.spur/build    [or squeak.stack.spur/build ]
./mvm
[ Answer `y` to "clean?" ]
```

"stack" => bytecode interpreter; "cog" => JIT compiler (faster)

If you build a 64bit VM, don't forget to use the 64bit image: e.g. Cuis-Smalltalk-Dev/Cuis6.0-5542.image
