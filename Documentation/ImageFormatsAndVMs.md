# On Image format variants and supported VMs

Most people don't need to care about image formats and just need to follow [Setting up and starting Cuis Smalltalk](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/GettingStarted.md). But if you have specific needs or are interested in this, keep reading.

The Squeak Smalltalk family (of which Cuis is part) has used several image formats over the years. See [ImageFormat](http://wiki.squeak.org/squeak/6290). Cuis offers broad compatibility with available VMs and various tradeoffs between performance and VM complexity.

There are currently three image formats that are being actively supported in Cuis. Checking this with the ckformat utility, we have this in Cuis:

   lewis@lewis-Gazelle-Pro:~/squeak/Cuis/Cuis-Smalltalk-Dev$ for i in *.image; do echo $i `ckformat $i`; done
   Cuis5.0-3196.image 68021
   Cuis5.0-3196-32.image 6521
   Cuis5.0-3196-v3.image 6505

* 68021 is 64-bit Spur, and works with 64-bit Spur VMs

* 6521 is 32-bit Spur, and works with 32 bit Spur VMs. It also works on [SqueakJS](https://squeak.js.org).

* 6505 is the original V3 32-bit image format that works with StackInterpreter, Cog, and the traditional Interpreter VM. It also works on [SqueakJS](https://squeak.js.org).
