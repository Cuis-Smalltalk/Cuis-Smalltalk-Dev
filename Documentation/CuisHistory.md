# A short story of Cuis #

## September, 2004 - Etoys free Morphic ##
This is the point where the Smalltalk image to be called Cuis is forked from Squeak 3.7. The initial objective was to remove Etoys and other applications, giving an app agnostic Morphic environment. Work started on September 2004, shortly after Squeak 3.7 was released.  
http://lists.squeakfoundation.org/pipermail/squeak-dev/2005-February/087571.html  
http://lists.squeakfoundation.org/pipermail/squeak-dev/2005-February/087756.html  
http://lists.squeakfoundation.org/pipermail/squeak-dev/2005-February/088763.html  
http://lists.squeakfoundation.org/pipermail/squeak-dev/2005-February/088787.html  
http://www.jvuletich.org/Squeak/EToysFreeMorphic/EtoysFreeMorphic.html  

## 2007-07-26 - Cheap, High Quality Fonts in Squeak ##
Until this moment, Squeak only had 1 bit bitmap fonts that used to look reasonable on cathode ray displays, but look extremely pixelated on any modern display. I (Juan) developed the techniques to allow high quality, anti-aliased, sub-pixel rendered bitmap fonts with the existing VM support, including colored and alpha-blended text. I also built several font sets, that got better over time. This work was developed for my fork, but was also adopted by Squeak and Pharo.  
http://www.jvuletich.org/issues/Issue0010.htm  
http://lists.squeakfoundation.org/pipermail/squeak-dev/2007-April/115930.html  
http://lists.squeakfoundation.org/pipermail/squeak-dev/2007-April/115948.html  

## 2009-03-27 - Cuis 1.0 ##
[Ann] Cuis: A new Squeak distribution  
When it started to be clear that neither Squeak nor Pharo would rebase their projects on my reduced kernel image, I (Juan) decided to turn it into an Open Source, Community Maintained, Smalltalk system, independent of all others.
http://lists.squeakfoundation.org/pipermail/squeak-dev/2009-March/134986.html  

## 2010-01-4 - Cuis 2.0 ##
Support for true BlockClosures. Requires a closures-enabled VM.  
When full block closures were implemented in Squeak, and support added to the VM, we adopted them. At the same time, Cuis keeps advancing towards our objectives, and many parts of the system get cleaned and simplified.

## 2011-01-14 - Cuis 3.0 ##
New, modern look. Themes. We keep improving Cuis usability, and make the development tools look better, including less colorful Dark, Light and HighContrast UI themes.   

## 2012-04-21 - Cuis 4.0 ##
Packages. In addition to the ever shrinking Kernel Image, we enable the development of code Packages that can be loaded as needed. This lets us decouple and better structure different parts of the system, and better distribute their development amongst developers. Over time, we developed over 30 packages that are distributed with Cuis, and over 20 GitHub repositories with additional packages developed and maintained by community members.   

## 2012-05-16 - New Cuis mail list ##
We decided that a discussion forum specific for Cuis is a good idea. Still, most people are also active members of the Squeak and/or Pharo communities.  
http://lists.squeakfoundation.org/pipermail/squeak-dev/2012-May/164142.html  

## 2013-07-14 - GitHub Repo ##
https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev  
Adopting GitHub as our code repository greately eases project managing, gives us wider visibility, and protects the future of the project.

## 2016-11-7 - Cuis5.0 ##
The [OpenSmalltalk](http://www.opensmalltalk.org) project keeps developing modern VMs for Open Source Smalltalk systems. We add Cuis images in the new Spur 32 and 64 bits formats to be used with them, in addition to the existing V3 32 bit image for existing VMs. Cuis is the only Smalltalk system that runs with exactly the same source code on 32 and 64 bits, and with many VM flavors and platforms, including:
- Cog Spur 64 (High Performance 64 bits, jitted, for Intel)  
- Cog Spur 32 (High Performance 32 bits, jitted, for Intel)  
- Cog V3  (Good performance 32 bits, jitted, for Intel) 
- V3 Classic Interpreter (32 bits, portable code. Runs on any processor)
- SqueakJS (32 bits, runs in a web browser on any platform)
So we can run in at least one VM flavor in MacOS, Linux (Intel, ARM), Windows and Web Browsers. With some effort it is possible to run on Android, IOS, RISC OS. We also have ran on Solaris, OS/2, and bare metal.  
All this also means that we don't need to maintain forked code bases to support this wide array of VMs and platforms. Tool and Application developers also need to focus on a single code base and always get platform independence.   
