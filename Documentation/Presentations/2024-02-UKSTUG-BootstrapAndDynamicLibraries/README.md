# Bootstrap and Dynamic Libraries

[Presentation](BootstrapAndDynamicLibraries.pdf) given by Juan at the [UK Smalltalk User Group Feb 2024](https://www.uksmalltalk.org/2024/02/juan-vuletich-bootstrap-dynamic-cuis.html) online meeting.

The video of the presentation will be available online soon.

### Bootstrap: Creating Minimal Images from Scratch

The last ancestor of Cuis Smalltalk that was bootstrapped from scratch was Smalltalk-76. Since then, various released images of Smalltalk-80, Squeak and Cuis were derived by applying updates to the previous one. A new tool called 'Bootstrap' allows the creation of minimal Smalltalk images from scratch. These images are in the Spur 32 and 64 bit formats, compatible with the OpenSmalltalk VM. 'Bootstrap' gives developers complete control over what is included in the new image. It is also compact, relatively simple, and easy to extend and adapt.

### Dynamic Cuis Libraries: A binary format for Cuis code that is powerful and quick to load

Dynamic Cuis Libraries are a binary files with pre-compiled code that can be loaded into a running Cuis image. They can add new classes and extend existing ones. For existing classes, there are no requirements on the shape of that class in the image loading the library. Missing variables are added, extra variables are kept, and both existing and newly loaded methods are adjusted to whatever shape the class ends up having.
