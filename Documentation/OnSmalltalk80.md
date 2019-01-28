# Cuis is a Smalltalk-80 system. Why? #

We say that Cuis is a Smalltalk-80 system. But Cuis is a significant evolution of Squeak, that is in turn a significant evolution of Smalltalk-80. Cuis is a 64 bit system that runs on modern platforms and makes good use of modern hardware. Cuis includes many aids, tools and ideas that didn't exist in Smalltalk-80. Our development community is open, diverse and distributed all over the world. So, 35 years after Smalltalk-80, what does it mean to say that Cuis is a Smalltalk-80 system?  

Essentially it means that we follow ["Design Principles Behind Smalltalk"](http://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html). We keep those features of Smalltalk-80 that give the user unlimited access to the system internals. Cuis is open for you to learn, experiment and improve.  

## Smalltalk-80 object model ##
This is something shared by all Smalltalk system, incuding those that don't call themselves Smalltalk-80 because they departed in some other way.

## Independence of the underlying platform ##
The VM provides an abstraction and interface to the hardware and operating system, so Cuis doesn't need to know much about them. This means that almost all Smalltalk code is platform agnostic and portable. It also means that the User Interface is written in Smalltalk, and that the platform services we use (display and user input) are called in a platform independent way. In Smalltalk-80 the UI was MVC and in Cuis it is Morphic, but this doesn't sacrifice portability or adaptability. Where needed, applications might call specific platform facilities via FFI.

## Smalltalk-80 execution semantics ##
We use green threads on a single host OS process. Process switch can be controlled from within Smalltalk. Process scheduling is deterministic.
