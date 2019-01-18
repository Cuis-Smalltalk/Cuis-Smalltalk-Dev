# Cuis is a Smalltalk-80 system. Why? #

We say that Cuis is a Smalltalk-80 system. What do we mean by that? Actually, we mean several things.

## Smalltalk-80 object model ##
This is something shared by all Smalltalk system, incuding those that don't call themselves Smalltalk-80.

## Independence of the underlying platform ##
The VM provides an abstraction and interface to the hardware and operating system, so Cuis doesn't need to know much about them. This means that Smalltalk code is usually platform agnostic and portable. It also means that the User Interface is written in Smalltalk, and that the platform services we use (display and user input) are called in a platform independent way. In Smalltalk-80 the UI was MVC and in Cuis it is Morphic, but this doesn't sacrifice portability or adaptability. Where needed, applications might call specific platform facilities via FFI.

## Smalltalk-80 execution semantics ##
We use green threads on a single host OS process.
