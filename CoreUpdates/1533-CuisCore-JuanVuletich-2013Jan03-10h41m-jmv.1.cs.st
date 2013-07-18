'From Cuis 4.1 of 12 December 2012 [latest update: #1532] on 3 January 2013 at 10:44:11 am'!

!ObjectExplorer commentStamp: '<historical>' prior: 0!
ObjectExplorer provides a hierarchical alternative to #inspect. Simply evaluate an expression like:

self runningWorld explore

and enjoy.!


!PointerExplorer commentStamp: '<historical>' prior: 0!
A variant on the ObjectExlorer that works "backwards": like the ObjectExplorer, it shows a tree of objects, but expanding a node won't show the objects which that node references, but rather the objects that reference that node.  Its main use is to track down memory leaks: if you want to know why a particular object is still alive, open a PointerExplorer on it and drill down until you find the root object that's referencing it.  For example, find all the references to the symbol #zot with:

self runningWorld explorePointers

For the "name" of the object, the PointerExplorer shows each object's identityHash, to allow the user to identify when two similar objects are identical and notice cycles.!


!PointerExplorer commentStamp: '<historical>' prior: 0!
A variant on the ObjectExlorer that works "backwards": like the ObjectExplorer, it shows a tree of objects, but expanding a node won't show the objects which that node references, but rather the objects that reference that node.  Its main use is to track down memory leaks: if you want to know why a particular object is still alive, open a PointerExplorer on it and drill down until you find the root object that's referencing it.  For example, find all the references to the symbol #zot with:

self runningWorld explorePointers

For the "name" of the object, the PointerExplorer shows each object's identityHash, to allow the user to identify when two similar objects are identical and notice cycles.!

