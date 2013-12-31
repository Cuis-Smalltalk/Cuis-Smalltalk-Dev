'From Cuis 4.0 of 21 April 2012 [latest update: #1495] on 4 December 2012 at 10:28:33 pm'!

!SpaceTally methodsFor: 'class analysis' stamp: 'jmv 12/4/2012 22:28'!
spaceTally: classes 
	"Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed by the code and instances of each class in the system. Note that code sizes do not currently report memory consumed by class variables. "
	"
	SpaceTally new spaceTally: (Array with: TextModelMorph with: Point)
	"
	self preAllocateResultsFor: classes.
	Smalltalk garbageCollect.
	self computeSpaceUsage.
	^ results
		 sort: [ :a :b | a spaceForInstances > b spaceForInstances ];
		 yourself! !

