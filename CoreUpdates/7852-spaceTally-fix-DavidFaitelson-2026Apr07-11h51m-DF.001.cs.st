'From Cuis7.7 [latest update: #7851] on 7 April 2026 at 11:51:29 am'!

!SpaceTally methodsFor: 'class analysis' stamp: 'DF 7/Apr/2026 11:51:10'!
spaceTally: classes 
	"Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed by the code and instances of each class in the system. Note that code sizes do not currently report memory consumed by class variables. "
	"
	SpaceTally new spaceTally: (Array with: TextModelMorph with: Point)
	"
	self preAllocateResultsFor: classes.
	Smalltalk garbageCollect.
	self computeSpaceUsage.
	^ results
		 sort: [ :a :b | a spaceForInstances first > b spaceForInstances first ];
		 yourself! !

