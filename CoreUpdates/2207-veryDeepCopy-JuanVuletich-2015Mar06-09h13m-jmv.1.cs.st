'From Cuis 4.2 of 25 July 2013 [latest update: #2206] on 6 March 2015 at 9:15:27.138318 am'!

!Object methodsFor: 'copying' stamp: 'jmv 3/6/2015 09:15'!
veryDeepCopy
	"Do a full copy of an object graph.
	Some classes might chose not to include some ivars.
	For example, aMorph owner is only included 'wakely'.
	See #storeDataOn:, and in particular Morph>>storeDataOn:
	
	This method is included for comatibility with other Smalltalks and for reference.
	Each class should decide how to copy its instances.
	For example, Morph>>copy.
	Implementing #copy appropriately, and just calling it is better style than calling #veryDeepCopy (except, maybe, in low level, system code)"

	^ Object unStream: (ReferenceStream streamedRepresentationOf: self)! !

