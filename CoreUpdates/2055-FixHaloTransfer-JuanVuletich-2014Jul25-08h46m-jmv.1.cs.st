'From Cuis 4.2 of 25 July 2013 [latest update: #2054] on 25 July 2014 at 8:58:02.184356 am'!

!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 7/25/2014 08:57'!
morphContainsPoint: aLocalPoint

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"We behave as if we were a rectangle. I.e., we want (specifically mouse button) events that happen inside our bounds"
	^ (0@0 extent: self morphExtent) containsPoint: aLocalPoint! !

