'From Cuis 4.2 of 25 July 2013 [latest update: #2404] on 7 July 2015 at 10:28:57.285389 am'!

!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 7/7/2015 10:28'!
morphContainsPoint: aLocalPoint

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"If we have a target, we won't be drawn"
	target ifNotNil: [ ^false ].

	"We behave as if we were a rectangle. I.e., we want (specifically mouse button) events that happen inside our bounds"
	^ self morphLocalBounds containsPoint: aLocalPoint! !

