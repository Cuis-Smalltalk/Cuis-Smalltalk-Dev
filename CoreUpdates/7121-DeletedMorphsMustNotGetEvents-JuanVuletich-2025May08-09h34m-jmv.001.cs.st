'From Cuis7.3 [latest update: #7120] on 8 May 2025 at 9:45:50 am'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 5/8/2025 09:34:46'!
rejectsEventFully: aMorphicEvent
	"Return true to reject the given event, for thereceiver and all submorphs."

	^ self isLocked or: [ self visible not or: [self isInWorld not]]! !

