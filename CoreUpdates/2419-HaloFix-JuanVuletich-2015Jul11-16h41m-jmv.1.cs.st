'From Cuis 4.2 of 25 July 2013 [latest update: #2418] on 11 July 2015 at 5:00:10.122543 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 7/11/2015 16:59'!
wantsHalo
	^self visible! !


!HaloMorph methodsFor: 'testing' stamp: 'jmv 7/11/2015 16:53'!
wantsHalo
	^false! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 7/11/2015 16:59'!
transferHalo: event from: formerHaloOwner
	"Progressively transfer the halo to the next likely recipient"
	| eventLocalPos |

	formerHaloOwner == self
		ifFalse: [ ^self addHalo: event from: formerHaloOwner ].

	eventLocalPos _ self internalizeFromWorld: event eventPosition.
	event shiftPressed ifTrue: [
		"Pass it outwards"
		owner ifNotNil: [ ^owner transferHalo: event from: formerHaloOwner ].
		"We're at the top level; just keep it on ourselves"
		^self ].

	self submorphsDo: [ :m |
		(m wantsHalo and: [ m fullContainsPoint: (m internalize: eventLocalPos) ])
			ifTrue: [ ^m transferHalo: event from: formerHaloOwner ]].
	"We're at the bottom most level; just keep halo on ourselves"! !


!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 12/20/2014 14:54'!
morphContainsPoint: aLocalPoint

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"We behave as if we were a rectangle. I.e., we want (specifically mouse button) events that happen inside our bounds"
	^ self morphLocalBounds containsPoint: aLocalPoint! !

