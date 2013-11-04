'From Cuis 4.2 of 25 July 2013 [latest update: #1855] on 28 October 2013 at 6:47:53.710918 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 10/28/2013 18:47'!
transferHalo: event from: formerHaloOwner
	"Progressively transfer the halo to the next likely recipient"
	| w eventLocalPos |

	"Never transfer halo to top-most world"
	(self isWorldMorph and:[owner isNil]) ifFalse: [
		(formerHaloOwner ~~ self) 
			ifTrue: [ ^self addHalo: event from: formerHaloOwner ]].

	eventLocalPos _ self internalizeFromWorld: event eventPosition.
	event shiftPressed ifTrue: [
		"Pass it outwards"
		owner ifNotNil: [ ^owner transferHalo: event from: formerHaloOwner ].
		"We're at the top level; throw the event back in to find recipient"
		formerHaloOwner removeHalo.
		^self dispatchEvent: event copy resetHandlerFields localPosition: eventLocalPos.
	].
	self submorphsDo: [ :m |
		(m visible and: [ m fullContainsPoint: (m internalize: eventLocalPos) ])
			ifTrue: [ ^m transferHalo: event from: formerHaloOwner ].
	].
	"We're at the bottom most level; throw the event back up to the root to find recipient"
	formerHaloOwner removeHalo.
	(w _ self world) ifNil: [ ^self ].
	^w dispatchEvent: event copy resetHandlerFields localPosition: event eventPosition! !


!Taskbar methodsFor: 'stepping' stamp: 'jmv 10/28/2013 18:37'!
step

	"My dimensions are constrained live."
	self morphExtent: (self internalize: self world morphExtent) x @ 18.
	self morphPosition: 0@ (self world morphExtent y - (self externalizeDistance: extent ) y)! !

