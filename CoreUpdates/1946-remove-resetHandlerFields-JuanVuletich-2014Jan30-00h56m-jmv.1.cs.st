'From Cuis 4.2 of 25 July 2013 [latest update: #1940] on 30 January 2014 at 1:11:04.432407 am'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 1/30/2014 01:10'!
transferHalo: event from: formerHaloOwner
	"Progressively transfer the halo to the next likely recipient"
	| eventLocalPos |

	"Never transfer halo to top-most world"
	(self isWorldMorph and:[owner isNil]) ifFalse: [
		(formerHaloOwner ~~ self) 
			ifTrue: [ ^self addHalo: event from: formerHaloOwner ]].

	eventLocalPos _ self internalizeFromWorld: event eventPosition.
	event shiftPressed ifTrue: [
		"Pass it outwards"
		owner ifNotNil: [ ^owner transferHalo: event from: formerHaloOwner ].
		"We're at the top level; just keep it on ourselves"
		^self ].
	self submorphsDo: [ :m |
		(m visible and: [ m fullContainsPoint: (m internalize: eventLocalPos) ])
			ifTrue: [ ^m transferHalo: event from: formerHaloOwner ]].
	"We're at the bottom most level; just keep halo on ourselves"! !

!methodRemoval: MouseButtonEvent #resetHandlerFields!
MouseButtonEvent removeSelector: #resetHandlerFields!
!methodRemoval: UserInputEvent #resetHandlerFields!
UserInputEvent removeSelector: #resetHandlerFields!
!methodRemoval: DropEvent #resetHandlerFields!
DropEvent removeSelector: #resetHandlerFields!
!methodRemoval: MorphicEvent #resetHandlerFields!
MorphicEvent removeSelector: #resetHandlerFields!
