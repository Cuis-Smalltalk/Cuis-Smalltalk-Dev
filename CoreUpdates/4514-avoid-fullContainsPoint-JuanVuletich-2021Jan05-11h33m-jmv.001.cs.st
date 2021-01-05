'From Cuis 5.0 [latest update: #4513] on 5 January 2021 at 11:35:12 am'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 1/5/2021 11:33:43'!
transferHalo: event from: formerHaloOwner
	"Progressively transfer the halo to the next likely recipient"

	formerHaloOwner == self
		ifFalse: [ ^self addHalo: event ].

	event shiftPressed ifTrue: [
		"Pass it outwards"
		owner ifNotNil: [ ^owner transferHalo: event from: formerHaloOwner ].
		"We're at the top level; just keep it on ourselves"
		^self ].

	self submorphsDo: [ :m |
		(m wantsHalo and: [ m fullContainsGlobalPoint: event eventPosition ])
			ifTrue: [ ^m transferHalo: event from: formerHaloOwner ]].
	"We're at the bottom most level; just keep halo on ourselves"! !

