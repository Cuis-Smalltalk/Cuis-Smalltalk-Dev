'From Cuis 5.0 [latest update: #4552] on 5 April 2021 at 12:31:08 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 4/5/2021 12:26:36'!
addHalo
	"Invoke a halo programatically (e.g., not from a meta gesture)"
	self addHalo: nil! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 4/5/2021 12:29:16'!
transferHalo: event from: formerHaloOwner
	"Progressively transfer the halo to the next likely recipient"

	formerHaloOwner == self
		ifFalse: [
			self addHalo: event.
			^self ].

	event shiftPressed ifTrue: [
		"Pass it outwards"
		owner ifNotNil: [
			owner transferHalo: event from: formerHaloOwner.
			^self ].
		"We're at the top level; just keep it on ourselves"
		^self ].

	self submorphsDo: [ :m |
		(m wantsHalo and: [ m fullContainsGlobalPoint: event eventPosition ])
			ifTrue: [
				m transferHalo: event from: formerHaloOwner.
				^self ]].
	"We're at the bottom most level; just keep halo on ourselves"! !


!HaloMorph methodsFor: 'event handling' stamp: 'jmv 4/5/2021 12:28:21'!
transferHalo: event localPosition: localEventPosition
	"Transfer the halo to the next likely recipient"
	target ifNil: [
		self delete.
		^ self ].
	target transferHalo: event from: target.! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 4/5/2021 12:27:42'!
delete
	activeSubMenu ifNotNil: [activeSubMenu delete].
	self itemsDo: [ :i | i deselect ].
	super delete! !


!SmalltalkCompleterMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 4/5/2021 12:27:30'!
delete

	selectorDocumentation ifNotNil: [
		selectorDocumentation delete.
		selectorDocumentation := nil ].

	super delete ! !

