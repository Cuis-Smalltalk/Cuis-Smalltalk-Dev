'From Cuis 5.0 [latest update: #4375] on 14 September 2020 at 11:19:51 am'!

!Morph methodsFor: 'events' stamp: 'jmv 9/14/2020 10:56:37'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition
	"Special gestures (cmd-mouse on the Macintosh; Alt-mouse on Windows and Unix) allow a mouse-sensitive morph to be moved or bring up a halo for the morph."
	| h doNotDrag |
	h _ aMouseButtonEvent hand halo.
	"Prevent wrap around halo transfers originating from throwing the event back in"
	doNotDrag _ false.
	h ifNotNil: [
		(h target == self) ifTrue: [ doNotDrag _ true].
		(h target hasOwner: self) ifTrue: [ doNotDrag _ true].
		(self hasOwner: h target) ifTrue: [ doNotDrag _ true]].

	"cmd-drag on flexed morphs works better this way"
	h _ self addHalo: aMouseButtonEvent.
	doNotDrag ifTrue: [ ^self ].
	h ifNotNil: [
		"Initiate drag transition if requested"
		"good gesture. implement it"
		aMouseButtonEvent hand 
			waitForClicksOrDrag: h
			event: aMouseButtonEvent
			clkSel: nil
			dblClkSel: nil.
		"Pass focus explicitly here"
		aMouseButtonEvent hand newMouseFocus: h ]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/14/2020 10:54:31'!
addHalo: evt
	| halo |
	self displayBoundsForHalo ifNotNil: [ :r |
		halo _ HaloMorph new.
		halo popUpFor: self event: evt.
		halo morphBounds: r ].
	^halo! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/14/2020 10:52:56'!
transferHalo: event from: formerHaloOwner
	"Progressively transfer the halo to the next likely recipient"
	| eventLocalPos |

	formerHaloOwner == self
		ifFalse: [ ^self addHalo: event ].

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


!HaloMorph methodsFor: 'private' stamp: 'jmv 9/14/2020 10:59:33'!
addHandles

	self removeAllMorphs.  "remove old handles, if any"
	target displayBoundsForHalo ifNotNil: [ :r |
		self morphBounds: r.  "update my size"
		haloBox _ self basicBox.
		target addHandlesTo: self box: haloBox.
		self addNameString: (target printStringLimitedTo: 40).
		growingOrRotating _ false.
		self redrawNeeded ].! !

!methodRemoval: Morph #addHalo:from: stamp: 'jmv 9/14/2020 11:19:37'!
Morph removeSelector: #addHalo:from:!
