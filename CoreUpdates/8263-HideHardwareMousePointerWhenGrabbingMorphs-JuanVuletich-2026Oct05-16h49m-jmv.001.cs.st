'From Cuis7.9 [latest update: #8262] on 5 October 2026 at 4:49:57 pm'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 5/Oct/2026, 7:49:33 pm (UTC)'!
grabMorph: aMorph delta: delta
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner formerPositionInWorld |
	self releaseMouseFocus. "Break focus"
	self hideHardwareCursor.

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		(h target == aMorph or: [h target hasOwner: aMorph]) ifTrue: [
			self addMorphBack: h position: h positionInWorld - self positionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	formerOwner := aMorph owner.
	formerOwner ifNotNil: [
		formerPositionInWorld := aMorph positionInWorld.
		grabMorphData
			at: aMorph 
			put: { formerOwner. formerPositionInWorld. } ].
	self addMorphBack: aMorph position: delta.

	aMorph displayBounds ifNotNil: [ :r | | d |
		d := formerPositionInWorld ifNotNil: [aMorph positionInWorld - formerPositionInWorld].
		(d isNil or: [d isZero not]) ifTrue: [
			aMorph adjustDisplayBoundsBy: d ]].

	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		"Morph was in the world"
		lastPosition := self morphPosition ].! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 5/Oct/2026, 7:49:21 pm (UTC)'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates tx bounds |
	self releaseMouseFocus.	"Break focus"
	grabbed := aMorph.
	aMorph owner ifNotNil: [ :o | grabbed := o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed := grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	self redrawNeeded.

	moveUnderHand ifTrue: [
		bounds := grabbed displayBounds ifNil: [grabbed fullBoundsInOwner ].
		positionInHandCoordinates := (bounds center + bounds bottomRight //2) negated + 
			grabbed morphPosition.
		self grabMorph: grabbed delta: positionInHandCoordinates.
		^self ].

	positionInHandCoordinates := (grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph])
		positionInWorld - self positionInWorld.

	tx := GeometryTransformation identity.
	(grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph]) withAllOwnersDo: [ :o |
		tx := o location composedWith: tx ].
	self withAllOwnersReverseDo: [ :o |
		tx := o location inverseTransformation composedWith: tx ].
	self grabMorph: grabbed delta: positionInHandCoordinates.

	grabbed location: tx.! !

