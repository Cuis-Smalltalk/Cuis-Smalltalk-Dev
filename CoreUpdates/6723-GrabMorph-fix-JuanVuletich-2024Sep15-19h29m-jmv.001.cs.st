'From Cuis7.1 [latest update: #6722] on 15 September 2024 at 7:42:56 pm'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 9/15/2024 19:41:46'!
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
	self hideHardwareCursor.
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

