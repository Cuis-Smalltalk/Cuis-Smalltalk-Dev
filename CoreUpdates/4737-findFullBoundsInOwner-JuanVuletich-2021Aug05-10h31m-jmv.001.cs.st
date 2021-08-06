'From Cuis 5.0 [latest update: #4736] on 5 August 2021 at 10:36:30 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/5/2021 10:33:19'!
findFullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!!"

	"#morphExtent also valid in owner, because no VectorCanvas => no scaling."
	^(self morphPosition extent: self morphExtent) encompassingIntegerRectangle.! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 8/5/2021 09:51:40'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates tx bounds |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o | grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].

	moveUnderHand ifTrue: [
		bounds _ nil.
		grabbed isInWorld ifTrue: [
			grabbed displayBounds ifNotNil: [ :r |
				bounds _ r translatedBy: grabbed morphPositionInWorld negated ]].
		bounds ifNil: [ bounds _ grabbed findFullBoundsInOwner ].
		positionInHandCoordinates _ (bounds center + bounds bottomRight //2) negated.
		self grabMorph: grabbed delta: positionInHandCoordinates.
		^self ].

	positionInHandCoordinates _ (grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph])
		morphPositionInWorld - self morphPositionInWorld.

	tx _ GeometryTransformation identity.
	(grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph]) withAllOwnersDo: [ :o |
		tx _ o location composedWith: tx ].
	self withAllOwnersReverseDo: [ :o |
		tx _ o location inverseTransformation composedWith: tx ].
	self grabMorph: grabbed delta: positionInHandCoordinates.

	grabbed location: tx.! !

