'From Cuis 5.0 [latest update: #4583] on 3 May 2021 at 5:40:42 pm'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 5/3/2021 17:39:58'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates tx |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o | grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].

	moveUnderHand
		ifTrue: [
			"We can possibly do better, especially for non WidgetMorphs"
			positionInHandCoordinates _ -30 @ -10.
			grabbed isInWorld ifTrue: [
				grabbed displayBounds ifNotNil: [ :r |
					positionInHandCoordinates _ (r extent // 2) negated ]].
			self
				grabMorph: grabbed
				delta: positionInHandCoordinates.
			^self ].

	positionInHandCoordinates _ (grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph])
		morphPositionInWorld - self morphPositionInWorld.

	tx _ GeometryTransformation identity.
	(grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph]) withAllOwnersDo: [ :o |
		tx _ o location composedWith: tx ].
	self withAllOwnersReverseDo: [ :o |
		tx _ o location inverseTransformation composedWith: tx ].
	self
		grabMorph: grabbed
		delta: positionInHandCoordinates.

	grabbed location: tx.! !

