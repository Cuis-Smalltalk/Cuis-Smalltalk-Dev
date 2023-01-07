'From Cuis 6.0 [latest update: #5623] on 7 January 2023 at 9:51:59 am'!

!Morph methodsFor: 'copying' stamp: 'jmv 1/6/2023 17:12:48'!
copyForClipboard
	"The answer is used both for internal (Smalltalk) and external (Host OS) clipboard.
	Some subclasses might need specific behavior."

	self okayToDuplicate ifFalse: [ ^ nil ].
	^self halt copy
		removeProperty: #contour;
		removeProperty: #contourY0;
		removeProperty: #contourY1;
		yourself! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 1/6/2023 17:12:27'!
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
	self hideHardwareCursor.
	self redrawNeeded.

	moveUnderHand ifTrue: [
		bounds _ nil.
		grabbed isInWorld ifTrue: [
			bounds _ (grabbed displayBounds ifNil: [grabbed fullBoundsInOwner ])
				translatedBy: grabbed morphPositionInWorld negated ].
		bounds ifNil: [ bounds _ grabbed fullBoundsInOwner ].
		positionInHandCoordinates _ (bounds center + bounds bottomRight //2) negated + 
			grabbed morphPosition.
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

