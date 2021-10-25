'From Cuis 5.0 [latest update: #4944] on 25 October 2021 at 4:49:05 pm'!

!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/25/2021 16:43:07'!
displayFullBoundsForPatch
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!!"

	| handBounds fullBounds submorphBounds |
	handBounds _ self basicDisplayBounds.
	submorphBounds _ nil.
	self submorphsDo: [ :m |
		submorphBounds _ m displayFullBounds quickMerge: submorphBounds ].
	fullBounds _ handBounds quickMerge: submorphBounds.
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Update rectangle to current hand position."
			fullBounds _ fullBounds quickMerge: (submorphBounds translatedBy: self morphPosition-lastPos) ].
	^fullBounds encompassingIntegerRectangle! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 10/25/2021 16:40:28'!
updateHandsDisplayBounds: aWorldMorph
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aWorldMorph handsDo: [ :hand |
		hand hasSubmorphs ifTrue: [
			self into: hand.
			hand submorphsDo: [ :morph |
				morph basicDisplayBounds ifNil: [
					self fullUpdateProtrudingBounds: morph ]].
			self outOfMorph ]].! !

