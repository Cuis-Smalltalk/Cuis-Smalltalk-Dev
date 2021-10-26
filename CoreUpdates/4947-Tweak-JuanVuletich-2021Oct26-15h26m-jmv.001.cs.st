'From Cuis 5.0 [latest update: #4947] on 26 October 2021 at 3:26:57 pm'!

!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/26/2021 15:26:47'!
displayFullBoundsForPatch
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!!"

	| handBounds fullBounds submorphBounds |
	handBounds _ self basicDisplayBounds.
	submorphBounds _ nil.
	self submorphsDo: [ :m |
		m displayFullBounds ifNotNil: [ :b |
			submorphBounds _ b quickMerge: submorphBounds ]].
	fullBounds _ handBounds quickMerge: submorphBounds.
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Update rectangle to current hand position."
			fullBounds _ fullBounds quickMerge: (submorphBounds translatedBy: self morphPosition-lastPos) ].
	^fullBounds encompassingIntegerRectangle! !

