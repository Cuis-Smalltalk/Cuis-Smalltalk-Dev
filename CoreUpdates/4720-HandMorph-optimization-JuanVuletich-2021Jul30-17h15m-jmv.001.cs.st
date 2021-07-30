'From Cuis 5.0 [latest update: #4719] on 30 July 2021 at 5:30:29 pm'!

!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/30/2021 17:29:15'!
displayFullBoundsForPatch
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!!"

	| fullBounds |
	fullBounds _ self displayBounds.
	self submorphsDo: [ :m |
		fullBounds _ fullBounds quickMerge: m displayFullBounds].
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Update rectangle to current hand position."
			^(fullBounds translatedBy: self morphPosition-lastPos) encompassingIntegerRectangle ].
	^fullBounds encompassingIntegerRectangle! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/30/2021 17:24:48'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	| fullBounds |
	fullBounds _ self displayFullBoundsForPatch intersect: (`0@0` extent: aCanvas extent).
	savedPatch _ aCanvas savePatch: savedPatch bounds: fullBounds.
	prevFullBounds _ fullBounds.
	aStream nextPut: fullBounds.! !

!methodRemoval: HandMorph #displayFullBounds stamp: 'jmv 7/30/2021 17:30:04'!
HandMorph removeSelector: #displayFullBounds!
