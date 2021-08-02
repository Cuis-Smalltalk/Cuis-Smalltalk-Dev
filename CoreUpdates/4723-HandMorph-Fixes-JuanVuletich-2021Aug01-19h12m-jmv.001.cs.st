'From Cuis 5.0 [latest update: #4722] on 1 August 2021 at 7:18:57 pm'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/1/2021 19:05:12'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."

	self needsRedraw: false.
	prevFullBounds ifNotNil: [
		aCanvas restorePatch: savedPatch bounds: prevFullBounds.
		self invalidateDisplayRect: prevFullBounds for: nil.
		submorphs isEmpty ifTrue: [
			"Dropped carried morph.
			Make the transition to using hardware cursor. Clear savedPatch and
			 report one final damage rectangle to erase the image of the software cursor."
			Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
			"show hardware cursor"
			prevFullBounds _ nil ]].! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/30/2021 17:24:48'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	| fullBounds |
	fullBounds _ self displayFullBoundsForPatch intersect: (`0@0` extent: aCanvas extent).
	savedPatch _ aCanvas savePatch: savedPatch bounds: fullBounds.
	prevFullBounds _ fullBounds.
	aStream nextPut: fullBounds.! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 8/1/2021 19:14:29'!
displayFullBoundsForPatch
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!!"

	| handBounds fullBounds |
	handBounds _ self displayBounds.
	fullBounds _ handBounds.
	self submorphsDo: [ :m |
		fullBounds _ fullBounds quickMerge: m displayFullBounds].
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Update rectangle to current hand position."
			fullBounds _ (fullBounds translatedBy: self morphPosition-lastPos) quickMerge: handBounds ].
	^fullBounds encompassingIntegerRectangle! !

