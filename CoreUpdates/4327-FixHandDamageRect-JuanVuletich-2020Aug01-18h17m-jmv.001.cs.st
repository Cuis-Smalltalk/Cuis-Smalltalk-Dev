'From Cuis 5.0 [latest update: #4326] on 1 August 2020 at 6:37:52 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/1/2020 18:33:03'!
updateHandsDisplayBounds: aPasteUpMorph addDamageTo: aDamageRecorder
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aPasteUpMorph handsDo: [ :hand |
		self into: hand.
		hand submorphsDo: [ :morph |
			self updateDisplayBounds: morph ].
		aDamageRecorder recordInvalidRect: hand displayFullBounds.
		self outOfMorph ]! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 8/1/2020 18:30:18'!
displayFullBounds	
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around)."
	| fullBounds |
	fullBounds _ self displayBounds.
	self submorphsDo: [ :m |
		fullBounds _ fullBounds quickMerge: m displayFullBounds].
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Include also the rectangle updated to current hand position."
			fullBounds _ fullBounds quickMerge: (fullBounds translatedBy: self morphPosition-lastPos) ].
	^fullBounds! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/1/2020 18:34:10'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rectsToRepair previousRectsToRepair reuse previousCount |
	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder."
	self updatingMorphBoundsDo: [
		aPasteUpMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
		self updateHandsDisplayBounds: aPasteUpMorph addDamageTo: aDamageRecorder ].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ aDamageRecorder invalidRectsFullBounds: aPasteUpMorph viewBox.

	rectsToRepair size = 0 ifTrue: [
		^rectsToRepair].

	rectsToRepair size > 1 ifTrue: [									"Or there is no point in attempting to merge them!!"
		previousRectsToRepair _ OrderedCollection new.

		"Until no more rectangles are merged, i.e. each morph intersects at most one rectangle."
		previousCount _ 0.
		[rectsToRepair size = previousCount] whileFalse: [
			previousCount _ rectsToRepair size.
			aPasteUpMorph submorphsReverseDo: [ :morph | | thisMorphRectangle thisMorphBounds |
				morph visible ifTrue: [
					reuse _ previousRectsToRepair.
					previousRectsToRepair _ rectsToRepair.
					rectsToRepair _ reuse removeAll.
					thisMorphRectangle _ nil.
					thisMorphBounds _ morph displayFullBounds.
					previousRectsToRepair do: [ :r |
						(thisMorphBounds intersects: r)
							ifTrue: [ thisMorphRectangle _ r quickMerge: thisMorphRectangle ]
							ifFalse: [ rectsToRepair add: r ]].
					thisMorphRectangle
						ifNotNil: [rectsToRepair add: thisMorphRectangle]]]].
	].

	"Draw World"
	rectsToRepair do: [ :r |
		aPasteUpMorph drawOn: (self newClipRect: r) ].

	"Repair rects. Draw morphs"
	rectsToRepair do: [ :r |
		self newClipRect: r.
		aPasteUpMorph submorphsReverseDo: [ :morph | | morphBounds |
			morph visible ifTrue: [
				morphBounds _ morph displayFullBounds.
				(morphBounds intersects: r) ifTrue: [
					self fullDraw: morph ]]]].

	^ rectsToRepair! !

!methodRemoval: MorphicCanvas #updateHandsDisplayBounds: stamp: 'jmv 8/1/2020 18:32:19'!
MorphicCanvas removeSelector: #updateHandsDisplayBounds:!
