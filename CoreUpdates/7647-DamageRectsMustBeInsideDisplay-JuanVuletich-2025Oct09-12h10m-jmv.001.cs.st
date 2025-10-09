'From Cuis7.5 [latest update: #7645] on 9 October 2025 at 12:12:09 pm'!

!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/9/2025 10:14:05'!
displayFullBoundsForPatchIn: aCanvas
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!!"

	| canvasBounds fullBounds |
	canvasBounds := `0@0` extent: aCanvas extent.
	fullBounds := self basicDisplayBounds.
	self submorphsDo: [ :m |
		m displayFullBounds ifNotNil: [ :b |
			fullBounds := fullBounds quickMerge: b intersectionWith: canvasBounds ]].
	lastPosition
		ifNotNil: [ :lastPos | "When already carrying morphs around. Update rectangle to current hand position."
			fullBounds := fullBounds quickMerge: (fullBounds translatedBy: self morphPosition-lastPos) ].
	^fullBounds encompassingIntegerRectangle! !


!WorldMorph methodsFor: 'drawing' stamp: 'jmv 10/9/2025 10:36:50'!
displayWorldOn: aMorphicCanvas
	"Update this world's display on the argument."

	| allDamageRect |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"Restore world drawing under hands and their carried morphs"
	(Preferences at: #cacheDisplayContentWhenMovingMorphs) ifTrue: [
		activeHand restoreSavedPatchOn: aMorphicCanvas ].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	aMorphicCanvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamageRect := aMorphicCanvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	self submorphNeedsRedraw: false.
	self needsRedraw: false.
	aMorphicCanvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self ifHandToBeDrawnForDamage: allDamageRect do: [
		(Preferences at: #cacheDisplayContentWhenMovingMorphs)
			ifTrue: [ allDamageRect := (activeHand savePatchFrom: aMorphicCanvas) quickMerge: allDamageRect ]
			ifFalse: [ allDamageRect := (activeHand displayFullBoundsForPatchIn: aMorphicCanvas) quickMerge: allDamageRect ].
		aMorphicCanvas fullDrawHand: activeHand.
		activeHand needsRedraw: false.
		activeHand submorphNeedsRedraw: false ].

	"Copy altered rects to Display"
	allDamageRect ifNotNil: [
		DisplayScreen forceDeferredToScreen: allDamageRect ].! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/9/2025 10:14:48'!
savePatchFrom: aCanvas
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	| fullBounds answer |
	fullBounds := self displayFullBoundsForPatchIn: aCanvas.
	savedPatch := aCanvas savePatch: savedPatch bounds: fullBounds.
	answer := fullBounds.
	prevFullBounds ifNotNil: [ answer := answer quickMerge: prevFullBounds ].
	prevFullBounds := fullBounds.
	^answer! !


!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 10/9/2025 11:53:49'!
fullAddCurrentMorphTo: aDamageRecorder

	currentMorph isRedrawNeeded ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		currentMorph displayBounds ifNotNil: [ :r | | wb |
			wb := world localBounds.
			(wb intersects: r) ifTrue: [
				aDamageRecorder
					recordInvalidRect: (r intersect: wb)
					for: currentMorph ]]].
	currentMorph isSubmorphRedrawNeeded ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m |
				self fullAddRedrawRect: m to: aDamageRecorder ]]].! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 10/9/2025 11:53:56'!
fullAddCurrentMorphTo: aDamageRecorder

	currentMorph isRedrawNeeded ifTrue: [
		currentMorph needsRedraw: false.
		self updateCurrentBounds.
		currentMorph displayBounds ifNotNil: [ :r | | wb |
			wb := world localBounds.
			(wb intersects: r) ifTrue: [
				aDamageRecorder
					recordInvalidRect: (r intersect: wb)
					for: currentMorph ]]].
	currentMorph isSubmorphRedrawNeeded ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m |
				self fullAddRedrawRect: m to: aDamageRecorder ]]].! !

!methodRemoval: HandMorph #displayFullBoundsForPatch stamp: 'jmv 10/9/2025 12:11:46'!
HandMorph removeSelector: #displayFullBoundsForPatch!
