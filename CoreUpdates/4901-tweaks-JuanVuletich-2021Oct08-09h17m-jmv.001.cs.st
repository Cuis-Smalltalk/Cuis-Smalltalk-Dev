'From Cuis 5.0 [latest update: #4900] on 8 October 2021 at 9:18:15 am'!

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 10/7/2021 12:24:39'!
on: aRectangle ifCovered: partiallyOrTotallyCoveredBlock uncoveredPartsDo: uncoveredPartBlock else: notCoveredAtAllBlock
	"Find rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included.
	
	If aRectangle is not covered at all, or for whatever reason we don't know, evaluate notCoveredAtAllBlock.
	Othewise evaluate partiallyOrTotallyCoveredBlock once, and also evaluate uncoveredPartBlock for each part in aRectangle we don't cover."

	 | bounds r e r2 |
	self isOrAnyOwnerIsRotated ifTrue: [
		^ notCoveredAtAllBlock value ].

	color mightBeTranslucent ifTrue: [
		^ notCoveredAtAllBlock value ].

	bounds _ self displayBounds.
	bounds ifNil: [
		^ notCoveredAtAllBlock value ].

	(aRectangle intersects: bounds) ifFalse: [
		^ notCoveredAtAllBlock value ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!!"
	partiallyOrTotallyCoveredBlock value.
	aRectangle areasOutside: bounds do: [ :rect | uncoveredPartBlock value: rect ].

	Theme current roundWindowCorners ifTrue: [
		r _ Theme current roundedWindowRadius.
		e _ r@r.
		r2 _ aRectangle intersect: (bounds topLeft extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		r2 _ aRectangle intersect: (bounds topRight - (r@0) extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		r2 _ aRectangle intersect: (bounds bottomLeft - (0@r)extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		r2 _ aRectangle intersect: (bounds bottomRight - (r@r) extent: e).
		r2 hasPositiveExtent ifTrue: [ uncoveredPartBlock value: r2 ].
		].! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 10/7/2021 12:23:35'!
updateBoundsIn: aWorldMorph addDamageTo: aDamageRecorder

	aWorldMorph haloMorphsDo: [ :halo |
		(halo target isRedrawNeeded or: [halo target isSubmorphRedrawNeeded]) ifTrue: [
			"Invalidation of halos requires this specific sequence:"
			halo redrawNeeded.												"invalidate old halo bounds"
			self fullAddRedrawRect: halo to: aDamageRecorder ]].												"recompute & invalidate halo bounds"
	"bogus iteration on halos below is harmless:
		It is now marked as neither #isRedrawNeeded nor #isSubmorphRedrawNeeded."

	aWorldMorph submorphsDo: [ :morph |
		self fullAddRedrawRect: morph to: aDamageRecorder ].
	self updateHandsDisplayBounds: aWorldMorph.! !

