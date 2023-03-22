'From Cuis 6.0 [latest update: #5712] on 21 March 2023 at 5:13:55 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 3/21/2023 17:07:58'!
fullyCoveredRectangleOf: aMorph

	"Assume a bit of Anti Aliasing"
	aMorph requiresVectorCanvas ifTrue: [
		^aMorph fullyCoveredRectangle ifNotNil: [ :r | r insetBy: 2 ]].

	^aMorph fullyCoveredRectangle! !


!AbstractVectorCanvas methodsFor: 'morphic world' stamp: 'jmv 3/21/2023 17:08:06'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid _after_ drawing currentMorph.
	In VectorCanvas we support clipping by any shape, not just rectangles. Then, this rectangle is used only as an optimization of the area to be redrawn."

	| boundingRect |
	"A good optimization in many cases.
	But for morphs where #drawOn: might not be able to find bounds (InnerTextMorph and LayoutMorph,
	for example) this is essential."
	currentMorph knowsOwnLocalBounds ifTrue: [
		boundingRect := (currentTransformation externalBoundingRectOf: currentMorph morphLocalBounds)
			encompassingIntegerRectangle.
		"Include an extra pixel to cover possible anti aliasing."
		currentMorph requiresVectorCanvas ifTrue: [
			boundingRect := boundingRect outsetBy: 1 ].
		^ clippingMorphDisplayBounds
			ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
			ifNil: [ boundingRect ]].

	"Use the bounds bound by the engine during #drawOn.
	This method must therefore be called after calling #drawOn:."
	boundingRect := engine morphBoundsAfterDraw.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !

!methodRemoval: HybridCanvas #fullyCoveredRectangleOf: stamp: 'jmv 3/21/2023 17:13:13'!
HybridCanvas removeSelector: #fullyCoveredRectangleOf:!
!methodRemoval: AbstractVectorCanvas #fullyCoveredRectangleOf: stamp: 'jmv 3/21/2023 17:13:11'!
AbstractVectorCanvas removeSelector: #fullyCoveredRectangleOf:!
