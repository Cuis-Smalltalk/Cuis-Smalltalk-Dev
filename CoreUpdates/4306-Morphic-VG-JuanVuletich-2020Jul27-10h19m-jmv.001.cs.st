'From Cuis 5.0 [latest update: #4302] on 27 July 2020 at 10:25:14 am'!

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 7/27/2020 10:22:15'!
boundingRectOfCurrentMorphAfterDraw
	"This rectangle is used for clipping submorphs in BitBltCanvas.
	Clipped to owner if appropriate.
	In VectorCanvas we support clipping by any shape, not just rectangles. Then, this rectangle is used as an optimization of the area to be redrawn."

	self subclassResponsibility! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 7/27/2020 10:21:41'!
boundingRectOfCurrentMorph
	"In targetForm coordinates.
	Answer morph bounds, ignoring possible clipping by owner."

	^currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds! !

!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 7/27/2020 10:21:10'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate."

	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | self boundingRectOfCurrentMorph intersect: ownerClips ]
		ifNil: [ self boundingRectOfCurrentMorph ]! !

