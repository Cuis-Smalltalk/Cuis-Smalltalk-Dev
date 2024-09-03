'From Cuis7.1 [latest update: #6669] on 2 September 2024 at 7:29:41 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 9/2/2024 19:28:59'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid for morphs where #localBounds is known.
	Subclasses may raise this restriction. See inherintance."

	| boundingRect |
	boundingRect := self boundingRectOfCurrentIfTrivial.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !

