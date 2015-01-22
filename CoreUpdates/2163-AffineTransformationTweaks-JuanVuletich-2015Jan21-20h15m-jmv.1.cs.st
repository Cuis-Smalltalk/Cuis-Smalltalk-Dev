'From Cuis 4.2 of 25 July 2013 [latest update: #2162] on 21 January 2015 at 8:46:10.200766 pm'!

!AffineTransformation methodsFor: 'transforming rects' stamp: 'jmv 1/21/2015 20:23'!
displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
		and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Primitive rounds and answers integers.
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here."

	| dstRect |
	dstRect _ Rectangle new.
	(self primDisplayBoundsOfTransformOf: aRectangle into: dstRect) ifNotNil: [
		dstRect topLeft > (0@0) ifTrue: [ ^dstRect  ]].
	^Rectangle encompassing: (aRectangle corners collect: [ :pt |
		(self transform: pt) rounded ])! !

!methodRemoval: AffineTransformation #displayBoundsOfTransformOf:into:!
AffineTransformation removeSelector: #displayBoundsOfTransformOf:into:!
!methodRemoval: AffineTransformation #fixRoundingInPrimitives:!
AffineTransformation removeSelector: #fixRoundingInPrimitives:!
