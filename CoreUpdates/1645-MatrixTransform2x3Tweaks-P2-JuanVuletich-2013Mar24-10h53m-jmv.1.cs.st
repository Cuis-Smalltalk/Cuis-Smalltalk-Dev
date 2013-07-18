'From Cuis 4.1 of 12 December 2012 [latest update: #1642] on 24 March 2013 at 11:01:42 am'!

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 3/24/2013 10:54'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint."

	^DoIntegerTransformations
		ifTrue: [ self intInverseTransform: aPoint ]
		ifFalse: [ self floatInverseTransform: aPoint ]! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 3/24/2013 10:54'!
transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint."

	^DoIntegerTransformations
		ifTrue: [ self intTransform: aPoint ]
		ifFalse: [ self floatTransform: aPoint ]! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 3/24/2013 11:00'!
displayBoundsOfInverseTransformOf: aRectangle
	"Internalize srcRect, and find a bounding rectangle with horizontal and vertical bounds (in the inner space) and integer coordinates (i.e. a displayBounds).
	(MatrixTransform2x3 withRadians: 0.3) displayBoundsOfInverseTransformOf: (10@10 extent: 20@30)
	"
	^self displayBoundsOfInverseTransformOf: aRectangle into: Rectangle new! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 3/24/2013 10:59'!
displayBoundsOfInverseTransformOf: srcRect into: dstRect
	"Internalize srcRect, and find a bounding rectangle with horizontal and vertical bounds (in the inner space) and 
	integer coordinates (i.e. adisplayBounds). Store result into dstRect."

	"Warning: the answer is rounded to integers by the primitive"
	<primitive: 'primitiveInvertRectInto' module: 'Matrix2x3Plugin'>

	| floatResult |
	floatResult _ Rectangle encompassing: (self inverseTransformPoints: srcRect corners).
	dstRect
		setOrigin: floatResult origin rounded
		corner: floatResult corner rounded.
	^dstRect! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 3/24/2013 11:00'!
displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds).
	(MatrixTransform2x3 withRadians: 0.3) displayBoundsOfTransformOf: (10@10 extent: 20@30)
	"

	^self displayBoundsOfTransformOf: aRectangle into: Rectangle new! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 3/24/2013 11:00'!
displayBoundsOfTransformOf: srcRect into: dstRect
	"Externalize srcRect, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Store result into dstRect."

	"Warning: the answer is rounded to integers by the primitive"
	<primitive: 'primitiveTransformRectInto' module: 'Matrix2x3Plugin'>

	| floatResult |
	floatResult _ Rectangle encompassing: (self transformPositions: srcRect corners).
	dstRect
		setOrigin: floatResult origin rounded
		corner: floatResult corner rounded.
	^dstRect! !

