'From Cuis 4.1 of 12 December 2012 [latest update: #1616] on 26 February 2013 at 10:42:08 pm'!

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 2/26/2013 22:15'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint."

	"Warning: the answer is rounded to integers by the primitive"
	<primitive: 'primitiveInvertPoint' module: 'Matrix2x3Plugin'>
	| x y det a11 a12 a21 a22 detX detY |
	self flag: #jmvVer2.

	x _ aPoint x asFloat - (self a13).
	y _ aPoint y asFloat - (self a23).
	a11 _ self a11.
	a12 _ self a12.
	a21 _ self a21.
	a22 _ self a22.
	det _ (a11 * a22) - (a12 * a21).
	det = 0.0 ifTrue: [ ^0@0 ].		"So we have at least a valid result"
	det _ 1.0 / det.
	detX _ (x * a22) - (a12 * y).
	detY _ (a11 * y) - (x * a21).
	^(detX * det) @ (detY * det)! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 2/26/2013 22:15'!
transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint."

	"Warning: the answer is rounded to integers by the primitive"
	<primitive: 'primitiveTransformPoint' module: 'Matrix2x3Plugin'>
	| x y |
	self flag: #jmvVer2.

	x _ (aPoint x * self a11) + (aPoint y * self a12) + self a13.
	y _ (aPoint x * self a21) + (aPoint y * self a22) + self a23.
	^x @ y! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 2/26/2013 21:59'!
displayBoundsOfInverseTransformOf: aRectangle
	"Internalize srcRect, and find a bounding rectangle with horizontal and vertical bounds (in the inner space) and integer coordinates (i.e. a displayBounds).
	(MatrixTransform2x3 withRadians: 0.3) displayBoundsOfInverseTransformOf: (10@10 extent: 20@30)
	"
	^self displayBoundsOfInverseTransformOf: aRectangle into: Rectangle new! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 2/26/2013 22:14'!
displayBoundsOfInverseTransformOf: srcRect into: dstRect
	"Internalize srcRect, and find a bounding rectangle with horizontal and vertical bounds (in the inner space) and 
	integer coordinates (i.e. adisplayBounds). Store result into dstRect."

	"Warning: the answer is rounded to integers by the primitive"
	<primitive: 'primitiveInvertRectInto' module: 'Matrix2x3Plugin'>
	self flag: #jmvVer2.

	^Rectangle encompassing: (self inverseTransformPoints: srcRect corners)! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 2/26/2013 22:14'!
displayBoundsOfTransformOf: srcRect into: dstRect
	"Externalize srcRect, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Store result into dstRect."

	"Warning: the answer is rounded to integers by the primitive"
	<primitive: 'primitiveTransformRectInto' module: 'Matrix2x3Plugin'>
	self flag: #jmvVer2.

	^Rectangle encompassing: (self transformPositions: srcRect corners)! !


!Morph methodsFor: 'layout' stamp: 'jmv 2/26/2013 16:29'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."
	"Only specific subclasses do layout. They redefine this method.
	Remember to call super, or set layoutNeeded ivar to false!!"

	layoutNeeded _ false! !

!Morph methodsFor: 'updating' stamp: 'jmv 2/26/2013 16:54'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Include submorphs if we don't clip!!
	Think about it. We don't to know about a specific display rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"

	self invalidateRect: (0@0 extent: self morphExtent).
	self clipsSubmorphs ifFalse: [
		self submorphsDo: [ :m | m redrawNeeded ]]! !

