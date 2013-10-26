'From Cuis 4.0 of 21 April 2012 [latest update: #1415] on 28 August 2012 at 6:21:22 pm'!

!MatrixTransform2x3 commentStamp: 'jmv 8/28/2012 15:55' prior: 0!
This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix. 

The direct transformation is equivalent as multiplying a vector by the matrix. The inverse transformation is multiplying a vector by the inverse of the matrix. By convention, we say that the direct transform is outwards (#externalizePosition:). Therefore, the inverse transform is called #internalizePosition: .

Direct transform (#externalizePosition:) is fast and cheap, while inverse transform (#internalizePosition:) is relatively expensive.

Implementation Note: In the original version, from Squeak, it is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here). In this version, point transformations are for Float points. The plugin needs fixing, so the plugin primitives that round to integer points are commented. Rectangle transformations were removed (they were are for Display rectangles, and are done in integer coordinates by the plugin).!


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 8/28/2012 16:29'!
externalizePosition: aPoint
	"Answer coordinates for aPoint in the space we are in.
	 aPoint is expressed in us."

	^self transform: aPoint! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 8/28/2012 16:28'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint."

	"Warning: The plugin rounds result to integers. We want Float points."
	"<primitive: 'primitiveInvertPoint' module: 'Matrix2x3Plugin'>"

	| x y det a11 a12 a21 a22 detX detY |
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

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 8/28/2012 16:34'!
inverseTransformPoints: inArray
	"Transform all the points of inArray from global into local coordinates"

	^inArray collect: [ :pt | self inverseTransform: pt ]! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 8/28/2012 16:05'!
transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint."

	"Warning: The plugin rounds result to integers. We want Float points."
	"<primitive: 'primitiveTransformPoint' module: 'Matrix2x3Plugin'>"

	| x y |
	x _ (aPoint x * self a11) + (aPoint y * self a12) + self a13.
	y _ (aPoint x * self a21) + (aPoint y * self a22) + self a23.
	^x @ y! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 8/28/2012 16:32'!
transformPositions: inArray
	"Transform all the points of inArray from local into global coordinates"

	^inArray collect: [ :pt | self transform: pt ]! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 8/28/2012 16:33'!
displayBoundsOfInverseTransformOf: aRectangle
	"Internalize srcRect, and find a bounding rectangle with horizontal and vertical bounds (in the inner space) and integer coordinates (i.e. a displayBounds).
	Warning. Pimitive and Smalltalk code might round differently"
	^self displayBoundsOfInverseTransformOf: aRectangle into: Rectangle new! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 8/28/2012 16:34'!
displayBoundsOfInverseTransformOf: srcRect into: dstRect
	"Internalize srcRect, and find a bounding rectangle with horizontal and vertical bounds (in the inner space) and integer coordinates (i.e. adisplayBounds). Store result into dstRect.
	Warning. Pimitive and Smalltalk code might round differently"
	<primitive: 'primitiveInvertRectInto' module: 'Matrix2x3Plugin'>
	^Rectangle encompassing: (self inverseTransformPoints: srcRect corners)! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 8/28/2012 16:33'!
displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds)."

	^self displayBoundsOfTransformOf: aRectangle into: Rectangle new! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 8/28/2012 16:32'!
displayBoundsOfTransformOf: srcRect into: dstRect
	"Externalize srcRect, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds). Store result into dstRect.
	Warning. Pimitive would round"
"	<primitive: 'primitiveTransformRectInto' module: 'Matrix2x3Plugin'>"

	^Rectangle encompassing: (self transformPositions: srcRect corners)! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 8/28/2012 14:59'!
forScaleFactor: sf originalExtent: e
	"Answer a Transfor appropriate for image rescaling. sf is the scale factor to apply (i.e. 0.5 means result is half the size of original image). e is the extent of the original image."

	^self transformFromLocal: (0.5@0.5 extent: e) toExternal: (0.5@0.5 extent: sf*e)! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 8/28/2012 14:57'!
transformFromLocal: localBounds toExternal: externalBounds
	"Answer a Transform to translate coordinates inside localBounds into coordinates inside externalBounds.
	Parameters are instances of Rectangle. Therefore, there's no rotation involved, just scale and offset."

	^((self withOffset: (externalBounds topLeft + externalBounds bottomRight / 2.0)) composedWith:
		(self withScale: (externalBounds extent / localBounds extent) asFloatPoint)) composedWith:
			(self withOffset: (localBounds topLeft + localBounds bottomRight / 2.0) negated)! !


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 8/28/2012 16:29'!
internalizePosition: aPoint
	"Answer our coordinates for aPoint.
	 aPoint is expressed in the space we are in."

	^self inverseTransform: aPoint! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 8/28/2012 16:34'!
inverseTransformation
	"Return the inverse transformation of the receiver.
	The inverse transformation is computed by first calculating
	the inverse offset and then computing transformations
	for the two identity vectors (1@0) and (0@1)"
	| r1 r2 r3 m |
	r3 := self inverseTransform: 0@0.
	r1 := (self inverseTransform: 1@0) - r3.
	r2 := (self inverseTransform: 0@1) - r3.
	m := self species new.
	m
		a11: r1 x; a12: r2 x; a13: r3 x;
		a21: r1 y; a22: r2 y; a23: r3 y.
	^m! !

!methodRemoval: MatrixTransform2x3 class #transformFromLocal:toGlobal:!
MatrixTransform2x3 class removeSelector: #transformFromLocal:toGlobal:!
!methodRemoval: MatrixTransform2x3 #displayBoundsOfExternalizedOf:!
MatrixTransform2x3 removeSelector: #displayBoundsOfExternalizedOf:!
!methodRemoval: MatrixTransform2x3 #displayBoundsOfExternalizedOf:into:!
MatrixTransform2x3 removeSelector: #displayBoundsOfExternalizedOf:into:!
!methodRemoval: MatrixTransform2x3 #displayBoundsOfInternalizedOf:!
MatrixTransform2x3 removeSelector: #displayBoundsOfInternalizedOf:!
!methodRemoval: MatrixTransform2x3 #displayBoundsOfInternalizedOf:into:!
MatrixTransform2x3 removeSelector: #displayBoundsOfInternalizedOf:into:!
!methodRemoval: MatrixTransform2x3 #externalize:!
MatrixTransform2x3 removeSelector: #externalize:!
!methodRemoval: MatrixTransform2x3 #externalizePositions:!
MatrixTransform2x3 removeSelector: #externalizePositions:!
!methodRemoval: MatrixTransform2x3 #globalBounds:toLocal:!
MatrixTransform2x3 removeSelector: #globalBounds:toLocal:!
!methodRemoval: MatrixTransform2x3 #globalBoundsToLocal:!
MatrixTransform2x3 removeSelector: #globalBoundsToLocal:!
!methodRemoval: MatrixTransform2x3 #internalizePositions:!
MatrixTransform2x3 removeSelector: #internalizePositions:!
!methodRemoval: MatrixTransform2x3 #localBounds:toGlobal:!
MatrixTransform2x3 removeSelector: #localBounds:toGlobal:!
!methodRemoval: MatrixTransform2x3 #localBoundsToGlobal:!
MatrixTransform2x3 removeSelector: #localBoundsToGlobal:!
