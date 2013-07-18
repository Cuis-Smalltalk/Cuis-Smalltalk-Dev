'From Cuis 4.1 of 12 December 2012 [latest update: #1671] on 12 April 2013 at 11:18:29 am'!

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 4/12/2013 10:54'!
fixRoundingInPrimitives: aPointOrNil
	"Matrix2x3Plugin does integer rounding using C (int) cast.
	This is broken for negative numbers, as it always rounds towards zero
	(and not towards negative infinity).
	If aPoint has positive coordinates, it is ok, answer it.
	If any coordinate is negative, fix it and answer the new point.
	If any coordinate is zero, we can not fix it, as we don't know if the original value was in [0.0..1.0), and zero is the right answer, or in (-1.0..0.0), where -1 would be the right answer. Answer nil, so caller finds another solution. 
	Try to do it quickly!!"

	| x y |
	aPointOrNil ifNil: [ ^nil ].
	x _ aPointOrNil x.
	y _ aPointOrNil y.
	(x > 0 and: [ y > 0 ]) ifTrue: [
		^aPointOrNil ].
	(x = 0 or: [ y = 0 ]) ifTrue: [
		^nil ].
	^(x>0 ifTrue: [x] ifFalse: [x-1]) @
		(y>0 ifTrue: [y] ifFalse: [y-1])! !


!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 4/12/2013 11:18'!
displayBoundsOfInverseTransformOf: srcRect into: dstRect
	"Externalize srcRect, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Store result into dstRect.
	Primitive rounds and answers integers.
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here.

	MatrixTransform2x3 identity displayBoundsOfInverseTransformOf: (-2@ 2 extent: 10@10) into: Rectangle new
	MatrixTransform2x3 identity displayBoundsOfInverseTransformOf: (-12@ 12 extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 2)  displayBoundsOfInverseTransformOf: (-4@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 4)  displayBoundsOfInverseTransformOf: (-2@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 2)  displayBoundsOfInverseTransformOf: (-14@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 4)  displayBoundsOfInverseTransformOf: (-12@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: -2)  displayBoundsOfInverseTransformOf: (4@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: -4)  displayBoundsOfInverseTransformOf: (2@ 2  extent: 10@10) into: Rectangle new
	"

	| result |
	(self primDisplayBoundsOfInverseTransformOf: srcRect into: dstRect) ifNotNil: [
		(self fixRoundingInPrimitives: dstRect origin) ifNotNil: [ :o |
			(self fixRoundingInPrimitives: dstRect corner) ifNotNil: [ :c |
				^ dstRect setOrigin: o corner: c ]]].
	result _ Rectangle encompassing: (srcRect corners collect: [ :pt | self intInverseTransform: pt ]).
	^dstRect setOrigin: result origin corner: result corner! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 4/12/2013 11:18'!
displayBoundsOfTransformOf: srcRect into: dstRect
	"Externalize srcRect, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Store result into dstRect.
	Primitive rounds and answers integers.
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here.

	MatrixTransform2x3 identity displayBoundsOfTransformOf: (-2@ 2 extent: 10@10) into: Rectangle new
	MatrixTransform2x3 identity displayBoundsOfTransformOf: (-12@ 12 extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 2)  displayBoundsOfTransformOf: (-4@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: -4)  displayBoundsOfTransformOf: (2@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 2)  displayBoundsOfTransformOf: (-14@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 4)  displayBoundsOfTransformOf: (-12@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: -4)  displayBoundsOfTransformOf: (12@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: -2)  displayBoundsOfTransformOf: (4@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 4)  displayBoundsOfTransformOf: (-2@ 2  extent: 10@10) into: Rectangle new
	"

	| result |
	(self primDisplayBoundsOfTransformOf: srcRect into: dstRect) ifNotNil: [
		(self fixRoundingInPrimitives: dstRect origin) ifNotNil: [ :o |
			(self fixRoundingInPrimitives: dstRect corner) ifNotNil: [ :c |
				^ dstRect setOrigin: o corner: c ]]].
	result _ Rectangle encompassing: (srcRect corners collect: [ :pt | self intTransform: pt ]).
	^dstRect setOrigin: result origin corner: result corner! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 4/12/2013 10:55'!
intInverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer is not strictly positive, it is off by one. 
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here.

	MatrixTransform2x3 identity intInverseTransform: (-2@ 2)
	(MatrixTransform2x3 withTranslation: 2)  intInverseTransform: (-4@ 2)
	(MatrixTransform2x3 withTranslation: 4)  intInverseTransform: (-2@ 2)
	(MatrixTransform2x3 withTranslation: -2)  intInverseTransform: (4@ 2)
	(MatrixTransform2x3 withTranslation: -4)  intInverseTransform: (2@ 2)
	"
	^(self fixRoundingInPrimitives: (self primInverseTransform: aPoint))
		ifNil: [ (self floatInverseTransform: aPoint) rounded ]! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 4/12/2013 10:56'!
intTransform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here.

	MatrixTransform2x3 identity intTransform: (-2@ 2)
	(MatrixTransform2x3 withTranslation: 2)  intTransform: (-4@ 2)
	(MatrixTransform2x3 withTranslation: 4)  intTransform: (-2@ 2)
	(MatrixTransform2x3 withTranslation: -4)  intTransform: (2@ 2)
	(MatrixTransform2x3 withTranslation: -2)  intTransform: (4@ 2)
	"
	^(self fixRoundingInPrimitives: (self primTransform: aPoint))
		ifNil: [ (self floatTransform: aPoint) rounded ]! !

!methodRemoval: MatrixTransform2x3 #intInverseTransformPoints:!
MatrixTransform2x3 removeSelector: #intInverseTransformPoints:!
!methodRemoval: MatrixTransform2x3 #intTransformPositions:!
MatrixTransform2x3 removeSelector: #intTransformPositions:!
