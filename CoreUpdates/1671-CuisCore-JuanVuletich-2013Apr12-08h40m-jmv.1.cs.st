'From Cuis 4.1 of 12 December 2012 [latest update: #1670] on 12 April 2013 at 9:04:24 am'!

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 4/12/2013 09:02'!
primDisplayBoundsOfInverseTransformOf: srcRect into: dstRect
	"Internalize srcRect, and find a bounding rectangle with horizontal and vertical bounds (in the inner space) and 
	integer coordinates (i.e. adisplayBounds). Store result into dstRect."

	"Warning: the answer is rounded to integers by the primitive
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	MatrixTransform2x3 identity primDisplayBoundsOfInverseTransformOf: (-2@ 2 extent: 10@10) into: Rectangle new
	MatrixTransform2x3 identity primDisplayBoundsOfInverseTransformOf: (-12@ 12 extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 2)  primDisplayBoundsOfInverseTransformOf: (-4@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 4)  primDisplayBoundsOfInverseTransformOf: (-2@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 2)  primDisplayBoundsOfInverseTransformOf: (-14@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 4)  primDisplayBoundsOfInverseTransformOf: (-12@ 2  extent: 10@10) into: Rectangle new
	
	These are Ok (answer is positive)
	(MatrixTransform2x3 withTranslation: -2)  primDisplayBoundsOfInverseTransformOf: (4@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: -4)  primDisplayBoundsOfInverseTransformOf: (2@ 2  extent: 10@10) into: Rectangle new
	"
	<primitive: 'primitiveInvertRectInto' module: 'Matrix2x3Plugin'>

	^nil! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 4/12/2013 09:02'!
primDisplayBoundsOfTransformOf: srcRect into: dstRect
	"Externalize srcRect, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Store result into dstRect."

	"Warning: the answer is rounded to integers by the primitive
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	MatrixTransform2x3 identity primDisplayBoundsOfTransformOf: (-2@ 2 extent: 10@10) into: Rectangle new
	MatrixTransform2x3 identity primDisplayBoundsOfTransformOf: (-12@ 12 extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 2)  primDisplayBoundsOfTransformOf: (-4@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: -4)  primDisplayBoundsOfTransformOf: (2@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 2)  primDisplayBoundsOfTransformOf: (-14@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 4)  primDisplayBoundsOfTransformOf: (-12@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: -4)  primDisplayBoundsOfTransformOf: (12@ 2  extent: 10@10) into: Rectangle new
	
	These are Ok (answer is positive)
	(MatrixTransform2x3 withTranslation: -2)  primDisplayBoundsOfTransformOf: (4@ 2  extent: 10@10) into: Rectangle new
	(MatrixTransform2x3 withTranslation: 4)  primDisplayBoundsOfTransformOf: (-2@ 2  extent: 10@10) into: Rectangle new
	"
	<primitive: 'primitiveTransformRectInto' module: 'Matrix2x3Plugin'>

	^nil! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 4/12/2013 08:31'!
primInverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	MatrixTransform2x3 identity primInverseTransform: (-2@ 2)
	(MatrixTransform2x3 withTranslation: 2)  primInverseTransform: (-4@ 2)
	(MatrixTransform2x3 withTranslation: 4)  primInverseTransform: (-2@ 2)
	
	These are Ok (answer is positive)
	(MatrixTransform2x3 withTranslation: -2)  primInverseTransform: (4@ 2)
	(MatrixTransform2x3 withTranslation: -4)  primInverseTransform: (2@ 2)
	"

	<primitive: 'primitiveInvertPoint' module: 'Matrix2x3Plugin'>
	^nil! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 4/12/2013 08:53'!
primTransform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	MatrixTransform2x3 identity primTransform: (-2@ 2)
	(MatrixTransform2x3 withTranslation: 2)  primTransform: (-4@ 2)
	(MatrixTransform2x3 withTranslation: -4)  primTransform: (2@ 2)

	This is ok (answer is positive)
	(MatrixTransform2x3 withTranslation: 4)  primTransform: (-2@ 2)
	(MatrixTransform2x3 withTranslation: -2)  primTransform: (4@ 2)
	"

	<primitive: 'primitiveTransformPoint' module: 'Matrix2x3Plugin'>
	^nil! !


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/12/2013 08:44'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	if DoIntegerTransformations, answer rounded integer values. "

	^ DoIntegerTransformations
		ifTrue: [ self intInverseTransform: aPoint ]
		ifFalse: [ self floatInverseTransform: aPoint ]! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/12/2013 08:44'!
transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	if DoIntegerTransformations, answer rounded integer values ."

	^ DoIntegerTransformations
		ifTrue: [ self intTransform: aPoint ]
		ifFalse: [ self floatTransform: aPoint ]! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 4/12/2013 09:03'!
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
		(dstRect origin > (0@0) and: [ dstRect corner > (0@0) ])
			ifTrue: [ ^dstRect ]].
	result _ Rectangle encompassing: (self intInverseTransformPoints: srcRect corners).
	dstRect
		setOrigin: result origin
		corner: result corner.
	^dstRect! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 4/12/2013 09:02'!
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
		(dstRect origin > (0@0) and: [ dstRect corner > (0@0) ])
			ifTrue: [ ^dstRect ]].
	result _ Rectangle encompassing: (self intTransformPositions: srcRect corners).
	dstRect
		setOrigin: result origin
		corner: result corner.
	^dstRect! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 4/12/2013 08:43'!
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

	(self primInverseTransform: aPoint) ifNotNil: [ :answer |
		answer > (0@0)
			ifTrue: [ ^answer ]].
	^ (self floatInverseTransform: aPoint) rounded! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 4/12/2013 08:34'!
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

	(self primTransform: aPoint) ifNotNil: [ :answer |
		answer > (0@0)
			ifTrue: [ ^answer ]].
	^ (self floatTransform: aPoint) rounded! !

