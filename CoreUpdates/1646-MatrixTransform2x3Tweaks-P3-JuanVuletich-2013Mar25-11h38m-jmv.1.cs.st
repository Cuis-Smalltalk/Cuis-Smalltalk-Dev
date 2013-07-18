'From Cuis 4.1 of 12 December 2012 [latest update: #1645] on 25 March 2013 at 11:52:27 am'!

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 3/25/2013 11:41'!
intInverseTransformPoints: inArray
	"Transform all the points of inArray from global into local coordinates"

	^inArray collect: [ :pt | self intInverseTransform: pt ]! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 3/25/2013 11:48'!
intTransformPositions: inArray
	"Transform all the points of inArray from local into global coordinates"

	^inArray collect: [ :pt | self intTransform: pt ]! !


!FormCanvas methodsFor: 'drawing' stamp: 'jmv 3/25/2013 11:46'!
line: pt1 to: pt2 width: w color: c
	| offset p1 p2 |
	p1 _ currentTransformation intTransform: pt1.
	p2 _ currentTransformation intTransform: pt2.
	offset _ w // 2.
	self setPaintColor: c.
	port
		width: w;
		height: w;
		drawFrom: p1 + offset to: p2 + offset! !

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 3/25/2013 11:47'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |
	p _ currentTransformation intTransform: aPoint.
	self isShadowDrawing ifTrue: [
		^self stencil: aForm at: aPoint sourceRect: sourceRect color: shadowColor ].
	r _ (self depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
	port colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	port image: aForm at: p sourceRect: sourceRect rule: r! !

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 3/25/2013 11:46'!
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |
	p _ currentTransformation intTransform: aPoint.
	self setPaintColor: aColor.
	port colorMap: stencilForm maskingMap.
	port stencil: stencilForm
		at: p
		sourceRect: sourceRect! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/25/2013 11:46'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor kern: kernOrNil

	| p1 font kern insideColor |
	p1 _ currentTransformation intTransform: aPoint.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ StrikeFont default ].
	kern _ kernOrNil ifNil: [ font baseKern negated ].
	insideColor _ shadowColor ifNil: [ aColor ].
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		strikeFont: font
		color: insideColor
		kern: kern! !


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 3/25/2013 11:49'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	if DoIntegerTransformations, answer rounded integer values, for better performance. "

	^DoIntegerTransformations
		ifTrue: [ self intInverseTransform: aPoint ]
		ifFalse: [ self floatInverseTransform: aPoint ]! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 3/25/2013 11:49'!
transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	if DoIntegerTransformations, answer rounded integer values, for better performance."

	^DoIntegerTransformations
		ifTrue: [ self intTransform: aPoint ]
		ifFalse: [ self floatTransform: aPoint ]! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 3/25/2013 11:42'!
inverseTransformation
	"Return the inverse transformation of the receiver.
	The inverse transformation is computed by first calculating
	the inverse offset and then computing transformations
	for the two identity vectors (1@0) and (0@1)"
	| r1 r2 r3 m |
	r3 := self floatInverseTransform: 0@0.
	r1 := (self floatInverseTransform: 1@0) - r3.
	r2 := (self floatInverseTransform: 0@1) - r3.
	m := self species new.
	m
		a11: r1 x; a12: r2 x; a13: r3 x;
		a21: r1 y; a22: r2 y; a23: r3 y.
	^m! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 3/25/2013 11:41'!
displayBoundsOfInverseTransformOf: srcRect into: dstRect
	"Internalize srcRect, and find a bounding rectangle with horizontal and vertical bounds (in the inner space) and 
	integer coordinates (i.e. adisplayBounds). Store result into dstRect."

	"Warning: the answer is rounded to integers by the primitive"
	<primitive: 'primitiveInvertRectInto' module: 'Matrix2x3Plugin'>

	| floatResult |
	floatResult _ Rectangle encompassing: (self intInverseTransformPoints: srcRect corners).
	dstRect
		setOrigin: floatResult origin
		corner: floatResult corner.
	^dstRect! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 3/25/2013 11:48'!
displayBoundsOfTransformOf: srcRect into: dstRect
	"Externalize srcRect, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Store result into dstRect."

	"Warning: the answer is rounded to integers by the primitive"
	<primitive: 'primitiveTransformRectInto' module: 'Matrix2x3Plugin'>

	| floatResult |
	floatResult _ Rectangle encompassing: (self intTransformPositions: srcRect corners).
	dstRect
		setOrigin: floatResult origin
		corner: floatResult corner.
	^dstRect! !

!methodRemoval: MatrixTransform2x3 #inverseTransformPoints:!
MatrixTransform2x3 removeSelector: #inverseTransformPoints:!
!methodRemoval: MatrixTransform2x3 #transformPositions:!
MatrixTransform2x3 removeSelector: #transformPositions:!
