'From Cuis 4.2 of 25 July 2013 [latest update: #2156] on 21 January 2015 at 12:37:55.618272 pm'!
!classDefinition: #AffineTransformation category: #'Morphic-Kernel'!
MorphicLocation variableWordSubclass: #AffineTransformation
	instanceVariableNames: ''
	classVariableNames: 'DoIntegerTransformations '
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/21/2015 12:21'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Use Smalltalk code, and not Matrix2x3Plugin, because we want Float conversion."
	| x y det a11 a12 a21 a22 detX detY |

	x _ aPoint x - self a13.
	y _ aPoint y - self a23.
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

!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/21/2015 12:19'!
transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Use Smalltalk code, and not Matrix2x3Plugin, because we want Float conversion."

	| x y |
	x _ (aPoint x * self a11) + (aPoint y * self a12) + self a13.
	y _ (aPoint x * self a21) + (aPoint y * self a22) + self a23.
	^x @ y! !

!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 1/21/2015 12:20'!
inverseTransformation
	"Return the inverse transformation of the receiver.
	The inverse transformation is computed by first calculating
	the inverse offset and then computing transformations
	for the two identity vectors (1@0) and (0@1)"
	| r1 r2 r3 m |
	r3 _ self inverseTransform: 0@0.
	r1 _ (self inverseTransform: 1@0) - r3.
	r2 _ (self inverseTransform: 0@1) - r3.
	m _ self species new.
	m
		a11: r1 x; a12: r2 x; a13: r3 x;
		a21: r1 y; a22: r2 y; a23: r3 y.
	^m! !

!AffineTransformation methodsFor: 'transforming rects' stamp: 'jmv 1/21/2015 12:17'!
displayBoundsOfTransformOf: srcRect into: dstRect
	"Externalize srcRect, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Store result into dstRect.
	Primitive rounds and answers integers.
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here."

	| result |
	(self primDisplayBoundsOfTransformOf: srcRect into: dstRect) ifNotNil: [
		(self fixRoundingInPrimitives: dstRect origin) ifNotNil: [ :o |
			(self fixRoundingInPrimitives: dstRect corner) ifNotNil: [ :c |
				^ dstRect setOrigin: o corner: c ]]].
	result _ Rectangle encompassing: (srcRect corners collect: [ :pt |
		(self transform: pt) rounded ]).
	^dstRect setOrigin: result origin corner: result corner! !


!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 1/21/2015 12:16'!
line: pt1 to: pt2 width: wp color: c
	|  p1 p2 w |
	p1 _ (currentTransformation transform: pt1) rounded.
	p2 _ (currentTransformation transform: pt2) rounded.
	w _ currentTransformation externalizeScalar: wp.
	self setPaintColor: c.
	port
		width: w;
		height: w;
		drawFrom: p1 to: p2! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 1/21/2015 12:17'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |
	p _ (currentTransformation transform: aPoint) rounded.
	self isShadowDrawing ifTrue: [
		^self stencil: aForm at: aPoint sourceRect: sourceRect color: shadowColor ].
	r _ (self depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
	port colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	port image: aForm at: p sourceRect: sourceRect rule: r! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 1/21/2015 12:16'!
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |
	p _ (currentTransformation transform: aPoint) rounded.
	self setPaintColor: aColor.
	port colorMap: stencilForm maskingMap.
	port stencil: stencilForm
		at: p
		sourceRect: sourceRect! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 1/21/2015 12:17'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	| p1 font colorToUse |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^self ].
	p1 _ (currentTransformation transform: aPoint) rounded.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ AbstractFont default ].
	colorToUse _ shadowColor ifNil: [ aColor ].
	font on:	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: colorToUse! !

!methodRemoval: MorphicTranslation #intTransform:!
MorphicTranslation removeSelector: #intTransform:!

!MorphicTranslation reorganize!
('converting coordinates' externalizeDelta: externalizePosition: externalizeScalar: internalizeDelta: internalizePosition: internalizeScalar: inverseTransform: transform:)
('accessing' degrees inverseTransformation radians scale translation)
('comparing' = hash)
('composing' composedWith: composedWith:into: innerComposedWithAffineTransformation: innerComposedWithTranslation:)
('printing' printOn:)
('testing' doesNotRotate isIdentity isPureTranslation isTranslation:)
('private' setTranslation:)
('modifying' italizing italizing2 rotatedBy: scaledBy: scaledByNumber:rotatedBy: translatedBy: withRotation:scale: withTranslation: withYAxisNegated)
('transforming rects' displayBoundsOfTransformOf:)
('initialization' initialize)
!

!methodRemoval: AffineTransformation class #doIntegerTransformations:!
AffineTransformation class removeSelector: #doIntegerTransformations:!
!methodRemoval: AffineTransformation class #initialize!
AffineTransformation class removeSelector: #initialize!
AffineTransformation initialize!
!methodRemoval: AffineTransformation #floatInverseTransform:!
AffineTransformation removeSelector: #floatInverseTransform:!
!methodRemoval: AffineTransformation #floatTransform:!
AffineTransformation removeSelector: #floatTransform:!
!methodRemoval: AffineTransformation #intInverseTransform:!
AffineTransformation removeSelector: #intInverseTransform:!
!methodRemoval: AffineTransformation #intTransform:!
AffineTransformation removeSelector: #intTransform:!
!methodRemoval: AffineTransformation #primDisplayBoundsOfInverseTransformOf:into:!
AffineTransformation removeSelector: #primDisplayBoundsOfInverseTransformOf:into:!
!methodRemoval: AffineTransformation #primInverseTransform:!
AffineTransformation removeSelector: #primInverseTransform:!
!methodRemoval: AffineTransformation #primTransform:!
AffineTransformation removeSelector: #primTransform:!
!classDefinition: #AffineTransformation category: #'Morphic-Kernel'!
MorphicLocation variableWordSubclass: #AffineTransformation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
