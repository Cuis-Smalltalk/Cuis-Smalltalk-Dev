'From Cuis 4.2 of 25 July 2013 [latest update: #2154] on 12 January 2015 at 5:22:04.703438 pm'!

!MatrixTransform2x3 commentStamp: 'jmv 1/12/2015 12:58' prior: 0!
This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix. 

The direct transformation is equivalent as multiplying the matrix by a column vector (with an extra element of value 1). The inverse transformation is multiplying the inverse of the matrix by a column vector (with an extra element of value 1). By convention, we say that the direct transform is outwards (#externalizePosition:). Therefore, the inverse transform is called #internalizePosition: .

Direct transform (#externalizePosition:) is fast and cheap, while inverse transform (#internalizePosition:) is relatively expensive.

We can consider the matrix to be of 3x3 with an extra row with #( 0 0 1) at the bottom, especially for having a properly defined inverse matrix.

Implementation Note: In the original version, from Squeak, it is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here). In this version, both alternatives are available, with explicit protocols, and a global setting for the Squeak protocol.

Note: Methods that modify an instance answer the result. The result might be the receiver itself, or it might be a new instance. Therefore:
- Don't assume the receiver will be modified. Always take the result.
- Don't assume the answer will be a new instance. The receiver might be modified!! (make a copy if needed)!


!MorphicTranslation commentStamp: '<historical>' prior: 0!
Can replace a MatrixTransform2x3 when there is only a translation applied (no scale change, no rotation, no skew).

Note: Methods that modify an instance answer the result. The result might be the receiver itself, or it might be a new instance. Therefore:
- Don't assume the receiver will be modified. Always take the result.
- Don't assume the answer will be a new instance. The receiver might be modified!! (make a copy if needed)!


!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/12/2015 13:28'!
italizing2
	"a little shear
	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12 - (self scale*0.2).
	self setTranslation: (self scale*0.2)@0 + self translation.
	^self! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/12/2015 13:33'!
scaledByNumber: aNumber rotatedBy: radians
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling.

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a13 a21 a22 a23|
	s _ radians sin.
	c _ radians cos.
	a11 _ self a11 * aNumber.
	a12 _ self a12 * aNumber.
	a13 _ self a13 * aNumber.
	a21 _ self a21 * aNumber.
	a22 _ self a22 * aNumber.
	a23 _ self a23 * aNumber.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a13: (c * a13) - (s * a23).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	self a23: (s * a13) + (c * a23).
	^self! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/12/2015 15:46'!
translatedBy: aPoint
	"add an offset in the receiver

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	self a13: self a13 + pt x.
	self a23: self a23 + pt y.
	^ self! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/11/2015 16:50'!
withRotation: radians scale: scale
	"Set rotation and scaling according to parameters.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a MatrixTransform2x3."

	self setRadians: radians scale: scale.
	^self! !


!Morph methodsFor: 'geometry' stamp: 'jmv 1/11/2015 16:46'!
rotateBy: radians
	"Change the scale of this morph. Argument is an angle."
	self redrawNeeded.
	location _ location rotatedBy: radians.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 1/11/2015 16:50'!
rotation: radians scale: scale
	"Change the scale of this morph. Arguments are an angle and a scale."
	self redrawNeeded.
	location _ location withRotation: radians scale: scale.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 1/11/2015 16:44'!
scaleBy: scaleFactor
	"Change the scale of this morph. Argument is a factor."
	self redrawNeeded.
	location _ location scaledBy: scaleFactor.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !


!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/12/2015 13:28'!
italizing2
	"a little shear
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a MatrixTransform2x3."

	^(MatrixTransform2x3 withTranslation: self translation) italizing2! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/12/2015 13:33'!
scaledByNumber: aNumber rotatedBy: radians
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling.

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a MatrixTransform2x3."

	^(MatrixTransform2x3 withTranslation: self translation) scaledByNumber: aNumber rotatedBy: radians! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/12/2015 15:46'!
translatedBy: aPoint
	"add an offset in the receiver

	Answer the modified object. I this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or such) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."
	| pt |
	pt _ aPoint asPoint.
	deltaX _ deltaX + pt x.
	deltaY _ deltaY + pt y.
	^self! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/12/2015 13:23'!
withRotation: radians scale: scale
	"Set rotation and scaling according to parameters.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a MatrixTransform2x3."

	^(MatrixTransform2x3 withTranslation: self translation) withRotation: radians scale: scale! !


!Point methodsFor: 'transforming' stamp: 'jmv 1/12/2015 13:32'!
inverseRotatedBy: angle about: center
	"This method considers angle to be in standard math convention (counter clock wise) while at the same time it considers the points to be in display convention (y axis increases downwards)"

	| p r theta |
	p _ self - center.
	r _ p r.
	theta _ angle asFloat - p theta.
	^ (center x asFloat + (r * theta cos)) @
	  (center y asFloat - (r * theta sin))! !


!WarpBlt class methodsFor: 'examples' stamp: 'jmv 1/12/2015 13:32'!
test1
	"
	Display restoreAfter: [WarpBlt test1]
	"
	"Demonstrates variable scale and rotate"
	| warp pts r1 p0 p ext |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 _ Rectangle originFromUser: 50@50.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 _ Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp _ (self toForm: Display)
		clipRect: (0@0 extent: r1 extent*5);
		sourceForm: Display;
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ {r1 topLeft. r1 bottomLeft. r1 bottomRight. r1 topRight}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		ext _ (r1 extent*((p-p0) r / 20.0 max: 0.1)) asIntegerPoint.
		warp copyQuad: pts toRect: (r1 extent*5-ext//2 extent: ext)]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 1/12/2015 13:32'!
test12
	"
	Display restoreAfter: [WarpBlt test12]
	"
	"Just like test1, but comparing smooth to non-smooth warps"
	| warp pts r1 p0 p ext warp2 |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 _ Rectangle originFromUser: 50@50.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 _ Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp _ (self toForm: Display)
		cellSize: 2;  "installs a colormap"
		clipRect: (0@0 extent: r1 extent*5);
		sourceForm: Display;
		combinationRule: Form over.
	warp2 _ (self toForm: Display)
		clipRect: ((0@0 extent: r1 extent*5) translatedBy: 250@0);
		sourceForm: Display;
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ {r1 topLeft. r1 bottomLeft. r1 bottomRight. r1 topRight}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		ext _ (r1 extent*((p-p0) r / 20.0 max: 0.1)) asIntegerPoint.
		warp copyQuad: pts toRect: (r1 extent*5-ext//2 extent: ext).
		warp2 copyQuad: pts toRect: ((r1 extent*5-ext//2 extent: ext) translatedBy: 250@0).
		]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 1/12/2015 13:32'!
test3
	"
	Display restoreAfter: [WarpBlt test3]
	"
	"The Squeak Release Mandala - 9/23/96 di"

	"Move the mouse near the center of the square.
	Up and down affects shrink/grow
	Left and right affect rotation angle"
	| warp pts p0 p box map d t |
	box _ 100@100 extent: 300@300.
	Display border: (box expandBy: 2) width: 2.

	"Make a color map that steps through the color space"
	map _ (Display depth > 8
		ifTrue: ["RGB is a bit messy..."
				d _ Display depth = 16 ifTrue: [5] ifFalse: [8].
				(1 to: 512) collect: [:i | t _ i bitAnd: 511.
					((t bitAnd: 16r7) bitShift: d-3)
					+ ((t bitAnd: 16r38) bitShift: d-3*2)
					+ ((t bitAnd: 16r1C0) bitShift: d-3*3)]]
		ifFalse: ["otherwise simple"
				1 to: (1 bitShift: Display depth)])
			as: Bitmap.
	warp _ (WarpBlt toForm: Display)
		clipRect: box;
		sourceForm: Display;
		colorMap: map;
		combinationRule: Form over.
	p0 _ box center.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ (box insetBy: p y - p0 y) innerCorners
			collect: [:pt | pt inverseRotatedBy: p x - p0 x / 50.0 about: p0].
		warp copyQuad: pts toRect: box]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 1/12/2015 13:32'!
test4
	"
	Display restoreAfter: [WarpBlt test4]
	"
	"The Squeak Release Mandala - 9/23/96 di
	This version does smoothing"

	"Move the mouse near the center ofhe square.
	Up and dn affects shrink/grow
	Left and right affect rotation angle"
	| warp pts p0 p box |
	box _ 100@100 extent: 300@300.
	Display border: (box expandBy: 2) width: 2.

	warp _ (WarpBlt toForm: Display)
		clipRect: box;
		sourceForm: Display;
		cellSize: 2;  "installs a colormap"
		combinationRule: Form over.
	p0 _ box center.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ (box insetBy: p y - p0 y) innerCorners
			collect: [:pt | pt inverseRotatedBy: p x - p0 x / 50.0 about: p0].
		warp copyQuad: pts toRect: box]! !

!WarpBlt class methodsFor: 'examples' stamp: 'jmv 1/12/2015 13:32'!
test5
	"
	Display restoreAfter: [WarpBlt test5]
	"
	"Demonstrates variable scale and rotate"
	| warp pts r1 p0 p |
	Utilities informUser: 'Choose a rectangle with interesting stuff'
		during: [r1 _ Rectangle fromUser.
				Sensor waitNoButton].
	Utilities informUser: 'Now click down and up
and move the mouse around the dot'
		during: [p0 _ Sensor waitClickButton.
				(Form dotOfSize: 8) displayAt: p0].
	warp _ (self toForm: Display)
		cellSize: 1;
		sourceForm: Display;
		cellSize: 2;  "installs a colormap"
		combinationRule: Form over.
	[Sensor isAnyButtonPressed] whileFalse:
		[p _ Sensor mousePoint.
		pts _ {r1 topLeft. r1 bottomLeft. r1 bottomRight. r1 topRight}
			collect: [:pt | pt inverseRotatedBy: (p-p0) theta about: r1 center].
		warp copyQuad: pts toRect: (r1 translatedBy: r1 width@0)]! !

!WarpBlt class methodsFor: 'form rotation' stamp: 'jmv 1/12/2015 13:32'!
rotate: srcForm degrees: angleInDegrees center: aPoint scaleBy: scalePoint smoothing: cellSize
	"Rotate the given Form the given number of degrees about the given center and scale its width and height by x and y of the given scale point. Smooth using the given cell size, an integer between 1 and 3, where 1 means no smoothing. Return a pair where the first element is the rotated Form and the second is the position offset required to align the center of the rotated Form with that of the original. Note that the dimensions of the resulting Form generally differ from those of the original."

	| srcRect center radians dstOrigin dstCorner p dstRect inverseScale quad dstForm newCenter warpSrc |
	srcRect _ srcForm boundingBox.
	center _ srcRect center.
	radians _ angleInDegrees degreesToRadians.
	dstOrigin _ dstCorner _ center.
	srcRect corners do: [:corner |
		"find the limits of a rectangle that just encloses the rotated
		 original; in general, this rectangle will be larger than the
		 original (e.g., consider a square rotated by 45 degrees)"
		p _ ((corner - center) scaledBy: scalePoint) + center.
		p _ (p inverseRotatedBy: radians about: center) rounded.
		dstOrigin _ dstOrigin min: p.
		dstCorner _ dstCorner max: p].

	"rotate the enclosing rectangle back to get the source quadrilateral"
	dstRect _ dstOrigin corner: dstCorner.
	inverseScale _ (1.0 / scalePoint x)@(1.0 / scalePoint y).
	quad _ dstRect innerCorners collect: [:corner |
		p _ corner inverseRotatedBy: radians negated about: center.
		((p - center) scaledBy: inverseScale) + center].

	"make a Form to hold the result and do the rotation"
	warpSrc _ srcForm.
	(srcForm is: #ColorForm)
		ifTrue: [
			cellSize > 1 | true "ar 12/27/2001: Always enable - else sketches won't work"
				ifTrue: [
					warpSrc _ Form extent: srcForm extent depth: 16.
					srcForm displayOn: warpSrc.
					dstForm _ Form extent: dstRect extent depth: 16]  "use 16-bit depth to allow smoothing"
				ifFalse: [
					dstForm _ srcForm class extent: dstRect extent depth: srcForm depth]]
		ifFalse: [
			dstForm _ srcForm class extent: dstRect extent depth: srcForm depth].

	(WarpBlt toForm: dstForm)
		sourceForm: warpSrc;
		colorMap: (warpSrc colormapIfNeededFor: dstForm);
		cellSize: cellSize;  "installs a new colormap if cellSize > 1"
		combinationRule: Form paint;
		copyQuad: quad toRect: dstForm boundingBox.

	(dstForm is: #ColorForm) ifTrue: [ dstForm colors: srcForm colors copy ].
	newCenter _ (center inverseRotatedBy: radians about: aPoint) truncated.
	^ Array with: dstForm with: dstRect origin + (newCenter - center)
! !


!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 1/12/2015 13:29'!
italizing
	"a little shear
	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12 + (self scale*0.2).
	^self! !


!FunctionGraphMorph methodsFor: 'initialization' stamp: 'jmv 1/12/2015 15:47'!
initialize
	super initialize.
 	location _ (location
		withYAxisNegated
		translatedBy: 300)
		scaledBy: 40.
	functions _ OrderedCollection new.
	colors _ OrderedCollection new! !


!MorphicTranslation methodsFor: 'composing' stamp: 'jmv 1/12/2015 13:40'!
innerComposedWithMatrixTransform2x3: aMatrixTransform2x3
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^aMatrixTransform2x3 composedWith: (MatrixTransform2x3 withTranslation: self translation)
" 	^ aMatrixTransform2x3 copy offsetBy: self translation" ! !

!MorphicTranslation methodsFor: 'composing' stamp: 'jmv 1/12/2015 15:47'!
innerComposedWithTranslation: aMorphicTranslation
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^ self copy translatedBy: aMorphicTranslation translation! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/12/2015 12:54'!
withTranslation: aPoint
	"set an offset in the receiver

	Answer the modified object. I this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or such) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	deltaX _ pt x.
	deltaY _ pt y.
	^self! !


!MatrixTransform2x3Test methodsFor: 'testing' stamp: 'jmv 1/12/2015 15:45'!
testComposition
	"
	MatrixTransform2x3Test new testComposition
	"
	| composition inner outer |

	outer _ MatrixTransform2x3 withTranslation: 3@5.
	inner _ MatrixTransform2x3 withRadians: 0.3.
	composition _ outer composedWith: inner.
	self assert: composition translation = outer translation.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).


	outer _ MatrixTransform2x3 withRadians: 0.3.
	inner _ MatrixTransform2x3 withTranslation: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: ((outer externalizePosition: (inner externalizePosition: 3@4)) - (composition externalizePosition: 3@4)) r < 0.0001.
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: ((inner internalizePosition: (outer internalizePosition: 3@4)) - (composition internalizePosition: 3@4)) r < 0.0001.
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !

!MatrixTransform2x3Test methodsFor: 'testing' stamp: 'jmv 1/12/2015 15:45'!
testInverseTransformation
	"
	MatrixTransform2x3Test new testInverseTransformation
	"
	| forward inverse |

	forward _ MatrixTransform2x3 withTranslation: 3@5.
	inverse _ forward inverseTransformation.
	
	self assert: inverse translation = forward translation negated.
	self assert: (inverse externalizePosition: 3@4) = (forward internalizePosition: 3@4).
	self assert: (inverse externalizeDelta: 3@4) = (forward internalizeDelta: 3@4).
	self assert: (inverse externalizeScalar: 7) = (forward internalizeScalar: 7).
	self assert: (inverse internalizePosition: 3@4) = (forward externalizePosition: 3@4).
	self assert: (inverse internalizeDelta: 3@4) = (forward externalizeDelta: 3@4).
	self assert: (inverse internalizeScalar: 7) = (forward externalizeScalar: 7).


	forward _ MatrixTransform2x3 withRadians: 0.25.
	inverse _ forward inverseTransformation.

	self assert: inverse radians = forward radians negated.
	self assert: ((inverse externalizePosition: 3@4) - (forward internalizePosition: 3@4)) r < 0.0001.
	self assert: ((inverse externalizeDelta: 3@4) - (forward internalizeDelta: 3@4)) r < 0.0001.
	self assert: ((inverse externalizeScalar: 7) - (forward internalizeScalar: 7)) abs < 0.0001.
	self assert: ((inverse internalizePosition: 3@4) - (forward externalizePosition: 3@4)) r < 0.0001.
	self assert: ((inverse internalizeDelta: 3@4) - (forward externalizeDelta: 3@4)) r < 0.0001.
	self assert: ((inverse internalizeScalar: 7) - (forward externalizeScalar: 7)) abs < 0.0001.! !


!MorphicLocationTest methodsFor: 'testing' stamp: 'jmv 1/12/2015 15:45'!
testComposition
	"
	MorphicLocationTest new testComposition
	"
	| composition inner outer |

	outer _ MorphicTranslation withTranslation: 3@5.
	inner _ MatrixTransform2x3 withRadians: 0.3.
	composition _ outer composedWith: inner.
	self assert: composition translation = outer translation.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).


	outer _ MatrixTransform2x3 withRadians: 0.3.
	inner _ MorphicTranslation withTranslation: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: ((outer externalizePosition: (inner externalizePosition: 3@4)) - (composition externalizePosition: 3@4)) r < 0.0001.
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: ((inner internalizePosition: (outer internalizePosition: 3@4)) - (composition internalizePosition: 3@4)) r < 0.0001.
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !


!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/12/2015 15:46'!
testComposition
	"
	MorphicTranslationTest new testComposition
	"
	| composition inner outer |

	outer _ MorphicTranslation withTranslation: 3@5.
	inner _ MorphicTranslation withTranslation: -1@2.
	composition _ outer composedWith: inner.
	self assert: composition translation = (outer translation + inner translation).
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).


	outer _ MorphicTranslation withTranslation: -1@2.
	inner _ MorphicTranslation withTranslation: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/12/2015 15:46'!
testInverseTransformation
	"
	MorphicTranslationTest new testInverseTransformation
	"
	| forward inverse |

	forward _ MorphicTranslation withTranslation: 3@5.
	inverse _ forward inverseTransformation.
	
	self assert: inverse translation = forward translation negated.
	self assert: (inverse externalizePosition: 3@4) = (forward internalizePosition: 3@4).
	self assert: (inverse externalizeDelta: 3@4) = (forward internalizeDelta: 3@4).
	self assert: (inverse externalizeScalar: 7) = (forward internalizeScalar: 7).
	self assert: (inverse internalizePosition: 3@4) = (forward externalizePosition: 3@4).
	self assert: (inverse internalizeDelta: 3@4) = (forward externalizeDelta: 3@4).
	self assert: (inverse internalizeScalar: 7) = (forward externalizeScalar: 7)! !

!methodRemoval: Point #inverseRotateBy:about:!
Point removeSelector: #inverseRotateBy:about:!
!methodRemoval: MorphicTranslation class #identity!
MorphicTranslation class removeSelector: #identity!
!methodRemoval: MorphicTranslation class #withPosition:!
MorphicTranslation class removeSelector: #withPosition:!
!methodRemoval: MorphicTranslation #scaledByNumber:rotateBy:!
MorphicTranslation removeSelector: #scaledByNumber:rotateBy:!
!methodRemoval: MorphicTranslation #withAddedOffset:!
MorphicTranslation removeSelector: #withAddedOffset:!
!methodRemoval: MatrixTransform2x3 class #withPosition:!
MatrixTransform2x3 class removeSelector: #withPosition:!
!methodRemoval: MatrixTransform2x3 #scaledByNumber:rotateBy:!
MatrixTransform2x3 removeSelector: #scaledByNumber:rotateBy:!
!methodRemoval: MatrixTransform2x3 #withAddedOffset:!
MatrixTransform2x3 removeSelector: #withAddedOffset:!
