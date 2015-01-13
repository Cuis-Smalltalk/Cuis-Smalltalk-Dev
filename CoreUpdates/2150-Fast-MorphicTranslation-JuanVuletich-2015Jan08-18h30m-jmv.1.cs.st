'From Cuis 4.2 of 25 July 2013 [latest update: #2148] on 8 January 2015 at 6:32:37.068056 pm'!
!classDefinition: #MorphicTranslation category: #'Morphic-Kernel'!
Object subclass: #MorphicTranslation
	instanceVariableNames: 'deltaX deltaY'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!MorphicTranslation commentStamp: '<historical>' prior: 0!
Can replace a MatrixTransform2x3 when there is only a translation applied (no scale change, no rotation, no skew)!

!classDefinition: #MatrixTransform2x3Test category: #'Morphic-Kernel tests'!
TestCase subclass: #MatrixTransform2x3Test
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel tests'!

!MatrixTransform2x3Test commentStamp: '<historical>' prior: 0!
Tests for MatrixTransform2x3!

!classDefinition: #MorphicLocationTest category: #'Morphic-Kernel tests'!
TestCase subclass: #MorphicLocationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel tests'!

!MorphicLocationTest commentStamp: '<historical>' prior: 0!
Tests for composition of MatrixTransform2x3 and MorphicTranslation!

!classDefinition: #MorphicTranslationTest category: #'Morphic-Kernel tests'!
TestCase subclass: #MorphicTranslationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel tests'!

!MorphicTranslationTest commentStamp: '<historical>' prior: 0!
Tests for MorphicTranslation!


!MatrixTransform2x3 methodsFor: 'composing' stamp: 'jmv 1/8/2015 10:42'!
innerComposedWithMatrixTransform2x3: aMatrixTransform2x3
	"Return the composition of the receiver and the transformation passed in.
	 We know the class of the argument (through double dispatching)"

	^ aMatrixTransform2x3 composedWith: self into: self class new! !

!MatrixTransform2x3 methodsFor: 'composing' stamp: 'jmv 1/8/2015 10:42'!
innerComposedWithTranslation: aMorphicTranslation
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^(MatrixTransform2x3 withTranslation: aMorphicTranslation translation) composedWith: self! !


!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 1/7/2015 20:56'!
externalizeDelta: aPoint
	"Externalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position.
	We keep scale of stuff."

	^aPoint! !

!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 8/28/2012 16:29'!
externalizePosition: aPoint
	"Answer coordinates for aPoint in the space we are in.
	 aPoint is expressed in us."

	^self transform: aPoint! !

!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 1/7/2015 20:55'!
externalizeScalar: aNumber
	"Externalize a distance (without a direction).
	 We keep scale of stuff."

	^aNumber! !

!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 1/7/2015 20:56'!
internalizeDelta: aPoint
	"Internalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position.
	We keep scale of stuff."

	^aPoint! !

!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 8/28/2012 16:29'!
internalizePosition: aPoint
	"Answer our coordinates for aPoint.
	 aPoint is expressed in the space we are in."

	^self inverseTransform: aPoint! !

!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 1/7/2015 20:56'!
internalizeScalar: aNumber
	"Internalize a distance (without a direction). 
	We keep scale of stuff."

	^aNumber! !

!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 1/7/2015 20:57'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint."

	| x y |
	x _ aPoint x - deltaX.
	y _ aPoint y - deltaY.
	^x @ y! !

!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 1/7/2015 20:57'!
transform: aPoint
	"Apply the direct transformation to aPoint."

	| x y |
	x _ aPoint x + deltaX.
	y _ aPoint y + deltaY.
	^x @ y! !

!MorphicTranslation methodsFor: 'accessing' stamp: 'jmv 1/6/2015 23:11'!
degrees
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ 0! !

!MorphicTranslation methodsFor: 'accessing' stamp: 'jmv 1/7/2015 20:59'!
inverseTransformation
	"Return the inverse transformation of the receiver."

	^MorphicTranslation withTranslation: self translation negated! !

!MorphicTranslation methodsFor: 'accessing' stamp: 'jmv 1/1/2015 22:34'!
radians
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ 0.0! !

!MorphicTranslation methodsFor: 'accessing' stamp: 'jmv 1/1/2015 22:34'!
scale
	"Answer the *scalar* scale applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^1! !

!MorphicTranslation methodsFor: 'accessing' stamp: 'jmv 1/6/2015 23:11'!
translation
	"Translation and position are the same.
	Use the word translation when thinking about coordinate transformation, but use
	the word position when thinking about morph locations"

	^deltaX @ deltaY ! !

!MorphicTranslation methodsFor: 'comparing' stamp: 'jmv 1/1/2015 22:32'!
= aMorphicTranslation 
	^self translation = aMorphicTranslation translation! !

!MorphicTranslation methodsFor: 'comparing' stamp: 'jmv 1/1/2015 22:32'!
hash
	^self translation hash! !

!MorphicTranslation methodsFor: 'composing' stamp: 'jmv 1/8/2015 10:44'!
composedWith: innerTransformation
	"Return the composition of the receiver and the transformation passed in.
	The result is a translation  that has the following effect:
		self externalize: (innerTransformation externalize: aPoint)
		innerTransformation internalize: (self internalize: aPoint)."

	^innerTransformation innerComposedWithTranslation: self! !

!MorphicTranslation methodsFor: 'composing' stamp: 'jmv 1/7/2015 22:33'!
composedWith: aTransformation into: result
	"Return the composition of the receiver and the transformation passed in.
	Store the composed matrix into result.
	Please see the comment at: #composedWith:"

	result setTranslation: self translation + aTransformation translation! !

!MorphicTranslation methodsFor: 'composing' stamp: 'jmv 1/8/2015 10:50'!
innerComposedWithMatrixTransform2x3: aMatrixTransform2x3
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^aMatrixTransform2x3 composedWith: (MatrixTransform2x3 withTranslation: self translation)
" 	^ aMatrixTransform2x3 copy withAddedOffset: self translation" ! !

!MorphicTranslation methodsFor: 'composing' stamp: 'jmv 1/8/2015 10:42'!
innerComposedWithTranslation: aMorphicTranslation
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^ self copy withAddedOffset: aMorphicTranslation translation! !

!MorphicTranslation methodsFor: 'printing' stamp: 'jmv 1/6/2015 23:14'!
printOn: aStream
	"Note:
	Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."
	aStream
		nextPutAll: self class name;
		nextPutAll: '(translation '.
	self translation printOn: aStream.
	aStream nextPutAll: ') '! !

!MorphicTranslation methodsFor: 'testing' stamp: 'jmv 1/6/2015 22:46'!
doesNotRotate
	"Answer true if #radians would answer 0.0. Performance optimization."

	^true! !

!MorphicTranslation methodsFor: 'testing' stamp: 'jmv 1/6/2015 22:55'!
isIdentity
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."

	^deltaX = 0 and: [deltaY = 0]! !

!MorphicTranslation methodsFor: 'testing' stamp: 'jmv 1/6/2015 22:48'!
isPureTranslation
	"Return true if the receiver specifies no rotation or scaling."
	^true! !

!MorphicTranslation methodsFor: 'testing' stamp: 'jmv 1/6/2015 23:03'!
isTranslation: aPoint
	"The position of 0@0 in the external coordinate system.
	It is the translation we apply when transforming points."
	^deltaX = aPoint x and: [ deltaY = aPoint y ]! !

!MorphicTranslation methodsFor: 'private' stamp: 'jmv 1/7/2015 07:56'!
setTranslation: aPoint
	"private. Set the raw offset in the receiver"

	| pt |
	pt _ aPoint asPoint.
	deltaX _ pt x .
	deltaY _ pt y! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:40'!
italizing
	"a little shear
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a MatrixTransform2x3."

	^(MatrixTransform2x3 withTranslation: self translation) italizing! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:40'!
rotatedBy: radians
	"rotate the receiver by radians angle.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a MatrixTransform2x3."

	^(MatrixTransform2x3 withTranslation: self translation) rotatedBy: radians! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:41'!
scaledBy: aPoint
	"Multiply by a scale.
	Argument can be a point, applying different scaling in x and in y directions.
	Keep the transformed position of 0@0, i.e. don't change offset.

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a MatrixTransform2x3."

	^(MatrixTransform2x3 withTranslation: self translation) scaledBy: aPoint! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:41'!
scaledByNumber: aNumber rotateBy: radians
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling.

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a MatrixTransform2x3."

	^(MatrixTransform2x3 withTranslation: self translation) scaledByNumber: aNumber rotateBy: radians! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/7/2015 08:21'!
withAddedOffset: aPoint
	"add an offset in the receiver

	Answer the modified object. I this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or sch) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."
	| pt |
	pt _ aPoint asPoint.
	deltaX _ deltaX + pt x.
	deltaY _ deltaY + pt y.
	^self! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/7/2015 08:25'!
withTranslation: aPoint
	"set an offset in the receiver

	Answer the modified object. I this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or sch) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	deltaX _ pt x.
	deltaY _ pt y.
	^self! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/7/2015 09:42'!
withYAxisNegated
	"Swap inneer point Y sign.
	Make y increment upwards.
	This makes the any matrix transform from standard mathematical coordinates
	to standard display coordinates (in addition to the transform it was already doing)

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a MatrixTransform2x3."

	^(MatrixTransform2x3 withTranslation: self translation) withYAxisNegated! !

!MorphicTranslation methodsFor: 'transforming rects' stamp: 'jmv 1/8/2015 10:57'!
displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
	and vertical bounds and integer coordinates (i.e. adisplayBounds)."

	^aRectangle translatedBy: self translation! !

!MorphicTranslation methodsFor: 'private - primitives' stamp: 'jmv 1/7/2015 21:34'!
intTransform: aPoint
	"Apply the direct transformation to aPoint
	Round the result

	MorphicTranslation identity intTransform: (-2@ 2)
	(MorphicTranslation withTranslation: 2)  intTransform: (-4@ 2)
	(MorphicTranslation withTranslation: 4)  intTransform: (-2@ 2)
	(MorphicTranslation withTranslation: -4)  intTransform: (2@ 2)
	(MorphicTranslation withTranslation: -2)  intTransform: (4@ 2)
	"
	^(self transform: aPoint) rounded! !

!MorphicTranslation methodsFor: 'initialization' stamp: 'jmv 1/6/2015 23:22'!
initialize
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	deltaX _ 0.
	deltaY _ 0! !


!MorphicTranslation class methodsFor: 'instance creation' stamp: 'jmv 1/6/2015 23:21'!
identity
	^self new! !

!MorphicTranslation class methodsFor: 'instance creation' stamp: 'jmv 1/7/2015 09:32'!
withPosition: aPoint

	^self new setTranslation: aPoint! !

!MorphicTranslation class methodsFor: 'instance creation' stamp: 'jmv 1/7/2015 09:32'!
withTranslation: aPoint

	^self new setTranslation: aPoint! !


!MatrixTransform2x3Test methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:11'!
testComposition
	"
	MatrixTransform2x3Test new testComposition
	"
	| composition inner outer |

	outer _ MatrixTransform2x3 withPosition: 3@5.
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
	inner _ MatrixTransform2x3 withPosition: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !

!MatrixTransform2x3Test methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:17'!
testDisplayBounds
	"
	MatrixTransform2x3Test new testDisplayBounds
	"
	self assert:
		((MatrixTransform2x3 withRadians: 0.3) displayBoundsOfTransformOf: (10@10 extent: 20@30))
			= (-2@13 corner: 26@47).

	self assert: (MatrixTransform2x3 new displayBoundsOfTransformOf: (-2@ 2 extent: 10@10)) = (-2@2 corner: 8@12).
	self assert: (MatrixTransform2x3 new displayBoundsOfTransformOf: (-12@ 12 extent: 10@10)) = (-12@12 corner: -2@22).
	self assert: ((MatrixTransform2x3 withTranslation: 2)  displayBoundsOfTransformOf: (-4@ 2  extent: 10@10)) = (-2@4 corner: 8@14).
	self assert: ((MatrixTransform2x3 withTranslation: -4)  displayBoundsOfTransformOf: (2@ 2  extent: 10@10)) = (-2@-2 corner: 8@8).
	self assert: ((MatrixTransform2x3 withTranslation: 2)  displayBoundsOfTransformOf: (-14@ 2  extent: 10@10)) = (-12@4 corner: -2@14).
	self assert: ((MatrixTransform2x3 withTranslation: 4)  displayBoundsOfTransformOf: (-12@ 2  extent: 10@10)) = (-8@6 corner: 2@16).
	self assert: ((MatrixTransform2x3 withTranslation: -4)  displayBoundsOfTransformOf: (12@ 2  extent: 10@10)) = (8@-2 corner: 18@8).
	self assert: ((MatrixTransform2x3 withTranslation: -2)  displayBoundsOfTransformOf: (4@ 2  extent: 10@10)) = (2@0 corner: 12@10).
	self assert: ((MatrixTransform2x3 withTranslation: 4)  displayBoundsOfTransformOf: (-2@ 2  extent: 10@10)) = (2@6 corner: 12@16).! !

!MatrixTransform2x3Test methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:11'!
testFloatInverseTransform
	"
	MatrixTransform2x3Test new testFloatInverseTransform
	"
	self assert: (MatrixTransform2x3 new floatInverseTransform: (-2@ 2)) = (-2@2).
	self assert: ((MatrixTransform2x3 withTranslation: 2)  floatInverseTransform: (-4@ 2)) = (-6@0).
	self assert: ((MatrixTransform2x3 withTranslation: 4)  floatInverseTransform: (-2@ 2)) = (-6@-2).
	self assert: ((MatrixTransform2x3 withTranslation: -2)  floatInverseTransform: (4@ 2)) = (6@4).
	self assert: ((MatrixTransform2x3 withTranslation: -4)  floatInverseTransform: (2@ 2)) = (6@6)! !

!MatrixTransform2x3Test methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:11'!
testFloatTransform
	"
	MatrixTransform2x3Test new testFloatTransform
	"
	self assert: (MatrixTransform2x3 new floatTransform: (-2@ 2)) = (-2@2).
	self assert: ((MatrixTransform2x3 withTranslation: 2)  floatTransform: (-4@ 2)) = (-2@4).
	self assert: ((MatrixTransform2x3 withTranslation: 4)  floatTransform: (-2@ 2)) = (2@6).
	self assert: ((MatrixTransform2x3 withTranslation: -2)  floatTransform: (4@ 2)) = (2@0).
	self assert: ((MatrixTransform2x3 withTranslation: -4)  floatTransform: (2@ 2)) = (-2@-2)! !

!MatrixTransform2x3Test methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:11'!
testIntInverseTransform
	"
	MatrixTransform2x3Test new testIntInverseTransform
	"
	self assert: (MatrixTransform2x3 new intInverseTransform: (-2@ 2)) = (-2@2).
	self assert: ((MatrixTransform2x3 withTranslation: 2)  intInverseTransform: (-4@ 2)) = (-6@0).
	self assert: ((MatrixTransform2x3 withTranslation: 4)  intInverseTransform: (-2@ 2)) = (-6@-2).
	self assert: ((MatrixTransform2x3 withTranslation: -2)  intInverseTransform: (4@ 2)) = (6@4).
	self assert: ((MatrixTransform2x3 withTranslation: -4)  intInverseTransform: (2@ 2)) = (6@6)! !

!MatrixTransform2x3Test methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:11'!
testIntTransform
	"
	MatrixTransform2x3Test new testIntTransform
	"
	self assert: (MatrixTransform2x3 new intTransform: (-2@ 2)) = (-2@2).
	self assert: ((MatrixTransform2x3 withTranslation: 2)  intTransform: (-4@ 2)) = (-2@4).
	self assert: ((MatrixTransform2x3 withTranslation: 4)  intTransform: (-2@ 2)) = (2@6).
	self assert: ((MatrixTransform2x3 withTranslation: -2)  intTransform: (4@ 2)) = (2@0).
	self assert: ((MatrixTransform2x3 withTranslation: -4)  intTransform: (2@ 2)) = (-2@-2)! !

!MatrixTransform2x3Test methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:11'!
testInverseTransformation
	"
	MatrixTransform2x3Test new testInverseTransformation
	"
	| forward inverse |

	forward _ MatrixTransform2x3 withPosition: 3@5.
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
	self assert: (inverse externalizePosition: 3@4) = (forward internalizePosition: 3@4).
	self assert: ((inverse externalizeDelta: 3@4) - (forward internalizeDelta: 3@4)) r < 0.0001.
	self assert: ((inverse externalizeScalar: 7) - (forward internalizeScalar: 7)) abs < 0.0001.
	self assert: (inverse internalizePosition: 3@4) = (forward externalizePosition: 3@4).
	self assert: ((inverse internalizeDelta: 3@4) - (forward externalizeDelta: 3@4)) r < 0.0001.
	self assert: ((inverse internalizeScalar: 7) - (forward externalizeScalar: 7)) abs < 0.0001.! !


!MorphicLocationTest methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:25'!
testComposition
	"
	MorphicLocationTest new testComposition
	"
	| composition inner outer |

	outer _ MorphicTranslation withPosition: 3@5.
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
	inner _ MorphicTranslation withPosition: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !


!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:13'!
testComposition
	"
	MorphicTranslationTest new testComposition
	"
	| composition inner outer |

	outer _ MorphicTranslation withPosition: 3@5.
	inner _ MorphicTranslation withPosition: -1@2.
	composition _ outer composedWith: inner.
	self assert: composition translation = (outer translation + inner translation).
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).


	outer _ MorphicTranslation withPosition: -1@2.
	inner _ MorphicTranslation withPosition: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:19'!
testDisplayBounds
	"
	MorphicTranslationTest new testDisplayBounds
	"

	self assert: (MorphicTranslation new displayBoundsOfTransformOf: (-2@ 2 extent: 10@10)) = (-2@2 corner: 8@12).
	self assert: (MorphicTranslation new displayBoundsOfTransformOf: (-12@ 12 extent: 10@10)) = (-12@12 corner: -2@22).
	self assert: ((MorphicTranslation withTranslation: 2)  displayBoundsOfTransformOf: (-4@ 2  extent: 10@10)) = (-2@4 corner: 8@14).
	self assert: ((MorphicTranslation withTranslation: -4)  displayBoundsOfTransformOf: (2@ 2  extent: 10@10)) = (-2@-2 corner: 8@8).
	self assert: ((MorphicTranslation withTranslation: 2)  displayBoundsOfTransformOf: (-14@ 2  extent: 10@10)) = (-12@4 corner: -2@14).
	self assert: ((MorphicTranslation withTranslation: 4)  displayBoundsOfTransformOf: (-12@ 2  extent: 10@10)) = (-8@6 corner: 2@16).
	self assert: ((MorphicTranslation withTranslation: -4)  displayBoundsOfTransformOf: (12@ 2  extent: 10@10)) = (8@-2 corner: 18@8).
	self assert: ((MorphicTranslation withTranslation: -2)  displayBoundsOfTransformOf: (4@ 2  extent: 10@10)) = (2@0 corner: 12@10).
	self assert: ((MorphicTranslation withTranslation: 4)  displayBoundsOfTransformOf: (-2@ 2  extent: 10@10)) = (2@6 corner: 12@16).! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:20'!
testIntTransform
	"
	MorphicTranslationTest new testIntTransform
	"
	self assert: (MorphicTranslation new intTransform: (-2@ 2)) = (-2@2).
	self assert: ((MorphicTranslation withTranslation: 2)  intTransform: (-4@ 2)) = (-2@4).
	self assert: ((MorphicTranslation withTranslation: 4)  intTransform: (-2@ 2)) = (2@6).
	self assert: ((MorphicTranslation withTranslation: -2)  intTransform: (4@ 2)) = (2@0).
	self assert: ((MorphicTranslation withTranslation: -4)  intTransform: (2@ 2)) = (-2@-2)! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:21'!
testInverseTransform
	"
	MorphicTranslationTest new testInverseTransform
	"
	self assert: (MorphicTranslation new inverseTransform: (-2@ 2)) = (-2@2).
	self assert: ((MorphicTranslation withTranslation: 2)  inverseTransform: (-4@ 2)) = (-6@0).
	self assert: ((MorphicTranslation withTranslation: 4)  inverseTransform: (-2@ 2)) = (-6@-2).
	self assert: ((MorphicTranslation withTranslation: -2)  inverseTransform: (4@ 2)) = (6@4).
	self assert: ((MorphicTranslation withTranslation: -4)  inverseTransform: (2@ 2)) = (6@6)! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:43'!
testInverseTransformation
	"
	MorphicTranslationTest new testInverseTransformation
	"
	| forward inverse |

	forward _ MorphicTranslation withPosition: 3@5.
	inverse _ forward inverseTransformation.
	
	self assert: inverse translation = forward translation negated.
	self assert: (inverse externalizePosition: 3@4) = (forward internalizePosition: 3@4).
	self assert: (inverse externalizeDelta: 3@4) = (forward internalizeDelta: 3@4).
	self assert: (inverse externalizeScalar: 7) = (forward internalizeScalar: 7).
	self assert: (inverse internalizePosition: 3@4) = (forward externalizePosition: 3@4).
	self assert: (inverse internalizeDelta: 3@4) = (forward externalizeDelta: 3@4).
	self assert: (inverse internalizeScalar: 7) = (forward externalizeScalar: 7)! !

!MorphicTranslationTest methodsFor: 'testing' stamp: 'jmv 1/8/2015 10:22'!
testTransform
	"
	MorphicTranslationTest new testTransform
	"
	self assert: (MorphicTranslation new transform: (-2@ 2)) = (-2@2).
	self assert: ((MorphicTranslation withTranslation: 2)  transform: (-4@ 2)) = (-2@4).
	self assert: ((MorphicTranslation withTranslation: 4)  transform: (-2@ 2)) = (2@6).
	self assert: ((MorphicTranslation withTranslation: -2)  transform: (4@ 2)) = (2@0).
	self assert: ((MorphicTranslation withTranslation: -4)  transform: (2@ 2)) = (-2@-2)! !


!MatrixTransform2x3 methodsFor: 'composing' stamp: 'jmv 1/8/2015 10:49'!
composedWith: innerTransformation
	"Return the composition of the receiver and the transformation passed in.
	The result is a translation  that has the following effect:
		self externalize: (innerTransformation externalize: aPoint)
		innerTransformation internalize: (self internalize: aPoint)."

 	^innerTransformation innerComposedWithMatrixTransform2x3: self! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 1/8/2015 09:48'!
displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
	and vertical bounds and integer coordinates (i.e. adisplayBounds)."

	^self displayBoundsOfTransformOf: aRectangle into: Rectangle new! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 1/8/2015 09:52'!
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
	result _ Rectangle encompassing: (srcRect corners collect: [ :pt | self intTransform: pt ]).
	^dstRect setOrigin: result origin corner: result corner! !

!MatrixTransform2x3 methodsFor: 'transforming rects' stamp: 'jmv 1/8/2015 09:53'!
primDisplayBoundsOfInverseTransformOf: srcRect into: dstRect
	"Internalize srcRect, and find a bounding rectangle with horizontal and vertical bounds (in the inner space) and 
	integer coordinates (i.e. adisplayBounds). Store result into dstRect.

	Unused. Just kept around to document the available primitives in Matrix2x3Plugin"

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

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 1/8/2015 09:56'!
intInverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer is not strictly positive, it is off by one. 
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here."

	^(self fixRoundingInPrimitives: (self primInverseTransform: aPoint))
		ifNil: [ (self floatInverseTransform: aPoint) rounded ]! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 1/8/2015 10:00'!
intTransform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here."

	^(self fixRoundingInPrimitives: (self primTransform: aPoint))
		ifNil: [ (self floatTransform: aPoint) rounded ]! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 1/8/2015 10:08'!
primInverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	MatrixTransform2x3 new primInverseTransform: (-2@ 2)
	(MatrixTransform2x3 withTranslation: 2)  primInverseTransform: (-4@ 2)
	(MatrixTransform2x3 withTranslation: 4)  primInverseTransform: (-2@ 2)
	
	These are Ok (answer is positive)
	(MatrixTransform2x3 withTranslation: -2)  primInverseTransform: (4@ 2)
	(MatrixTransform2x3 withTranslation: -4)  primInverseTransform: (2@ 2)
	"

	<primitive: 'primitiveInvertPoint' module: 'Matrix2x3Plugin'>
	^nil! !

!methodRemoval: MatrixTransform2x3 #displayBoundsOfInverseTransformOf:!
MatrixTransform2x3 removeSelector: #displayBoundsOfInverseTransformOf:!
!methodRemoval: MatrixTransform2x3 #displayBoundsOfInverseTransformOf:into:!
MatrixTransform2x3 removeSelector: #displayBoundsOfInverseTransformOf:into:!
