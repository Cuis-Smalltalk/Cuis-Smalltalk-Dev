"
Can replace an AffineTransformation when there is only a translation applied (no scale change, no rotation, no skew).

Note: Methods that modify an instance answer the result. The result might be the receiver itself, or it might be a new instance. Therefore:
- Don't assume the receiver will be modified. Always take the result.
- Don't assume the answer will be a new instance. The receiver might be modified! (make a copy if needed)

For consistency with AffineTransformation, always answer Float coordinates (Fraction coordinates are not handled by BitBlt or VectorEngine).
"
Class {
	#name : 'MorphicTranslation',
	#superclass : 'GeometryTransformation',
	#instVars : [
		'deltaX',
		'deltaY'
	],
	#category : 'Graphics-Primitives',
	#package : 'Graphics',
	#tag : 'Primitives'
}

{ #category : 'instance creation' }
MorphicTranslation class >> withTranslation: aPoint [

	^self new setTranslation: aPoint
]

{ #category : 'comparing' }
MorphicTranslation >> = aMorphicTranslation [
	self == aMorphicTranslation ifTrue: [ ^ true ].
	(aMorphicTranslation is: #GeometryTransformation) ifFalse: [ ^false ].
	aMorphicTranslation isPureTranslation ifFalse: [ ^false ].
	^self translation = aMorphicTranslation translation
]

{ #category : 'polymorphism with AffineTransformation' }
MorphicTranslation >> a11 [
	^1.0
]

{ #category : 'polymorphism with AffineTransformation' }
MorphicTranslation >> a12 [
	^0.0
]

{ #category : 'polymorphism with AffineTransformation' }
MorphicTranslation >> a13 [
	^deltaX
]

{ #category : 'polymorphism with AffineTransformation' }
MorphicTranslation >> a21 [
	^0.0
]

{ #category : 'polymorphism with AffineTransformation' }
MorphicTranslation >> a22 [
	^1.0
]

{ #category : 'polymorphism with AffineTransformation' }
MorphicTranslation >> a23 [
	^deltaY
]

{ #category : 'transforming rects' }
MorphicTranslation >> boundsOfInverseTransformOf: aRectangle [
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
	and vertical bounds and integer coordinates (i.e. adisplayBounds)."

	^aRectangle translatedBy: self translation negated
]

{ #category : 'composing' }
MorphicTranslation >> composedWith: innerTransformation [
	"Return the composition of the receiver and the transformation passed in.
	The result is a transformation whose application (transform:) is equivalent
	to first applying innerTransformation and then self.
	In other words
		self externalize: (innerTransformation externalize: aPoint)
		innerTransformation internalize: (self internalize: aPoint)."

	^innerTransformation innerComposedWithTranslation: self
]

{ #category : 'composing' }
MorphicTranslation >> composedWith: aTransformation into: result [
	"Return the composition of the receiver and the transformation passed in.
	Store the composed matrix into result.
	Please see the comment at: #composedWith:"

	result setTranslation: self translation + aTransformation translation.
	^ result
]

{ #category : 'accessing' }
MorphicTranslation >> degrees [
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ 0
]

{ #category : 'testing' }
MorphicTranslation >> doesNotRotate [
	"Answer true if #radians would answer 0.0. Performance optimization."

	^true
]

{ #category : 'transforming rects' }
MorphicTranslation >> externalBoundingRectOf: aRectangle [
	"Externalize aRectangle, and find a bounding rectangle with horizontal and vertical bounds"

	^ aRectangle translatedBy: self translation.
]

{ #category : 'converting coordinates' }
MorphicTranslation >> externalizeDelta: aPoint [
	"Externalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position.
	We keep scale of stuff."

	^aPoint asFloatPoint
]

{ #category : 'converting coordinates' }
MorphicTranslation >> externalizePosition: aPoint [
	"Answer coordinates for aPoint in the space we are in.
	 aPoint is expressed in us."

	^self transform: aPoint
]

{ #category : 'converting coordinates' }
MorphicTranslation >> externalizeScalar: aNumber [
	"Externalize a distance (without a direction).
	 We keep scale of stuff."

	^aNumber asFloat
]

{ #category : 'comparing' }
MorphicTranslation >> hash [
	^self translation hash
]

{ #category : 'initialization' }
MorphicTranslation >> initialize [
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	deltaX := 0.0.
	deltaY := 0.0
]

{ #category : 'composing' }
MorphicTranslation >> innerComposedWithAffineTransformation: anAffineTransformation [
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^anAffineTransformation composedWith: (AffineTransformation withTranslation: self translation)
" 	^ anAffineTransformation copy offsetBy: self translation" 
]

{ #category : 'composing' }
MorphicTranslation >> innerComposedWithTranslation: aMorphicTranslation [
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^ self copy translatedBy: aMorphicTranslation translation
]

{ #category : 'converting coordinates' }
MorphicTranslation >> internalizeDelta: aPoint [
	"Internalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position.
	We keep scale of stuff."

	^aPoint asFloatPoint
]

{ #category : 'converting coordinates' }
MorphicTranslation >> internalizePosition: aPoint [
	"Answer our coordinates for aPoint.
	 aPoint is expressed in the space we are in."

	^self inverseTransform: aPoint
]

{ #category : 'converting coordinates' }
MorphicTranslation >> internalizeScalar: aNumber [
	"Internalize a distance (without a direction). 
	We keep scale of stuff."

	^aNumber asFloat
]

{ #category : 'converting coordinates' }
MorphicTranslation >> inverseTransform: aPoint [
	"Apply the inverse transformation to aPoint."

	| x y |
	x := aPoint x - deltaX.
	y := aPoint y - deltaY.
	^x @ y
]

{ #category : 'accessing' }
MorphicTranslation >> inverseTransformation [
	"Return the inverse transformation of the receiver."

	^MorphicTranslation withTranslation: self translation negated
]

{ #category : 'testing' }
MorphicTranslation >> isIdentity [
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."

	^deltaX = 0 and: [deltaY = 0]
]

{ #category : 'testing' }
MorphicTranslation >> isPureTranslation [
	"Return true if the receiver specifies no rotation or scaling."
	^true
]

{ #category : 'testing' }
MorphicTranslation >> isTranslation: aPoint [
	"The position of 0@0 in the external coordinate system.
	It is the translation we apply when transforming points."
	^deltaX = aPoint x and: [ deltaY = aPoint y ]
]

{ #category : 'modifying' }
MorphicTranslation >> italizing [
	"a little shear
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) italizing
]

{ #category : 'modifying' }
MorphicTranslation >> italizing2 [
	"a little shear
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) italizing2
]

{ #category : 'printing' }
MorphicTranslation >> printOn: aStream [
	"Note:
	Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."
	aStream
		nextPutAll: self class name;
		nextPutAll: '(translation '.
	self translation printOn: aStream.
	aStream nextPutAll: ') '
]

{ #category : 'accessing' }
MorphicTranslation >> radians [
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ 0.0
]

{ #category : 'modifying' }
MorphicTranslation >> rotatedBy: radians [
	"rotate the receiver by radians angle.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) rotatedBy: radians
]

{ #category : 'accessing' }
MorphicTranslation >> scale [
	"Answer the *scalar* scale applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^1.0
]

{ #category : 'modifying' }
MorphicTranslation >> scaledBy: aPoint [
	"Multiply by a scale.
	Argument can be a point, applying different scaling in x and in y directions.
	Keep the transformed position of 0@0, i.e. don't change offset.

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) scaledBy: aPoint
]

{ #category : 'modifying' }
MorphicTranslation >> scaledByNumber: aNumber rotatedBy: radians [
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling.

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) scaledByNumber: aNumber rotatedBy: radians
]

{ #category : 'private' }
MorphicTranslation >> setTranslation: aPoint [
	"private. Set the raw offset in the receiver"

	| pt |
	pt := aPoint asPoint.
	deltaX := pt x asFloat.
	deltaY := pt y asFloat
]

{ #category : 'converting coordinates' }
MorphicTranslation >> transform: aPoint [
	"Apply the direct transformation to aPoint."

	| x y |
	x := aPoint x + deltaX.
	y := aPoint y + deltaY.
	^x @ y
]

{ #category : 'modifying' }
MorphicTranslation >> translatedBy: aPoint [
	"add an offset in the receiver

	Answer the modified object. I this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or such) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."
	| pt |
	pt := aPoint asPoint.
	deltaX := deltaX + pt x.
	deltaY := deltaY + pt y.
	^self
]

{ #category : 'accessing' }
MorphicTranslation >> translation [
	"Translation and position are the same.
	Use the word translation when thinking about coordinate transformation, but use
	the word position when thinking about morph locations"

	^deltaX @ deltaY 
]

{ #category : 'modifying' }
MorphicTranslation >> withCurrentYAxisInverted [
	"Answer an instance (either the receiver or a new one) that flips the current direction of the Y axis.
	This means that whatever we answer when externalizing x@y, it will answer when externalizing x @ -y.
	Senders should always use the returned object, but not assume it is a new one (like here):
		it could also be the receiver itself, like when the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) withCurrentYAxisInverted
]

{ #category : 'modifying' }
MorphicTranslation >> withRotation: radians scale: scale [
	"Set rotation and scaling according to parameters.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) withRotation: radians scale: scale
]

{ #category : 'modifying' }
MorphicTranslation >> withScale: scale [
	"Set scaling according to parameters.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already an AffineTransformation."

	^self scaledBy: scale
]

{ #category : 'modifying' }
MorphicTranslation >> withTranslation: aPoint [
	"set an offset in the receiver

	Answer the modified object. I this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or such) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt := aPoint asPoint.
	deltaX := pt x asFloat.
	deltaY := pt y asFloat.
	^self
]
