"
This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix. 

The direct transformation is equivalent as multiplying the matrix by a column vector (with an extra element of value 1). The inverse transformation is multiplying the inverse of the matrix by a column vector (with an extra element of value 1). By convention, we say that the direct transform is outwards (#externalizePosition:). Therefore, the inverse transform is called #internalizePosition: .

Direct transform (#externalizePosition:) is fast and cheap, while inverse transform (#internalizePosition:) is relatively expensive.

We can consider the matrix to be of 3x3 with an extra row with #( 0 0 1) at the bottom, especially for having a properly defined inverse matrix.

Implementation Note: In the original version, from Squeak, it is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here). In this version, both alternatives are available, with explicit protocols, and a global setting for the Squeak protocol.

Note: Methods that modify an instance answer the result. The result might be the receiver itself, or it might be a new instance. Therefore:
- Don't assume the receiver will be modified. Always take the result.
- Don't assume the answer will be a new instance. The receiver might be modified! (make a copy if needed)
"
Class {
	#name : 'AffineTransformation',
	#superclass : 'GeometryTransformation',
	#category : 'Graphics-Primitives',
	#package : 'Graphics',
	#tag : 'Primitives'
}

{ #category : 'aux' }
AffineTransformation class >> bytesPerBasicElement [
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self class bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	self isBytes ifTrue: [ ^1 ].
	self isDoubleBytes ifTrue: [ ^2 ].
	self isWords ifTrue: [ ^4 ].
	self isDoubleWords ifTrue: [ ^8 ].
	^Smalltalk wordSize.

]

{ #category : 'instance creation' }
AffineTransformation class >> from: aNumericArray [
	"For example:
	
	AffineTransformation from:
		#[ 	  0.85 	0.04 	0.00
			-0.04 	0.85 	1.60 ].
	"
	^self newFromStream: aNumericArray readStream
]

{ #category : 'instance creation' }
AffineTransformation class >> new [
	^(self basicNew: 6) initialize
]

{ #category : 'instance creation' }
AffineTransformation class >> new: s [
	self error: 'Please call AffineTransformation>>#new (without arguments).'.
	^self new
]

{ #category : 'instance creation' }
AffineTransformation class >> newFromStream: s [
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	^ s nextWordsInto: self new
]

{ #category : 'instance creation' }
AffineTransformation class >> transformFrom: originalBounds to: resultBounds [
	"Answer a Transform to translate coordinates inside originalBounds into coordinates inside resultBounds.
	Parameters are instances of Rectangle. Therefore, there's no rotation involved, just scale and offset."

	^((self withTranslation: (resultBounds topLeft + resultBounds bottomRight / 2.0)) composedWith:
		(self withPointScale: (resultBounds extent / originalBounds extent) asFloatPoint)) composedWith:
			(self withTranslation: (originalBounds topLeft + originalBounds bottomRight / 2.0) negated)
]

{ #category : 'instance creation' }
AffineTransformation class >> withPointScale: aPoint [
	"
	(AffineTransformation withPointScale: 4@3) transform: 1@1
	"
	^self new setPointScale: aPoint
]

{ #category : 'instance creation' }
AffineTransformation class >> withPointScale: aPoint position: otherPoint [
	"
	(AffineTransformation withPointScale: 4@3 position: 0.2@0.7) transform: 1@1
	"
	^self new
		setPointScale: aPoint;
		setTranslation: otherPoint
]

{ #category : 'instance creation' }
AffineTransformation class >> withRadians: radians [
	^self new setRadians: radians scale: 1.0
]

{ #category : 'instance creation' }
AffineTransformation class >> withRadians: radians around: rotationCenter [

	^(AffineTransformation withTranslation: rotationCenter) composedWith:
		((AffineTransformation withRadians: radians) composedWith:
			(AffineTransformation withTranslation: rotationCenter negated)).
]

{ #category : 'instance creation' }
AffineTransformation class >> withRadians: radians scale: aNumber position: aPoint [
	"Translation is added at the end. This means that aPoint is in the outer coordinate space.
	AffineTransformation withRadians: -3 scale: 12 position: 4.5@3
	"
	^self new
		setRadians: radians scale: aNumber;
		setTranslation: aPoint
]

{ #category : 'instance creation' }
AffineTransformation class >> withScale: aNumber [
	^self new setPointScale: aNumber@aNumber
]

{ #category : 'instance creation' }
AffineTransformation class >> withScale: aNumber position: otherPoint [
	"
	(AffineTransformation withScale: 4 position: 0.2@0.7) transform: 1@1
	"
	^self new
		setPointScale: aNumber@aNumber;
		setTranslation: otherPoint
]

{ #category : 'instance creation' }
AffineTransformation class >> withTranslation: aPoint [
	"scale is one, angle is zero"

	^self new setTranslation: aPoint
]

{ #category : 'comparing' }
AffineTransformation >> = anAffineTransformation  [
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	self == anAffineTransformation ifTrue: [ ^ true ].
	self class == anAffineTransformation class ifFalse: [^ false].
	length := self size.
	length = anAffineTransformation size ifFalse: [^ false].
	1 to: self size do: [:i | (self at: i)
			= (anAffineTransformation at: i) ifFalse: [^ false]].
	^ true
]

{ #category : 'element access' }
AffineTransformation >> a11 [
	^self at: 1
]

{ #category : 'element access' }
AffineTransformation >> a11: value [
	 self at: 1 put: value
]

{ #category : 'element access' }
AffineTransformation >> a12 [
	^self at: 2
]

{ #category : 'element access' }
AffineTransformation >> a12: value [
	 self at: 2 put: value
]

{ #category : 'element access' }
AffineTransformation >> a13 [
	^self at: 3
]

{ #category : 'element access' }
AffineTransformation >> a13: value [
	 self at: 3 put: value
]

{ #category : 'element access' }
AffineTransformation >> a21 [
	 ^self at: 4
]

{ #category : 'element access' }
AffineTransformation >> a21: value [
	 self at: 4 put: value
]

{ #category : 'element access' }
AffineTransformation >> a22 [
	 ^self at: 5
]

{ #category : 'element access' }
AffineTransformation >> a22: value [
	 self at: 5 put: value
]

{ #category : 'element access' }
AffineTransformation >> a23 [
	 ^self at: 6
]

{ #category : 'element access' }
AffineTransformation >> a23: value [
	 self at: 6 put: value
]

{ #category : 'accessing' }
AffineTransformation >> at: index [
	"Used to be:
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>"
	<primitive: 238 error: ec>
	^Float fromIEEE32Bit: (self basicAt: index)
]

{ #category : 'accessing' }
AffineTransformation >> at: index put: value [
	"Used to be:
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>"
	<primitive: 239 error: ec>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value
]

{ #category : 'transforming rects' }
AffineTransformation >> boundsOfInverseTransformOf: aRectangle [
	"Internalize aRectangle, and find a bounding rectangle with horizontal 
		and vertical bounds"

	^Rectangle encompassing: (aRectangle corners collect: [ :pt |
		self inverseTransform: pt ])
]

{ #category : 'objects from disk' }
AffineTransformation >> byteSize [
	^self basicSize * self class bytesPerBasicElement
]

{ #category : 'composing' }
AffineTransformation >> composedWith: innerTransformation [
	"Return the composition of the receiver and the transformation passed in.
	The result is a transformation whose application (transform:) is equivalent
	to first applying innerTransformation and then self.
	In other words
		self externalize: (innerTransformation externalize: aPoint)
		innerTransformation internalize: (self internalize: aPoint)."

 	^innerTransformation innerComposedWithAffineTransformation: self
]

{ #category : 'composing' }
AffineTransformation >> composedWith: aTransformation into: result [
	"Return the composition of the receiver and the transformation passed in.
	Store the composed matrix into result.
	Please see the comment at: #composedWith:"

	| a11 a12 a13 a21 a22 a23 b11 b12 b13 b21 b22 b23 matrix |
	<primitive: 'primitiveComposeMatrix' module: 'Matrix2x3Plugin'>
	matrix := aTransformation.
	a11 := self a11.		b11 := matrix a11.
	a12 := self a12.		b12 := matrix a12.
	a13 := self a13.		b13 := matrix a13.
	a21 := self a21.		b21 := matrix a21.
	a22 := self a22.		b22 := matrix a22.
	a23 := self a23.		b23 := matrix a23.
	result a11: (a11 * b11) + (a12 * b21).
	result a12: (a11 * b12) + (a12 * b22).
	result a13: a13 + (a11 * b13) + (a12 * b23).
	result a21: (a21 * b11) + (a22 * b21).
	result a22: (a21 * b12) + (a22 * b22).
	result a23: a23 + (a21 * b13) + (a22 * b23).
	^result
]

{ #category : 'inspecting' }
AffineTransformation >> customizeExplorerContents [

	^ true
]

{ #category : 'accessing' }
AffineTransformation >> degrees [
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ self radians radiansToDegrees
]

{ #category : 'testing' }
AffineTransformation >> doesMirror [
	"Return true if the receiver mirrors points around some rect.
	Usually this is interpreted as (and used for) inverting the direction of the Y axis between the inner and the outer coordinates systems."

	| f |
	f := self a11 * self a22.
	^ f = 0.0
		ifTrue: [ self a12 * self a21 > 0.0]
		ifFalse: [ f < 0.0 ]
]

{ #category : 'testing' }
AffineTransformation >> doesNotRotate [
	"Answer true if #radians would answer 0.0. Performance optimization."

	^self a21 = 0.0 and: [ self a11 sign >= 0 ]
]

{ #category : 'inspecting' }
AffineTransformation >> explorerContents [

	^{
		ObjectExplorerWrapper
			with: self scale
			name: 'scale'
			model: self. 
		ObjectExplorerWrapper
			with: self degrees
			name: 'degrees'
			model: self. 
		ObjectExplorerWrapper
			with: self translation
			name: 'translation'
			model: self }
]

{ #category : 'transforming rects' }
AffineTransformation >> externalBoundingRectOf: aRectangle [
	"Externalize aRectangle, and find a bounding rectangle with horizontal and vertical bounds"

	^Rectangle encompassing: 
		(aRectangle corners
			collect: [ :pt | self transform: pt ])
]

{ #category : 'converting coordinates' }
AffineTransformation >> externalizeDelta: aPoint [
	"Externalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y |
	x := (aPoint x * self a11) + (aPoint y * self a12).
	y := (aPoint x * self a21) + (aPoint y * self a22).
	^x @ y
]

{ #category : 'converting coordinates' }
AffineTransformation >> externalizePosition: aPoint [
	"Answer coordinates for aPoint in the space we are in.
	 aPoint is expressed in us."

	^self transform: aPoint
]

{ #category : 'converting coordinates' }
AffineTransformation >> externalizeScalar: aNumber [
	"Externalize a distance (without a direction).
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber * self scale
]

{ #category : 'comparing' }
AffineTransformation >> hash [
	| result |
	<primitive: 'primitiveHashArray' module: 'FloatArrayPlugin'>
	result := 0.
	1 to: self size do:[:i| result := result + (self basicAt: i) ].
	^result bitAnd: 16r1FFFFFFF
]

{ #category : 'initialization' }
AffineTransformation >> initialize [
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	self
		a11: 1.0;
		a22: 1.0
]

{ #category : 'composing' }
AffineTransformation >> innerComposedWithAffineTransformation: anAffineTransformation [
	"Return the composition of the receiver and the transformation passed in.
	 We know the class of the argument (through double dispatching)"

	^ anAffineTransformation composedWith: self into: self class new
]

{ #category : 'composing' }
AffineTransformation >> innerComposedWithTranslation: aMorphicTranslation [
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^(AffineTransformation withTranslation: aMorphicTranslation translation) composedWith: self
]

{ #category : 'inspecting' }
AffineTransformation >> inspectorClass  [
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^SequenceableCollectionInspector
]

{ #category : 'converting coordinates' }
AffineTransformation >> internalizeDelta: aPoint [
	"Internalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y det a11 a12 a21 a22 detX detY |
	x := aPoint x.
	y := aPoint y.
	a11 := self a11.
	a12 := self a12.
	a21 := self a21.
	a22 := self a22.
	det := (a11 * a22) - (a12 * a21).
	det = 0.0 ifTrue: [ ^`0@0` ].		"So we have at least a valid result"
	det := 1.0 / det.
	detX := (x * a22) - (a12 * y).
	detY := (a11 * y) - (x * a21).
	^ (detX * det) @ (detY * det)
]

{ #category : 'converting coordinates' }
AffineTransformation >> internalizePosition: aPoint [
	"Answer our coordinates for aPoint.
	 aPoint is expressed in the space we are in."

	^self inverseTransform: aPoint
]

{ #category : 'converting coordinates' }
AffineTransformation >> internalizeScalar: aNumber [
	"Internalize a distance (without a direction). 
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber / self scale
]

{ #category : 'converting coordinates' }
AffineTransformation >> inverseTransform: aPoint [
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Use Smalltalk code, and not Matrix2x3Plugin, because we want Float conversion."
	| x y det a11 a12 a21 a22 detX detY |

	x := aPoint x - self a13.
	y := aPoint y - self a23.
	a11 := self a11.
	a12 := self a12.
	a21 := self a21.
	a22 := self a22.
	det := (a11 * a22) - (a12 * a21).
	det = 0.0 ifTrue: [ ^`0@0` ].		"So we have at least a valid result"
	det := 1.0 / det.
	detX := (x * a22) - (a12 * y).
	detY := (a11 * y) - (x * a21).
	^ (detX * det) @ (detY * det)
]

{ #category : 'accessing' }
AffineTransformation >> inverseTransformation [
	"Return the inverse transformation of the receiver.
	The inverse transformation is computed by first calculating
	the inverse offset and then computing transformations
	for the two identity vectors (1@0) and (0@1)"
	| r1 r2 r3 m |
	r3 := self inverseTransform: `0@0`.
	r1 := (self inverseTransform: `1@0`) - r3.
	r2 := (self inverseTransform: `0@1`) - r3.
	m := self species new.
	m
		a11: r1 x; a12: r2 x; a13: r3 x;
		a21: r1 y; a22: r2 y; a23: r3 y.
	^ m
]

{ #category : 'testing' }
AffineTransformation >> isIdentity [
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."
	<primitive: 'primitiveIsIdentity' module: 'Matrix2x3Plugin'>
	^self isPureTranslation and:[self a13 = 0.0 and:[self a23 = 0.0]]
]

{ #category : 'testing' }
AffineTransformation >> isPureMirroring [
	"Return true if the receiver specifies no translation, rotation or scaling, and just mirrors the Y axis."

	^self a11 = 1.0 and: [ self a12 = 0.0 and: [
	  self a21 = 0.0 and: [ self a22 = -1.0 and: [
	  self a13 = 0.0 and: [ self a23 = 0.0 ]]]]]
]

{ #category : 'testing' }
AffineTransformation >> isPureTranslation [
	"Return true if the receiver specifies no rotation or scaling."

	<primitive: 'primitiveIsPureTranslation' module: 'Matrix2x3Plugin'>
	^self a11 = 1.0 and: [ self a12 = 0.0 and: [
	  self a21 = 0.0 and: [ self a22 = 1.0 ]]]
]

{ #category : 'testing' }
AffineTransformation >> isTranslation: aPoint [
	"The position of 0@0 in the external coordinate system.
	It is the translation we apply when transforming points."
	^self a13 = aPoint x and: [ self a23 = aPoint y ]
]

{ #category : 'modifying' }
AffineTransformation >> italizing [
	"a little shear
	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12 + (self scale*0.2).
	^self
]

{ #category : 'modifying' }
AffineTransformation >> italizing2 [
	"a little shear
	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12 - (self scale*0.2).
	self setTranslation: (self scale*0.2)@0 + self translation.
	^self
]

{ #category : 'printing' }
AffineTransformation >> matrixPrintString [
	"
	(AffineTransformation withRadians: -3 scale: 12 position: 4.5@3) matrixPrintString
	"
	^String streamContents: [ :strm | self printMatrixOn: strm ]
]

{ #category : 'printing' }
AffineTransformation >> print [
	self printOn: Transcript.
	Transcript newLine
]

{ #category : 'printing' }
AffineTransformation >> printMatrixOn: aStream [
	aStream
		newLine;
		nextPutAll: '| '.
	self a11 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a12 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a13 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream nextPutAll: ' |'.

	aStream
		newLine;
		nextPutAll: '| '.
	self a21 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a22 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a23 printOn: aStream integerDigits: 1 fractionDigits: 3.

	aStream
		nextPutAll: ' |';
		newLine
]

{ #category : 'printing' }
AffineTransformation >> printOn: aStream [
	"Note:
	Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."
	aStream
		nextPutAll: self class name;
		nextPutAll: '(scale: '.
	self scale printOn: aStream.
	aStream nextPutAll: '. degrees: '.
	self degrees printOn: aStream.
	aStream nextPutAll: '. translation '.
	self translation printOn: aStream.
	aStream nextPutAll: ') '
]

{ #category : 'accessing' }
AffineTransformation >> radians [
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ self a21 arcTan: self a11
]

{ #category : 'private' }
AffineTransformation >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"This destructively replaces elements from start to stop in the receiver
	starting at index, repStart, in the collection, replacement.  Answer the
	receiver.  No range checks are performed - this may be primitively implemented."

	| index repOff |
	repOff := repStart - start.
	index := start - 1.
	[(index := index + 1) <= stop]
		whileTrue: [self at: index put: (replacement at: repOff + index)]
]

{ #category : 'objects from disk' }
AffineTransformation >> restoreEndianness [
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Swap each pair of bytes (16-bit word), if the current machine is Little Endian.
	Why is this the right thing to do?  We are using memory as a byteStream.  High and low bytes are reversed in each 16-bit word, but the stream of words ascends through memory.  Different from a Bitmap."

	| w b1 b2 b3 b4 |
	Smalltalk  isLittleEndian ifTrue: [
		1 to: self basicSize do: [:i |
			w := self basicAt: i.
			b1 := w digitAt: 1.
			b2 := w digitAt: 2.
			b3 := w digitAt: 3.
			b4 := w digitAt: 4.
			w := (b1 << 24) + (b2 << 16) + (b3 << 8) + b4.
			self basicAt: i put: w.
		]
	].


]

{ #category : 'modifying' }
AffineTransformation >> rotatedBy: radians [
	"rotate the receiver by radians angle.
	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a21 a22 |
	s := radians sin.
	c := radians cos.
	a11 := self a11.
	a12 := self a12.
	a21 := self a21.
	a22 := self a22.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	^ self
]

{ #category : 'accessing' }
AffineTransformation >> scale [
	"Answer the *scalar* scale applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^(self a11 squared + self a21 squared) sqrt
]

{ #category : 'modifying' }
AffineTransformation >> scaledBy: aPointOrNumber [
	"Multiply by a scale.
	Argument can be a point, applying different scaling in x and in y directions.
	Keep the transformed position of 0@0, i.e. don't change offset.

	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt sx sy |
	pt := aPointOrNumber asPoint.
	sx := pt x.
	sy := pt y.
	self a11: self a11 * sx.
	self a12: self a12 * sx.
	self a21: self a21 * sy.
	self a22: self a22 * sy.
	^ self
]

{ #category : 'modifying' }
AffineTransformation >> scaledByNumber: aNumber rotatedBy: radians [
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling.

	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a21 a22 |
	s := radians sin.
	c := radians cos.
	a11 := self a11 * aNumber.
	a12 := self a12 * aNumber.
	a21 := self a21 * aNumber.
	a22 := self a22 * aNumber.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	^self
]

{ #category : 'private' }
AffineTransformation >> setPointScale: scalePoint [
	"Set the raw scale in x and y in the receiver. No rotation. private"

	self a11: scalePoint x.
	self a12: 0.
	self a21: 0.
	self a22: scalePoint y
]

{ #category : 'private' }
AffineTransformation >> setRadians: radians scale: scaleNumber [
	"Set the raw rotation angle and scale in the receiver. private"

	| s c |
	s := radians sin * scaleNumber.
	c := radians cos * scaleNumber.
	self a11: c.
	self a12: s negated.
	self a21: s.
	self a22: c
]

{ #category : 'private' }
AffineTransformation >> setTranslation: aPoint [
	"private. Set the raw offset in the receiver"

	| pt |
	pt := aPoint asPoint.
	self a13: pt x.
	self a23: pt y
]

{ #category : 'converting coordinates' }
AffineTransformation >> transform: aPoint [
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Use Smalltalk code, and not Matrix2x3Plugin, because we want Float conversion."
	"
	Faster approach: Keep 2 instances, to be able to call prim.
	Cons: who cares? This is just for M3, that inlines most calls... Besides, immediate floats makes this bogus.
t := AffineTransformation withRadians: -3 scale: 12 position: 4.5@3.
[ 100000 timesRepeat: [ t transform: 2@3 ]] timeToRun.
[ 100000 timesRepeat: [ 2 ]] timeToRun.
z1 := AffineTransformation withTranslation: 2@3.
z2 := AffineTransformation new.
[ 100000 timesRepeat: [ t composedWith: z1 into: z2. z2 a13@z2 a23 ]] timeToRun.
[ 100000 timesRepeat: [ z1 := AffineTransformation withTranslation: 2@3.z2 := AffineTransformation new.t composedWith: z1 into: z2. z2 a13@z2 a23 ]] timeToRun.
"

	| x y |
	x := (aPoint x * self a11) + (aPoint y * self a12) + self a13.
	y := (aPoint x * self a21) + (aPoint y * self a22) + self a23.
	^x @ y
]

{ #category : 'modifying' }
AffineTransformation >> translatedBy: aPoint [
	"add an offset in the receiver

	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt := aPoint asPoint.
	self a13: self a13 + pt x.
	self a23: self a23 + pt y.
	^ self
]

{ #category : 'accessing' }
AffineTransformation >> translation [
	"Translation and position are the same.
	Use the word translation when thinking about coordinate transformation, but use
	the word position when thinking about morph locations"

	^self a13 @ self a23
]

{ #category : 'modifying' }
AffineTransformation >> withCurrentYAxisInverted [
	"Answer an instance (either the receiver or a new one) that flips the current direction of the Y axis.
	This means that whatever we answer when externalizing x@y, it will answer when externalizing x @ -y.
	Senders should always use the returned object, but not assume it is a new one (like for MorphicTranslation):
		it could also be the receiver itself, like when the receiver is already a AffineTransformation."

	self a12: self a12  negated.
	self a22: self a22  negated.
	^self
]

{ #category : 'modifying' }
AffineTransformation >> withRotation: radians scale: scale [
	"Set rotation and scaling according to parameters.
	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self setRadians: radians scale: scale.
	^self
]

{ #category : 'modifying' }
AffineTransformation >> withScale: scale [
	"Set scaling according to parameters.
	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self scaledBy: scale / self scale.
	^self
]

{ #category : 'modifying' }
AffineTransformation >> withTranslation: aPoint [
	"set an offset in the receiver

	Answer the modified object. In this implementation it is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or sch) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt := aPoint asPoint.
	self a13: pt x.
	self a23: pt y.
	^ self
]

{ #category : 'objects from disk' }
AffineTransformation >> writeOn: aStream [
	aStream nextUint32WordsPutAll: self.
]
