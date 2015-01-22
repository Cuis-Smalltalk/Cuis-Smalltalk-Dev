'From Cuis 4.2 of 25 July 2013 [latest update: #2156] on 21 January 2015 at 12:12:41.894272 pm'!
!classDefinition: #GeometryTransformation category: #'Graphics-Primitives'!
Object subclass: #GeometryTransformation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!GeometryTransformation commentStamp: '<historical>' prior: 0!
Superclass of several Geometry Transformations used mainly to specify locations of Morphs.!

!classDefinition: #MorphicLocation category: #'Morphic-Kernel'!
GeometryTransformation subclass: #MorphicLocation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
!classDefinition: #AffineTransformation category: #'Morphic-Kernel'!
MorphicLocation variableWordSubclass: #AffineTransformation
	instanceVariableNames: ''
	classVariableNames: 'DoIntegerTransformations'
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!AffineTransformation commentStamp: '<historical>' prior: 0!
This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix. 

The direct transformation is equivalent as multiplying the matrix by a column vector (with an extra element of value 1). The inverse transformation is multiplying the inverse of the matrix by a column vector (with an extra element of value 1). By convention, we say that the direct transform is outwards (#externalizePosition:). Therefore, the inverse transform is called #internalizePosition: .

Direct transform (#externalizePosition:) is fast and cheap, while inverse transform (#internalizePosition:) is relatively expensive.

We can consider the matrix to be of 3x3 with an extra row with #( 0 0 1) at the bottom, especially for having a properly defined inverse matrix.

Implementation Note: In the original version, from Squeak, it is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here). In this version, both alternatives are available, with explicit protocols, and a global setting for the Squeak protocol.

Note: Methods that modify an instance answer the result. The result might be the receiver itself, or it might be a new instance. Therefore:
- Don't assume the receiver will be modified. Always take the result.
- Don't assume the answer will be a new instance. The receiver might be modified!! (make a copy if needed)!

!classDefinition: #MorphicTranslation category: #'Morphic-Kernel'!
MorphicLocation subclass: #MorphicTranslation
	instanceVariableNames: 'deltaX deltaY '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!MorphicTranslation commentStamp: 'jmv 1/14/2015 14:32' prior: 0!
Can replace an AffineTransformation when there is only a translation applied (no scale change, no rotation, no skew).

Note: Methods that modify an instance answer the result. The result might be the receiver itself, or it might be a new instance. Therefore:
- Don't assume the receiver will be modified. Always take the result.
- Don't assume the answer will be a new instance. The receiver might be modified!! (make a copy if needed)!


!Morph commentStamp: '<historical>' prior: 0!
A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://minnow.cc.gatech.edu/squeak/30. 

Morphs exist in a tree, rooted at a World (generally a PasteUpMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets drawn. To hide a Morph and its submorphs, set its #visible property to false using the #visible: method. 

Structure:
instance var 	Type 					Description 
owner 			Morph		 			My parent Morph, or nil for the top-level Morph, which is a
 				or nil					world, typically a PasteUpMorph.
submorphs 	Array 					My child Morphs. 
location 		MorphicLocation		Specifies position (and possibly, angle of rotation and scale change) inside owner
extension 		MorphExtension 		Allows extra properties to be stored without adding a
				or nil  					storage burden to all morphs. 
!


!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/14/2015 14:19'!
externalizeDelta: aPoint
	"Externalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y |
	x _ (aPoint x * self a11) + (aPoint y * self a12).
	y _ (aPoint x * self a21) + (aPoint y * self a22).
	^x @ y! !

!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/14/2015 14:19'!
externalizePosition: aPoint
	"Answer coordinates for aPoint in the space we are in.
	 aPoint is expressed in us."

	^self transform: aPoint! !

!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/14/2015 14:19'!
externalizeScalar: aNumber
	"Externalize a distance (without a direction).
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber * self scale! !

!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/14/2015 14:19'!
internalizeDelta: aPoint
	"Internalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y det a11 a12 a21 a22 detX detY |
	x _ aPoint x.
	y _ aPoint y.
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

!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/14/2015 14:19'!
internalizePosition: aPoint
	"Answer our coordinates for aPoint.
	 aPoint is expressed in the space we are in."

	^self inverseTransform: aPoint! !

!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/14/2015 14:19'!
internalizeScalar: aNumber
	"Internalize a distance (without a direction). 
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber / self scale! !

!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/14/2015 14:19'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	if DoIntegerTransformations, answer rounded integer values."

	^ DoIntegerTransformations
		ifTrue: [ self intInverseTransform: aPoint ]
		ifFalse: [ self floatInverseTransform: aPoint ]! !

!AffineTransformation methodsFor: 'converting coordinates' stamp: 'jmv 1/14/2015 14:19'!
transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	if DoIntegerTransformations, answer rounded integer values ."

	^ DoIntegerTransformations
		ifTrue: [ self intTransform: aPoint ]
		ifFalse: [ self floatTransform: aPoint ]! !

!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 1/14/2015 14:19'!
at: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 1/14/2015 14:19'!
at: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !

!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 1/14/2015 14:19'!
degrees
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ self radians radiansToDegrees! !

!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 1/14/2015 14:19'!
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

!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 1/14/2015 14:19'!
radians
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ self a21 arcTan: self a11! !

!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 1/14/2015 14:19'!
scale
	"Answer the *scalar* scale applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^(self a11 squared + self a21 squared) sqrt! !

!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 1/14/2015 14:19'!
translation
	"Translation and position are the same.
	Use the word translation when thinking about coordinate transformation, but use
	the word position when thinking about morph locations"

	^self a13 @ self a23! !

!AffineTransformation methodsFor: 'comparing' stamp: 'jmv 1/14/2015 14:19'!
= anAffineTransformation 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	self class = anAffineTransformation class ifFalse: [^ false].
	length := self size.
	length = anAffineTransformation size ifFalse: [^ false].
	1 to: self size do: [:i | (self at: i)
			= (anAffineTransformation at: i) ifFalse: [^ false]].
	^ true! !

!AffineTransformation methodsFor: 'comparing' stamp: 'jmv 1/14/2015 14:19'!
hash
	| result |
	<primitive: 'primitiveHashArray' module: 'FloatArrayPlugin'>
	result := 0.
	1 to: self size do:[:i| result := result + (self basicAt: i) ].
	^result bitAnd: 16r1FFFFFFF! !

!AffineTransformation methodsFor: 'composing' stamp: 'jmv 1/14/2015 14:25'!
composedWith: innerTransformation
	"Return the composition of the receiver and the transformation passed in.
	The result is a translation  that has the following effect:
		self externalize: (innerTransformation externalize: aPoint)
		innerTransformation internalize: (self internalize: aPoint)."

 	^innerTransformation innerComposedWithAffineTransformation: self! !

!AffineTransformation methodsFor: 'composing' stamp: 'jmv 1/14/2015 14:19'!
composedWith: aTransformation into: result
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
	^result! !

!AffineTransformation methodsFor: 'composing' stamp: 'jmv 1/14/2015 14:24'!
innerComposedWithAffineTransformation: anAffineTransformation
	"Return the composition of the receiver and the transformation passed in.
	 We know the class of the argument (through double dispatching)"

	^ anAffineTransformation composedWith: self into: self class new! !

!AffineTransformation methodsFor: 'composing' stamp: 'jmv 1/14/2015 14:26'!
innerComposedWithTranslation: aMorphicTranslation
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^(AffineTransformation withTranslation: aMorphicTranslation translation) composedWith: self! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a11
	^self at: 1! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a11: value
	 self at: 1 put: value! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a12
	^self at: 2! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a12: value
	 self at: 2 put: value! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a13
	^self at: 3! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a13: value
	 self at: 3 put: value! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a21
	 ^self at: 4! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a21: value
	 self at: 4 put: value! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a22
	 ^self at: 5! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a22: value
	 self at: 5 put: value! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a23
	 ^self at: 6! !

!AffineTransformation methodsFor: 'element access' stamp: 'jmv 1/14/2015 14:19'!
a23: value
	 self at: 6 put: value! !

!AffineTransformation methodsFor: 'objects from disk' stamp: 'jmv 1/14/2015 14:19'!
byteSize
	^self basicSize * self bytesPerBasicElement! !

!AffineTransformation methodsFor: 'objects from disk' stamp: 'jmv 1/14/2015 14:19'!
bytesPerBasicElement
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	^4! !

!AffineTransformation methodsFor: 'objects from disk' stamp: 'jmv 1/14/2015 14:19'!
restoreEndianness
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

! !

!AffineTransformation methodsFor: 'objects from disk' stamp: 'jmv 1/14/2015 14:19'!
writeOn: aStream
	aStream nextWordsPutAll: self.! !

!AffineTransformation methodsFor: 'printing' stamp: 'jmv 1/14/2015 14:26'!
matrixPrintString
	"
	(AffineTransformation withRadians: -3 scale: 12 position: 4.5@3) matrixPrintString
	"
	^String streamContents: [ :strm | self printMatrixOn: strm ]! !

!AffineTransformation methodsFor: 'printing' stamp: 'jmv 1/14/2015 14:19'!
print
	self printOn: Transcript.
	Transcript newLine! !

!AffineTransformation methodsFor: 'printing' stamp: 'jmv 1/14/2015 14:19'!
printMatrixOn: aStream
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
		newLine! !

!AffineTransformation methodsFor: 'printing' stamp: 'jmv 1/14/2015 14:19'!
printOn: aStream
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
	aStream nextPutAll: ') '! !

!AffineTransformation methodsFor: 'testing' stamp: 'jmv 1/14/2015 14:19'!
doesNotRotate
	"Answer true if #radians would answer 0.0. Performance optimization."

	^self a21 = 0.0 and: [ self a11 sign >= 0 ]! !

!AffineTransformation methodsFor: 'testing' stamp: 'jmv 1/14/2015 14:19'!
isIdentity
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."
	<primitive: 'primitiveIsIdentity' module: 'Matrix2x3Plugin'>
	^self isPureTranslation and:[self a13 = 0.0 and:[self a23 = 0.0]]! !

!AffineTransformation methodsFor: 'testing' stamp: 'jmv 1/14/2015 14:19'!
isPureTranslation
	"Return true if the receiver specifies no rotation or scaling."
	<primitive: 'primitiveIsPureTranslation' module: 'Matrix2x3Plugin'>
	^self a11 = 1.0 and:[self a12 = 0.0 and:[self a22 = 0.0 and:[self a21 = 1.0]]]! !

!AffineTransformation methodsFor: 'testing' stamp: 'jmv 1/14/2015 14:19'!
isTranslation: aPoint
	"The position of 0@0 in the external coordinate system.
	It is the translation we apply when transforming points."
	^self a13 = aPoint x and: [ self a23 = aPoint y ]! !

!AffineTransformation methodsFor: 'private' stamp: 'jmv 1/14/2015 14:19'!
setRadians: radians scale: scale
	"Set the raw rotation angle in the receiver. private"

	| s c |
	s _ radians sin * scale.
	c _ radians cos * scale.
	self a11: c.
	self a12: s negated.
	self a21: s.
	self a22: c! !

!AffineTransformation methodsFor: 'private' stamp: 'jmv 1/14/2015 14:19'!
setTranslation: aPoint
	"private. Set the raw offset in the receiver"

	| pt |
	pt := aPoint asPoint.
	self a13: pt x.
	self a23: pt y! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:19'!
italizing
	"a little shear
	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12 + (self scale*0.2).
	^self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:19'!
italizing2
	"a little shear
	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12 - (self scale*0.2).
	self setTranslation: (self scale*0.2)@0 + self translation.
	^self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:19'!
rotatedBy: radians
	"rotate the receiver by radians angle.
	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a13 a21 a22 a23|
	s _ radians sin.
	c _ radians cos.
	a11 _ self a11.
	a12 _ self a12.
	a13 _ self a13.
	a21 _ self a21.
	a22 _ self a22.
	a23 _ self a23.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a13: (c * a13) - (s * a23).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	self a23: (s * a13) + (c * a23).
	^ self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:19'!
scaledBy: aPoint
	"Multiply by a scale.
	Argument can be a point, applying different scaling in x and in y directions.
	Keep the transformed position of 0@0, i.e. don't change offset.

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt sx sy |
	pt _ aPoint asPoint.
	sx _ pt x.
	sy _ pt y.
	self a11: self a11 * sx.
	self a12: self a12 * sx.
	self a21: self a21 * sy.
	self a22: self a22 * sy.
	^ self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:19'!
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

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:19'!
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

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:27'!
withRotation: radians scale: scale
	"Set rotation and scaling according to parameters.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already an AffineTransformation."

	self setRadians: radians scale: scale.
	^self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:19'!
withTranslation: aPoint
	"set an offset in the receiver

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or sch) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	self a13: pt x.
	self a23: pt y.
	^ self! !

!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:19'!
withYAxisNegated
	"Swap inneer point Y sign.
	Make y increment upwards.
	This makes the any matrix transform from standard mathematical coordinates
	to standard display coordinates (in addition to the transform it was already doing)

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12  negated.
	self a22: self a22  negated! !

!AffineTransformation methodsFor: 'transforming rects' stamp: 'jmv 1/14/2015 14:19'!
displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
	and vertical bounds and integer coordinates (i.e. adisplayBounds)."

	^self displayBoundsOfTransformOf: aRectangle into: Rectangle new! !

!AffineTransformation methodsFor: 'transforming rects' stamp: 'jmv 1/14/2015 14:19'!
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

!AffineTransformation methodsFor: 'transforming rects' stamp: 'jmv 1/14/2015 14:26'!
primDisplayBoundsOfInverseTransformOf: srcRect into: dstRect
	"Internalize srcRect, and find a bounding rectangle with horizontal and vertical bounds (in the inner space) and 
	integer coordinates (i.e. adisplayBounds). Store result into dstRect.

	Unused. Just kept around to document the available primitives in Matrix2x3Plugin"

	"Warning: the answer is rounded to integers by the primitive
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	AffineTransformation new primDisplayBoundsOfInverseTransformOf: (-2@ 2 extent: 10@10) into: Rectangle new
	AffineTransformation new primDisplayBoundsOfInverseTransformOf: (-12@ 12 extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 2)  primDisplayBoundsOfInverseTransformOf: (-4@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 4)  primDisplayBoundsOfInverseTransformOf: (-2@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 2)  primDisplayBoundsOfInverseTransformOf: (-14@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 4)  primDisplayBoundsOfInverseTransformOf: (-12@ 2  extent: 10@10) into: Rectangle new
	
	These are Ok (answer is positive)
	(AffineTransformation withTranslation: -2)  primDisplayBoundsOfInverseTransformOf: (4@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: -4)  primDisplayBoundsOfInverseTransformOf: (2@ 2  extent: 10@10) into: Rectangle new
	"
	<primitive: 'primitiveInvertRectInto' module: 'Matrix2x3Plugin'>

	^nil! !

!AffineTransformation methodsFor: 'transforming rects' stamp: 'jmv 1/14/2015 14:26'!
primDisplayBoundsOfTransformOf: srcRect into: dstRect
	"Externalize srcRect, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Store result into dstRect."

	"Warning: the answer is rounded to integers by the primitive
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	AffineTransformation new primDisplayBoundsOfTransformOf: (-2@ 2 extent: 10@10) into: Rectangle new
	AffineTransformation new primDisplayBoundsOfTransformOf: (-12@ 12 extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 2)  primDisplayBoundsOfTransformOf: (-4@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: -4)  primDisplayBoundsOfTransformOf: (2@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 2)  primDisplayBoundsOfTransformOf: (-14@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 4)  primDisplayBoundsOfTransformOf: (-12@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: -4)  primDisplayBoundsOfTransformOf: (12@ 2  extent: 10@10) into: Rectangle new
	
	These are Ok (answer is positive)
	(AffineTransformation withTranslation: -2)  primDisplayBoundsOfTransformOf: (4@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 4)  primDisplayBoundsOfTransformOf: (-2@ 2  extent: 10@10) into: Rectangle new
	"
	<primitive: 'primitiveTransformRectInto' module: 'Matrix2x3Plugin'>

	^nil! !

!AffineTransformation methodsFor: 'inspecting' stamp: 'jmv 1/14/2015 14:19'!
customizeExplorerContents

	^ true! !

!AffineTransformation methodsFor: 'inspecting' stamp: 'jmv 1/14/2015 14:19'!
explorerContents

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
			model: self }! !

!AffineTransformation methodsFor: 'inspecting' stamp: 'jmv 1/14/2015 14:19'!
inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^OrderedCollectionInspector! !

!AffineTransformation methodsFor: 'private - primitives' stamp: 'jmv 1/14/2015 14:19'!
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

!AffineTransformation methodsFor: 'private - primitives' stamp: 'jmv 1/14/2015 14:19'!
floatInverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Smalltalk code for Float conversion."
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

!AffineTransformation methodsFor: 'private - primitives' stamp: 'jmv 1/14/2015 14:19'!
floatTransform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Smalltalk code for Float conversion."

	| x y |
	x _ (aPoint x * self a11) + (aPoint y * self a12) + self a13.
	y _ (aPoint x * self a21) + (aPoint y * self a22) + self a23.
	^x @ y! !

!AffineTransformation methodsFor: 'private - primitives' stamp: 'jmv 1/14/2015 14:19'!
intInverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer is not strictly positive, it is off by one. 
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here."

	^(self fixRoundingInPrimitives: (self primInverseTransform: aPoint))
		ifNil: [ (self floatInverseTransform: aPoint) rounded ]! !

!AffineTransformation methodsFor: 'private - primitives' stamp: 'jmv 1/14/2015 14:19'!
intTransform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here."

	^(self fixRoundingInPrimitives: (self primTransform: aPoint))
		ifNil: [ (self floatTransform: aPoint) rounded ]! !

!AffineTransformation methodsFor: 'private - primitives' stamp: 'jmv 1/14/2015 14:27'!
primInverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	AffineTransformation new primInverseTransform: (-2@ 2)
	(AffineTransformation withTranslation: 2)  primInverseTransform: (-4@ 2)
	(AffineTransformation withTranslation: 4)  primInverseTransform: (-2@ 2)
	
	These are Ok (answer is positive)
	(AffineTransformation withTranslation: -2)  primInverseTransform: (4@ 2)
	(AffineTransformation withTranslation: -4)  primInverseTransform: (2@ 2)
	"

	<primitive: 'primitiveInvertPoint' module: 'Matrix2x3Plugin'>
	^nil! !

!AffineTransformation methodsFor: 'private - primitives' stamp: 'jmv 1/14/2015 14:27'!
primTransform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	AffineTransformation new primTransform: (-2@ 2)
	(AffineTransformation withTranslation: 2)  primTransform: (-4@ 2)
	(AffineTransformation withTranslation: -4)  primTransform: (2@ 2)

	This is ok (answer is positive)
	(AffineTransformation withTranslation: 4)  primTransform: (-2@ 2)
	(AffineTransformation withTranslation: -2)  primTransform: (4@ 2)
	"

	<primitive: 'primitiveTransformPoint' module: 'Matrix2x3Plugin'>
	^nil! !

!AffineTransformation methodsFor: 'initialization' stamp: 'jmv 1/14/2015 14:19'!
initialize
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	self
		a11: 1.0;
		a22: 1.0! !


!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/14/2015 14:19'!
forScaleFactor: sf originalExtent: e
	"Answer a Transfor appropriate for image rescaling. sf is the scale factor to apply (i.e. 0.5 means result is half the size of original image). e is the extent of the original image."

	^self transformFromLocal: (0.5@0.5 extent: e) toExternal: (0.5@0.5 extent: sf*e)! !

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/14/2015 14:19'!
new
	^(self basicNew: 6) initialize! !

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/14/2015 14:27'!
new: s
	self error: 'Please call AffineTransformation>>#new (without arguments).'.
	^self new! !

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/14/2015 14:19'!
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	^ s nextWordsInto: self new! !

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/14/2015 14:19'!
transformFromLocal: localBounds toExternal: externalBounds
	"Answer a Transform to translate coordinates inside localBounds into coordinates inside externalBounds.
	Parameters are instances of Rectangle. Therefore, there's no rotation involved, just scale and offset."

	^((self withTranslation: (externalBounds topLeft + externalBounds bottomRight / 2.0)) composedWith:
		(self withScale: (externalBounds extent / localBounds extent) asFloatPoint)) composedWith:
			(self withTranslation: (localBounds topLeft + localBounds bottomRight / 2.0) negated)! !

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/14/2015 14:19'!
withRadians: radians
	^self new setRadians: radians scale: 1.0! !

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/14/2015 14:27'!
withRadians: radians scale: aNumber position: aPoint
	"Translation is added at the end. This means that aPoint is in the outer coordinate space.
	AffineTransformation withRadians: -3 scale: 12 position: 4.5@3
	"
	^self new
		setRadians: radians scale: aNumber;
		setTranslation: aPoint! !

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/14/2015 14:19'!
withScale: aPoint
	^self new setRadians: 0.0 scale: aPoint! !

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/14/2015 14:19'!
withTranslation: aPoint
	"scale is one, angle is zero"

	^self new setTranslation: aPoint! !

!AffineTransformation class methodsFor: 'class initialization' stamp: 'jmv 1/14/2015 14:19'!
doIntegerTransformations: aBoolean

	DoIntegerTransformations _ aBoolean! !

!AffineTransformation class methodsFor: 'class initialization' stamp: 'jmv 1/14/2015 14:19'!
initialize
	DoIntegerTransformations _ true.! !


!MorphicTranslation methodsFor: 'composing' stamp: 'jmv 1/14/2015 14:25'!
innerComposedWithAffineTransformation: anAffineTransformation
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^anAffineTransformation composedWith: (AffineTransformation withTranslation: self translation)
" 	^ anAffineTransformation copy offsetBy: self translation" ! !


!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:32'!
italizing
	"a little shear
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) italizing! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:32'!
italizing2
	"a little shear
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) italizing2! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:32'!
rotatedBy: radians
	"rotate the receiver by radians angle.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) rotatedBy: radians! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:32'!
scaledBy: aPoint
	"Multiply by a scale.
	Argument can be a point, applying different scaling in x and in y directions.
	Keep the transformed position of 0@0, i.e. don't change offset.

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) scaledBy: aPoint! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:32'!
scaledByNumber: aNumber rotatedBy: radians
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling.

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) scaledByNumber: aNumber rotatedBy: radians! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:32'!
withRotation: radians scale: scale
	"Set rotation and scaling according to parameters.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) withRotation: radians scale: scale! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 1/14/2015 14:32'!
withYAxisNegated
	"Swap inneer point Y sign.
	Make y increment upwards.
	This makes the any matrix transform from standard mathematical coordinates
	to standard display coordinates (in addition to the transform it was already doing)

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) withYAxisNegated! !


!Morph methodsFor: 'geometry' stamp: 'jmv 1/14/2015 14:33'!
morphPositionInWorld: newPositionInWorld
	"Change the position of this morph."
	"El tema es, que tipo de coordenadas tenemos?
	En un mundo relativista, no hay un marco de referencia absoluto.
	No tiene sentido hablar de coordenadas del mundo... El mundo podria estar escalado... 
		Que tienen de especial las coordenadas del mundo?
	Coordenadas 'del hardware'? No deberia saber mucho sobre el... Puede haber multiples displays, hands de diverso tipo, remotas, virtuales...
	
	En ppio, un par de coordenadas pueden ser relativas a cualquier morph. Pareciera que necesito metodos de conversion de cualquier morph hacia mi, y de mi hacia cualquier morph... Como encontrar un marco de referencia comun????
	Dejar esto para despues. En realidad, para empezar, preciso menos: Solo preciso saber si las coordenadas estan en el morph o en su owner. Nada mas. Los eventos se iran transformando apropiadamente al moverse por el arbol, o al menos, llevaran consigo una transformacion (AffineTransformation) que se ira actualizando"

	| newPositionInOwner |
	self flag: #jmvVer2.
	"This method MUST die"

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].

	(location isTranslation: newPositionInOwner) ifTrue: [
		^ self ].		"Null change".

	self redrawNeeded.
	location _ location withTranslation: newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 1/14/2015 14:31'!
intoLocation: anAffineTransformation

	| last |
	cti _ cti + 1.
	transformations size < cti
		ifTrue: [	
			currentTransformation _ currentTransformation composedWith: anAffineTransformation.
			transformations add: currentTransformation ]
		ifFalse: [
			last _ currentTransformation.
			currentTransformation _ transformations at: cti.
			"reuse the instance if possible"
			(last class == anAffineTransformation class and: [ last class == currentTransformation class ])
				ifTrue: [
					last composedWith: anAffineTransformation into: currentTransformation ]
				ifFalse: [
					currentTransformation _ last composedWith: anAffineTransformation.
					transformations at: cti put: currentTransformation ]]! !

!methodRemoval: MorphicTranslation #innerComposedWithMatrixTransform2x3:!
MorphicTranslation removeSelector: #innerComposedWithMatrixTransform2x3:!
!classDefinition: #MorphicTranslation category: #'Morphic-Kernel'!
MorphicLocation subclass: #MorphicTranslation
	instanceVariableNames: 'deltaX deltaY'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
AffineTransformation initialize!
!classRemoval: #MatrixTransform2x3!
Smalltalk removeClassNamed: #MatrixTransform2x3!
