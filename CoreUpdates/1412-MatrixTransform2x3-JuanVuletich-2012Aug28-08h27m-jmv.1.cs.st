'From Cuis 4.0 of 21 April 2012 [latest update: #1411] on 28 August 2012 at 8:29:09 am'!
!classDefinition: #MatrixTransform2x3 category: #'Morphic-Kernel'!
Object variableWordSubclass: #MatrixTransform2x3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!MatrixTransform2x3 commentStamp: '<historical>' prior: 0!
This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix containing the transformation from the local coordinate system in the external coordinate system. Thus, transforming points from internal to external coordinates is fast and cheap whereas transformations from external to local coordinate systems are relatively expensive.

Implementation Note: In the original version, from Squeak, it is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here). In this version, point transformations are for Float points. The plugin needs fixing, so the plugin primitives that round to integer points are commented. Rectangle transformations were removed (they were are for Display rectangles, and are done in integer coordinates by the plugin).!


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/28/2011 08:59'!
externalizeDistance: aPoint
	"Externalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y |
	x _ (aPoint x * self a11) + (aPoint y * self a12).
	y _ (aPoint x * self a21) + (aPoint y * self a22).
	^x @ y! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/28/2011 09:00'!
externalizePosition: aPoint
	"Answer coordinates for aPoint in the space we are in.
	 aPoint is expressed in us."

	"Warning: The plugin rounds result to integers. We want Float points."
	"<primitive: 'primitiveTransformPoint' module: 'Matrix2x3Plugin'>"

	| x y |
	x _ (aPoint x * self a11) + (aPoint y * self a12) + self a13.
	y _ (aPoint x * self a21) + (aPoint y * self a22) + self a23.
	^x @ y! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/28/2011 09:00'!
externalizePositions: inArray
	"Transform all the points of inArray from local into global coordinates"

	^inArray collect: [ :pt | self externalizePosition: pt ]! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/28/2011 08:34'!
externalizeScalar: aNumber
	"Externalize a distance (without a direction).
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber * self scalarScaleFactor! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/28/2011 09:00'!
internalizeDistance: aPoint
	"Internalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y det a11 a12 a21 a22 detX detY |
	x _ aPoint x asFloat.
	y _ aPoint y asFloat.
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

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/28/2011 08:58'!
internalizePosition: aPoint
	"Answer our coordinates for aPoint.
	 aPoint is expressed in the space we are in."

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

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/28/2011 09:00'!
internalizePositions: inArray
	"Transform all the points of inArray from global into local coordinates"

	^inArray collect: [ :pt | self internalizePosition: pt ]! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/28/2011 08:33'!
internalizeScalar: aNumber
	"Internalize a distance (without a direction). 
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber / self scalarScaleFactor! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'ar 2/2/2001 15:47'!
at: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'ar 2/2/2001 15:47'!
at: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 4/28/2011 08:55'!
inverseTransformation
	"Return the inverse transformation of the receiver.
	The inverse transformation is computed by first calculating
	the inverse offset and then computing transformations
	for the two identity vectors (1@0) and (0@1)"
	| r1 r2 r3 m |
	r3 := self internalizePosition: 0@0.
	r1 := (self internalizePosition: 1@0) - r3.
	r2 := (self internalizePosition: 0@1) - r3.
	m := self species new.
	m
		a11: r1 x; a12: r2 x; a13: r3 x;
		a21: r1 y; a22: r2 y; a23: r3 y.
	^m! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'ar 11/2/1998 23:19'!
offset
	^self a13 @ self a23! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'ar 11/2/1998 23:05'!
offset: aPoint
	self a13: aPoint x asFloat.
	self a23: aPoint y asFloat.! !

!MatrixTransform2x3 methodsFor: 'comparing' stamp: 'ar 2/2/2001 15:47'!
= aMatrixTransform2x3 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	self class = aMatrixTransform2x3 class ifFalse: [^ false].
	length := self size.
	length = aMatrixTransform2x3 size ifFalse: [^ false].
	1 to: self size do: [:i | (self at: i)
			= (aMatrixTransform2x3 at: i) ifFalse: [^ false]].
	^ true! !

!MatrixTransform2x3 methodsFor: 'comparing' stamp: 'ar 5/3/2001 13:02'!
hash
	| result |
	<primitive: 'primitiveHashArray' module: 'FloatArrayPlugin'>
	result := 0.
	1 to: self size do:[:i| result := result + (self basicAt: i) ].
	^result bitAnd: 16r1FFFFFFF! !

!MatrixTransform2x3 methodsFor: 'composing' stamp: 'jmv 8/27/2012 18:54'!
composedWith: aTransformation
	"Return the composition of the receiver and the transformation passed in.

	The result is a MT2x3 that has the following effect:
		self externalize: (aTransformation externalize: aPoint)
		aTransformation internalize: (self internalize: aPoint).

	This means that aTransformation is considered the inner transform, and we are considered the outer transform.
	
	These are equivalent:
		((MatrixTransform2x3 withRadians: 0.3) composedWith: (MatrixTransform2x3 withOffset: 3@5)) print
		((MatrixTransform2x3 withRadians: 0.3) asMatrix * (MatrixTransform2x3 withOffset: 3@5) asMatrix) print

	These are also equivalent:
		((MatrixTransform2x3 withOffset: 3@5) composedWith: (MatrixTransform2x3 withRadians: 0.3)) print
		((MatrixTransform2x3 withOffset: 3@5) asMatrix * (MatrixTransform2x3 withRadians: 0.3) asMatrix) print
	"
	^self composedWith: aTransformation into: self class new! !

!MatrixTransform2x3 methodsFor: 'composing' stamp: 'jmv 8/27/2012 18:51'!
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

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!
a11
	^self at: 1! !

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!
a11: value
	 self at: 1 put: value! !

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!
a12
	^self at: 2! !

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!
a12: value
	 self at: 2 put: value! !

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!
a13
	^self at: 3! !

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!
a13: value
	 self at: 3 put: value! !

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!
a21
	 ^self at: 4! !

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!
a21: value
	 self at: 4 put: value! !

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!
a22
	 ^self at: 5! !

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!
a22: value
	 self at: 5 put: value! !

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:56'!
a23
	 ^self at: 6! !

!MatrixTransform2x3 methodsFor: 'element access' stamp: 'ar 11/2/1998 22:57'!
a23: value
	 self at: 6 put: value! !

!MatrixTransform2x3 methodsFor: 'initialize' stamp: 'ar 11/2/1998 23:17'!
setIdentiy
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	self
		a11: 1.0; a12: 0.0; a13: 0.0;
		a21: 0.0; a22: 1.0; a23: 0.0.! !

!MatrixTransform2x3 methodsFor: 'objects from disk' stamp: 'nk 3/17/2004 16:06'!
byteSize
	^self basicSize * self bytesPerBasicElement! !

!MatrixTransform2x3 methodsFor: 'objects from disk' stamp: 'nk 3/17/2004 15:04'!
bytesPerBasicElement
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	^4! !

!MatrixTransform2x3 methodsFor: 'objects from disk' stamp: 'jmv 4/27/2011 15:21'!
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

!MatrixTransform2x3 methodsFor: 'objects from disk' stamp: 'ar 8/6/2001 17:52'!
writeOn: aStream
	aStream nextWordsPutAll: self.! !

!MatrixTransform2x3 methodsFor: 'printing' stamp: 'jmv 8/27/2012 18:53'!
print
	self printOn: Transcript.
	Transcript newLine! !

!MatrixTransform2x3 methodsFor: 'printing' stamp: 'jmv 8/27/2012 18:53'!
printOn: aStream
	aStream
		newLine;
		nextPutAll: '| ';
		nextPutAll: (self a11 printPaddedLeft: 1 decimalPlaces: 3);
		space; space;
		nextPutAll: (self a12 printPaddedLeft: 1 decimalPlaces: 3);
		space; space;
		nextPutAll: (self a13 printPaddedLeft: 1 decimalPlaces: 3);
		nextPutAll: ' |';

		newLine;
		nextPutAll: '| ';
		nextPutAll: (self a21 printPaddedLeft: 1 decimalPlaces: 3);
		space; space;
		nextPutAll: (self a22 printPaddedLeft: 1 decimalPlaces: 3);
		space; space;
		nextPutAll: (self a23 printPaddedLeft: 1 decimalPlaces: 3);
		nextPutAll: ' |';
		newLine! !

!MatrixTransform2x3 methodsFor: 'testing' stamp: 'ar 2/2/2001 15:47'!
isIdentity
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."
	<primitive: 'primitiveIsIdentity' module: 'Matrix2x3Plugin'>
	^self isPureTranslation and:[self a13 = 0.0 and:[self a23 = 0.0]]! !

!MatrixTransform2x3 methodsFor: 'testing' stamp: 'ar 2/2/2001 15:47'!
isPureTranslation
	"Return true if the receiver specifies no rotation or scaling."
	<primitive: 'primitiveIsPureTranslation' module: 'Matrix2x3Plugin'>
	^self a11 = 1.0 and:[self a12 = 0.0 and:[self a22 = 0.0 and:[self a21 = 1.0]]]! !

!MatrixTransform2x3 methodsFor: 'private' stamp: 'jmv 4/28/2011 08:33'!
scalarScaleFactor
	"Warning: Only valid if we preserve aspect ratio."

	^(self a11 squared + self a21 squared) sqrt! !

!MatrixTransform2x3 methodsFor: 'private' stamp: 'ar 11/2/1998 23:17'!
setOffset: aPoint
	"Set the raw offset in the receiver"
	| pt |
	pt := aPoint asPoint.
	self a13: pt x asFloat.
	self a23: pt y asFloat.! !

!MatrixTransform2x3 methodsFor: 'private' stamp: 'jmv 4/27/2011 15:28'!
setRadians: radians
	"Set the raw rotation angle in the receiver"
	| s c |
	s _ radians sin.
	c _ radians cos.
	self a11: c.
	self a12: s negated.
	self a21: s.
	self a22: c! !

!MatrixTransform2x3 methodsFor: 'private' stamp: 'ar 11/2/1998 23:16'!
setScale: aPoint
	"Set the raw scale in the receiver"
	| pt |
	pt := aPoint asPoint.
	self a11: pt x asFloat.
	self a22: pt y asFloat.! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 4/27/2011 22:41'!
addOffset: aPoint
	"add an offset in the receiver"
	| pt |
	pt _ aPoint asPoint.
	self a13: self a13 + pt x asFloat.
	self a23: self a23 + pt y asFloat.! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 4/27/2011 22:41'!
rotateBy: radians
	"rotate the receiver by radians angle"
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
	self a23: (s * a13) + (c * a23).! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 4/28/2011 08:02'!
scaleBy: aPoint
	"multiply by a scale. Argument can be a point, applying different scaling in x and in y directions"
	| pt sx sy |
	pt _ aPoint asPoint.
	sx _ pt x asFloat.
	sy _ pt y asFloat.
	self a11: self a11 * sx.
	self a12: self a12 * sx.
	self a13: self a13 * sx.
	self a21: self a21 * sy.
	self a22: self a22 * sy.
	self a23: self a23 * sy.! !

!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 4/28/2011 07:48'!
scaleByNumber: aNumber rotateBy: radians
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling"
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
	self a23: (s * a13) + (c * a23).! !

!MatrixTransform2x3 methodsFor: 'converting' stamp: 'jmv 8/27/2012 18:40'!
asMatrix
	"Answer a FloatMatrix."

	| answer |
	answer _ FloatMatrix newHeight: 3 width: 3.
	answer i: 1 j: 1 put: self a11.
	answer i: 1 j: 2 put: self a12.
	answer i: 1 j: 3 put: self a13.
	answer i: 2 j: 1 put: self a21.
	answer i: 2 j: 2 put: self a22.
	answer i: 2 j: 3 put: self a23.
	answer i: 3 j: 3 put: 1.
	^answer! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'ar 11/2/1998 22:50'!
identity
	^self new setScale: 1.0! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'ar 7/9/1998 20:09'!
new
	^self new: 6! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 8/27/2012 18:12'!
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	^ s nextWordsInto: (self new: 6)! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'ar 11/3/1998 02:52'!
withOffset: aPoint
	^self identity setOffset: aPoint! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 4/27/2011 15:28'!
withRadians: radians
	^self new setRadians: radians! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'ar 11/2/1998 02:49'!
withScale: aPoint
	^self new setScale: aPoint! !


!FloatMatrix methodsFor: 'printing' stamp: 'jmv 8/27/2012 18:31'!
printOn: aStream
	aStream newLine.
	1 to: height do: [ :i |
		aStream nextPutAll: '| '.
		1 to: width do: [ :j |
			aStream nextPutAll: ((self i: i j: j) printPaddedLeft: 1 decimalPlaces: 3).
			"aStream print: ((self i: i j: j) roundTo: 0.001)."
			aStream nextPut: $  ].
		aStream nextPut: $|; newLine ]! !

!methodRemoval: MatrixTransform2x3 class #transformFromLocal:toGlobal:!
MatrixTransform2x3 class removeSelector: #transformFromLocal:toGlobal:!
!methodRemoval: MatrixTransform2x3 #bytesPerElement!
MatrixTransform2x3 removeSelector: #bytesPerElement!
!methodRemoval: MatrixTransform2x3 #composedWithInner:!
MatrixTransform2x3 removeSelector: #composedWithInner:!
!methodRemoval: MatrixTransform2x3 #composedWithInner:into:!
MatrixTransform2x3 removeSelector: #composedWithInner:into:!
!methodRemoval: MatrixTransform2x3 #globalBounds:toLocal:!
MatrixTransform2x3 removeSelector: #globalBounds:toLocal:!
!methodRemoval: MatrixTransform2x3 #globalBoundsToLocal:!
MatrixTransform2x3 removeSelector: #globalBoundsToLocal:!
!methodRemoval: MatrixTransform2x3 #localBounds:toGlobal:!
MatrixTransform2x3 removeSelector: #localBounds:toGlobal:!
!methodRemoval: MatrixTransform2x3 #localBoundsToGlobal:!
MatrixTransform2x3 removeSelector: #localBoundsToGlobal:!

!MatrixTransform2x3 reorganize!
('converting coordinates' externalizeDistance: externalizePosition: externalizePositions: externalizeScalar: internalizeDistance: internalizePosition: internalizePositions: internalizeScalar:)
('accessing' at: at:put: inverseTransformation offset offset:)
('comparing' = hash)
('composing' composedWith: composedWith:into:)
('element access' a11 a11: a12 a12: a13 a13: a21 a21: a22 a22: a23 a23:)
('initialize' setIdentiy)
('objects from disk' byteSize bytesPerBasicElement restoreEndianness writeOn:)
('printing' print printOn:)
('testing' isIdentity isPureTranslation)
('private' scalarScaleFactor setOffset: setRadians: setScale:)
('modifying' addOffset: rotateBy: scaleBy: scaleByNumber:rotateBy:)
('converting' asMatrix)
!

