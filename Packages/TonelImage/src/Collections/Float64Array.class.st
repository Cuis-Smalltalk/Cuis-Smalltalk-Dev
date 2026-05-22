"
Float64Arrays store 64bit IEEE floating point numbers, i.e. instances of the Float class.

Can be created as literals like:
#[ 1.0 2.0 3.0 ]
"
Class {
	#name : 'Float64Array',
	#superclass : 'FloatArray',
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'objects from disk' }
Float64Array class >> newFromStream: aStream [
	"Read bigEndian / PowerPC order."

	| len answer |
	len := aStream nextUint32BigEndian: true.
	answer := self new: len.
	1 to: len do: [ :i |
		answer at: i put: (aStream nextFloat64BigEndian: true) ].
	^ answer
]

{ #category : 'converting' }
Float64Array >> asIEEE32BitPrecisionFloat [
	| answer s |
	self class == Float64Array ifFalse: [
		self error: 'please implement' ].
	s := self size.
	answer := Float32Array new: s.
	1 to: s do: [ :i | answer at: i put: (self at: i) ].
	^answer
]

{ #category : 'accessing' }
Float64Array >> at: index [
	"Answer the Float at index in the receiver.  This method converts from either a 32-bit IEEE representation,
	 or a 64-bit IEEE representation to a Squeak Float object.  Primitive. Optional."
	<primitive: 238 error: ec>
	^ Float fromIEEE64Bit: (self basicAt: index)
]

{ #category : 'accessing' }
Float64Array >> at: index put: value [
	"Store the Float value at index in the receiver.  This method converts from a Squeak Float object,
	 or an Integer, into either a 32-bit IEEE representation, or a 64-bit IEEE representation. Primitive. Optional."
	<primitive: 239 error: ec>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE64BitWord]
		ifFalse: [self at: index put: value asFloat].
	^value
]

{ #category : 'arithmetic' }
Float64Array >> dot: aFloatVector [
	"Primitive. Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."

	| result |
	<primitive: 'primitiveDotProduct' module: 'Float64ArrayPlugin'>
	self size = aFloatVector size ifFalse:[^self error:'Must be equal size'].
	result := 0.0.
	1 to: self size do:[:i|
		result := result + ((self at: i) * (aFloatVector at: i)).
	].
	^result
]

{ #category : 'comparing' }
Float64Array >> hashFull [
	<primitive:'primitiveHashArray' module: 'Float64ArrayPlugin'>
	^super hashFull
]

{ #category : 'testing' }
Float64Array >> isLiteral [
	"so that 
	#(1 #[1.0 2 3] 5)
	prints itself"
	^self class == Float64Array and: [self allSatisfy: [:ea| ea isFinite]]
]

{ #category : 'accessing' }
Float64Array >> length [
	"Return the length of the receiver"
	<primitive: 'primitiveLength' module: 'Float64ArrayPlugin'>
	^super length
]

{ #category : 'arithmetic' }
Float64Array >> normalize [
	"Unsafely normalize the receiver in-place (become a unit vector).
 	 Div-by-Zero raised if len 0."
	<primitive: 'primitiveNormalize' module: 'Float64ArrayPlugin'>
	self /= self length.
]

{ #category : 'primitives-plugin' }
Float64Array >> primAddArray: floatArray [
	<primitive: 'primitiveAddFloat64Array' module: 'Float64ArrayPlugin'>
	^super primAddArray: floatArray
]

{ #category : 'primitives-plugin' }
Float64Array >> primAddScalar: scalarValue [
	<primitive: 'primitiveAddScalar' module: 'Float64ArrayPlugin'>
	^super primAddScalar: scalarValue
]

{ #category : 'primitives-plugin' }
Float64Array >> primDivArray: floatArray [
	<primitive: 'primitiveDivFloat64Array' module: 'Float64ArrayPlugin'>
	^super primDivArray: floatArray
]

{ #category : 'primitives-plugin' }
Float64Array >> primDivScalar: scalarValue [

	<primitive: 'primitiveDivScalar' module: 'Float64ArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) / scalarValue].
]

{ #category : 'primitives-plugin' }
Float64Array >> primFromFloat32Array: aFloat32Array [
"
self 
(Float64Array new:3) primFromFloat32Array: #[1.0 2.0 3.0] asFloat32Array
"
	<primitive: 'primitiveFromFloatArray' module: 'Float64ArrayPlugin'>
	^self fillWith: aFloat32Array
]

{ #category : 'primitives-plugin' }
Float64Array >> primMulArray: floatArray [

	<primitive: 'primitiveMulFloat64Array' module: 'Float64ArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) * (floatArray at: i)].
]

{ #category : 'primitives-plugin' }
Float64Array >> primMulScalar: scalarValue [

	<primitive: 'primitiveMulScalar' module: 'Float64ArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) * scalarValue].
]

{ #category : 'primitives-plugin' }
Float64Array >> primSubArray: floatArray [

	<primitive: 'primitiveSubFloat64Array' module: 'Float64ArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) - (floatArray at: i)].
]

{ #category : 'primitives-plugin' }
Float64Array >> primSubScalar: scalarValue [

	<primitive: 'primitiveSubScalar' module: 'Float64ArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) - scalarValue].
]

{ #category : 'comparing' }
Float64Array >> primitiveEqual: aFloat64Array  [
	<primitive: 'primitiveEqual' module: 'Float64ArrayPlugin'>
	^super primitiveEqual: aFloat64Array
]

{ #category : 'printing' }
Float64Array >> printAsLiteralOn: aStream [

	aStream nextPut: $#; nextPut: $[.
	self
		do: [ :each | each storeOn: aStream ]
		separatedBy: [ aStream nextPut: $ ].
	aStream nextPut: $]
]

{ #category : 'printing' }
Float64Array >> printOn: aStream [

	self storeOn: aStream
]

{ #category : 'printing' }
Float64Array >> storeOn: aStream [

	self isLiteral
		ifTrue: [self printAsLiteralOn: aStream]
		ifFalse: [super storeOn: aStream]
]

{ #category : 'primitives-plugin' }
Float64Array >> sum [

	<primitive: 'primitiveSum' module: 'Float64ArrayPlugin'>
	^ super sum
]

{ #category : 'objects from disk' }
Float64Array >> writeOn: aStream [
	"Store self on the argument, aStream.
	Write bigEndian / PowerPC order."

	aStream nextUint32Put: self size bigEndian: true.
	self do: [ :f |
		aStream nextFloat64Put: f bigEndian: true ]
]
