"
Float32rrays store 32bit IEEE floating point numbers, i.e. what usually called float in the C world.

A possible way to create literal Float32Arrays is by using backticks:
`#[ 1.0 2.0 3.0 ] asFloat32Array`
"
Class {
	#name : 'Float32Array',
	#superclass : 'FloatArray',
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'accessing' }
Float32Array >> at: index [
	<primitive: 238 error: ec>
	^Float fromIEEE32Bit: (self basicAt: index)
]

{ #category : 'accessing' }
Float32Array >> at: index put: value [
	<primitive: 239 error: ec>
	value isFloat 
		ifTrue: [self basicAt: index put: value asIEEE32BitWord]
		ifFalse: [self at: index put: value asFloat].
	^value
]

{ #category : 'arithmetic' }
Float32Array >> divideByArray: floatArray ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue [

	(self primDivArray: floatArray) == #primitiveFailure ifTrue: [
		super
			divideByArray: floatArray
			ifDivisorZero: zeroDivisionBlockOrValue
			ifBothZero: indeterminateBlockOrValue ]
]

{ #category : 'arithmetic' }
Float32Array >> dot: aFloatVector [
	"Primitive. Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."

	<primitive: 'primitiveDotProduct' module: 'FloatArrayPlugin'>
	^super dot: aFloatVector
]

{ #category : 'comparing' }
Float32Array >> hashFull [
	<primitive:'primitiveHashArray' module: 'FloatArrayPlugin'>
	^super hashFull
]

{ #category : 'testing' }
Float32Array >> is: aSymbol [
	^ aSymbol == #Float32Array or: [ super is: aSymbol ]
]

{ #category : 'accessing' }
Float32Array >> length [
	"Return the length of the receiver"
	<primitive: 'primitiveLength' module: 'FloatArrayPlugin'>
	^super length
]

{ #category : 'arithmetic' }
Float32Array >> normalize [
	"Unsafely normalize the receiver in-place (become a unit vector).
 	 Div-by-Zero raised if len 0."

	<primitive: 'primitiveNormalize' module: 'FloatArrayPlugin'>
	^super normalize
]

{ #category : 'primitives-plugin' }
Float32Array >> primAddArray: floatArray [

	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>
	^super primAddArray: floatArray
]

{ #category : 'primitives-plugin' }
Float32Array >> primAddScalar: scalarValue [

	<primitive: 'primitiveAddScalar' module: 'FloatArrayPlugin'>
	^super primAddScalar: scalarValue
]

{ #category : 'primitives-plugin' }
Float32Array >> primDivArray: floatArray [

	<primitive: 'primitiveDivFloatArray' module: 'FloatArrayPlugin'>
	^#primitiveFailure
]

{ #category : 'primitives-plugin' }
Float32Array >> primDivScalar: scalarValue [
	"This primitive doesn't fail if argument is zero. It fills result with infinity or nan.
	For consistency with division with arrays, and general practice, an exception block or value might be used in public protocol. If needed, call directly this method instead.

	#[1.0 2.0 3.141592 0.0] asFloat32Array primDivScalar: 0.0.
	#[1.0 2.0 3.141592 0.0] asFloat32Array / 0.0.
	#[1.0 2.0 3.141592 0.0] asFloat32Array divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200
	"
	<primitive: 'primitiveDivScalar' module: 'FloatArrayPlugin'>
	^super primDivScalar: scalarValue
]

{ #category : 'primitives-plugin' }
Float32Array >> primMulArray: floatArray [

	<primitive: 'primitiveMulFloatArray' module: 'FloatArrayPlugin'>
	^super primMulArray: floatArray
]

{ #category : 'primitives-plugin' }
Float32Array >> primMulScalar: scalarValue [

	<primitive: 'primitiveMulScalar' module: 'FloatArrayPlugin'>
	^super primMulScalar: scalarValue
]

{ #category : 'primitives-plugin' }
Float32Array >> primSubArray: floatArray [

	<primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>
	^super primSubArray: floatArray
]

{ #category : 'primitives-plugin' }
Float32Array >> primSubScalar: scalarValue [

	<primitive: 'primitiveSubScalar' module: 'FloatArrayPlugin'>
	^super primSubScalar: scalarValue
]

{ #category : 'comparing' }
Float32Array >> primitiveEqual: aFloatArray  [

	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	^super primitiveEqual: aFloatArray
]

{ #category : 'printing' }
Float32Array >> printElementsOn: aStream [
	"FloatArray elements are answered as 64 bit Float, but are really 32 bit Float.
	When printing, print them as 32 bit Float."
	aStream nextPut: $(.
	self do: [ :element |
		element printAsIEEE32BitPrecisionFloatOn: aStream base: 10.
		aStream space].
	self isEmpty ifFalse: [aStream skipBack].
	aStream nextPut: $)
]

{ #category : 'arithmetic' }
Float32Array >> sum [

	<primitive: 'primitiveSum' module: 'FloatArrayPlugin'>
	^ super sum
]
