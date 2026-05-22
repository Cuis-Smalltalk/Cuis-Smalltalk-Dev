"
Common behavior of Float32Array and Float64Array
"
Class {
	#name : 'FloatArray',
	#superclass : 'ArrayedCollection',
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'byte based hash' }
FloatArray class >> minSizeForHashQuick [
	"Answer the size above which we prefer using the #hashQuick method"

	^256
]

{ #category : 'arithmetic' }
FloatArray >> * anObject [

	^self copy *= anObject
]

{ #category : 'arithmetic' }
FloatArray >> *= anObject [
	^anObject isNumber
		ifTrue:[self primMulScalar: anObject asFloat]
		ifFalse:[self primMulArray: anObject]
]

{ #category : 'arithmetic' }
FloatArray >> + anObject [

	^self copy += anObject
]

{ #category : 'arithmetic' }
FloatArray >> += anObject [
	^anObject isNumber
		ifTrue:[self primAddScalar: anObject asFloat]
		ifFalse:[self primAddArray: anObject]
]

{ #category : 'arithmetic' }
FloatArray >> - anObject [

	^self copy -= anObject
]

{ #category : 'arithmetic' }
FloatArray >> -= anObject [
	^anObject isNumber
		ifTrue:[self primSubScalar: anObject asFloat]
		ifFalse:[self primSubArray: anObject]
]

{ #category : 'arithmetic' }
FloatArray >> / anObject [

	^self copy /= anObject
]

{ #category : 'arithmetic' }
FloatArray >> /= anObject [

	^self divideBy: anObject
		ifDivisorZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]
		ifBothZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]
]

{ #category : 'comparing' }
FloatArray >> = another  [
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^ false ].
	self size > FloatArray minSizeForHashQuick ifTrue: [
		self hashQuick = another hashQuick ifFalse: [ ^false ]].
	^self primitiveEqual: another
]

{ #category : 'arithmetic' }
FloatArray >> \\= other [

	other isNumber ifTrue: [
		1 to: self size do: [:i |
			self at: i put: (self at: i) \\ other
		].
		^ self.
	].
	1 to: (self size min: other size) do: [:i |
		self at: i put: (self at: i) \\ (other at: i).
	].


]

{ #category : 'arithmetic' }
FloatArray >> adaptToNumber: rcvr andSend: selector [
	"If I am involved in arithmetic with a Number. If possible,
	convert it to a float and perform the (more efficient) primitive operation."
	selector == #+ ifTrue:[^self + rcvr].
	selector == #* ifTrue:[^self * rcvr].
	selector == #- ifTrue:[^self negated += rcvr].
	selector == #/ ifTrue:[
		"DO NOT USE TRIVIAL CODE
			^self reciprocal * rcvr
		BECAUSE OF GRADUAL UNDERFLOW
		self should: (1.0e-39 / (FloatArray with: 1.0e-39)) first < 2."
			^(self class new: self size withAll: rcvr) / self
		].
	^super adaptToNumber: rcvr andSend: selector
]

{ #category : 'accessing' }
FloatArray >> atAllPut: anObject [
	self isEmpty ifTrue: [^self].
	"Note: #primFill: (primitiveConstantFill) does only handle unsigned integer.
	Let at:put: take care of properly encoding anObject as bits"
	self at: 1 put: anObject.
	self primFill: (self basicAt: 1)
]

{ #category : 'accessing' }
FloatArray >> defaultElement [
	"Return the default element of the receiver"
	^0.0
]

{ #category : 'arithmetic' }
FloatArray >> divideBy: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue [
	^aFloatArrayOrNumber isNumber
		ifTrue:[self divideByScalar: aFloatArrayOrNumber asFloat ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]
		ifFalse:[self divideByArray: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]
]

{ #category : 'arithmetic' }
FloatArray >> divideByArray: floatArray ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue [

	1 to: self size do: [ :i | | dividend divisor quotient |
		dividend := self at: i.
		divisor := floatArray at: i.
		quotient := 
			divisor isZero 
				ifTrue: [
					dividend isZero
						ifTrue: indeterminateBlockOrValue
						ifFalse: zeroDivisionBlockOrValue ]
				ifFalse: [dividend / divisor].
		self at: i put: quotient]
]

{ #category : 'arithmetic' }
FloatArray >> divideByScalar: scalarValue ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue [

	"This primitive doesn't fail if argument is zeros, just fills with infinity or nan"
	scalarValue isZero ifFalse: [
		^ self primDivScalar: scalarValue ].
	1 to: self size do:[:i| | dividend quotient |
		dividend := self at: i.
		quotient := dividend isZero
				ifTrue: indeterminateBlockOrValue
				ifFalse: zeroDivisionBlockOrValue.
		self at: i put: quotient]
]

{ #category : 'arithmetic' }
FloatArray >> dot: aFloatVector [
	"Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."

	| result |
	self size = aFloatVector size ifFalse: [ ^self error:'Must be equal size' ].
	result := 0.0.
	1 to: self size do: [ :i |
		result := result + ((self at: i) * (aFloatVector at: i)) ].
	^result
]

{ #category : 'comparing' }
FloatArray >> hash [
	self size > FloatArray minSizeForHashQuick ifTrue: [ ^ self hashQuick ].
	^ self hashFull
]

{ #category : 'comparing' }
FloatArray >> hashFull [
	| hash |
	hash := (self species hash + self size hash) hashMultiply.
	1 to: self size do: [:i | hash := (hash + (self basicAt: i)) hashMultiply].
	^hash
]

{ #category : 'inspecting' }
FloatArray >> inspectorClass  [
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^SequenceableCollectionInspector
]

{ #category : 'interpolating' }
FloatArray >> interpolateValues: valuesArray at: x [
	"Interpret self as a domain and valuesArray as a function samples."
	
	^self
		findBinaryIndex: [ :arg | x - arg ]
		do: [ :i | valuesArray at: i ]
		ifNone: [ :i :j  |
			((valuesArray at: i) interpolateTo: (valuesArray at: j) at: (x - (self at: i)) / ((self at: j) - (self at: i)))]
]

{ #category : 'interpolating' }
FloatArray >> interpolatedValueAt: floatIndex [
	"Do a linear interpolation.
	Gives usual error if argument outside bounds:
	#[ 4 5 ] asFloat32Array interpolatedValueAt: 0.999
	#[ 4 5 ] asFloat32Array interpolatedValueAt: 1.0
	#[ 4 5 ] asFloat32Array interpolatedValueAt: 1.5
	#[ 4 5 ] asFloat32Array interpolatedValueAt: 2.0
	#[ 4 5 ] asFloat32Array interpolatedValueAt: 2.000001
	#[ 4 5 ] asFloat32Array interpolatedValueAt: 3
	"
	| size index0 index1 weight0 weight1 |

	size := self size.
	index0 := floatIndex truncated. 		"Could be #floor. But as we only care for values >=1, it is the same. But faster."

	weight1 := floatIndex - index0.
	weight0 := 1.0 - weight1.

	index1 := (index0 = size and: [ weight1 = 0.0 ]) 			"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ index0 + 1 ]
		ifTrue: [ index0 ].

	"/* perform interpolation */"
	^ (weight0 * (self at: index0)) + (weight1 * (self at: index1))
]

{ #category : 'accessing' }
FloatArray >> length [
	"Return the length of the receiver"
	^self squaredLength sqrt
]

{ #category : 'initialization' }
FloatArray >> loadFrom: srcObject [

	self == srcObject ifTrue: [ ^self ].
	self class == srcObject class
		ifTrue: [ self replaceFrom: 1 to: self size with: srcObject startingAt: 1 ]
		ifFalse: [ self privateLoadFrom: srcObject ]
]

{ #category : 'arithmetic' }
FloatArray >> negated [

	^self copy *= -1
]

{ #category : 'arithmetic' }
FloatArray >> normalize [
	"Unsafely normalize the receiver in-place (become a unit vector).
 	 Div-by-Zero raised if len 0."

	self /= self length.
]

{ #category : 'primitives-plugin' }
FloatArray >> primAddArray: floatArray [

	1 to: self size do: [ :i | self at: i put: (self at: i) + (floatArray at: i) ].
]

{ #category : 'primitives-plugin' }
FloatArray >> primAddScalar: scalarValue [

	1 to: self size do: [ :i | self at: i put: (self at: i) + scalarValue ].
]

{ #category : 'primitives-plugin' }
FloatArray >> primDivArray: floatArray [
	1 to: self size do:[:i| self at: i put: (self at: i) / (floatArray at: i)]
]

{ #category : 'primitives-plugin' }
FloatArray >> primDivScalar: scalarValue [
	"This primitive doesn't fail if argument is zero. It fills result with infinity or nan.
	For consistency with division with arrays, and general practice, an exception block or value might be used in public protocol. If needed, call directly this method instead.

	#[1.0 2.0 3.141592 0.0] asFloat32Array primDivScalar: 0.0.
	#[1.0 2.0 3.141592 0.0] asFloat32Array / 0.0.
	#[1.0 2.0 3.141592 0.0] asFloat32Array divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200
	"
	1 to: self size do: [ :i | self at: i put: (self at: i) / scalarValue].
]

{ #category : 'private' }
FloatArray >> primFill: aPositiveInteger [
	"Fill the receiver, an indexable bytes or words object, with the given positive integer.
	The range of possible fill values is :
	- [0..255] for byte arrays;
	- [0..65535] for double byte arrays;
	- [0..(2^32 - 1)] for word arrays;
	- [0..(2^64 - 1)] for double word arrays."

	<primitive: 145>
	self errorImproperStore.
]

{ #category : 'primitives-plugin' }
FloatArray >> primMulArray: floatArray [

	1 to: self size do: [ :i | self at: i put: (self at: i) * (floatArray at: i) ].
]

{ #category : 'primitives-plugin' }
FloatArray >> primMulScalar: scalarValue [

	1 to: self size do: [ :i | self at: i put: (self at: i) * scalarValue ].
]

{ #category : 'primitives-plugin' }
FloatArray >> primSubArray: floatArray [

	1 to: self size do: [ :i | self at: i put: (self at: i) - (floatArray at: i) ].
]

{ #category : 'primitives-plugin' }
FloatArray >> primSubScalar: scalarValue [

	1 to: self size do: [ :i | self at: i put: (self at: i) - scalarValue ].
]

{ #category : 'comparing' }
FloatArray >> primitiveEqual: aFloatArray  [
	| length |
	aFloatArray class == self class ifFalse: [^ false].
	length := self size.
	length = aFloatArray size ifFalse: [^ false].
	1 to: self size do: [:i | (self at: i)
			= (aFloatArray at: i) ifFalse: [^ false]].
	^ true
]

{ #category : 'initialization' }
FloatArray >> privateLoadFrom: srcObject [
	"Load the receiver from the given source object.
	See inheritance."
	self error: 'Cannot load a ', srcObject class name,' into a ', self class name
]

{ #category : 'private' }
FloatArray >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]

{ #category : 'accessing' }
FloatArray >> squaredLength [
	"Return the squared length of the receiver"
	^self dot: self
]
