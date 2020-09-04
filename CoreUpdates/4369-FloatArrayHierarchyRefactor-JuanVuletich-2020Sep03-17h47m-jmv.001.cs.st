'From Cuis 5.0 [latest update: #4367] on 3 September 2020 at 6:23:25 pm'!

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 9/3/2020 18:10:25'!
divideByArray: floatArray ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

	1 to: self size do: [ :i | | dividend divisor quotient |
		dividend _ self at: i.
		divisor _ floatArray at: i.
		quotient _ 
			divisor isZero 
				ifTrue: [
					dividend isZero
						ifTrue: indeterminateBlockOrValue
						ifFalse: zeroDivisionBlockOrValue ]
				ifFalse: [dividend / divisor].
		self at: i put: quotient]! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 9/3/2020 18:19:30'!
divideByScalar: scalarValue ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

	"This primitive doesn't fail if argument is zeros, just fills with infinity or nan"
	scalarValue isZero ifFalse: [
		^ self primDivScalar: scalarValue ].
	1 to: self size do:[:i| | dividend quotient |
		dividend _ self at: i.
		quotient _ dividend isZero
				ifTrue: indeterminateBlockOrValue
				ifFalse: zeroDivisionBlockOrValue.
		self at: i put: quotient]! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 9/3/2020 18:20:04'!
dot: aFloatVector
	"Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."

	| result |
	self size = aFloatVector size ifFalse: [ ^self error:'Must be equal size' ].
	result _ 0.0.
	1 to: self size do: [ :i |
		result := result + ((self at: i) * (aFloatVector at: i)) ].
	^result! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 9/3/2020 18:21:43'!
normalize
	"Unsafely normalize the receiver in-place (become a unit vector).
 	 Div-by-Zero raised if len 0."

	self /= self length.! !

!FloatArray methodsFor: 'comparing' stamp: 'jmv 6/11/2019 16:22:31'!
= another 
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^ false ].
	self size > 256 ifTrue: [
		self hashQuick = another hashQuick ifFalse: [ ^false ]].
	^self primitiveEqual: another! !

!FloatArray methodsFor: 'comparing' stamp: 'jmv 6/11/2019 16:10:44'!
hash
	self size > 256 ifTrue: [ ^ self hashQuick ].
	^ self hashFull! !

!FloatArray methodsFor: 'comparing' stamp: 'jmv 9/3/2020 17:59:40'!
hashFull
	| hash |
	hash _ (self species hash + self size hash) hashMultiply.
	1 to: self size do: [:i | hash _ (hash + (self basicAt: i)) hashMultiply].
	^hash! !

!FloatArray methodsFor: 'comparing' stamp: 'jmv 9/3/2020 18:02:29'!
primitiveEqual: aFloatArray 
	| length |
	aFloatArray class == self class ifFalse: [^ false].
	length _ self size.
	length = aFloatArray size ifFalse: [^ false].
	1 to: self size do: [:i | (self at: i)
			= (aFloatArray at: i) ifFalse: [^ false]].
	^ true! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:20:40'!
primAddArray: floatArray

	1 to: self size do: [ :i | self at: i put: (self at: i) + (floatArray at: i) ].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:20:36'!
primAddScalar: scalarValue

	1 to: self size do: [ :i | self at: i put: (self at: i) + scalarValue ].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:17:07'!
primDivArray: floatArray
	"Actually only called for Float32Array that redefines this method.
	Just a placeholder."

	^#primitiveFailure! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:19:24'!
primDivScalar: scalarValue
	"This primitive doesn't fail if argument is zero. It fills result with infinity or nan.
	For consistency with division with arrays, and general practice, an exception block or value might be used in public protocol. If needed, call directly this method instead.

	#[1.0 2.0 3.141592 0.0] asFloatArray primDivScalar: 0.0.
	#[1.0 2.0 3.141592 0.0] asFloatArray / 0.0.
	#[1.0 2.0 3.141592 0.0] asFloatArray divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200
	"
	1 to: self size do: [ :i | self at: i put: (self at: i) / scalarValue].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:20:33'!
primMulArray: floatArray

	1 to: self size do: [ :i | self at: i put: (self at: i) * (floatArray at: i) ].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:20:30'!
primMulScalar: scalarValue

	1 to: self size do: [ :i | self at: i put: (self at: i) * scalarValue ].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:20:27'!
primSubArray: floatArray

	1 to: self size do: [ :i | self at: i put: (self at: i) - (floatArray at: i) ].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:20:23'!
primSubScalar: scalarValue

	1 to: self size do: [ :i | self at: i put: (self at: i) - scalarValue ].! !


!Float32Array methodsFor: 'arithmetic' stamp: 'jmv 9/3/2020 18:11:02'!
divideByArray: floatArray ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

	(self primDivArray: floatArray) == #primitiveFailure ifTrue: [
		super
			divideByArray: floatArray
			ifDivisorZero: zeroDivisionBlockOrValue
			ifBothZero: indeterminateBlockOrValue ]! !

!Float32Array methodsFor: 'arithmetic' stamp: 'jmv 9/3/2020 18:18:38'!
dot: aFloatVector
	"Primitive. Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."

	<primitive: 'primitiveDotProduct' module: 'FloatArrayPlugin'>
	^super dot: aFloatVector! !

!Float32Array methodsFor: 'arithmetic' stamp: 'jmv 9/3/2020 18:22:00'!
normalize
	"Unsafely normalize the receiver in-place (become a unit vector).
 	 Div-by-Zero raised if len 0."

	<primitive: 'primitiveNormalize' module: 'FloatArrayPlugin'>
	^super normalize! !

!Float32Array methodsFor: 'comparing' stamp: 'jmv 9/3/2020 17:59:54'!
hashFull
	<primitive:'primitiveHashArray' module: 'FloatArrayPlugin'>
	^super hashFull! !

!Float32Array methodsFor: 'comparing' stamp: 'jmv 9/3/2020 18:00:15'!
primitiveEqual: aFloatArray 

	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	^super primitiveEqual: aFloatArray! !

!Float32Array methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:03:03'!
primAddArray: floatArray

	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>
	^super primAddArray: floatArray! !

!Float32Array methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:03:25'!
primAddScalar: scalarValue

	<primitive: 'primitiveAddScalar' module: 'FloatArrayPlugin'>
	^super primAddScalar: scalarValue! !

!Float32Array methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:08:15'!
primDivScalar: scalarValue
	"This primitive doesn't fail if argument is zero. It fills result with infinity or nan.
	For consistency with division with arrays, and general practice, an exception block or value might be used in public protocol. If needed, call directly this method instead.

	#[1.0 2.0 3.141592 0.0] asFloatArray primDivScalar: 0.0.
	#[1.0 2.0 3.141592 0.0] asFloatArray / 0.0.
	#[1.0 2.0 3.141592 0.0] asFloatArray divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200
	"
	<primitive: 'primitiveDivScalar' module: 'FloatArrayPlugin'>
	^super primDivScalar: scalarValue! !

!Float32Array methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:03:54'!
primMulArray: floatArray

	<primitive: 'primitiveMulFloatArray' module: 'FloatArrayPlugin'>
	^super primMulArray: floatArray! !

!Float32Array methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:04:14'!
primMulScalar: scalarValue

	<primitive: 'primitiveMulScalar' module: 'FloatArrayPlugin'>
	^super primMulScalar: scalarValue! !

!Float32Array methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:04:35'!
primSubArray: floatArray

	<primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>
	^super primSubArray: floatArray! !

!Float32Array methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:04:53'!
primSubScalar: scalarValue

	<primitive: 'primitiveSubScalar' module: 'FloatArrayPlugin'>
	^super primSubScalar: scalarValue! !

!methodRemoval: Float64Array #primSubScalar: stamp: 'jmv 9/3/2020 18:04:41'!
Float64Array removeSelector: #primSubScalar:!
!methodRemoval: Float64Array #primAddArray: stamp: 'jmv 9/3/2020 18:02:54'!
Float64Array removeSelector: #primAddArray:!
!methodRemoval: Float64Array #primMulScalar: stamp: 'jmv 9/3/2020 18:04:04'!
Float64Array removeSelector: #primMulScalar:!
!methodRemoval: Float64Array #primAddScalar: stamp: 'jmv 9/3/2020 18:03:12'!
Float64Array removeSelector: #primAddScalar:!
!methodRemoval: Float64Array #dot: stamp: 'jmv 9/3/2020 18:21:01'!
Float64Array removeSelector: #dot:!
!methodRemoval: Float64Array #divideByScalar:ifDivisorZero:ifBothZero: stamp: 'jmv 9/3/2020 18:14:49'!
Float64Array removeSelector: #divideByScalar:ifDivisorZero:ifBothZero:!
!methodRemoval: Float64Array #primSubArray: stamp: 'jmv 9/3/2020 18:04:24'!
Float64Array removeSelector: #primSubArray:!
!methodRemoval: Float64Array #divideByArray:ifDivisorZero:ifBothZero: stamp: 'jmv 9/3/2020 18:10:29'!
Float64Array removeSelector: #divideByArray:ifDivisorZero:ifBothZero:!
!methodRemoval: Float64Array #normalize stamp: 'jmv 9/3/2020 18:21:47'!
Float64Array removeSelector: #normalize!
!methodRemoval: Float64Array #primMulArray: stamp: 'jmv 9/3/2020 18:03:44'!
Float64Array removeSelector: #primMulArray:!
!methodRemoval: Float32Array #= stamp: 'jmv 9/3/2020 17:59:09'!
Float32Array removeSelector: #=!
!methodRemoval: Float32Array #divideByScalar:ifDivisorZero:ifBothZero: stamp: 'jmv 9/3/2020 18:14:12'!
Float32Array removeSelector: #divideByScalar:ifDivisorZero:ifBothZero:!
!methodRemoval: Float32Array #hash stamp: 'jmv 9/3/2020 17:59:32'!
Float32Array removeSelector: #hash!

!FloatArray reorganize!
('accessing' at: at:put: defaultElement length squaredLength)
('arithmetic' * *= + += - -= / /= \\= adaptToNumber:andSend: divideBy:ifDivisorZero:ifBothZero: divideByArray:ifDivisorZero:ifBothZero: divideByScalar:ifDivisorZero:ifBothZero: dot: negated normalize)
('comparing' = hash hashFull primitiveEqual:)
('interpolating' interpolateValues:at: interpolatedValueAt:)
('inspecting' inspectorClass)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
!


!Float32Array reorganize!
('accessing' floatAt: floatAt:put:)
('arithmetic' divideByArray:ifDivisorZero:ifBothZero: dot: normalize sum)
('comparing' hashFull primitiveEqual:)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('private' replaceFrom:to:with:startingAt:)
('testing' is:)
('printing' printElementsOn:)
!


!Float64Array reorganize!
('accessing' floatAt: floatAt:put: size)
('arithmetic')
('initialization' loadFrom:)
('private' privateLoadFrom: replaceWordsFrom:to:with:startingAt: swapWords)
('objects from disk' bytesPerElement writeOn:)
('converting' asIEEE32BitPrecisionFloat)
('testing' isLiteral)
('printing' printOn: storeOn:)
!

