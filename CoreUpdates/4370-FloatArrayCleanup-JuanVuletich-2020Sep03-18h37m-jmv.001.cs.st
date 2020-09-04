'From Cuis 5.0 [latest update: #4369] on 3 September 2020 at 6:44:44 pm'!

!Collection methodsFor: 'converting' stamp: 'jmv 9/3/2020 18:37:44'!
asFloat32Array
	"Answer a FloatArray whose elements are the elements of the receiver"

	^self as: Float32Array! !


!FloatArray methodsFor: 'initialization' stamp: 'jmv 9/3/2020 18:43:04'!
loadFrom: srcObject

	self == srcObject ifTrue: [ ^self ].
	self class == srcObject class
		ifTrue: [ self replaceFrom: 1 to: self size with: srcObject startingAt: 1 ]
		ifFalse: [ self privateLoadFrom: srcObject ]! !

!FloatArray methodsFor: 'initialization' stamp: 'jmv 9/3/2020 18:43:51'!
privateLoadFrom: srcObject
	"Load the receiver from the given source object.
	See inheritance."
	self error: 'Cannot load a ', srcObject class name,' into a ', self class name! !


!FloatArray methodsFor: 'interpolating' stamp: 'jmv 9/3/2020 18:40:49'!
interpolatedValueAt: floatIndex
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

	size _ self size.
	index0 _ floatIndex truncated. 		"Could be #floor. But as we only care for values >=1, it is the same. But faster."

	weight1 _ floatIndex - index0.
	weight0 _ 1.0 - weight1.

	index1 _ (index0 = size and: [ weight1 = 0.0 ]) 			"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ index0 + 1 ]
		ifTrue: [ index0 ].

	"/* perform interpolation */"
	^ (weight0 * (self at: index0)) + (weight1 * (self at: index1))! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:40:55'!
primDivScalar: scalarValue
	"This primitive doesn't fail if argument is zero. It fills result with infinity or nan.
	For consistency with division with arrays, and general practice, an exception block or value might be used in public protocol. If needed, call directly this method instead.

	#[1.0 2.0 3.141592 0.0] asFloat32Array primDivScalar: 0.0.
	#[1.0 2.0 3.141592 0.0] asFloat32Array / 0.0.
	#[1.0 2.0 3.141592 0.0] asFloat32Array divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200
	"
	1 to: self size do: [ :i | self at: i put: (self at: i) / scalarValue].! !


!Float32Array methodsFor: 'primitives-plugin' stamp: 'jmv 9/3/2020 18:40:33'!
primDivScalar: scalarValue
	"This primitive doesn't fail if argument is zero. It fills result with infinity or nan.
	For consistency with division with arrays, and general practice, an exception block or value might be used in public protocol. If needed, call directly this method instead.

	#[1.0 2.0 3.141592 0.0] asFloat32Array primDivScalar: 0.0.
	#[1.0 2.0 3.141592 0.0] asFloat32Array / 0.0.
	#[1.0 2.0 3.141592 0.0] asFloat32Array divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200
	"
	<primitive: 'primitiveDivScalar' module: 'FloatArrayPlugin'>
	^super primDivScalar: scalarValue! !

!methodRemoval: Float64Array #loadFrom: stamp: 'jmv 9/3/2020 18:43:10'!
Float64Array removeSelector: #loadFrom:!
!methodRemoval: Float64Array #privateLoadFrom: stamp: 'jmv 9/3/2020 18:44:12'!
Float64Array removeSelector: #privateLoadFrom:!
!methodRemoval: Collection #asFloatArray stamp: 'jmv 9/3/2020 18:41:06'!
Collection removeSelector: #asFloatArray!

!FloatArray reorganize!
('accessing' at: at:put: defaultElement length squaredLength)
('arithmetic' * *= + += - -= / /= \\= adaptToNumber:andSend: divideBy:ifDivisorZero:ifBothZero: divideByArray:ifDivisorZero:ifBothZero: divideByScalar:ifDivisorZero:ifBothZero: dot: negated normalize)
('comparing' = hash hashFull primitiveEqual:)
('initialization' loadFrom: privateLoadFrom:)
('interpolating' interpolateValues:at: interpolatedValueAt:)
('inspecting' inspectorClass)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
!

!Float64Array reorganize!
('accessing' floatAt: floatAt:put: size)
('private' replaceWordsFrom:to:with:startingAt: swapWords)
('objects from disk' bytesPerElement writeOn:)
('converting' asIEEE32BitPrecisionFloat)
('testing' isLiteral)
('printing' printOn: storeOn:)
!

