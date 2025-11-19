'From Cuis7.5 [latest update: #7702] on 20 November 2025 at 2:29:42 am'!
!classDefinition: #Float64Array category: #'Collections-Arrayed'!
FloatArray variableDoubleWordSubclass: #Float64Array
	instanceVariableNames: ''
	classVariableNames: 'LastSaveOrder '
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!Float64Array commentStamp: '<historical>' prior: 0!
Float64Arrays store 64bit IEEE floating point numbers, i.e. instances of the Float class.

Can be created as literals like:
#[ 1.0 2.0 3.0 ]!


!FloatArray methodsFor: 'accessing' stamp: 'len 11/20/2025 01:55:09'!
atAllPut: anObject
	self isEmpty ifTrue: [^self].
	"Note: #primFill: (primitiveConstantFill) does only handle unsigned integer.
	Let at:put: take care of properly encoding anObject as bits"
	self at: 1 put: anObject.
	self primFill: (self basicAt: 1)! !

!FloatArray methodsFor: 'private' stamp: 'len 11/20/2025 01:54:44'!
primFill: aPositiveInteger
	"Fill the receiver, an indexable bytes or words object, with the given positive integer.
	The range of possible fill values is :
	- [0..255] for byte arrays;
	- [0..65535] for double byte arrays;
	- [0..(2^32 - 1)] for word arrays;
	- [0..(2^64 - 1)] for double word arrays."

	<primitive: 145>
	self errorImproperStore.! !


!Float64Array methodsFor: 'arithmetic' stamp: 'ul 2/2/2011 23:42'!
dot: aFloatVector
	"Primitive. Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."

	| result |
	<primitive: 'primitiveDotProduct' module: 'Float64ArrayPlugin'>
	self size = aFloatVector size ifFalse:[^self error:'Must be equal size'].
	result := 0.0.
	1 to: self size do:[:i|
		result := result + ((self at: i) * (aFloatVector at: i)).
	].
	^result! !

!Float64Array methodsFor: 'arithmetic' stamp: 'ar 12/14/2010 08:20'!
normalize
	"Unsafely normalize the receiver in-place (become a unit vector).
 	 Div-by-Zero raised if len 0."
	<primitive: 'primitiveNormalize' module: 'Float64ArrayPlugin'>
	self /= self length.! !

!Float64Array methodsFor: 'comparing' stamp: 'len 11/20/2025 02:08:54'!
hashFull
	<primitive:'primitiveHashArray' module: 'Float64ArrayPlugin'>
	^super hashFull! !

!Float64Array methodsFor: 'comparing' stamp: 'len 11/20/2025 02:07:52'!
primitiveEqual: aFloat64Array 
	<primitive: 'primitiveEqual' module: 'Float64ArrayPlugin'>
	^super primitiveEqual: aFloat64Array! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'len 11/20/2025 02:10:13'!
primAddArray: floatArray
	<primitive: 'primitiveAddFloat64Array' module: 'Float64ArrayPlugin'>
	^super primAddArray: floatArray! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'len 11/20/2025 02:10:41'!
primAddScalar: scalarValue
	<primitive: 'primitiveAddScalar' module: 'Float64ArrayPlugin'>
	^super primAddScalar: scalarValue! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'len 11/20/2025 02:14:57'!
primDivArray: floatArray
	<primitive: 'primitiveDivFloat64Array' module: 'Float64ArrayPlugin'>
	^super primDivArray: floatArray! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'ar 2/2/2001 15:47'!
primDivScalar: scalarValue

	<primitive: 'primitiveDivScalar' module: 'Float64ArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) / scalarValue].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'len 11/20/2025 02:15:51'!
primMulArray: floatArray

	<primitive: 'primitiveMulFloat64Array' module: 'Float64ArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) * (floatArray at: i)].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'ar 2/2/2001 15:47'!
primMulScalar: scalarValue

	<primitive: 'primitiveMulScalar' module: 'Float64ArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) * scalarValue].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'len 11/20/2025 02:15:58'!
primSubArray: floatArray

	<primitive: 'primitiveSubFloat64Array' module: 'Float64ArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) - (floatArray at: i)].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'ar 2/2/2001 15:47'!
primSubScalar: scalarValue

	<primitive: 'primitiveSubScalar' module: 'Float64ArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) - scalarValue].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jcg 6/12/2003 17:54'!
sum

	<primitive: 'primitiveSum' module: 'Float64ArrayPlugin'>
	^ super sum! !


!Float methodsFor: 'converting' stamp: 'eem 7/20/2020 13:17'!
asIEEE64BitWord
	"Convert the receiver into a 64 bit Integer value representing the same number in IEEE 64 bit format.
	 Used for conversion in FloatArrays."
	
	^((self basicAt: 1) bitShift: 32) + (self basicAt: 2)! !


!Float class methodsFor: 'instance creation' stamp: 'eem 10/1/2021 14:33'!
fromIEEE64Bit: anInteger
	"Convert the given 64 bit word (which is supposed to be a positive 64-bit value) from
	  a 64 bit IEEE floating point representation into an actual Squeak float object (being
	  64 bits wide). Should only be used for conversion in FloatArrays or likewise objects."
	| value |
	value := self basicNew: 2.
	value
		basicAt: 1 put: (anInteger bitShift: -32);
		basicAt: 2 put: (anInteger bitAnd: 16rFFFFFFFF).
	^value * 1.0 "reduce to SmallFloat64 if possible"! !


!FloatArray methodsFor: 'primitives-plugin' stamp: 'len 11/20/2025 02:14:07'!
primDivArray: floatArray
	1 to: self size do:[:i| self at: i put: (self at: i) / (floatArray at: i)]! !


!Float64Array methodsFor: 'accessing' stamp: 'len 11/20/2025 02:00:29'!
at: index
	"Answer the Float at index in the receiver.  This method converts from either a 32-bit IEEE representation,
	 or a 64-bit IEEE representation to a Squeak Float object.  Primitive. Optional."
	<primitive: 238 error: ec>
	^ Float fromIEEE64Bit: (self basicAt: index)! !

!Float64Array methodsFor: 'accessing' stamp: 'len 11/20/2025 02:02:31'!
at: index put: value
	"Store the Float value at index in the receiver.  This method converts from a Squeak Float object,
	 or an Integer, into either a 32-bit IEEE representation, or a 64-bit IEEE representation. Primitive. Optional."
	<primitive: 239 error: ec>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE64BitWord]
		ifFalse: [self at: index put: value asFloat].
	^value! !

!methodRemoval: Float64Array class #initialize stamp: 'len 11/20/2025 01:58:06'!
Float64Array class removeSelector: #initialize!
!methodRemoval: Float64Array class #new: stamp: 'len 11/20/2025 01:58:09'!
Float64Array class removeSelector: #new:!
!methodRemoval: Float64Array class #swapWords stamp: 'len 11/20/2025 01:58:44'!
Float64Array class removeSelector: #swapWords!
!methodRemoval: Float64Array class #initClassCachedState stamp: 'len 11/20/2025 01:58:03'!
Float64Array class removeSelector: #initClassCachedState!
!methodRemoval: Float64Array class #numElements stamp: 'len 11/20/2025 01:58:41'!
Float64Array class removeSelector: #numElements!
!methodRemoval: Float64Array #swapWords stamp: 'len 11/20/2025 02:00:47'!
Float64Array removeSelector: #swapWords!
!methodRemoval: Float64Array #hash stamp: 'len 11/20/2025 02:08:59'!
Float64Array removeSelector: #hash!
!methodRemoval: Float64Array #= stamp: 'len 11/20/2025 02:08:03'!
Float64Array removeSelector: #=!
!methodRemoval: Float64Array #size stamp: 'len 11/20/2025 02:00:35'!
Float64Array removeSelector: #size!
!methodRemoval: Float64Array #replaceWordsFrom:to:with:startingAt: stamp: 'len 11/20/2025 02:00:43'!
Float64Array removeSelector: #replaceWordsFrom:to:with:startingAt:!

!FloatArray reorganize!
('accessing' atAllPut: defaultElement length squaredLength)
('arithmetic' * *= + += - -= / /= \\= adaptToNumber:andSend: divideBy:ifDivisorZero:ifBothZero: divideByArray:ifDivisorZero:ifBothZero: divideByScalar:ifDivisorZero:ifBothZero: dot: negated normalize)
('comparing' = hash hashFull primitiveEqual:)
('initialization' loadFrom: privateLoadFrom:)
('inspecting' inspectorClass)
('interpolating' interpolateValues:at: interpolatedValueAt:)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('private' primFill:)
!


!Float32Array reorganize!
('accessing' at: at:put:)
('arithmetic' divideByArray:ifDivisorZero:ifBothZero: dot: normalize sum)
('comparing' hashFull primitiveEqual:)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('printing' printElementsOn:)
('testing' is:)
('private' replaceFrom:to:with:startingAt:)
!

!classDefinition: #Float64Array category: #'Collections-Arrayed'!
FloatArray variableDoubleWordSubclass: #Float64Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!Float64Array reorganize!
('accessing' at: at:put:)
('arithmetic' dot: normalize)
('comparing' hashFull primitiveEqual:)
('converting' asIEEE32BitPrecisionFloat)
('objects from disk' writeOn:)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar: sum)
('printing' printAsLiteralOn: printOn: storeOn:)
('testing' isLiteral)
!

Float64Array initialize!

!Float64Array class reorganize!
('objects from disk' newFromStream:)
!

