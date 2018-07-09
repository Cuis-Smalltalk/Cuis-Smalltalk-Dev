'From Cuis 5.0 of 7 November 2016 [latest update: #3364] on 9 July 2018 at 10:16:59 am'!

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/8/2018 19:37:51'!
divideBy: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue
	^aFloatArrayOrNumber isNumber
		ifTrue:[self divideByScalar: aFloatArrayOrNumber asFloat ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]
		ifFalse:[self divideByArray: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 7/9/2018 09:45:37'!
divideByArray: floatArray ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array..."
	1 to: self size do:[:i| | dividend divisor quotient |
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

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 7/9/2018 09:45:23'!
divideByScalar: scalarValue ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array..."
	1 to: self size do:[:i| | dividend quotient |
		dividend _ self at: i.
		quotient _ 
			scalarValue isZero 
				ifTrue: [
					dividend isZero
						ifTrue: indeterminateBlockOrValue
						ifFalse: zeroDivisionBlockOrValue ]
				ifFalse: [dividend / scalarValue].
		self at: i put: quotient]! !


!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 7/9/2018 09:41:43'!
divideBy: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue
	^aFloatArrayOrNumber isNumber
		ifTrue:[self divideByScalar: aFloatArrayOrNumber asFloat ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]
		ifFalse:[self divideByArray: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 7/9/2018 09:44:49'!
divideByArray: floatArray ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

	(self primDivArray: floatArray) == #primitiveFailure ifTrue: [
		1 to: self size do:[:i| | dividend divisor quotient |
			dividend _ self at: i.
			divisor _ floatArray at: i.
			quotient _ 
				divisor isZero 
					ifTrue: [
						dividend isZero
							ifTrue: indeterminateBlockOrValue
							ifFalse: zeroDivisionBlockOrValue ]
					ifFalse: [dividend / divisor].
			self at: i put: quotient]]! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 7/8/2018 21:47:26'!
divideByScalar: scalarValue ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

	"This primitive doesn't fail if argument is zeros, just fills with infinity or nan"
	scalarValue isZero ifFalse: [
		^self primDivScalar: scalarValue ].
	1 to: self size do:[:i| | dividend quotient |
		dividend _ self at: i.
		quotient _ dividend isZero
				ifTrue: indeterminateBlockOrValue
				ifFalse: zeroDivisionBlockOrValue.
		self at: i put: quotient]! !


!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/8/2018 18:52:23'!
/= anObject

	^self divideBy: anObject
		ifDivisorZero: [^ZeroDivide signalWithDividend: self]
		ifBothZero: [^ZeroDivide signalWithDividend: self]! !


!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 7/8/2018 18:52:33'!
/= anObject

	^self divideBy: anObject
		ifDivisorZero: [^ZeroDivide signalWithDividend: self]
		ifBothZero: [^ZeroDivide signalWithDividend: self]! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 7/8/2018 19:29:26'!
primDivArray: floatArray

	<primitive: 'primitiveDivFloatArray' module: 'FloatArrayPlugin'>
	^#primitiveFailure! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jmv 7/8/2018 19:53:08'!
primDivScalar: scalarValue
	"This primitive doesn't fail if argument is zero. It fills result with infinity or nan.
	For consistency with division with arrays, and general practice, an exception block or value might be used in public protocol. If needed, call directly this method instead.

	#[1.0 2.0 3.141592 0.0] asFloatArray primDivScalar: 0.0.
	#[1.0 2.0 3.141592 0.0] asFloatArray / 0.0.
	#[1.0 2.0 3.141592 0.0] asFloatArray divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200
	"
	<primitive: 'primitiveDivScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) / scalarValue].! !

!methodRemoval: Float64Array #primDivArray:!
Float64Array removeSelector: #primDivArray:!
!methodRemoval: Float64Array #primDivScalar:!
Float64Array removeSelector: #primDivScalar:!
