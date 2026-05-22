"
My instances hold 64-bit Floats in heap objects.  This is the only representation on 32-bit systems.  But on 64-bit systems SmallFloat64 holds a subset of the full 64-bit double-precision range in immediate objects.
"
Class {
	#name : 'BoxedFloat64',
	#superclass : 'Float',
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'instance creation' }
BoxedFloat64 class >> basicNew [
	^self basicNew: 2
]

{ #category : 'instance creation' }
BoxedFloat64 class >> basicNew: sizeRequested  [
	"Primitive. Answer an instance of this class with the number
	 of indexable variables specified by the argument, sizeRequested.
	 Fail if this class is not indexable or if the argument is not a
	 positive Integer, or if there is not enough memory available. 
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 71>
	sizeRequested isInteger ifTrue:
		[^sizeRequested = 2
			ifTrue: "arg okay; space must be low."
				[OutOfMemory signal.
				 self basicNew: sizeRequested]  "retry if user proceeds"
			ifFalse:
				[self error: 'a Float shall always have two slots']].
	self primitiveFailed
]

{ #category : 'arithmetic' }
BoxedFloat64 >> * aNumber  [
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 49>
	^ aNumber adaptToFloat: self andSend: #*
]

{ #category : 'arithmetic' }
BoxedFloat64 >> + aNumber  [
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 41>
	^ aNumber adaptToFloat: self andSend: #+
]

{ #category : 'arithmetic' }
BoxedFloat64 >> - aNumber  [
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 42>
	^ aNumber adaptToFloat: self andSend: #-
]

{ #category : 'arithmetic' }
BoxedFloat64 >> / aNumber  [
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 50>
	aNumber isZero ifTrue: [^ ZeroDivide new signalReceiver: self selector: #/ argument: aNumber].
	^ aNumber adaptToFloat: self andSend: #/
]

{ #category : 'comparing' }
BoxedFloat64 >> < aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 43>
	^ aNumber adaptToFloat: self andSend: #<
]

{ #category : 'comparing' }
BoxedFloat64 >> <= aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 45>
	^ aNumber adaptToFloat: self andSend: #<=
]

{ #category : 'comparing' }
BoxedFloat64 >> = aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 47>
	aNumber isNumber ifFalse: [^ false].
	^ aNumber adaptToFloat: self andSend: #=
]

{ #category : 'comparing' }
BoxedFloat64 >> > aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 44>
	^ aNumber adaptToFloat: self andSend: #>
]

{ #category : 'comparing' }
BoxedFloat64 >> >= aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 46>
	^ aNumber adaptToFloat: self andSend: #>=
]

{ #category : 'comparing' }
BoxedFloat64 >> ~= aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 48>
	^super ~= aNumber
]

{ #category : 'mathematical functions' }
BoxedFloat64 >> arcTan [
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive.
	Note: If the purpose is to recover the angle of some vector, prefer #arcTan:
		See, for example, Complex>>#argument"

	<primitive: 57>
	^ self arcTanNonPrimitive
]

{ #category : 'mathematical functions' }
BoxedFloat64 >> cos [
	"Answer the cosine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 150>
	^ super cos
]

{ #category : 'mathematical functions' }
BoxedFloat64 >> exp [
	"Answer e raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 59>
	^ self expNonPrimitive
]

{ #category : 'accessing' }
BoxedFloat64 >> exponent [
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa between one and two  or between -1 and -2 (#significand).
	 Answer with the SmallInteger to whose power two is raised.
	Optional. See Object documentation whatIsAPrimitive.
	Note: invalid for infinities, NaN and zero.
	See also #signPart,  #significandAsInteger and #exponentPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. f significand . f exponent. f significand * (2 raisedToInteger: f exponent) = f } print ].
	"

	<primitive: 53>
	^super exponent
]

{ #category : 'accessing' }
BoxedFloat64 >> fractionPart [
	"Primitive. Answer a Float whose value is the difference between the 
	receiver and the receiver's asInteger value. Optional. See Object 
	documentation whatIsAPrimitive."

	<primitive: 52>
	^self - self truncated asFloat
]

{ #category : 'testing' }
BoxedFloat64 >> isDenormalized [
	"Denormalized numbers are only represented as BoxedFloat64"

	^ self partBits: [ :signBit :exponentBits :mantissaBits |
		exponentBits = 0 and: [mantissaBits ~=0]]
]

{ #category : 'testing' }
BoxedFloat64 >> isFinite [
	"simple, byte-order independent test for rejecting Not-a-Number and (Negative)Infinity"

	^(self - self) = 0.0
]

{ #category : 'testing' }
BoxedFloat64 >> isInfinite [
	"Return true if the receiver is positive or negative infinity."

	^ self = Infinity or: [self = NegativeInfinity]
]

{ #category : 'testing' }
BoxedFloat64 >> isNaN [
	"simple, byte-order independent test for Not-a-Number"

	^ self ~= self
]

{ #category : 'mathematical functions' }
BoxedFloat64 >> ln [
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 58>
	^ self lnNonPrimitive
]

{ #category : 'mathematical functions' }
BoxedFloat64 >> primSqrt [
	"Answer the square root of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive.
	Note: 
	-0.0 primSqrt 
	-0.0 sqrt
	both evaluate to -0.0
	"

	<primitive: 55>
	^Float nan
]

{ #category : 'mathematical functions' }
BoxedFloat64 >> raisedTo: exponent [
	"Answer the receiver raised to aNumber."

	self isNaN ifTrue: [ ^self ].
	^super raisedTo: exponent
]

{ #category : 'mathematical functions' }
BoxedFloat64 >> raisedToInteger: exponent [
	"Answer the receiver raised to aNumber."

	self isNaN ifTrue: [ ^self ].
	^super raisedToInteger: exponent
]

{ #category : 'accessing' }
BoxedFloat64 >> sign [
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	self isNaN ifTrue: [ self error: 'Can not handle Not-a-Number' ].
	^super sign
]

{ #category : 'mathematical functions' }
BoxedFloat64 >> sin [
	"Answer the sine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 56>
	^ self sinNonPrimitive
]

{ #category : 'printing' }
BoxedFloat64 >> storeOn: aStream base: base  [

	self isFinite
		ifTrue: [ super storeOn: aStream base: base ]
		ifFalse: [ self isNaN
			ifTrue: [aStream nextPutAll: 'Float nan']
			ifFalse: [self > 0.0
					ifTrue: [aStream nextPutAll: 'Float infinity']
					ifFalse: [aStream nextPutAll: 'Float infinity negated']]]
]

{ #category : 'mathematical functions' }
BoxedFloat64 >> timesTwoPower: anInteger [
	"Primitive. Answer with the receiver multiplied by 2 raised to the power of the argument.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 54>
	anInteger isInteger ifFalse: [ ^DomainError signal: '#timesTwoPower: only defined for Integer argument.'].
	self isFinite ifFalse: [^self].
	self isZero ifTrue: [^self].
	
	"Make sure that (2.0 raisedTo: Integer) can be safely used without overflow
	For example:
		Float fminNormalized * (2.0 raisedTo: 2000) = Float infinity.
	while:
		(Float fminNormalized timesTwoPower: 2000) = (2.0 raisedTo: 2000+Float emin)."	
	anInteger > Float emax ifTrue: [^(self timesTwoPower: Float emax) timesTwoPower: anInteger - Float emax].
	
	"In case of gradual underflow, timesTwoPower: is not exact, so greatest care must be taken
	because two consecutive timesTwoPower: might differ from a single one"
	anInteger < Float emin
		ifTrue: [
			| deltaToUnderflow |
			deltaToUnderflow := Float emin - self exponent max: Float emin.
			deltaToUnderflow >= 0 ifTrue: [
				"self is already near or past underflow, so don't care, result will be zero"
				deltaToUnderflow := Float emin].
			^(self timesTwoPower: deltaToUnderflow) timesTwoPower: anInteger - deltaToUnderflow].
	
	"If (2.0 raisedToInteger: anInteger) fit in a positive SmallInteger, then use faster SmallInteger conversion.
	Note that SmallInteger maxVal highBit = 30 in a 32 bits image, so 1 can be shifted 29 times."
	anInteger > -29 ifTrue: [
		anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat].
		anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat]].
	
	^ self * (2.0 raisedToInteger: anInteger)
]

{ #category : 'truncation and round off' }
BoxedFloat64 >> truncated [
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Raise an exception if no conversion to integer is possible, i.e. for Infinities and NaN.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 51>
	self isFinite ifFalse: [ ^self ].
	^ self partValues: [ :sign :exponent :mantissa |
		sign * (mantissa bitShift: exponent - 52) ]
]
