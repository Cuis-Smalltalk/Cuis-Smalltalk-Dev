"
My instances represent 64-bit Floats whose exponent fits in 8 bits as immediate objects. This representation is only available on 64-bit systems, not 32-bit systems.
"
Class {
	#name : 'SmallFloat64',
	#superclass : 'Float',
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'instance creation' }
SmallFloat64 class >> basicNew [
	self error: 'SmallFloat64s can only be created by performing arithmetic'
]

{ #category : 'instance creation' }
SmallFloat64 class >> basicNew: anInteger [
	^self basicNew
]

{ #category : 'arithmetic' }
SmallFloat64 >> * aNumber  [
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 549>
	^ aNumber adaptToFloat: self andSend: #*
]

{ #category : 'arithmetic' }
SmallFloat64 >> + aNumber  [
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 541>
	^ aNumber adaptToFloat: self andSend: #+
]

{ #category : 'arithmetic' }
SmallFloat64 >> - aNumber  [
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 542>
	^ aNumber adaptToFloat: self andSend: #-
]

{ #category : 'arithmetic' }
SmallFloat64 >> / aNumber  [
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 550>
	aNumber isZero ifTrue: [^ZeroDivide new signalReceiver: self selector: #/ argument: aNumber ].
	^ aNumber adaptToFloat: self andSend: #/
]

{ #category : 'comparing' }
SmallFloat64 >> < aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 543>
	^ aNumber adaptToFloat: self andSend: #<
]

{ #category : 'comparing' }
SmallFloat64 >> <= aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 545>
	^ aNumber adaptToFloat: self andSend: #<=
]

{ #category : 'comparing' }
SmallFloat64 >> = aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 547>
	aNumber isNumber ifFalse: [^ false].
	^ aNumber adaptToFloat: self andSend: #=
]

{ #category : 'comparing' }
SmallFloat64 >> > aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 544>
	^ aNumber adaptToFloat: self andSend: #>
]

{ #category : 'comparing' }
SmallFloat64 >> >= aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 546>
	^ aNumber adaptToFloat: self andSend: #>=
]

{ #category : 'comparing' }
SmallFloat64 >> ~= aNumber  [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 548>
	^super ~= aNumber
]

{ #category : 'mathematical functions' }
SmallFloat64 >> arcTan [
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive.
	Note: If the purpose is to recover the angle of some vector, prefer #arcTan:
		See, for example, Complex>>#argument"

	<primitive: 557>
	^self arcTanNonPrimitive
]

{ #category : 'mathematical functions' }
SmallFloat64 >> cos [
	"Answer the cosine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 560>
	^ super cos
]

{ #category : 'mathematical functions' }
SmallFloat64 >> exp [
	"Answer e raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 559>
	^ self expNonPrimitive
]

{ #category : 'accessing' }
SmallFloat64 >> exponent [
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa between one and two (#significand).
	 Answer with the SmallInteger to whose power two is raised.
	Optional. See Object documentation whatIsAPrimitive.
	Note: invalid for infinities, NaN and zero.
	See comment at BoxedFloat64"

	<primitive: 553>
	^super exponent
]

{ #category : 'accessing' }
SmallFloat64 >> fractionPart [
	"Primitive. Answer a Float whose value is the difference between the 
	receiver and the receiver's asInteger value. Optional. See Object 
	documentation whatIsAPrimitive."

	<primitive: 552>
	^self - self truncated asFloat
]

{ #category : 'comparing' }
SmallFloat64 >> identityHash [
	"Answer an integer unique to the receiver."
	<primitive: 171>
	^self primitiveFailed
]

{ #category : 'mathematical functions' }
SmallFloat64 >> ln [
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 558>
	^ self lnNonPrimitive
]

{ #category : 'mathematical functions' }
SmallFloat64 >> primSqrt [
	"Answer the square root of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 555>
	^Float nan
]

{ #category : 'mathematical functions' }
SmallFloat64 >> profilerFriendlyPrimTimesTwoPower: anInteger  [
	"
	This is an example on proper primitive reporting in AndreasSystemProfiler.
	See senders.
	"

	<primitive: 554>
	^nil
]

{ #category : 'mathematical functions' }
SmallFloat64 >> profilerFriendlyTimesTwoPower: anInteger  [

	"This is an example on proper primitive reporting in AndreasSystemProfiler.
	It is a reimplementation of #timesTwoPower: in a Profiler friendly way.

	Compare the results of 
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]].
	and
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 10000]].

	See #profilerFriendlyCall:
	"
	
	| primResult |
	primResult := self profilerFriendlyCall: [
		self profilerFriendlyPrimTimesTwoPower: anInteger ].
	primResult ifNotNil: [ :result | ^result ].
	
	"Make sure that (2.0 raisedTo: Integer) can be safely used without overflow
	For example:
		Float fminNormalized * (2.0 raisedTo: 2000) = Float infinity.
	while:
		(Float fminNormalized timesTwoPower: 2000) = (2.0 raisedTo: 2000+Float emin)."	
	anInteger > Float emax ifTrue: [^(self timesTwoPower: Float emax) timesTwoPower: anInteger - Float emax].
	
	"In case of gradual underflow, timesTwoPower: is not exact, so greatest care must be taken
	because two consecutive timesTwoPower: might differ from a single one"
	anInteger < Float emin
		ifTrue:
			[| deltaToUnderflow |
			deltaToUnderflow := Float emin - self exponent max: Float emin.
			deltaToUnderflow >= 0 ifTrue:
				["self is already near or past underflow, so don't care, result will be zero"
				deltaToUnderflow := Float emin].
			^(self timesTwoPower: deltaToUnderflow) timesTwoPower: anInteger - deltaToUnderflow].
	
	"If (2.0 raisedToInteger: anInteger) fit in a positive SmallInteger, then use faster SmallInteger conversion.
	Note that SmallInteger maxVal highBit = 30 in a 32 bits image, so 1 can be shifted 29 times."
	anInteger > -29 ifTrue: [
		anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat].
		anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat]].
	
	^ self * (2.0 raisedToInteger: anInteger)
]

{ #category : 'copying' }
SmallFloat64 >> shallowCopy [
	"Answer the receiver, because SmallFloat64s are unique."
	^self
]

{ #category : 'mathematical functions' }
SmallFloat64 >> sin [
	"Answer the sine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 556>
	^ self sinNonPrimitive
]

{ #category : 'mathematical functions' }
SmallFloat64 >> timesTwoPower: anInteger [
	"Primitive. Answer with the receiver multiplied by 2 raised to the power of the argument.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 554>
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
SmallFloat64 >> truncated [
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Raise an exception if no conversion to integer is possible, i.e. for Infinities and NaN.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 551>

	^ self partValues: [ :sign :exponent :mantissa |
		sign * (mantissa bitShift: exponent - 52) ]
]
