'From Cuis 5.0 [latest update: #3830] on 2 August 2019 at 11:49:09 am'!

!BoxedFloat64 methodsFor: 'arithmetic' stamp: 'jmv 8/2/2019 11:02:05'!
arg
	"Compatibility with Complex numbers."
	self isNaN ifTrue: [^self].
	^super arg! !

!BoxedFloat64 methodsFor: 'arithmetic' stamp: 'jmv 8/2/2019 11:02:02'!
argument
	"Compatibility with Complex numbers."
	self isNaN ifTrue: [^self].
	^super argument! !

!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 8/1/2019 19:23:08'!
raisedTo: exponent
	"Answer the receiver raised to aNumber."

	self isNaN ifTrue: [ ^self ].
	^super raisedTo: exponent! !

!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 10:36:02'!
raisedToInteger: exponent
	"Answer the receiver raised to aNumber."

	self isNaN ifTrue: [ ^self ].
	^super raisedToInteger: exponent! !

!BoxedFloat64 methodsFor: 'testing' stamp: 'jmv 8/1/2019 16:53:05'!
sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	self isNaN ifTrue: [ self error: 'Can not handle Not-a-Number' ].
	^super sign! !


!Number methodsFor: 'mathematical functions' stamp: 'jmv 8/1/2019 19:23:05'!
raisedTo: exponent
	"Answer the receiver raised to aNumber."

	exponent isInteger ifTrue: [
		"Do the special case of integer power"
		^ self raisedToInteger: exponent].
	exponent isFraction ifTrue: [
		"Special case for fraction power by Nicolas Cellier:
		If aNumber is a fraction, but result must be a Float, learn it as quickly as possible, and give quick Float answer
		Allows evaluating:
		(2009/2000) raisedTo: (3958333/100000)
		"
		^ self raisedToFraction: exponent].
	self < 0 ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedTo: argument: exponent].
	0.0 = exponent ifTrue: [^ self class one]. 	"Special case of exponent=0"
	1.0 = exponent ifTrue: [^ self].					"Special case of exponent=1"
	0 = self ifTrue: [									"Special case of self = 0"
		exponent isNaN ifTrue: [ ^exponent ].
		^exponent < 0
			ifTrue: [(self raisedTo: exponent negated) reciprocal]
			ifFalse: [self]].
	^ (exponent * self ln) exp							"Otherwise use logarithms"! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 09:47:02'!
raisedToFraction: aFraction
	"Raise an exception if argument is not a Fraction"
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	self isZero ifTrue: [
		aFraction negative ifTrue: [^ (self raisedToFraction: aFraction negated) reciprocal].
		^ self ].
	self negative ifFalse: [
		^ (self ln * aFraction) exp ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
	^ (self negated ln * aFraction) exp negated! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 11:47:20'!
raisedToInteger: exponent
	"The 0 raisedToInteger: 0 is an special case. In some contexts must be 1 and in others must
	be handled as an indeterminate form.
	Maybe further discussion is required on this topic."
	
	| bitProbe result |

	exponent negative ifTrue: [^self raisedToNegativeInteger: exponent ].
	exponent = 0 ifTrue: [^ self class one].
	exponent = 1 ifTrue: [^ self].

	bitProbe := 1 bitShift: exponent highBit - 1.
 	result := self class one.
 	[
		(exponent bitAnd: bitProbe) = 0 ifFalse: [result := result * self].
		bitProbe := bitProbe bitShift: -1.
		bitProbe > 0 ]
			whileTrue: [
				result := result * result].
	^result! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 10:46:33'!
arcTan: denominator
	"Answer the angle in radians.
	Implementation note: use sign in order to catch cases of negativeZero"

	self isNaN ifTrue: [ ^self ].
	denominator isNaN ifTrue: [ ^denominator class nan ].	"if Complex, answer complex nan"
	^self = 0.0
		ifTrue: [denominator sign >= 0
			ifTrue: [ 0.0 ]
			ifFalse: [ self sign >= 0
				ifTrue: [ Pi ]
				ifFalse: [ Pi negated ]]]
		ifFalse: [denominator = 0.0
			ifTrue: [self > 0.0
				ifTrue: [ Halfpi ]
				ifFalse: [ Halfpi negated ]]
			ifFalse: [denominator > 0.0
				ifTrue: [ (self / denominator) arcTan ]
				ifFalse: [self > 0.0
					ifTrue: [ ((self / denominator) arcTan) + Pi ]
					ifFalse: [ ((self / denominator) arcTan) - Pi ]]]]! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 10:49:29'!
copySignTo: aNumber
	"Return a number with same magnitude as aNumber and same sign as self.
	Implementation note: take care of Float negativeZero, which is considered as having a negative sign."

	self isNaN ifTrue: [ ^self ].
	(self > 0.0 or: [(self at: 1) = 0]) ifTrue: [^ aNumber abs].
	^aNumber withNegativeSign! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 8/1/2019 18:32:12'!
smoothStep
	"A Function that is
		x <= -1 		-> 		1
		-1 < x < 1 	->			smooth differentiable transition from 1 to 0
		1 <= x 		-> 		0
	
	It is a 'smooth' version of an #negative
	Has countless applications. For example in image and signal processing, but also in other fields."
	"
	| g |
	Feature require: 'Morphic-Widgets-Extras'.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < 1 ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothStep ] color: Color red.
	(g embeddedInMorphicWindowLabeled: 'graph') openInWorld
	"
	self isNaN ifTrue: [ ^self ].
	self > -1 ifFalse: [ ^ 1 ].
	1 > self ifFalse: [ ^ 0 ].
	^ (self +2 * Float halfPi) sin * 0.5 + 0.5! !


!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 8/1/2019 19:10:12'!
timesTwoPower: anInteger
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
	
	^ self * (2.0 raisedToInteger: anInteger)! !

!BoxedFloat64 methodsFor: 'truncation and round off' stamp: 'jmv 8/2/2019 09:34:43'!
truncated
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Raise an exception if no conversion to integer is possible, i.e. for Infinities and NaN.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 51>

	^ self partValues: [ :sign :exponent :mantissa |
		sign * (mantissa bitShift: exponent - 52) ]! !


!SmallFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 8/1/2019 19:10:21'!
timesTwoPower: anInteger
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
	
	^ self * (2.0 raisedToInteger: anInteger)! !

!SmallFloat64 methodsFor: 'truncation and round off' stamp: 'jmv 8/2/2019 09:34:46'!
truncated
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Raise an exception if no conversion to integer is possible, i.e. for Infinities and NaN.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 551>

	^ self partValues: [ :sign :exponent :mantissa |
		sign * (mantissa bitShift: exponent - 52) ]! !


!Fraction methodsFor: 'mathematical' stamp: 'jmv 8/2/2019 10:07:49'!
reciprocal 
	"Refer to the comment in Number|reciprocal."

	^denominator / numerator! !

!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 09:47:13'!
raisedToFraction: aFraction
	"Raise an exception if argument is not a Fraction"
	| root |
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
	root _ (self numerator nthRootTruncated: aFraction denominator) /
			(self denominator nthRootTruncated: aFraction denominator).
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction! !

!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 10:05:14'!
raisedToInteger: anInteger 
	"See Number | raisedToInteger:"
	"Raise an exception if argument is not a anInteger"

	^ (numerator raisedToInteger: anInteger) / (denominator raisedToInteger: anInteger)! !


!Integer methodsFor: 'arithmetic' stamp: 'jmv 8/1/2019 17:26:46'!
// aNumber 
	| q |
	aNumber = 0 ifTrue: [^ ZeroDivide new signalReceiver: self selector: #// argument: aNumber].
	aNumber isInteger ifFalse: [ ^super // aNumber ].
	self = 0 ifTrue: [^ 0].
	q _ self quo: aNumber.
	"Refer to the comment in Number>>#//."
	^(q negative
		ifTrue: [q * aNumber ~= self]
		ifFalse: [q = 0 and: [self negative ~= aNumber negative]])
			ifTrue: [q - 1"Truncate towards minus infinity."]
			ifFalse: [q]! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 10:56:57'!
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver.
	Answer an Integer if root is exactly this Integer, else answer the Float nearest the exact root."
  
	| guess excess scaled nBits |
	aPositiveInteger = 2 ifTrue: [
		^ self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ DomainError signal: 'nth root only defined for positive Integer n.'].

	self = 0 ifTrue: [ ^0 ].

	(self negative and: [ aPositiveInteger even ]) ifTrue: [
		^  NegativePowerError new signalReceiver: self selector: #nthRoot: argument: aPositiveInteger  ].

	guess _ self nthRootRounded: aPositiveInteger.
	excess _ (guess raisedTo: aPositiveInteger) - self.
	excess = 0 ifTrue: [ ^ guess ].

	nBits _ Float precision - guess highBitOfMagnitude.
	nBits <= 0 ifTrue: [ ^(Fraction numerator: guess * 4 - excess sign denominator: 4) asFloat].

	scaled _ self << (nBits * aPositiveInteger).
	guess _ scaled nthRootRounded: aPositiveInteger.
	excess _ (guess raisedTo: aPositiveInteger) - scaled.
	^(Fraction numerator: guess * 4 - excess sign denominator: 1 << (nBits + 2)) asFloat! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 8/2/2019 09:47:05'!
raisedToFraction: aFraction
	"Raise an exception if argument is not a Fraction"
	| root |
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	self = 0 ifTrue: [ ^0 ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
	root _ self nthRootTruncated: aFraction denominator.
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction! !

