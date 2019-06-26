'From Cuis 5.0 of 7 November 2016 [latest update: #3810] on 25 June 2019 at 6:48:07 pm'!
!classDefinition: #ArithmeticMessageError category: #'Exceptions Kernel'!
Error subclass: #ArithmeticMessageError
	instanceVariableNames: 'receiver selector arguments'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!
!classDefinition: #NegativePowerError category: #'Exceptions Kernel'!
ArithmeticMessageError subclass: #NegativePowerError
	instanceVariableNames: 'base argument '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!
!classDefinition: #ZeroDivide category: #'Exceptions Kernel'!
ArithmeticMessageError subclass: #ZeroDivide
	instanceVariableNames: 'dividend '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!ZeroDivide commentStamp: '<historical>' prior: 0!
ZeroDivide may be signaled when a mathematical division by 0 is attempted.!


!ArithmeticMessageError methodsFor: 'initialization' stamp: 'jmv 6/25/2019 18:26:00'!
receiver: aNumber selector: aSymbol argument: otherNumber
	self receiver: aNumber selector: aSymbol arguments: {otherNumber}! !

!ArithmeticMessageError methodsFor: 'initialization' stamp: 'jmv 6/25/2019 18:26:00'!
receiver: aNumber selector: aSymbol arguments: aCollection
	receiver _ aNumber.
	selector _ aSymbol.
	arguments _ aCollection! !

!ArithmeticMessageError methodsFor: 'exceptionDescription' stamp: 'jmv 6/25/2019 18:26:00'!
defaultAction
	(receiver isFloat or: [ arguments anySatisfy: [ :a | a isFloat ]]) ifTrue: [
		^self floatErrorValue ].
	^ super defaultAction! !

!ArithmeticMessageError methodsFor: 'signaling' stamp: 'jmv 6/25/2019 18:26:00'!
floatErrorValue
	^ self subclassResponsibility! !

!ArithmeticMessageError methodsFor: 'signaling' stamp: 'jmv 6/25/2019 18:26:00'!
signalReceiver: aNumber selector: aSymbol argument: otherNumber

	^self
		receiver: aNumber selector: aSymbol argument: otherNumber;
		signal! !

!ArithmeticMessageError methodsFor: 'signaling' stamp: 'jmv 6/25/2019 18:26:00'!
signalReceiver: aNumber selector: aSymbol arguments: aCollection

	^self
		receiver: aNumber selector: aSymbol arguments: aCollection;
		signal! !


!NegativePowerError methodsFor: 'signaling' stamp: 'jmv 6/25/2019 18:41:08'!
floatErrorValue
	^ Float nan! !


!ZeroDivide methodsFor: 'signaling' stamp: 'jmv 6/25/2019 18:47:06'!
floatErrorValue
	receiver isZero ifTrue: [ ^ Float nan ].
	^ (receiver * arguments first) sign = -1
		ifTrue: [ Float negativeInfinity ]
		ifFalse: [ Float infinity ]! !


!Number methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:06:04'!
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
	0 = exponent ifTrue: [^ self class one].	"Special case of exponent=0"
	1 = exponent ifTrue: [^ self].	"Special case of exponent=1"
	0 = self ifTrue: [				"Special case of self = 0"
		exponent < 0
			ifTrue: [^ ZeroDivide new signalReceiver: self selector: #raisedTo: argument: exponent]
			ifFalse: [^ self]].
	^ (exponent * self ln) exp		"Otherwise use logarithms"! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:06:12'!
raisedToFraction: aFraction
	self isZero ifTrue: [
		aFraction negative ifTrue: [
			^ZeroDivide new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
		^ self ].
	self negative ifFalse: [
		^ (self ln * aFraction) exp ].
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
	^ (self negated ln * aFraction) exp negated! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:17:58'!
arCosh
	"Answer receiver's area hyperbolic cosine.
	That is the inverse function of cosh."

	self < 1 
		ifTrue: [^ Float nan].
	^self + 1 = self 
		ifTrue: [self abs ln + 2 ln]
		ifFalse: [((self squared - 1) sqrt + self) ln]! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:18:29'!
arTanh
	"Answer receiver's area hyperbolic tangent.
	That is the inverse function of tanh."

	self = 0.0 ifTrue: [^self].	"Handle negativeZero"
	self abs = 1 ifTrue: [^self copySignTo: Float infinity].
	self abs > 1 ifTrue: [^ Float nan].
	^((1 + self) / (1 - self)) ln / 2! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:18:55'!
arcSin
	"Answer the angle in radians."

	((self < -1.0) or: [self > 1.0]) ifTrue: [^ Float nan].
	^((self = -1.0) or: [self = 1.0])
		ifTrue: [Halfpi * self]
		ifFalse: [(self / (1.0 - (self * self)) sqrt) arcTan]! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:19:05'!
lnNonPrimitive
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	| expt n mant x div pow delta sum |

	"Taylor series"
	self <= 0.0 ifTrue: [^ Float nan].

	"get a rough estimate from binary exponent"
	expt := self exponent.
	n := Ln2 * expt.
	mant := self timesTwoPower: 0 - expt.

	"compute fine correction from mantinssa in Taylor series"
	"mant is in the range [0..2]"
	"we unroll the loop to avoid use of abs"
	x := mant - 1.0.
	div := 1.0.
	pow := delta := sum := x.
	x := x negated.  "x <= 0"
	[delta > (n + sum) ulp] whileTrue: [
		"pass one: delta is positive"
		div := div + 1.0.
		pow := pow * x.
		delta := pow / div.
		sum := sum + delta.
		"pass two: delta is negative"
		div := div + 1.0.
		pow := pow * x.
		delta := pow / div.
		sum := sum + delta].

	^ n + sum

	"Float e ln 1.0"! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:07:09'!
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."
	aPositiveInteger = 2 ifTrue: [
		^self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ DomainError signal: 'nth root only defined for positive Integer n.'].
	
	^self negative
		ifTrue: [
			aPositiveInteger even
				ifTrue: [ NegativePowerError new signalReceiver: self selector: #nthRoot: argument: aPositiveInteger ]
				ifFalse: [ (self negated nthRoot: aPositiveInteger) negated ]]
		ifFalse: [ self raisedTo: 1.0 / aPositiveInteger ]! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:04:46'!
sqrtNonPrimitive
	"Answer the square root of the receiver. "
	| exp guess delta |

	self = 0.0 ifTrue: [
		^self ].	"Answer 0.0 for 0.0, but -0.0 for -0.0. See IEEE 754 standard"

	self <= 0.0
		ifTrue: [
			^NegativePowerError new signalReceiver: self selector: #sqrtNonPrimitive arguments: {} ].

	"NaN and Infinity"
	self isFinite ifFalse: [
		^ self ].

	"Newton-Raphson"
	"first guess is half the exponent"
	exp _ self exponent // 2.
	guess _ self timesTwoPower: 0 - exp.
	[
		delta _ self - (guess * guess) / (guess * 2.0).
		delta abs >= guess ulp ]
	whileTrue: [
		guess _ guess + delta ].
	^ guess! !


!BoxedFloat64 methodsFor: 'arithmetic' stamp: 'jmv 6/25/2019 17:44:27'!
/ aNumber 
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 50>
	aNumber isZero ifTrue: [^ ZeroDivide new signalReceiver: self selector: #/ argument: aNumber].
	^ aNumber adaptToFloat: self andSend: #/! !


!SmallFloat64 methodsFor: 'arithmetic' stamp: 'jmv 6/25/2019 18:01:31'!
/ aNumber 
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 550>
	aNumber isZero ifTrue: [^ZeroDivide new signalReceiver: self selector: #/ argument: aNumber ].
	^ aNumber adaptToFloat: self andSend: #/! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:21:44'!
ln
	"This function is defined because super ln might overflow."
	| res |
	self <= 0 ifTrue: [^DomainError signal: 'ln is only defined for x > 0'].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [^self reciprocal ln negated].
	res := super ln.
	res isFinite ifTrue: [^res].
	^numerator ln - denominator ln! !

!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:21:48'!
log
	"
	(1/(10 raisedTo: 215)) log
	(1/((10 raisedTo: 215)+(10 raisedTo: 213))) log
	"
	| res |
	self <= 0 ifTrue: [^DomainError signal: 'log is only defined for x > 0'].

	"Integer answer if possible!!"
	numerator = 1
		ifTrue: [ ^denominator log negated ].

	"This because super log might overflow."
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [ ^self reciprocal log negated ].
	res := super log.
	res isFinite ifTrue: [^res].
	^numerator log - denominator log! !

!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:07:13'!
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."
	| guess |
	aPositiveInteger = 2 ifTrue: [
		^ self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ DomainError signal: 'nth root only defined for positive Integer n.'].

	(self negative and: [ aPositiveInteger even ]) ifTrue: [
		^ NegativePowerError new signalReceiver: self selector: #nthRoot: argument: aPositiveInteger  ].
	
	guess _ (numerator nthRootTruncated: aPositiveInteger) /
				(denominator nthRootTruncated: aPositiveInteger).
	(guess raisedTo: aPositiveInteger) = self ifTrue: [
		^ guess ].
	"There is no exact nth root, so answer a Float approximation"
	^ (self abs ln / aPositiveInteger) exp * self sign! !

!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:05:14'!
raisedToFraction: aFraction
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

!Fraction methodsFor: 'private' stamp: 'jmv 6/25/2019 17:57:12'!
setNumerator: n denominator: d

	d = 0
		ifTrue: [^ZeroDivide new signalReceiver: self selector: #setNumerator:denominator: arguments: {n.d}]
		ifFalse: [
			numerator _ n asInteger.
			denominator _ d asInteger abs. "keep sign in numerator"
			d < 0 ifTrue: [numerator _ numerator negated]]! !


!Integer methodsFor: 'arithmetic' stamp: 'jmv 6/25/2019 17:47:55'!
// aNumber 
	| q |
	aNumber = 0 ifTrue: [^ ZeroDivide new signalReceiver: self selector: #// argument: aNumber].
	self = 0 ifTrue: [^ 0].
	q _ self quo: aNumber.
	"Refer to the comment in Number>>#//."
	^(q negative
		ifTrue: [q * aNumber ~= self]
		ifFalse: [q = 0 and: [self negative ~= aNumber negative]])
			ifTrue: [q - 1"Truncate towards minus infinity."]
			ifFalse: [q]! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:21:59'!
ln
	"This function is defined because super ln might overflow."
	| res h |
	self <= 0 ifTrue: [^DomainError signal: 'ln is only defined for x > 0'].
	res := super ln.
	res isFinite ifTrue: [^res].
	h := self highBit.
	^2 ln * h + (self / (1 << h)) asFloat ln! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:22:05'!
log
	"This function is defined because super log might overflow.
	(10 raisedTo: 215) log
	((10 raisedTo: 215)+(10 raisedTo: 213)) log
	Answers an integer number if appropriate. Doing this is somewhat expensive. If you care about performance and not about using Floats, do 'aNumber asFloat log: another'.
	"
	| floatAnswer roundedAnswer |
	self <= 0 ifTrue: [^DomainError signal: 'log is only defined for x > 0'].
	floatAnswer _ self floatLog.
	roundedAnswer _ floatAnswer rounded.
	(10 raisedToInteger: roundedAnswer) = self
		ifTrue: [ ^roundedAnswer ].
	^floatAnswer! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:22:11'!
log: aNumber
	"Answer the log base aNumber of the receiver.
	(3 raisedTo: 215) log: 3
	((3 raisedTo: 215)+(3 raisedTo: 213)) log: 3
	Answers an integer number if appropriate. Doing this is somewhat expensive. If you care about performance and not about using Floats, do 'aNumber asFloat log: another'.
	"
	| floatAnswer roundedAnswer |
	self <= 0 ifTrue: [^DomainError signal: 'log is only defined for x > 0'].
	floatAnswer _ self asFloat log: aNumber.
	roundedAnswer _ floatAnswer rounded.
	(aNumber raisedToInteger: roundedAnswer) = self
		ifTrue: [ ^roundedAnswer ].
	^floatAnswer! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:07:17'!
nthRoot: aPositiveInteger
          "Answer the nth root of the receiver.
          Answer an Integer if root is exactly this Integer, else answer the Float nearest the exact root."
  
	| guess excess scaled nBits |
	self = 0 ifTrue: [ ^0 ].

	aPositiveInteger = 2 ifTrue: [
		^ self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ DomainError signal: 'nth root only defined for positive Integer n.'].

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

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:07:44'!
nthRootRounded: aPositiveInteger
    "Answer the integer nearest the nth root of the receiver.
http://stackoverflow.com/questions/39794338/precise-nth-root

http://smallissimo.blogspot.com.ar/2011/09/clarifying-and-optimizing.html
Ojo 32/64!!

Tambien
http://smallissimo.blogspot.com.ar/2011/09/reviewing-fraction-asfloat.html
"
    | guess |
    self = 0 ifTrue: [^0].
    self negative
        ifTrue: [
            aPositiveInteger even ifTrue: [ ^DomainError signal: 'Negative numbers don''t have even roots.' ].
            ^(self negated nthRootRounded: aPositiveInteger) negated].
    guess := self nthRootTruncated: aPositiveInteger.
    ^self * 2 > ((guess + 1 raisedTo: aPositiveInteger) + (guess raisedTo: aPositiveInteger))
        ifTrue: [guess + 1]
        ifFalse: [guess]! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:08:30'!
nthRootTruncated: aPositiveInteger
          "Answer the integer part of the nth root of the receiver."
          | guess guessToTheNthMinusOne nextGuess |
          self = 0 ifTrue: [^0].
          self negative
                  ifTrue: [
                          aPositiveInteger even ifTrue: [ ^DomainError signal: 'Negative numbers don''t have even roots.' ].
                          ^(self negated nthRootTruncated: aPositiveInteger) negated].
          guess := 1 bitShift: self highBitOfMagnitude + aPositiveInteger - 1 // aPositiveInteger.
          [
                  guessToTheNthMinusOne := guess raisedTo: aPositiveInteger - 1.
                  nextGuess := (aPositiveInteger - 1 * guess * guessToTheNthMinusOne + self) // (guessToTheNthMinusOne * aPositiveInteger).
                 nextGuess >= guess ] whileFalse:
                          [ guess := nextGuess ].
          ( guess raisedTo: aPositiveInteger) > self  ifTrue:
                          [ guess := guess - 1 ].
          ^guess! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:05:31'!
raisedToFraction: aFraction
	| root |
	self = 0 ifTrue: [ ^0 ].
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
	root _ self nthRootTruncated: aFraction denominator.
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction! !

!Integer methodsFor: 'private' stamp: 'jmv 6/25/2019 17:58:27'!
digitDiv: arg neg: ng 
	"Answer with an array of (quotient, remainder)."
	| quo rem ql d div dh dnh dl qhi qlo j l hi lo r3 a t |
	<primitive: 'primDigitDivNegative' module:'LargeIntegers'>
	arg = 0 ifTrue: [^ ZeroDivide new signalReceiver: self selector: #digitDiv:neg: arguments: {arg.ng}].
	"TFEI added this line"
	l _ self digitLength - arg digitLength + 1.
	l <= 0 ifTrue: [^ Array with: 0 with: self].
	"shortcut against #highBit"
	d _ 8 - arg lastDigit highBitOfMagnitude.
	div _ arg digitLshift: d.
	div _ div growto: div digitLength + 1.
	"shifts so high order word is >=128"
	rem _ self digitLshift: d.
	rem digitLength = self digitLength ifTrue: [rem _ rem growto: self digitLength + 1].
	"makes a copy and shifts"
	quo _ Integer new: l neg: ng.
	dl _ div digitLength - 1.
	"Last actual byte of data"
	ql _ l.
	dh _ div digitAt: dl.
	dnh _ dl = 1
				ifTrue: [0]
				ifFalse: [div digitAt: dl - 1].
	1 to: ql do: 
		[:k | 
		"maintain quo*arg+rem=self"
		"Estimate rem/div by dividing the leading to bytes of rem by dh."
		"The estimate is q = qhi*16+qlo, where qhi and qlo are nibbles."
		j _ rem digitLength + 1 - k.
		"r1 _ rem digitAt: j."
		(rem digitAt: j)
			= dh
			ifTrue: [qhi _ qlo _ 15
				"i.e. q=255"]
			ifFalse: 
				["Compute q = (r1,r2)//dh, t = (r1,r2)\\dh.  
				Note that r1,r2 are bytes, not nibbles.  
				Be careful not to generate intermediate results exceeding 13  
				bits."
				"r2 _ (rem digitAt: j - 1)."
				t _ ((rem digitAt: j)
							bitShift: 4)
							+ ((rem digitAt: j - 1)
									bitShift: -4).
				qhi _ t // dh.
				t _ (t \\ dh bitShift: 4)
							+ ((rem digitAt: j - 1)
									bitAnd: 15).
				qlo _ t // dh.
				t _ t \\ dh.
				"Next compute (hi,lo) _ q*dnh"
				hi _ qhi * dnh.
				lo _ qlo * dnh + ((hi bitAnd: 15)
								bitShift: 4).
				hi _ (hi bitShift: -4)
							+ (lo bitShift: -8).
				lo _ lo bitAnd: 255.
				"Correct overestimate of q.  
				Max of 2 iterations through loop -- see Knuth vol. 2"
				r3 _ j < 3
							ifTrue: [0]
							ifFalse: [rem digitAt: j - 2].
				[(t < hi
					or: [t = hi and: [r3 < lo]])
					and: 
						["i.e. (t,r3) < (hi,lo)"
						qlo _ qlo - 1.
						lo _ lo - dnh.
						lo < 0
							ifTrue: 
								[hi _ hi - 1.
								lo _ lo + 256].
						hi >= dh]]
					whileTrue: [hi _ hi - dh].
				qlo < 0
					ifTrue: 
						[qhi _ qhi - 1.
						qlo _ qlo + 16]].
		"Subtract q*div from rem"
		l _ j - dl.
		a _ 0.
		1 to: div digitLength do: 
			[:i | 
			hi _ (div digitAt: i)
						* qhi.
			lo _ a + (rem digitAt: l) - ((hi bitAnd: 15)
							bitShift: 4) - ((div digitAt: i)
							* qlo).
			rem digitAt: l put: lo - (lo // 256 * 256).
			"sign-tolerant form of (lo bitAnd: 255)"
			a _ lo // 256 - (hi bitShift: -4).
			l _ l + 1].
		a < 0
			ifTrue: 
				["Add div back into rem, decrease q by 1"
				qlo _ qlo - 1.
				l _ j - dl.
				a _ 0.
				1 to: div digitLength do: 
					[:i | 
					a _ (a bitShift: -8)
								+ (rem digitAt: l) + (div digitAt: i).
					rem digitAt: l put: (a bitAnd: 255).
					l _ l + 1]].
		quo digitAt: quo digitLength + 1 - k put: (qhi bitShift: 4)
				+ qlo].
	rem _ rem
				digitRshift: d
				bytes: 0
				lookfirst: dl.
	^ Array with: quo with: rem! !


!LargeNegativeInteger methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:05:55'!
sqrt
	"Answer the square root of the receiver."
	^NegativePowerError new signalReceiver: self selector: #sqrt arguments: {}! !


!SmallInteger methodsFor: 'arithmetic' stamp: 'jmv 6/25/2019 18:01:37'!
/ aNumber 
	"Primitive. This primitive (for /) divides the receiver by the argument
	and returns the result if the division is exact. Fail if the result is not a
	whole integer. Fail if the argument is 0 or is not a SmallInteger. Optional.
	No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 10>
	aNumber isZero ifTrue: [^ZeroDivide new signalReceiver: self selector: #/ argument: aNumber ].
	^(aNumber isMemberOf: SmallInteger)
		ifTrue: [(Fraction numerator: self denominator: aNumber) reduced]
		ifFalse: [super / aNumber]! !

!SmallInteger methodsFor: 'arithmetic' stamp: 'jmv 6/25/2019 18:01:48'!
quo: aNumber 
	"Primitive. Divide the receiver by the argument and answer with the 
	result. Round the result down towards zero to make it a whole integer. 
	Fail if the argument is 0 or is not a SmallInteger. Optional. See Object 
	documentation whatIsAPrimitive."
	<primitive: 13>
	aNumber = 0 ifTrue: [^ZeroDivide new signalReceiver: self selector: #quo: argument: aNumber ].
	(aNumber isMemberOf: SmallInteger)
		ifFalse: [^ super quo: aNumber].
	(aNumber = -1 and: [self = self class minVal])
		ifTrue: ["result is aLargeInteger" ^ self negated].
	self primitiveFailed! !

!SmallInteger methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2019 18:06:25'!
sqrt

	self positive ifTrue: [^super sqrt].
	^NegativePowerError new signalReceiver: self selector: #sqrt arguments: {}! !


!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 6/25/2019 17:43:40'!
/= anObject

	^self divideBy: anObject
		ifDivisorZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]
		ifBothZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]! !


!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 6/25/2019 17:43:52'!
/= anObject

	^self divideBy: anObject
		ifDivisorZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]
		ifBothZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 6/25/2019 18:09:04'!
maxExternalSemaphores: aSize
	"Changes the size of table where external semaphores are registered. 
	The size can only grow, and will always be the next power of two larger than the parameter.
	
	Setting this at any time other than start-up can potentially lose requests.
	 i.e. during the realloc new storage is allocated, t
	he old contents are copied and then pointers are switched. 
	 Requests occurring during copying won't be seen if they occur to indices already copied. 
	The intended use is to set the table to some adequate maximum at start-up"
	
	self isRunningCog ifFalse: [^0].
	"The vm-header field is a short, maximum 64k entries. Well, on most platforms anyways "
	(aSize < 0 or: [aSize > 16rFFFF]) ifTrue: [^Error signal: 'Must be in the range (0 to: 16rFFFF)'].
	^self vmParameterAt: 49 put: aSize! !

!methodRemoval: ZeroDivide class #dividend:!
ZeroDivide class removeSelector: #dividend:!
!methodRemoval: ZeroDivide class #signalWithDividend:!
ZeroDivide class removeSelector: #signalWithDividend:!
!methodRemoval: ZeroDivide #dividend!
ZeroDivide removeSelector: #dividend!
!methodRemoval: ZeroDivide #dividend:!
ZeroDivide removeSelector: #dividend:!
!methodRemoval: NegativePowerError #base:selector:argument:!
NegativePowerError removeSelector: #base:selector:argument:!
!methodRemoval: NegativePowerError #signalBase:selector:argument:!
NegativePowerError removeSelector: #signalBase:selector:argument:!
!classDefinition: #NegativePowerError category: #'Exceptions Kernel'!
ArithmeticMessageError subclass: #NegativePowerError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!NegativePowerError reorganize!
('signaling' floatErrorValue)
!

!classDefinition: #ZeroDivide category: #'Exceptions Kernel'!
ArithmeticMessageError subclass: #ZeroDivide
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!ZeroDivide reorganize!
('exceptionDescription' isResumable)
('signaling' floatErrorValue)
!

