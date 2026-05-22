"
Fraction provides methods for dealing with rational numbers like 1/3 as true fractions (not approximations as a Float 0.33333...). All public arithmetic operations answer reduced fractions, or Integers when the denominator is 1.

Examples: (note the parentheses required to get the right answers in Smalltalk):

(2/3) + (2/3)
(6/4)		 		"Fractions are reduced to the smallest numerator and denominator possible"
(1 / -3)			"Denominator is kept positive, the sign is always in the numerator"
(2/3) + (1/3)	"When the denominator reduces to 1, the answer is an Integer"

"
Class {
	#name : 'Fraction',
	#superclass : 'Number',
	#instVars : [
		'numerator',
		'denominator'
	],
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'instance creation' }
Fraction class >> numerator: numInteger denominator: denInteger  [
	"Answer an instance of me (numInteger/denInteger).
	NOTE: This primitive initialization method will not reduce improper fractions,
	so normal usage should be coded as, eg,
		(Fraction numerator: a denominator: b) reduced
	or, more simply, as
		a / b."

	^self new setNumerator: numInteger denominator: denInteger
]

{ #category : 'approximations' }
Fraction class >> pi1000Digits [
	"
	
	Integer pi1000Digits asNumber
	Integer pi1000Digits asNumber class
	Integer pi1000Digits asNumber / (10 raisedToInteger: Integer pi1000Digits size - 1) :: asFloat
	Integer pi1000Digits asNumber+1 / (10 raisedToInteger: Integer pi1000Digits size - 1) :: asFloat
	"

	^'3141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587006606315588174881520920962829254091715364367892590360011330530548820466521384146951941511609433057270365759591953092186117381932611793105118548074462379962749567351885752724891227938183011949129833673362440656643086021394946395224737190702179860943702770539217176293176752384674818467669405132000568127145263560827785771342757789609173637178721468440901224953430146549585371050792279689258923542019956112129021960864034418159813629774771309960518707211349999998372978049951059731732816096318595024459455346908302642522308253344685035261931188171010003137838752886587533208381420617177669147303598253490428755468731159562863882353787593751957781857780532171226806613001927876611195909216420198' .
]

{ #category : 'approximations' }
Fraction class >> piAsFraction: numberOfDecimals [
	"Answer a Fraction built with the first numberOfDigits decimal digits of Pi.
	Note: It will always be less than the Real Pi. It is not a rounded version of Pi, but a truncated version of pi.
	Fraction piAsFraction: 5
	"
	^(Fraction piDigitsAsInteger: numberOfDecimals) / (10 raisedToInteger: numberOfDecimals)
]

{ #category : 'approximations' }
Fraction class >> piDigitsAsInteger: numberOfDecimals [
	"Answer an integer number formed by the first numberOfDecimals+1 digits of pi.
	If numberOfDecimals=2, answer 314.
	If numberOfDecimals=4, answer 31415

	Fraction piDigitsAsInteger: 2
	Fraction piDigitsAsInteger: 4
	"
	| answer characterStream manyDigits |
	manyDigits := self pi1000Digits.
	numberOfDecimals < manyDigits size ifFalse: [
		^self error: 'Can not approximate pi with so many decimals' ].
	characterStream := manyDigits readStream.
	answer := characterStream next digitValue.
	numberOfDecimals timesRepeat: [
		answer := 10 * answer + characterStream next digitValue ].
	^answer
]

{ #category : 'arithmetic' }
Fraction >> * aNumber  [
	"Answer the result of multiplying the receiver by aNumber."
	| d1 d2 |
	aNumber isFraction ifTrue: 
		[d1 := numerator gcd: aNumber denominator.
		d2 := denominator gcd: aNumber numerator.
		(d2 = denominator and: [d1 = aNumber denominator])
			ifTrue: [^ numerator // d1 * (aNumber numerator // d2)].
		^ Fraction numerator: numerator // d1 * (aNumber numerator // d2)
				denominator: denominator // d2 * (aNumber denominator // d1)].
	^ aNumber adaptToFraction: self andSend: #*
]

{ #category : 'arithmetic' }
Fraction >> + aNumber  [
	"Answer the sum of the receiver and aNumber."
	| n d d1 d2 |
	aNumber isFraction ifTrue: 
		[d := denominator gcd: aNumber denominator.
		n := numerator * (d1 := aNumber denominator // d) + (aNumber numerator * (d2 := denominator // d)).
		d1 := d1 * d2.
		n := n // (d2 := n gcd: d).
		(d := d1 * (d // d2)) = 1 ifTrue: [^ n].
		^ Fraction numerator: n denominator: d].
	^ aNumber adaptToFraction: self andSend: #+
]

{ #category : 'arithmetic' }
Fraction >> - aNumber [
	"Answer the difference between the receiver and aNumber."
	aNumber isFraction ifTrue:
		[^ self + aNumber negated].
	^ aNumber adaptToFraction: self andSend: #-
]

{ #category : 'arithmetic' }
Fraction >> / aNumber [
	"Answer the result of dividing the receiver by aNumber."
	aNumber isFraction
		ifTrue: [^self * aNumber reciprocal].
	^ aNumber adaptToFraction: self andSend: #/
]

{ #category : 'comparing' }
Fraction >> < aNumber [
	aNumber isFraction ifTrue:
		[^ numerator * aNumber denominator < (aNumber numerator * denominator)].
	^ aNumber adaptToFraction: self andSend: #<
]

{ #category : 'comparing' }
Fraction >> <= aNumber [
	aNumber isFraction ifTrue:
		[^ numerator * aNumber denominator <= (aNumber numerator * denominator)].
	^ aNumber adaptToFraction: self andSend: #<=
]

{ #category : 'comparing' }
Fraction >> = aNumber [

	"Any object is equal to itself"
	self == aNumber ifTrue: [ ^ true ].

	aNumber isNumber ifFalse: [ ^ false ].
	aNumber isFraction
		ifTrue: [numerator = 0 ifTrue: [^ aNumber numerator = 0].
				^ (numerator * aNumber denominator) =
					(aNumber numerator * denominator)
				"Note: used to just compare num and denom,
					but this fails for improper fractions"].
	^ aNumber adaptToFraction: self andSend: #=
]

{ #category : 'converting' }
Fraction >> adaptToInteger: rcvr andSend: selector [
	"If I am involved in arithmetic with an Integer, convert it to a Fraction."
	^ rcvr asFraction perform: selector with: self
]

{ #category : 'converting' }
Fraction >> asFloat [
	"Answer a Float that closely approximates the value of the receiver.
	This implementation will answer the closest floating point number to the receiver.
	In case of a tie, it will use the IEEE 754 round to nearest even mode.
	In case of overflow, it will answer +/- Float infinity."

	| a b mantissa exponent floatMantissa hasTruncatedBits lostBit n ha hb hm |
	a := numerator abs.
	b := denominator.	"denominator is always positive"
	ha := a highBit.
	hb := b highBit.
	
	"Number of bits to keep in mantissa plus one to handle rounding."
	n := 1 + Float precision.

	"If both numerator and denominator are represented exactly as floating point number,
	float division is fastest."
	(ha < n and: [hb < n]) ifTrue: [^numerator asFloat / denominator asFloat].

	"Shift the fraction by a power of two exponent so as to obtain a mantissa with n bits.
	The first guess is approximate, the mantissa might have n+1 bits."
	exponent := ha - hb - n.
	exponent >= 0
		ifTrue: [b := b bitShift: exponent]
		ifFalse: [a := a bitShift: exponent negated].
	mantissa := a quo: b.
	hasTruncatedBits := a > (mantissa * b).
	hm := mantissa highBit.
	
	"Check for gradual underflow, in which case the mantissa will lose bits.
	Keep at least one bit to let underflow preserve the sign of zero."
	lostBit := Float emin - (exponent + hm - 1).
	lostBit > 0 ifTrue: [n := n - lostBit max: 1].

	"Remove excess bits in the mantissa."
	hm > n
		ifTrue:
			[exponent := exponent + hm - n.
			hasTruncatedBits := hasTruncatedBits or: [mantissa anyBitOfMagnitudeFrom: 1 to: hm - n].
			mantissa := mantissa bitShift: n - hm].

	"Check if mantissa must be rounded upward.
	The case of tie will be handled by Integer>>asFloat."
	(hasTruncatedBits and: [mantissa odd])
		ifTrue: [mantissa := mantissa + 1].

	floatMantissa := mantissa asFloat.
	self positive ifFalse: [floatMantissa := floatMantissa negated].
	^floatMantissa timesTwoPower: exponent
]

{ #category : 'converting' }
Fraction >> asFraction	 [
	"Answer with the receiver itself."
	^self
]

{ #category : 'mathematical functions' }
Fraction >> cubed [
	"See Fraction (Number) | cubed"
	^ Fraction numerator: numerator cubed denominator: denominator cubed
]

{ #category : 'arithmetic' }
Fraction >> denominator [
	^denominator
]

{ #category : 'mathematical functions' }
Fraction >> floorLog: radix [
	"Unlike super, this version is exact when radix is integer"
	
	| d n |
	radix isInteger ifFalse: [^super floorLog: radix].
	n := numerator floorLog: radix.
	d := denominator floorLog: radix.
	^(numerator * (radix raisedTo: d))
		< (denominator * (radix raisedTo: n))
		ifTrue: [n - d - 1]
		ifFalse: [n - d]
]

{ #category : 'comparing' }
Fraction >> hash [
	"Hash is reimplemented because = is implemented.
	Care is taken that a Fraction equal to a Float also have an equal hash"

	^ self asFloat hash
]

{ #category : 'testing' }
Fraction >> is: aSymbol [
	"Note: Senders might prefer #isFraction for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Fraction = aSymbol or: [ super is: aSymbol ]
]

{ #category : 'testing' }
Fraction >> isFraction [
	^ true
]

{ #category : 'printing' }
Fraction >> isLiteral [
	"
	1e-3 isLiteral
	(1/3) isLiteral
	"
	denominator
		ifMultipleOf2And5Do: [ :exponent2 :exponent5 |
			^true]
		otherwise: [
			^false]
]

{ #category : 'testing' }
Fraction >> isPowerOfTwo [
	^numerator = 1 and: [ denominator isPowerOfTwo ]
]

{ #category : 'mathematical functions' }
Fraction >> ln [
	"This function is defined because super ln might overflow."
	| res |
	self <= 0 ifTrue: [DomainError signal: 'ln is only defined for x > 0'].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [^self reciprocal ln negated].
	res := super ln.
	res isFinite ifTrue: [^res].
	^numerator ln - denominator ln
]

{ #category : 'mathematical functions' }
Fraction >> log [
	"
	(1/(10 raisedTo: 215)) log
	(1/((10 raisedTo: 215)+(10 raisedTo: 213))) log
	"
	| res |
	self <= 0 ifTrue: [^DomainError signal: 'log is only defined for x > 0'].

	"Integer answer if possible!"
	numerator = 1
		ifTrue: [ ^denominator log negated ].

	"This because super log might overflow."
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [ ^self reciprocal log negated ].
	res := super log.
	res isFinite ifTrue: [^res].
	^numerator log - denominator log
]

{ #category : 'mathematical functions' }
Fraction >> log: aNumber [
	"Answer the log base aNumber of the receiver.
	(1/(3 raisedTo: 215)) log: 3
	(1/((3 raisedTo: 215)+(3 raisedTo: 213))) log: 3
	"
	^numerator = 1
		ifTrue: [ (denominator log: aNumber) negated ]
		ifFalse: [ super log: aNumber ]
]

{ #category : 'mathematical functions' }
Fraction >> log2 [
	"This function is defined because super log2 might overflow."
	| res |
	self <= 0 ifTrue: [DomainError signal: 'log2 is only defined for x > 0'].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow.
	Also, give an integer answer if possible."
	numerator < denominator ifTrue: [^self reciprocal log2 negated].
	res := super log2.
	res isFinite ifTrue: [^res].
	^numerator log2 - denominator log2
]

{ #category : 'converting' }
Fraction >> moduloTwoPiAsFloat [
	"Answer the remainder of the division by 2 * pi, as a Floating Point number.
	Useful for periodic functions definded on Floats.
	Provides full Float precision of the remainder result, even for huge arguments.
	Evaluate comments below and compare results.
	See senders."
	"
	| bigNum |
	bigNum := 1e22 + (10202/10000).
	{'wrong!'. bigNum asFloat sin. bigNum asFloat cos} print.
	{'wrong!'. (bigNum+1) asFloat sin. (bigNum+1) asFloat cos} print.
	{'correct!'. bigNum moduloTwoPiAsFloat sin. bigNum moduloTwoPiAsFloat cos} print.
	{'correct!'. (bigNum+1) moduloTwoPiAsFloat sin. (bigNum+1) moduloTwoPiAsFloat cos} print.
	{'Reference:'.  0 sin. 0 cos} print.
	{'Reference:'.  1 sin. 1 cos} print.
	"
	"
	| bigNum |
	bigNum := 1e500 - (282532 / 100000).
	{'wrong!'. bigNum asFloat sin. bigNum asFloat cos} print.
	{'wrong!'. (bigNum+1) asFloat sin. (bigNum+1) asFloat cos} print.
	{'correct!'. bigNum moduloTwoPiAsFloat sin. bigNum moduloTwoPiAsFloat cos} print.
	{'correct!'. (bigNum+1) moduloTwoPiAsFloat sin. (bigNum+1) moduloTwoPiAsFloat cos} print.
	{'Reference:'.  0 sin. 0 cos} print.
	{'Reference:'.  1 sin. 1 cos} print.
	"
	"
	(1e22 \\ Float twoPi) print.
	(1e22+1 \\ Float twoPi) print.
	1e22 moduloTwoPiAsFloat print.
	(1e22+1) moduloTwoPiAsFloat print.
	(1e500 \\ Float twoPi) print.
	(1e500+1 \\ Float twoPi) print.
	1e500 moduloTwoPiAsFloat print.
	(1e500+1) moduloTwoPiAsFloat print.
	"

	| numberOfPiDecimals twoPiFraction |
	(self positive and: [ self <= (628/100) ]) ifTrue: [
		^self asFloat ].
	numberOfPiDecimals := ((numerator digitLength - denominator digitLength) * 256 log) ceiling + 21.
	twoPiFraction := (Fraction piAsFraction: numberOfPiDecimals) * 2.
	^ (self \\ twoPiFraction) asFloat.
]

{ #category : 'arithmetic' }
Fraction >> negated [
	^Fraction numerator: numerator negated denominator: denominator
]

{ #category : 'testing' }
Fraction >> negative [

	^numerator negative
]

{ #category : 'mathematical functions' }
Fraction >> nthRoot: aPositiveInteger [
	"Answer the nth root of the receiver."
	| guess |
	aPositiveInteger = 2 ifTrue: [
		^ self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ DomainError signal: 'nth root only defined for positive Integer n.'].

	(self negative and: [ aPositiveInteger even ]) ifTrue: [
		^ NegativePowerError new signalReceiver: self selector: #nthRoot: argument: aPositiveInteger  ].
	
	guess := (numerator nthRootTruncated: aPositiveInteger) /
				(denominator nthRootTruncated: aPositiveInteger).
	(guess raisedTo: aPositiveInteger) = self ifTrue: [
		^ guess ].
	"There is no exact nth root, so answer a Float approximation"
	^ (self abs ln / aPositiveInteger) exp * self sign
]

{ #category : 'arithmetic' }
Fraction >> numerator [
	^numerator
]

{ #category : 'testing' }
Fraction >> positive [

	^numerator positive
]

{ #category : 'printing' }
Fraction >> printAsLiteralIfAtAllPossibleOn: aStream [
	| n d base powers pow2 maximumBase |
	n := numerator.
	d := denominator.
	powers := Dictionary new.
	(pow2 := d lowBit - 1) = 0
		ifTrue: [
			base := 1]
		ifFalse: [
			base := 2.
			powers at: 2 put: pow2.
			d := d >> pow2].
	maximumBase := 36.
	Integer largePrimesUpTo: (d sqrtFloor max: 2) do: [ :p |
		| pow |
		(d rem: p) = 0
			ifTrue: [
				pow := 1.
				[((d := d quo: p) rem: p) = 0] whileTrue: [pow := pow + 1].
				base := base * p.
				base > maximumBase ifTrue: [
					"self halt: 'cannot print in base > ' , maximumBase printString."
					^self printOn: aStream.
					].
				powers at: p put: pow].
		p squared > d
			ifTrue: [ "Decomposition done, we can now print"
				| maximumPower b |
				d = 1 ifFalse: [base := base * d. powers at: d put: 1].
				base > maximumBase ifTrue: [
					"self halt: 'cannot print in base > ' , maximumBase printString."
					^self printOn: aStream.
					].
				base = 2 ifTrue: [base := 10. powers at: 5 put: 0].
				maximumPower := powers detectMax: [:each | each].
				powers keysAndValuesDo: [:k :v | n := n * (k raisedTo: maximumPower - v)].
				b := base.
				[maximumPower > 1 and: [b * base <= maximumBase]]
					whileTrue:
						[b := b * base.
						maximumPower := maximumPower - 1].
				n storeOn: aStream base: b.
				aStream nextPutAll: 'e-'.
				maximumPower storeOn: aStream.
				^self ]].
]

{ #category : 'printing' }
Fraction >> printAsLiteralOn: aStream [
	"
	{123e-12000} storeString
	{123e-12000} printString
	"
	self storeOn: aStream
]

{ #category : 'printing' }
Fraction >> printOn: aStream base: base [
	aStream isText
		ifTrue:
			[aStream nextPutAll: (numerator printStringBase: base) super; nextPut: $/; nextPutAll: (denominator printStringBase: base) sub.
			^ self].
	aStream nextPut: $(.
	numerator printOn: aStream base: base.
	aStream nextPut: $/.
	denominator printOn: aStream base: base.
	aStream nextPut: $).

]

{ #category : 'printing' }
Fraction >> printOn: aStream fractionDigits: placesDesired [
	"Same as super, but provides a faster implementation by inlining some Fraction protocol thus avoiding intermediate Fraction creation."
	
	| roundedFractionPart integerPart scaling |
	placesDesired > 0
		ifFalse: [self rounded printOn: aStream]
		ifTrue: [
			scaling := 10 raisedToInteger: placesDesired.
			integerPart := numerator abs quo: denominator.
			roundedFractionPart := (numerator abs - (integerPart * denominator)) * scaling * 2 + denominator quo: denominator * 2.
			roundedFractionPart = scaling
				ifTrue:
					[integerPart := integerPart + 1.
					roundedFractionPart := 0].
			"Don't print minus sign if result is rounded to zero"
			(numerator negative and: [integerPart > 0 or: [roundedFractionPart > 0]]) ifTrue: [aStream nextPut: $-].
			integerPart printOn: aStream.
			aStream nextPut: $..
			roundedFractionPart printOn: aStream base: 10 length: placesDesired padded: true].
]

{ #category : 'mathematical functions' }
Fraction >> raisedToFraction: aFraction [
	"Raise an exception if argument is not a Fraction"
	| root |
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
	root := (self numerator nthRootTruncated: aFraction denominator) /
			(self denominator nthRootTruncated: aFraction denominator).
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction
]

{ #category : 'mathematical functions' }
Fraction >> raisedToInteger: anInteger  [
	"See Number | raisedToInteger:"
	"Raise an exception if argument is not a anInteger"

	^ (numerator raisedToInteger: anInteger) / (denominator raisedToInteger: anInteger)
]

{ #category : 'mathematical functions' }
Fraction >> reciprocal  [
	"Refer to the comment in Number|reciprocal."

	^denominator / numerator
]

{ #category : 'private' }
Fraction >> reduced [
	| gcd numer denom |
	numerator = 0 ifTrue: [^0].
	gcd := numerator gcd: denominator.
	numer := numerator // gcd.
	denom := denominator // gcd.
	denom = 1 ifTrue: [^numer].
	^Fraction numerator: numer denominator: denom
]

{ #category : 'private' }
Fraction >> setNumerator: n denominator: d [

	d = 0
		ifTrue: [^ZeroDivide new signalReceiver: self selector: #setNumerator:denominator: arguments: {n.d}]
		ifFalse: [
			numerator := n asInteger.
			denominator := d asInteger abs. "keep sign in numerator"
			d < 0 ifTrue: [numerator := numerator negated]]
]

{ #category : 'mathematical functions' }
Fraction >> sqrt [
	| d n answer |
	n := numerator sqrt.
	d := denominator sqrt.
	"The #sqrt method in integer will only answer a Float if there's no exact square root.
	So, we need a float anyway."
	(n isInfinite or: [ d isInfinite ]) ifTrue: [
		^self asFloat sqrt ].
	answer := n / d.
	answer isNaN ifTrue: [
		^self asFloat sqrt ].
	^ answer
]

{ #category : 'mathematical functions' }
Fraction >> squared [
	"See Fraction (Number) | squared"
	^ Fraction numerator: numerator squared denominator: denominator squared
]

{ #category : 'printing' }
Fraction >> storeOn: aStream [
	
	"If possible, store as a literal of the form 9999e-99
	If denominator's prime factors are just 2 and 5, then we can be printed as a literal
	of the form 999999e-99"
	"
	123e-12 storeString
	123e-12 printString
	123e-12000 storeString
	123e-12000 printString
	(3/4) storeString
	(3/4) printString
	(-3/4) storeString
	(-3/4) printString
	(1/3) storeString
	(1/3) printString
	"
	denominator
		ifMultipleOf2And5Do: [ :exponent2 :exponent5 |
			| e f |
			exponent2 > exponent5
				ifTrue: [
					e := exponent2.
					f := 5 raisedToInteger: e-exponent5 ]
				ifFalse: [
					e := exponent5.
					f := 2 bitShift: e-exponent2-1 ].
			numerator*f storeOn: aStream base: 10.
			aStream nextPut: $e; nextPut: $-.
			e storeOn: aStream base: 10 ]
		otherwise: [
			super storeOn: aStream ]

]

{ #category : 'printing' }
Fraction >> storeOn: aStream base: base [

	aStream nextPut: $(.
	numerator storeOn: aStream base: base.
	aStream nextPut: $/.
	denominator storeOn: aStream base: base.
	aStream nextPut: $).

]

{ #category : 'truncation and round off' }
Fraction >> truncated [
	^numerator quo: denominator
]
