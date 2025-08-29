'From Cuis7.5 [latest update: #7456] on 28 August 2025 at 3:15:13 pm'!

!Number methodsFor: 'converting' library: #MinimalKernel03Float stamp: 'di 11/6/1998 13:21'!
adaptToFloat: rcvr andSend: selector 
	"If I am involved in arithmetic with a Float, convert me to a Float."
	^ rcvr perform: selector with: self asFloat! !


!Float methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'jmv 11/21/2018 17:04:24'!
hash
	"Hash is reimplemented because = is implemented."

	| asInteger |
	self isFinite ifTrue: [
		asInteger := self truncated.
		"See Integer>>#hash, LargePositiveInteger>>#hash and LargeNegativeInteger>>#hash"
		"Very big integers will answer true when asked #= with their own conversion to Float,
		But that Float will #asInteger to a different integer. Use Float hash in those cases, to ensure equal hash value.
		In addition, there is a performance improvement: the hash in LargeIntegers could
		get very slow for very big numbers"
		(asInteger = self and: [ asInteger abs < Float maxExactInteger ]) ifTrue: [
			^ asInteger hash ]].
	"Both words of the float are used. (The bitShift:'s ensure that the intermediate results do not become a large integer.)
	Care is taken to answer same hash as an equal Integer."
	^ ((self basicAt: 1) bitShift: -4) +
	   ((self basicAt: 2) bitShift: -4)! !

!Float methodsFor: 'mathematical functions' library: #MinimalKernel03Float stamp: 'tao 10/15/97 14:23'!
reciprocalLogBase2
	"optimized for self = 10, for use in conversion for printing"

	^ self = 10.0
		ifTrue: [Ln2 / Ln10]
		ifFalse: [Ln2 / self ln]! !

!Float methodsFor: 'converting' library: #MinimalKernel03Float stamp: 'di 11/6/1998 13:07'!
adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with an Integer, convert it to a Float."
	^ rcvr asFloat perform: selector with: self! !

!Float methodsFor: 'converting' library: #MinimalKernel03Float stamp: 'jmv 12/26/2018 14:33:41'!
partBits: aThreeArgumentBlock
	"
	Extract the bits for Sign, Mantissa and Exponent parts of the floating point representation.
	Just extract the bits. Do not add implicit bit. Do not correct denormals. Do not subtract exponent bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	| signBit exponentBits mantissaBits leastSignificativeWord mostSignificativeWord |

	mostSignificativeWord := self basicAt: 1.
	leastSignificativeWord := self basicAt: 2.
	signBit := mostSignificativeWord bitShift: -31 .
	exponentBits := (mostSignificativeWord bitShift: -20 ) bitAnd: 16r7FF.
	mantissaBits := ((mostSignificativeWord bitAnd: 16r000FFFFF) bitShift: 32) + leastSignificativeWord.

	"Evaluate the block"
	^aThreeArgumentBlock value: signBit value: exponentBits value: mantissaBits! !

!Float methodsFor: 'converting' library: #MinimalKernel03Float stamp: 'jmv 8/30/2010 23:39'!
partValues: aThreeArgumentBlock
	^ self
		partValues: aThreeArgumentBlock
		ifInfinite: [ self error: 'Can not handle infinity' ]
		ifNaN: [ self error: 'Can not handle Not-a-Number' ].! !

!Float methodsFor: 'converting' library: #MinimalKernel03Float stamp: 'jmv 12/20/2024 16:41:27'!
partValues: aThreeArgumentBlock ifInfinite: infinityBlock ifNaN: nanBlock
	"
	Float pi hex print
	Float pi partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	0.0 partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	For 0.0, exponent will be the minimum possible, i.e.  -1023, and mantissa will be 0.
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits |
		| sign exponent mantissa |

		"Extract the sign"
		sign := signBit = 0 ifTrue: [1] ifFalse: [-1].

		"Special cases: infinites and NaN"
		exponentBits = 16r7FF ifTrue: [
			^mantissaBits = 0
				ifTrue: [ infinityBlock valueWithPossibleArgument: self ]
				ifFalse: [ nanBlock valueWithPossibleArgument: self and: mantissaBits ]].

		"Unbias exponent: 16r3FF is bias"
		exponent := exponentBits - 16r3FF.
	
		exponentBits ~= 0
			ifTrue: [
				"Add back implicit leading 1 in fraction."
				mantissa := 16r0010000000000000 bitOr: mantissaBits ]
			ifFalse: [
				"If expPart = 0, I am +/-zero or a denormal value. In such cases, no implicit leading bit in mantissa."
				mantissa := mantissaBits.
				exponent := exponent + 1 ].

		"Evaluate the block"
		aThreeArgumentBlock value: sign value: exponent value: mantissa
	]! !

!Float methodsFor: 'testing' library: #MinimalKernel03Float stamp: 'jmv 4/8/2019 09:43:05'!
isDenormalized
	"Denormalized numbers are only represented as BoxedFloat64"

	^ false! !

!Float methodsFor: 'testing' library: #MinimalKernel03Float stamp: 'jmv 8/1/2019 16:43:28'!
isFinite
	"Infinities and Not a Number are only represented as BoxedFloat64"

	^ true! !

!Float methodsFor: 'testing' library: #MinimalKernel03Float stamp: 'jmv 8/2/2019 09:22:50'!
isInfinite
	"Infinities are only represented as BoxedFloat64"

	^ false
! !

!Float methodsFor: 'testing' library: #MinimalKernel03Float stamp: 'jmv 8/1/2019 16:40:18'!
isNaN
	"Not a Number are only represented as BoxedFloat64"

	^ false! !

!Float methodsFor: 'testing' library: #MinimalKernel03Float stamp: ''!
isZero
	^self = 0.0! !

!Float methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'ar 6/20/2010 14:28'!
at: index 
	"Avoid primitive in Object>>at:"

	^self basicAt: index! !

!Float methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'ar 6/20/2010 14:28'!
at: index put: value 
	"Avoid primitive in Object>>at:put:"

	^self basicAt: index put: value! !

!Float methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'eem 4/20/2009 09:34'!
basicAt: index
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. Do not override in a subclass. See 
	Object documentation whatIsAPrimitive.

	This version of basicAt: is specifically for floats, answering the most significant
	word for index 1 and the least significant word for index 2.  This alows the VM
	to store floats in whatever order it chooses while it appears to the image that
	they are always in big-endian/PowerPC order."

	<primitive: 38>
	| ec |
	ec == nil ifTrue: "primitive not implemented; floats are in big-endian/PowerPC order."
		[^super basicAt: index].
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!Float methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'eem 4/20/2009 09:35'!
basicAt: index put: value
	"Primitive. Assumes receiver is indexable. Store the second argument 
	value in the indexable element of the receiver indicated by index. Fail 
	if the index is not an Integer or is out of bounds. Or fail if the value is 
	not of the right type for this kind of collection. Answer the value that 
	was stored. Essential. Do not override in a subclass. See Object 
	documentation whatIsAPrimitive.

	This version of basicAt: is specifically for floats, answering the most significant
	word for index 1 and the least significant word for index 2.  This alows the VM
	to store floats in whatever order it chooses while it appears to the image that
	they are always in big-endian/PowerPC order."

	<primitive: 39>
	| ec |
	ec == nil ifTrue: "primitive not implemented; floats are in big-endian/PowerPC order."
		[^super basicAt: index put: value].
	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self basicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !

!Float methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'jmv 12/26/2018 14:33:27'!
exponentPart
	"
	Exponent part of the floating point representation.
	Valid for any floating point number (except zeros, infinities and NaNs).
	Includes correction of stored exponent bits for denormals (where it acts as a label, not a real exponent).
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self partValues: [ :sign :exponent :mantissa | exponent ]! !

!Float methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'jmv 12/26/2018 14:28:48'!
mantissaPart
	"Equivalent to #significandAsInteger."
	"The actual bits in the mantissa of the receiver, as an integer, including the implicit leading 1 if appropriate.
	Does not include the sign.
	See #exponentPart and #signPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f mantissaPart * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self partValues: [ :sign :exponent :mantissa | mantissa ]! !

!Float methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'jmv 12/20/2024 16:45:10'!
nextTowardsZero
	"Answer the Float with largest magnitude but smaller than ours, with the same sign.
	Only for finite, non zero numbers."

	^ self partValues: [ :sign :myExponent :myMantissa |
		| exponent mantissa |
		myMantissa isZero ifTrue: [ self error: 'Not for zero values' ].
		(myMantissa = 16r10000000000000 and: [myExponent > Float emin])
			ifTrue: [
				mantissa := 16r1FFFFFFFFFFFFF.
				exponent := myExponent -1 ]
			ifFalse: [
				mantissa := myMantissa-1.
				exponent := myExponent ].
		Float signPart: sign mantissaPart: mantissa exponentPart:  exponent ]! !

!Float methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'jmv 4/5/2019 16:14:46'!
sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0.
	Handle IEEE754 negative-zero by reporting a sign of -1
	Warning!! This makes Float negativeZero the only number in the system such that
		x sign negated = x negated sign
	evaluates to false!!
	This precludes the simpler implementation in #signPart
	0.0 sign  ->  0
	0.0 signPart   ->  1
	-0.0 sign   ->  -1
	-0.0 signPart   ->  -1
	"

	"Negative number or -0.0"
	self signBit = 1 ifTrue: [ ^ -1 ].

	"Zero"
	self = 0.0 ifTrue: [ ^ 0 ].

	"Positive number otherwise"
	^ 1! !

!Float methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'jmv 4/5/2019 16:14:21'!
signPart
	"The sign of the mantissa.
	1 means positive number or 0.0
	-1 means negative number or -0.0
	See #mantissaPart and #exponentPart"
	"
	| f |
	f := -2.0.
	(f signPart * f mantissaPart * (2 raisedToInteger: f exponentPart-52)) asFloat.
	"
	^self partValues: [ :sign :exponent :mantissa | sign ]! !

!Float methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'jmv 12/26/2018 15:00:07'!
significandAsInteger
	"The actual bits in the mantissa of the receiver, as an integer, including the implicit leading 1 if appropriate.
	See #mantissaPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self mantissaPart! !

!Float methodsFor: 'printing' library: #MinimalKernel03Float stamp: 'nice 4/20/2010 22:48'!
absPrintExactlyOn: aStream base: base
	"Print my value on a stream in the given base.  Assumes that my value is strictly
	positive; negative numbers, zero, and NaNs have already been handled elsewhere.
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	https://www.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf
	This version guarantees that the printed representation exactly represents my value
	by using exact integer arithmetic.
	See also #absPrintOn:base:mantissaSignificantBits:"

	| significand exp baseExpEstimate r s mPlus mMinus scale roundingIncludesLimits d tc1 tc2 fixedFormat decPointCount slowbit shead |
	self isInfinite ifTrue: [aStream nextPutAll: 'Infinity'. ^ self].
	significand := self significandAsInteger.
	roundingIncludesLimits := significand even.
	"What follows is equivalent, but faster than
		exp := self exponentPart - 52."
	exp := (self exponent - 52) max: MinValLogBase2.
	baseExpEstimate := (self exponent * base asFloat reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[significand ~= 16r10000000000000
				ifTrue:
					[r := significand bitShift: 1 + exp.
					s := 2.
					mPlus := mMinus := 1 bitShift: exp]
				ifFalse:
					[r := significand bitShift: 2 + exp.
					s := 4.
					mPlus := 2 * (mMinus := 1 bitShift: exp)]]
		ifFalse:
			[(exp = MinValLogBase2 or: [significand ~= 16r10000000000000])
				ifTrue:
					[r := significand bitShift: 1.
					s := 1 bitShift: 1 - exp.
					mPlus := mMinus := 1]
				ifFalse:
					[r := significand bitShift: 2.
					s := 1 bitShift: 2 - exp.
					mPlus := 2.
					mMinus := 1]].
	baseExpEstimate >= 0
		ifTrue: [s := s * (base raisedToInteger: baseExpEstimate)]
		ifFalse:
			[scale := base raisedToInteger: baseExpEstimate negated.
			r := r * scale.
			mPlus := mPlus * scale.
			mMinus := mMinus * scale].
	((r + mPlus < s) not and: [roundingIncludesLimits or: [r + mPlus > s]])
		ifTrue: [baseExpEstimate := baseExpEstimate + 1]
		ifFalse:
			[r := r * base.
			mPlus := mPlus * base.
			mMinus := mMinus * base].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
		ifFalse:
			[decPointCount := 1].
	slowbit := 1 - s lowBit .
	shead := s bitShift: slowbit.
	[d := (r bitShift: slowbit) // shead.
	r := r - (d * s).
	(tc1 := (r > mMinus) not and: [roundingIncludesLimits or: [r < mMinus]]) |
	(tc2 := (r + mPlus < s) not and: [roundingIncludesLimits or: [r + mPlus > s]])] whileFalse:
		[aStream nextPut: (Character digitValue: d).
		r := r * base.
		mPlus := mPlus * base.
		mMinus := mMinus * base.
		decPointCount := decPointCount - 1.
		decPointCount = 0 ifTrue: [aStream nextPut: $.]].
	tc2 ifTrue:
		[(tc1 not or: [r * 2 >= s]) ifTrue: [d := d + 1]].
	aStream nextPut: (Character digitValue: d).
	decPointCount > 0
		ifTrue:
		[decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
		aStream nextPutAll: '.0'].
	fixedFormat ifFalse:
		[aStream nextPut: $e.
		aStream nextPutAll: (baseExpEstimate - 1) printString]! !

!Float methodsFor: 'printing' library: #MinimalKernel03Float stamp: 'jmv 9/24/2018 10:56:53'!
absPrintOn: aStream base: base
	"In Cuis, print Floats with enough digits to be able to recover later exactly the same Float."

	self absPrintExactlyOn: aStream base: base! !

!Float methodsFor: 'printing' library: #MinimalKernel03Float stamp: 'jmv 5/31/2021 07:37:11'!
printOn: aStream base: base
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:" 

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self = 0.0
				ifTrue: [aStream nextPutAll: '0.0']
				ifFalse: [self negated absPrintOn: aStream base: base]]! !

!Float methodsFor: 'arithmetic' library: #MinimalKernel03Float stamp: 'nice 8/21/2010 22:31'!
abs
	"This is faster than using Number abs and works for negativeZero."
	self <= 0.0
		ifTrue: [^ 0.0 - self]
		ifFalse: [^ self]! !

!Float methodsFor: 'arithmetic' library: #MinimalKernel03Float stamp: 'nice 12/20/2012 23:16'!
negated
	"Answer a Number that is the negation of the receiver.
	Implementation note: this version cares of negativeZero."

	^-1.0 * self! !


!Float class methodsFor: 'instance creation' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
basicNew
	^BoxedFloat64 basicNew: 2! !

!Float class methodsFor: 'instance creation' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
basicNew: anInteger
	^BoxedFloat64 basicNew: 2! !

!Float class methodsFor: 'instance creation' library: #MinimalKernel03Float stamp: 'jmv 4/2/2013 14:00'!
new
	"Floats are 2 words of size"
	^self basicNew: 2! !

!Float class methodsFor: 'instance creation' library: #MinimalKernel03Float stamp: 'jmv 4/2/2013 14:04'!
new: s
	self error: 'Please call Float>>#new (without arguments).'.
	^self new! !

!Float class methodsFor: 'instance creation' library: #MinimalKernel03Float stamp: 'jmv 12/26/2018 13:54:36'!
signPart: signPart mantissaPart: mantissaPart exponentPart: exponentPart
	"
	Float signPart: Float pi signPart mantissaPart: Float pi mantissaPart exponentPart: Float pi exponentPart
	"
	^ mantissaPart asFloat * signPart timesTwoPower: exponentPart-52! !

!Float class methodsFor: 'constants' library: #MinimalKernel03Float stamp: 'nice 6/8/2009 15:33'!
fmin
	"Answer minimum positive representable value."
	
	^self denormalized
		ifTrue: [self fminDenormalized]
		ifFalse: [self fminNormalized]! !

!Float class methodsFor: 'constants' library: #MinimalKernel03Float stamp: 'jmv 4/5/2019 17:28:36'!
fminDenormalized
	"Answer the minimum denormalized value representable.
	Denormalized numbers guarantees that the result x - y is non-zero when x !!= y.
	"
	
	^1.0 timesTwoPower: MinValLogBase2! !

!Float class methodsFor: 'constants' library: #MinimalKernel03Float stamp: 'jmv 10/13/2011 19:57'!
maxExactInteger
	"Answer the biggest integer such that it is exactly represented in a float, and all smaller integers also are"
	^1 bitShift: self precision! !

!Float class methodsFor: 'constants' library: #MinimalKernel03Float stamp: 'GabrielOmarCotelli 5/25/2009 15:42'!
one
	
	^1.0! !

!Float class methodsFor: 'constants' library: #MinimalKernel03Float stamp: 'ST-80 5/31/1983 9:10:35'!
pi
	"Answer the constant, Pi."
	^Pi! !

!Float class methodsFor: 'constants' library: #MinimalKernel03Float stamp: 'jmv 12/26/2018 13:44:22'!
precision
	"Answer the apparent precision of the floating point representation.
	That is the maximum number of radix-based digits (bits if radix=2) representable in floating point without rounding error.
	Technically, 52 bits are stored in the representation, and normalized numbers have an implied leading 1 that is not stored.
	Note that denormalized floating point numbers don't have the implied leading 1, and thus gradually lose precision.
	This format implements the IEEE 754 binary64 format."
	
	^53! !

!Float class methodsFor: 'class initialization' library: #MinimalKernel03Float stamp: 'jmv 12/26/2018 13:08:28'!
initClassCachedState
	"Float initialize"
	"Constants from Computer Approximations, pp. 182-183:
		Pi = 3.14159265358979323846264338327950288
		Pi/2 = 1.57079632679489661923132169163975144
		Pi*2 = 6.28318530717958647692528676655900576
		Pi/180 = 0.01745329251994329576923690768488612
		2.0 ln = 0.69314718055994530941723212145817657
		2.0 sqrt = 1.41421356237309504880168872420969808"

	Pi := 3.14159265358979323846264338327950288.
	Halfpi := Pi / 2.0.
	Twopi := Pi * 2.0.
	RadiansPerDegree := Pi / 180.0.

	Ln2 := 0.69314718055994530941723212145817657.
	Ln10 := 10.0 ln.
	Sqrt2 := 1.41421356237309504880168872420969808.
	E := 2.718281828459045235360287471353.

	MaxVal := 1.7976931348623157e308.
	MaxValLn := 709.782712893384.
	MinValLogBase2 := -1074.

	Infinity := MaxVal * MaxVal.
	NegativeInfinity := 0.0 - Infinity.
	NaN := Infinity - Infinity.
	NegativeZero := Float fmin negated nextTowardsZero.
! !

!Float class methodsFor: 'class initialization' library: #MinimalKernel03Float stamp: 'jmv 12/14/2015 23:48'!
initialize
	"
	Float initialize
	"
	"Constants from Computer Approximations, pp. 182-183:
		Pi = 3.14159265358979323846264338327950288
		Pi/2 = 1.57079632679489661923132169163975144
		Pi*2 = 6.28318530717958647692528676655900576
		Pi/180 = 0.01745329251994329576923690768488612
		2.0 ln = 0.69314718055994530941723212145817657
		2.0 sqrt = 1.41421356237309504880168872420969808"

	self initClassCachedState! !

!Float class methodsFor: 'testing' library: #MinimalKernel03Float stamp: 'jmv 4/26/2019 09:02:14'!
denormalized
	"Answer whether implementation supports denormalized numbers.
	Allowing denormalized numbers guarantees that the result x - y is non-zero when x !!= y."
	
	^true! !


!BoxedFloat64 methodsFor: 'mathematical functions' library: #MinimalKernel03Float stamp: 'jmv 11/27/2018 11:14:45'!
ln
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 58>
	^ self lnNonPrimitive! !

!BoxedFloat64 methodsFor: 'mathematical functions' library: #MinimalKernel03Float stamp: 'jmv 8/2/2019 10:36:02'!
raisedToInteger: exponent
	"Answer the receiver raised to aNumber."

	self isNaN ifTrue: [ ^self ].
	^super raisedToInteger: exponent! !

!BoxedFloat64 methodsFor: 'mathematical functions' library: #MinimalKernel03Float stamp: 'jmv 8/1/2019 19:10:12'!
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

!BoxedFloat64 methodsFor: 'arithmetic' library: #MinimalKernel03Float stamp: 'di 11/6/1998 13:54'!
* aNumber 
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 49>
	^ aNumber adaptToFloat: self andSend: #*! !

!BoxedFloat64 methodsFor: 'arithmetic' library: #MinimalKernel03Float stamp: 'di 11/6/1998 13:22'!
+ aNumber 
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 41>
	^ aNumber adaptToFloat: self andSend: #+! !

!BoxedFloat64 methodsFor: 'arithmetic' library: #MinimalKernel03Float stamp: 'di 11/6/1998 13:55'!
- aNumber 
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 42>
	^ aNumber adaptToFloat: self andSend: #-! !

!BoxedFloat64 methodsFor: 'arithmetic' library: #MinimalKernel03Float stamp: 'jmv 6/25/2019 17:44:27'!
/ aNumber 
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 50>
	aNumber isZero ifTrue: [^ ZeroDivide new signalReceiver: self selector: #/ argument: aNumber].
	^ aNumber adaptToFloat: self andSend: #/! !

!BoxedFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'di 11/6/1998 13:55'!
< aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 43>
	^ aNumber adaptToFloat: self andSend: #<! !

!BoxedFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'di 11/6/1998 13:55'!
<= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 45>
	^ aNumber adaptToFloat: self andSend: #<=! !

!BoxedFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'di 11/6/1998 13:56'!
= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 47>
	aNumber isNumber ifFalse: [^ false].
	^ aNumber adaptToFloat: self andSend: #=! !

!BoxedFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'di 11/6/1998 13:57'!
> aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 44>
	^ aNumber adaptToFloat: self andSend: #>! !

!BoxedFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'eem 7/6/2009 11:11'!
>= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 46>
	^ aNumber adaptToFloat: self andSend: #>=! !

!BoxedFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: ''!
~= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 48>
	^super ~= aNumber! !

!BoxedFloat64 methodsFor: 'testing' library: #MinimalKernel03Float stamp: 'jmv 4/8/2019 09:43:27'!
isDenormalized
	"Denormalized numbers are only represented as BoxedFloat64"

	^ self partBits: [ :signBit :exponentBits :mantissaBits |
		exponentBits = 0 and: [mantissaBits ~=0]]! !

!BoxedFloat64 methodsFor: 'testing' library: #MinimalKernel03Float stamp: 'jmv 8/1/2019 16:42:44'!
isFinite
	"simple, byte-order independent test for rejecting Not-a-Number and (Negative)Infinity"

	^(self - self) = 0.0! !

!BoxedFloat64 methodsFor: 'testing' library: #MinimalKernel03Float stamp: 'jmv 8/1/2019 16:42:00'!
isInfinite
	"Return true if the receiver is positive or negative infinity."

	^ self = Infinity or: [self = NegativeInfinity]! !

!BoxedFloat64 methodsFor: 'testing' library: #MinimalKernel03Float stamp: 'jmv 8/1/2019 16:39:56'!
isNaN
	"simple, byte-order independent test for Not-a-Number"

	^ self ~= self! !

!BoxedFloat64 methodsFor: 'truncation and round off' library: #MinimalKernel03Float stamp: 'jmv 5/21/2020 22:23:44'!
truncated
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Raise an exception if no conversion to integer is possible, i.e. for Infinities and NaN.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 51>
	self isFinite ifFalse: [ ^self ].
	^ self partValues: [ :sign :exponent :mantissa |
		sign * (mantissa bitShift: exponent - 52) ]! !

!BoxedFloat64 methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'jmv 2/8/2021 18:54:45'!
exponent
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
	^super exponent! !

!BoxedFloat64 methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'jmv 8/1/2019 16:53:05'!
sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	self isNaN ifTrue: [ self error: 'Can not handle Not-a-Number' ].
	^super sign! !


!SmallFloat64 methodsFor: 'mathematical functions' library: #MinimalKernel03Float stamp: 'jmv 11/27/2018 11:14:36'!
ln
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 558>
	^ self lnNonPrimitive! !

!SmallFloat64 methodsFor: 'mathematical functions' library: #MinimalKernel03Float stamp: 'jmv 8/1/2019 19:10:21'!
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

!SmallFloat64 methodsFor: 'accessing' library: #MinimalKernel03Float stamp: 'jmv 2/8/2021 18:54:52'!
exponent
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa between one and two (#significand).
	 Answer with the SmallInteger to whose power two is raised.
	Optional. See Object documentation whatIsAPrimitive.
	Note: invalid for infinities, NaN and zero.
	See comment at BoxedFloat64"

	<primitive: 553>
	^super exponent! !

!SmallFloat64 methodsFor: 'arithmetic' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
* aNumber 
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 549>
	^ aNumber adaptToFloat: self andSend: #*! !

!SmallFloat64 methodsFor: 'arithmetic' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
+ aNumber 
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 541>
	^ aNumber adaptToFloat: self andSend: #+! !

!SmallFloat64 methodsFor: 'arithmetic' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
- aNumber 
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 542>
	^ aNumber adaptToFloat: self andSend: #-! !

!SmallFloat64 methodsFor: 'arithmetic' library: #MinimalKernel03Float stamp: 'jmv 6/25/2019 18:01:31'!
/ aNumber 
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 550>
	aNumber isZero ifTrue: [^ZeroDivide new signalReceiver: self selector: #/ argument: aNumber ].
	^ aNumber adaptToFloat: self andSend: #/! !

!SmallFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
< aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 543>
	^ aNumber adaptToFloat: self andSend: #<! !

!SmallFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
<= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 545>
	^ aNumber adaptToFloat: self andSend: #<=! !

!SmallFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 547>
	aNumber isNumber ifFalse: [^ false].
	^ aNumber adaptToFloat: self andSend: #=! !

!SmallFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
> aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 544>
	^ aNumber adaptToFloat: self andSend: #>! !

!SmallFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
>= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 546>
	^ aNumber adaptToFloat: self andSend: #>=! !

!SmallFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
~= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 548>
	^super ~= aNumber! !

!SmallFloat64 methodsFor: 'comparing' library: #MinimalKernel03Float stamp: 'eem 12/22/2014 12:36'!
identityHash
	"Answer an integer unique to the receiver."
	<primitive: 171>
	^self primitiveFailed! !

!SmallFloat64 methodsFor: 'truncation and round off' library: #MinimalKernel03Float stamp: 'jmv 8/2/2019 09:34:46'!
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

!SmallFloat64 methodsFor: 'copying' library: #MinimalKernel03Float stamp: 'eem 11/25/2014 07:54'!
shallowCopy
	"Answer the receiver, because SmallFloat64s are unique."
	^self! !


!SmallInteger methodsFor: 'converting' library: #MinimalKernel03Float stamp: 'Squeak1.0 9/20/1996 10:50:05'!
asFloat
	"Primitive. Answer a Float that represents the value of the receiver.
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 40>
	self primitiveFailed! !


!Number reorganize!
('arithmetic' * + - / // \\ ^ abs div: mod: negated quo: reciprocal rem:)
('comparing' % = hash)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: lg ln log log: log2 magnitude nthRoot: raisedTo: raisedToFraction: raisedToInteger: raisedToNegativeInteger: sign: sin sinh sqrt squared tan tanh)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart round4perMagnitudeOrder roundDownTo: roundTo: roundUpTo: rounded roundedHAFZ roundedHTE roundedToEven truncateTo: truncated)
('converting' @ adaptToCollection:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asFloat asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours inMinusPiToPiRange milliSeconds minutes moduloTwoPiAsFloat nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('testing' even ifNotZero: is: isDivisibleBy: isFinite isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
('intervals' to: to:by: to:by:do: to:count: to:do: to:do:separatedBy: toSelfPlus:)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
!


!Float reorganize!
('comparing' hash isWithin:floatsFrom: literalEqual:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan: arcTanNonPrimitive copySignTo: cos cosh degreeCos degreeSin expNonPrimitive floorLog: lnNonPrimitive log log: log2 nthRoot: reciprocal reciprocalFloorLog: reciprocalLogBase2 safeArcCos sinNonPrimitive sinh smoothIsAbsBelow: smoothStep sqrt sqrtNonPrimitive tan tanh)
('converting' adaptToFraction:andSend: adaptToInteger:andSend: asFloat asFraction asIEEE32BitPrecisionFloat asIEEE32BitWord asMatching32BitSingleDecimalRepresentation asTrueFraction degreesToRadians inMinusPiToPiRange partBits: partValues: partValues:ifInfinite:ifNaN: radiansToDegrees withNegativeSign)
('testing' isDenormalized isFinite isFloat isFloatOrFloatComplex isInfinite isLiteral isNaN isZero)
('accessing' at: at:put: basicAt: basicAt:put: byteSize exponent exponentBits exponentPart floatsAwayFrom: integerPart mantissaBits mantissaPart nextAwayFromZero nextTowardsZero predecessor roundedHAFZ roundedHTE sign signBit signPart significand significandAsInteger successor ulp)
('printing' absPrintExactlyOn:base: absPrintOn:base: absPrintOn:base:mantissaSignificantBits: hex printAsIEEE32BitPrecisionFloatOn:base: printAsLiteralOn: printOn:base: printOn:fractionDigits: storeOn:base:)
('intervals' to:count:do: to:hops:do:)
('private' absPrintOn:base:digitCount: replaceWordsFrom:to:with:startingAt:)
('inspecting' hasContentsInExplorer)
('arithmetic' abs negated)
!

Float initialize!

!Float class reorganize!
('instance creation' basicNew basicNew: fromIEEE32Bit: new new: readFrom: signBit:mantissaBits:exponentBits: signPart:mantissaPart:exponentPart:)
('constants' e emax emin epsilon fmax fmin fminDenormalized fminNormalized halfPi infinity ln10 ln2 maxExactInteger nan negativeInfinity negativeZero one pi precision radix twoPi zero)
('class initialization' initClassCachedState initialize releaseClassCachedState)
('testing' denormalized nativeWordOrdering)
!


!BoxedFloat64 reorganize!
('mathematical functions' arcTan cos exp ln primSqrt raisedTo: raisedToInteger: sin timesTwoPower:)
('accessing and round off' fractionPart)
('printing' storeOn:base:)
('arithmetic' * + - /)
('comparing' < <= = > >= ~=)
('testing' isDenormalized isFinite isInfinite isNaN)
('truncation and round off' truncated)
('accessing' exponent sign)
!


!SmallFloat64 reorganize!
('mathematical functions' arcTan cos exp ln primSqrt profilerFriendlyPrimTimesTwoPower: profilerFriendlyTimesTwoPower: sin timesTwoPower:)
('accessing' exponent fractionPart)
('arithmetic' * + - /)
('comparing' < <= = > >= ~= identityHash)
('truncation and round off' truncated)
('copying' shallowCopy)
!


!SmallInteger reorganize!
('arithmetic' * + - / // \\ gcd: quo:)
('bit manipulation' bitAnd: bitOr: bitShift: bitXor: byteReversed lowBit)
('testing' even isLarge odd)
('printing' decimalDigitLength numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printString printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength nextInstance nextObject)
('mathematical functions' sqrt)
('comparing' < <= = > >= ~= hashMultiply identityHash)
('copying' shallowCopy)
('converting' asFloat)
!

