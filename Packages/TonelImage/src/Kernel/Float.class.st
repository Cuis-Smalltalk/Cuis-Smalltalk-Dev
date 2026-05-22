"
A note About Floating Point numbers and Floating Point Arithmetic.

The following is not specific to Cuis or Smalltalk at all. This is about the properties of Float numbers in any computer implementation.

If you haven't done so already, read https://en.wikipedia.org/wiki/Floating-point_arithmetic

But if you find the Wikipedia article too detailed, or hard to read, then try http://fabiensanglard.net/floating_point_visually_explained/ (get past "How Floating Point are usually explained" and read "A different way to explain...").

Other great reads are:
	"Why don't my numbers add up?":
		http://floating-point-gui.de/
and
	"What Every Computer Scientist Should Know About Floating-Point Arithmetic":
		http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
and also maybe
	"Comparing floating point numbers"
		https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/

Now that you read them, and we are on the same boat, some further comments (from jmv):

Floats are (conceptually) approximate real numbers. That's why trig and other transcendental functions always answer Floats. That's why it is ok to round the result of operations. That's why Float is considered more general than Fraction in ST-80 and most Smalltalks. So, when we have a Float value, we must not think about it as a Rational but as a Real (actually as some unknown Real that could hopefully be close to the Rational we can actually represent). Keep this in mind when dealing with Floats, and especially avoid comparing them for equality.

When doing mixed operations with Floats and Fractions, Cuis, as most other Smalltalks, converts all values to Floats. Some other systems, including Pharo Smalltalk, Scheme and Lisp have two rules: when the answer is a Number, they convert to Float. But when the answer is a boolean (#<, #=, #<=, etc.) they convert to Fraction. We think this is a mistake. There should never be implicit conversions from Float to Fraction. Fractions are to hold exact values, and people expect Fractions to be exact. On the other hand, Floats are to hold approximations (and people should be aware of that!). But an implicit conversion from Float to Fraction would give a Fraction that should not be considered an exact value (the value comes from an inexact Float), but that knowledge is lost, as it is an instance of Fraction.

If you want exact arithmetic, usual mathematical properties (like transitivity of equality), can live in the limited world of Rational numbers, and can afford a slight performance penalty, use Fraction instead. Avoid trascendental functions and never convert to Float.

In any case, most numeric computation is done on Float numbers. There are good reasons for that. One is that in most cases we don't need an exact answer. And in many cases we can't really have it: the inputs to algorithms already have a limited precision, or they use transcendental functions. And even when exact arithmetic is possible, if we are doing sound synthesis, 24 bits of resolution is enough. For image processing and graphics, the result is never more than 16 bits per channel. So, these fields don't really need 64 bit Doubles. 32 bit Floats are enough. Other fields do need 64 bit Doubles, like physics simulations and geometry. Games usually prefer special, faster 32 bit Float operations in GPUs that have greater errors but are faster.

There are some things that can be done to increase the confidence you can have on Float results. One is to do an error propagation analysis on the code you are running. This is not easy, but it is done for any widely used numerical method. Then, you can know real bounds and/or estimates of the errors made. So, understanding your inputs and your algorithms (for example error propagation, condition number, numeric stability), and using Float number if appropriate, is the usual advice.

Perhaps you have heard about "interval arithmetic". It is a bit better than simple Float, but doesn't really fix the problems.

The ultimate solution is to do Monte Carlo analysis, with random perturbation of inputs. After the Monte Carlo run, it is needed to do statistical analysis of possible correlations between the distributions of the random noise added to imputs and the result of the algorithm.

Additional food for thought: http://www.cs.berkeley.edu/~wkahan/Mindless.pdf . According to this, doing Monte Carlo as described above attacks a slightly different problem. This might be yet another reason (besides performance) to try something like the next paragraph. I (jmv) came up with it, and I don't really know if it has been described and or tried before or not. Mhhh. Maybe a defensive publication is in order.

A possibility that could be a practical solution, being much cheaper than Monte Carlo, but better than interval arithmetic, is to represent each value by 2 Floats: an estimation of the real value (i.e. an estimation of the mean value of the distribution of the corresponding Monte Carlo result), and an estimation of the error (i.e. an estimation of the standard deviation of the corresponding Monte Carlo result). Or perhaps even 3 of them. In addition to the estimation of the real value and an estimation of the error, we could add a hard bound on the error. In many cases it will be useless, because the error can not really be bound. But in those cases where it is possible to bound it, applications could really know about the quality of computed values.

=======================================================================

My instances represent IEEE 754 floating-point double-precision numbers. They have about 16 decimal digits of accuracy and their range is between plus and minus 10^307. Some valid examples are:
	
	8.0 13.3 0.3 2.5e6 1.27e-30 1.27e-31 -12.987654e12

Mainly: no embedded blanks, little e for tens power, and a digit on both sides of the decimal point. It is actually possible to specify a radix for Float constants.  This is great for teaching about numbers, but may be confusing to the average reader:

	3r20.2 --> 6.66666666666667
	8r20.2 --> 16.25

If you don't have access to the definition of IEEE754, you can figure out what is going on by printing various simple values in Float hex.  It may help you to know that the basic format is...
	sign		1 bit
	exponent	11 bits with bias of 1023 (16r3FF), substracted to produce an actual exponent in the range -1022 .. +1023
				- 16r000:
					significand = 0: Float zero
					significand ~= 0: Denormal number (actual exponent is -1022, not -1023. No implicit leading '1' bit in mantissa)
				- 16r7FF:
					significand = 0: Infinity
					significand ~= 0: Not A Number (NaN) representation
	mantissa	53 bits, but only 52 are stored (20 in the first word, 32 in the second).  This is because a normalized mantissa, by definition, has a 1 to the right of its floating point, and IEEE 754 omits this redundant bit to gain an extra bit of precision instead.  People talk about the mantissa without its leading one as the FRACTION, and with its leading 1 as the SIGNFICAND.

The single-precision format is...
	sign		1 bit
	exponent	8 bits with bias of 127 (16r7F, substracted to produce an actual exponent in the range -126 .. +127
				- 16r00:
					significand = 0: Float zero
					significand ~= 0: Denormal number (actual exponent is -126, not -127. No implicit leading '1' bit in mantissa)
				- 16rFF:
					significand = 0: Infinity
					significand ~= 0: Not A Number (NaN) representation
	mantissa	24 bits, but only 23 are stored
This format is used in FloatArray (qv), and much can be learned from the conversion routines, Float asIEEE32BitWord, and Float class fromIEEE32Bit:.

You might also check https://en.wikipedia.org/wiki/IEEE_754_revision

Other great reads (covering broader but interesting issues):
https://randomascii.wordpress.com/2013/07/16/floating-point-determinism/
https://web.archive.org/web/20150909015114/http://www.ima.umn.edu/2010-2011/W1.10-14.11/activities/Leeser-Miriam/Leeser-GPU-IMA-Jan2011.pdf

"
Class {
	#name : 'Float',
	#superclass : 'Number',
	#classVars : [
		'E',
		'Halfpi',
		'Infinity',
		'Ln10',
		'Ln2',
		'MaxVal',
		'MaxValLn',
		'MinValLogBase2',
		'NaN',
		'NegativeInfinity',
		'NegativeZero',
		'Pi',
		'RadiansPerDegree',
		'Sqrt2',
		'Twopi'
	],
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'instance creation' }
Float class >> basicNew [
	^BoxedFloat64 basicNew: 2
]

{ #category : 'instance creation' }
Float class >> basicNew: anInteger [
	^BoxedFloat64 basicNew: 2
]

{ #category : 'testing' }
Float class >> denormalized [
	"Answer whether implementation supports denormalized numbers.
	Allowing denormalized numbers guarantees that the result x - y is non-zero when x != y."
	
	^true
]

{ #category : 'constants' }
Float class >> e [
	"Answer the constant, e."

	^E
]

{ #category : 'constants' }
Float class >> emax [
	"Answer exponent of the maximal representable finite value"
	
	^1023
]

{ #category : 'constants' }
Float class >> emin [
	"Answer the exponent of the non-denormal value with smallest magnitude"
	
	^-1022
]

{ #category : 'constants' }
Float class >> epsilon [
	"Answer the difference between 1.0 and the next representable value.
	Note: does not equal 1.0 ulp."
	
	^1.0 successor - 1.0
]

{ #category : 'constants' }
Float class >> fmax [
	"Answer the maximum finite floating point value representable."
	
	^MaxVal
]

{ #category : 'constants' }
Float class >> fmin [
	"Answer minimum positive representable value."
	
	^self denormalized
		ifTrue: [self fminDenormalized]
		ifFalse: [self fminNormalized]
]

{ #category : 'constants' }
Float class >> fminDenormalized [
	"Answer the minimum denormalized value representable.
	Denormalized numbers guarantees that the result x - y is non-zero when x != y.
	"
	
	^1.0 timesTwoPower: MinValLogBase2
]

{ #category : 'constants' }
Float class >> fminNormalized [
	"Answer the minimum normalized value representable."
	
	^1.0 timesTwoPower: -1022
]

{ #category : 'instance creation' }
Float class >> fromIEEE32Bit: word [
  	"Convert the given 32 bit word (which is supposed to be a positive 32bit value) from
	a 32bit IEEE floating point representation into an actual Float object (being
	64bit wide). Should only be used for conversion in Float32Arrays or likewise objects."
  	
 	| sign exponent mantissa exponentBits fractionBits answerFractionBits delta signBit answerExponent |
  	word negative ifTrue: [ ^ self error: 'Cannot deal with negative numbers' ].
  	word = 0 ifTrue: [ ^ Float zero ].
 	word = 16r80000000 ifTrue: [ ^Float negativeZero ].
  	
	signBit := word bitAnd: 16r80000000.
	sign := (word bitShift: -31) = 0 ifTrue: [1] ifFalse: [-1].
  	exponentBits := (word bitShift: -23) bitAnd: 16rFF.
  	fractionBits := word bitAnd: 16r7FFFFF.
  
	" Special cases: infinites and NaN"
	exponentBits = 16rFF ifTrue: [
  		fractionBits = 0 ifFalse: [ ^ Float nan ].
  		^ sign positive
  			ifTrue: [ Float infinity ]
  			ifFalse: [ Float negativeInfinity ]].

	" Unbias exponent: 16r3FF is bias"
	exponent := exponentBits - 16r7F.

"Older version."
false ifTrue: [
	" Replace omitted leading 1 in fraction if appropriate"
	"If expPart = 0, I am +/-zero or a denormal value. In such cases, no implicit leading bit in mantissa"
 	exponentBits = 0
 		ifTrue: [
 			"gradual underflow (denormalized number)
 			There is no implied one, but the exponent is -126"
			mantissa := fractionBits.
			answerExponent := exponent + 1 ]
 		ifFalse: [
			mantissa := fractionBits + 16r800000.
			answerExponent := exponent ].
		^ (sign * mantissa) asFloat timesTwoPower: answerExponent - 23 ].

	"Newer version"
 	exponentBits = 0
		ifTrue: [
			"gradual underflow (denormalized number)
			 Remove first bit of mantissa and adjust exponent"
			delta := fractionBits highBit.
			answerFractionBits := (fractionBits bitAnd: (1 bitShift: delta - 1) - 1) bitShift: 24 - delta.
			answerExponent := exponent + delta - 23]
		ifFalse: [
			answerFractionBits := fractionBits.
			answerExponent := exponent ].
  
  	"Create new float"
	^ (self basicNew: 2)
		basicAt: 1 put: ((signBit bitOr: (1023 + answerExponent bitShift: 20)) bitOr: (answerFractionBits bitShift: -3));
		basicAt: 2 put: ((answerFractionBits bitAnd: 7) bitShift: 29);
		* 1.0. "reduce to SmallFloat64 if possible"

"
Float fromIEEE32Bit: Float pi asIEEE32BitWord 
(Float fromIEEE32Bit: Float pi asIEEE32BitWord ) = Float pi
(Float fromIEEE32Bit: Float pi asIEEE32BitWord ) - Float pi

Float fromIEEE32Bit: (Float pi / 1e40) asIEEE32BitWord
(Float fromIEEE32Bit: (Float pi / 1e40) asIEEE32BitWord)  = (Float pi / 1e40)
(Float fromIEEE32Bit: (Float pi / 1e40) asIEEE32BitWord)  - (Float pi / 1e40)
"
]

{ #category : 'instance creation' }
Float class >> fromIEEE64Bit: anInteger [
	"Convert the given 64 bit word (which is supposed to be a positive 64-bit value) from
	  a 64 bit IEEE floating point representation into an actual Squeak float object (being
	  64 bits wide). Should only be used for conversion in FloatArrays or likewise objects."
	| value |
	value := self basicNew: 2.
	value
		basicAt: 1 put: (anInteger bitShift: -32);
		basicAt: 2 put: (anInteger bitAnd: 16rFFFFFFFF).
	^value * 1.0 "reduce to SmallFloat64 if possible"
]

{ #category : 'constants' }
Float class >> halfPi [
	^ Halfpi
]

{ #category : 'constants' }
Float class >> infinity [
	"Answer the value used to represent an infinite magnitude"

	^ Infinity
]

{ #category : 'class initialization' }
Float class >> initClassCachedState [
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

]

{ #category : 'class initialization' }
Float class >> initialize [
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

	self initClassCachedState
]

{ #category : 'constants' }
Float class >> ln10 [
	^ Ln10
]

{ #category : 'constants' }
Float class >> ln2 [
	^ Ln2
]

{ #category : 'constants' }
Float class >> maxExactInteger [
	"Answer the biggest integer such that it is exactly represented in a float, and all smaller integers also are"
	^1 bitShift: self precision
]

{ #category : 'constants' }
Float class >> nan [
	"Answer the canonical value used to represent Not-A-Number"

	^ NaN
]

{ #category : 'testing' }
Float class >> nativeWordOrdering [
	"True if this image stores float objects internally in native word order.
	If false, double word floats are stored in big-endian order regardless
	of the machine native word order."
	
	^ Smalltalk imageFormatVersion anyMask: 1

]

{ #category : 'constants' }
Float class >> negativeInfinity [
	"Answer the value used to represent an negative infinite magnitude"

	^ NegativeInfinity
]

{ #category : 'constants' }
Float class >> negativeZero [
	"Negative Zero is a very special number
	-0.0 = 0.0  	evaluates to true
	Any function evaluated in -0.0 gives the same result as evaluated in 0.0.
	Exceptions are:
		0.0 sign  		->		0
		-0.0 sign  		->		-1 

		0.0 negated  	->		-0.0
		-0.0 negated  	->		0.0

		0.0 sqrt  		->		0.0
		-0.0 sqrt  		->		-0.0 
	The behavior of negative zero is specified in IEEE 754
	"

	^ NegativeZero
]

{ #category : 'instance creation' }
Float class >> new [
	"Floats are 2 words of size"
	^self basicNew: 2
]

{ #category : 'instance creation' }
Float class >> new: s [
	self error: 'Please call Float>>#new (without arguments).'.
	^self new
]

{ #category : 'constants' }
Float class >> one [
	
	^1.0
]

{ #category : 'constants' }
Float class >> pi [
	"Answer the constant, Pi."
	^Pi
]

{ #category : 'constants' }
Float class >> precision [
	"Answer the apparent precision of the floating point representation.
	That is the maximum number of radix-based digits (bits if radix=2) representable in floating point without rounding error.
	Technically, 52 bits are stored in the representation, and normalized numbers have an implied leading 1 that is not stored.
	Note that denormalized floating point numbers don't have the implied leading 1, and thus gradually lose precision.
	This format implements the IEEE 754 binary64 format."
	
	^53
]

{ #category : 'constants' }
Float class >> radix [
	"Answer the radix used for internal floating point representation."
	
	^2
]

{ #category : 'instance creation' }
Float class >> readFrom: aStream  [
	"Answer a new Float as described on the stream, aStream."

	^(super readFrom: aStream) asFloat
]

{ #category : 'class initialization' }
Float class >> releaseClassCachedState [

	Pi := nil.
	Halfpi := nil.
	Twopi := nil.
	RadiansPerDegree := nil.

	Ln2 := nil.
	Ln10 := nil.
	Sqrt2 := nil.
	E := nil.

	MaxVal := nil.
	MaxValLn := nil.
	MinValLogBase2 := nil.

	Infinity := nil.
	NegativeInfinity := nil.
	NaN := nil.
	NegativeZero := nil
]

{ #category : 'instance creation' }
Float class >> signBit: signBit mantissaBits: mantissaBits exponentBits: exponentBits  [
	"
	Float signBit: Float pi signBit mantissaBits: Float pi mantissaBits exponentBits: Float pi exponentBits
	"
	| answer leastSignificativeWord mostSignificativeWord |
	answer := BoxedFloat64 new.
	mostSignificativeWord := (signBit bitShift: 31) + (exponentBits bitShift: 20) + (mantissaBits bitShift: -32).
	leastSignificativeWord := mantissaBits bitAnd: 16rFFFFFFFF.
	answer basicAt: 1 put: mostSignificativeWord.
	answer basicAt: 2 put: leastSignificativeWord.
	^ answer
]

{ #category : 'instance creation' }
Float class >> signPart: signPart mantissaPart: mantissaPart exponentPart: exponentPart [
	"
	Float signPart: Float pi signPart mantissaPart: Float pi mantissaPart exponentPart: Float pi exponentPart
	"
	^ mantissaPart asFloat * signPart timesTwoPower: exponentPart-52
]

{ #category : 'constants' }
Float class >> twoPi [

	^ Twopi

]

{ #category : 'constants' }
Float class >> zero [

	^0.0
]

{ #category : 'arithmetic' }
Float >> abs [
	"This is faster than using Number abs and works for negativeZero."
	self <= 0.0
		ifTrue: [^ 0.0 - self]
		ifFalse: [^ self]
]

{ #category : 'printing' }
Float >> absPrintExactlyOn: aStream base: base [
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
		aStream nextPutAll: (baseExpEstimate - 1) printString]
]

{ #category : 'printing' }
Float >> absPrintOn: aStream base: base [
	"In Cuis, print Floats with enough digits to be able to recover later exactly the same Float."

	self absPrintExactlyOn: aStream base: base
]

{ #category : 'private' }
Float >> absPrintOn: aStream base: base digitCount: digitCount  [
	"Print me in the given base, using digitCount significant figures."

	| fuzz x exp q fBase scale logScale xi |
	self isInfinite ifTrue: [^ aStream nextPutAll: 'Inf'].
	fBase := base asFloat.
	"x is myself normalized to [1.0, fBase), exp is my exponent"
	exp := 
		self < 1.0
			ifTrue: [self reciprocalFloorLog: fBase]
			ifFalse: [self floorLog: fBase].
	scale := 1.0.
	logScale := 0.
	[(x := fBase raisedTo: (exp + logScale)) = 0]
		whileTrue:
			[scale := scale * fBase.
			logScale := logScale + 1].
	x := self * scale / x.
	fuzz := fBase raisedTo: 1 - digitCount.
	"round the last digit to be printed"
	x := 0.5 * fuzz + x.
	x >= fBase
		ifTrue: 
			["check if rounding has unnormalized x"
			x := x / fBase.
			exp := exp + 1].
	(exp < 6 and: [exp > -4])
		ifTrue: 
			["decimal notation"
			q := 0.
			exp < 0 ifTrue: [1 to: 1 - exp do: [:i | aStream nextPut: ('0.0000'
at: i)]]]
		ifFalse: 
			["scientific notation"
			q := exp.
			exp := 0].
	[x >= fuzz]
		whileTrue: 
			["use fuzz to track significance"
			xi := x asInteger.
			aStream nextPut: (Character digitValue: xi).
			x := x - xi asFloat * fBase.
			fuzz := fuzz * fBase.
			exp := exp - 1.
			exp = -1 ifTrue: [aStream nextPut: $.]].
	[exp >= -1]
		whileTrue: 
			[aStream nextPut: $0.
			exp := exp - 1.
			exp = -1 ifTrue: [aStream nextPut: $.]].
	q ~= 0
		ifTrue: 
			[aStream nextPut: $e.
			q printOn: aStream]
]

{ #category : 'printing' }
Float >> absPrintOn: aStream base: base mantissaSignificantBits: significantBits [
	"Print my value on a stream in the given base.  Assumes that my value is strictly
	positive; negative numbers, zero, and NaNs have already been handled elsewhere.
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	https://www.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf
	Note: This version performs all calculations with Floats instead of LargeIntegers, and loses
	about 3 lsbs of accuracy compared to an exact conversion. Therefore it is likely to be correct
	with significantBits up to 51.
	See also #absPrintExactlyOn:base:"

	| fBase exp baseExpEstimate r s mPlus mMinus scale d tc1 tc2 fixedFormat decPointCount |
	self isInfinite ifTrue: [aStream nextPutAll: 'Infinity'. ^ self].
	fBase := base asFloat.
	exp := self exponent.
	baseExpEstimate := (exp * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[r := self.
			s := 1.0.
			mPlus := 1.0 timesTwoPower: exp - significantBits.
			mMinus := self significand ~= 1.0 ifTrue: [mPlus] ifFalse: [mPlus / 2.0]]
		ifFalse:
			[r := self timesTwoPower: significantBits.
			s := 1.0 timesTwoPower:  significantBits.
			mMinus := 1.0 timesTwoPower: (exp max: -1024).
			mPlus :=
				(exp = MinValLogBase2) | (self significand ~= 1.0)
					ifTrue: [mMinus]
					ifFalse: [mMinus * 2.0]].
	baseExpEstimate >= 0
		ifTrue:
			[exp = 1023
				ifTrue:   "scale down to prevent overflow to Infinity during conversion"
					[r := r / fBase.
					s := s * (fBase raisedToInteger: baseExpEstimate - 1).
					mPlus := mPlus / fBase.
					mMinus := mMinus / fBase]
				ifFalse:
					[s := s * (fBase raisedToInteger: baseExpEstimate)]]
		ifFalse:
			[exp < -1023
				ifTrue:   "scale up to prevent denorm reciprocals overflowing to Infinity"
					[d := (53 * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
					scale := fBase raisedToInteger: d.
					r := r * scale.
					mPlus := mPlus * scale.
					mMinus := mMinus * scale.
					scale := fBase raisedToInteger: (baseExpEstimate + d) negated]
				ifFalse:
				[scale := fBase raisedToInteger: baseExpEstimate negated].
			s := s / scale].
	(r + mPlus >= s)
		ifTrue: [baseExpEstimate := baseExpEstimate + 1]
		ifFalse:
			[s := s / fBase].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
		ifFalse:
			[decPointCount := 1].
	[d := (r / s) truncated.
	r := r - (d * s).
	(tc1 := r <= mMinus) | (tc2 := r + mPlus >= s)] whileFalse:
		[aStream nextPut: (Character digitValue: d).
		r := r * fBase.
		mPlus := mPlus * fBase.
		mMinus := mMinus * fBase.
		decPointCount := decPointCount - 1.
		decPointCount = 0 ifTrue: [aStream nextPut: $.]].
	tc2 ifTrue:
		[tc1 not | (tc1 & (r*2.0 >= s)) ifTrue: [d := d + 1]].
	aStream nextPut: (Character digitValue: d).
	decPointCount > 0
		ifTrue:
		[decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
		aStream nextPutAll: '.0'].
	fixedFormat ifFalse:
		[aStream nextPut: $e.
		aStream nextPutAll: (baseExpEstimate - 1) printString]
]

{ #category : 'converting' }
Float >> adaptToFraction: rcvr andSend: selector [
	"If I am involved in arithmetic with a Fraction, convert it to a Float."
	^ rcvr asFloat perform: selector with: self
]

{ #category : 'converting' }
Float >> adaptToInteger: rcvr andSend: selector [
	"If I am involved in arithmetic with an Integer, convert it to a Float."
	^ rcvr asFloat perform: selector with: self
]

{ #category : 'mathematical functions' }
Float >> arCosh [
	"Answer receiver's area hyperbolic cosine.
	That is the inverse function of cosh."

	self < 1 
		ifTrue: [^ Float nan].
	^self + 1 = self 
		ifTrue: [self abs ln + 2 ln]
		ifFalse: [((self squared - 1) sqrt + self) ln]
]

{ #category : 'mathematical functions' }
Float >> arSinh [
	"Answer receiver's area hyperbolic sine.
	That is the inverse function of sinh."

	self = 0.0 ifTrue: [^self].	"Handle negativeZero"  
	^self + 1 = self 
		ifTrue: [(self abs ln + 2 ln) * self sign]
		ifFalse: [((self squared + 1) sqrt + self) ln]
]

{ #category : 'mathematical functions' }
Float >> arTanh [
	"Answer receiver's area hyperbolic tangent.
	That is the inverse function of tanh."

	self = 0.0 ifTrue: [^self].	"Handle negativeZero"
	self = 1 ifTrue: [^ Float infinity].
	self = -1 ifTrue: [^Float negativeInfinity].
	self abs > 1 ifTrue: [^ Float nan].
	^((1 + self) / (1 - self)) ln / 2
]

{ #category : 'mathematical functions' }
Float >> arcCos [
	"Answer the angle in radians."

	^ Halfpi - self arcSin
]

{ #category : 'mathematical functions' }
Float >> arcSin [
	"Answer the angle in radians."

	((self < -1.0) or: [self > 1.0]) ifTrue: [^ Float nan].
	^((self = -1.0) or: [self = 1.0])
		ifTrue: [Halfpi * self]
		ifFalse: [(self / (1.0 - (self * self)) sqrt) arcTan]
]

{ #category : 'mathematical functions' }
Float >> arcTan: denominator [
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
					ifFalse: [ ((self / denominator) arcTan) - Pi ]]]]
]

{ #category : 'mathematical functions' }
Float >> arcTanNonPrimitive [
	"Answer the angle in radians."

	| theta delta sinTheta cosTheta minPossibleDelta |

	"Newton-Raphson"
	self < 0.0 ifTrue: [ ^ 0.0 - (0.0 - self) arcTan ].

	"first guess"
	theta := (self * Halfpi) / (self + 1.0).

	"iterate"
	[
		sinTheta := theta sin.
		cosTheta := theta cos.
		delta := (sinTheta * cosTheta) - (self * cosTheta * cosTheta).
		minPossibleDelta := theta ulp.
		delta abs >= minPossibleDelta ]
		whileTrue: [
			theta := theta - delta.
			"There is a risk of not converging if #sin and #cos are not good enough.
			Still, there's no point in trying to further reduce delta, meaning we won't find
			a better approximation of the result."
			delta abs = minPossibleDelta ifTrue: [
				^theta ]].
	^ theta
]

{ #category : 'converting' }
Float >> asFloat [
	"Answer with the receiver itself."
	^self
]

{ #category : 'converting' }
Float >> asFraction [
	^ self asTrueFraction 
]

{ #category : 'converting' }
Float >> asIEEE32BitPrecisionFloat [
	"Answer a 64-bit Float, but using only the precision of a 32-bit Float, as used in FloatArray and 'float' in the C world."
	
	^ Float fromIEEE32Bit: self asIEEE32BitWord
]

{ #category : 'converting' }
Float >> asIEEE32BitWord [
	"Convert the receiver into a 32 bit Integer value representing the same number in IEEE 32 bit format.
	Used for conversion in Float32Arrays only."
	
	| word1 word2 sign mantissa exponent destWord truncatedBits mask roundToUpper |
	
	"quickly skip positive and negative zero"
	self isZero ifTrue: [^self basicAt: 1].
	
	"retrieve 64 bits of IEEE 754 double"
	word1 := self basicAt: 1.
	word2 := self basicAt: 2.
	
	"prepare sign exponent and mantissa of 32 bits float"
	sign := word1 bitAnd: 16r80000000.
	exponent := ((word1 bitShift: -20) bitAnd: 16r7FF) - 1023 + 127.
	mantissa := (word2 bitShift: -29) + ((word1 bitAnd:  16rFFFFF) bitShift: 3).
	truncatedBits := (word2 bitAnd: 16r1FFFFFFF).

	"We must now honour default IEEE rounding mode (round to nearest even)"
	
	"we are below gradual underflow, even if rounded to upper mantissa"
	exponent < -24 ifTrue: [^sign "this can be negative zero"].
	
	"BEWARE: rounding occurs on less than 23bits when gradual underflow"
	exponent <= 0
		ifTrue:
			[mask := 1 bitShift: exponent negated.
			mantissa := mantissa bitOr: 16r800000.
			roundToUpper := (mantissa bitAnd: mask) isZero not
				and: [truncatedBits isZero not
					or: [(mantissa bitAnd: mask - 1) isZero not
						or: [(mantissa bitAnd: mask*2) isZero not]]].
			mantissa := mantissa bitShift: exponent - 1.
			"exponent := exponent + 1"]
		ifFalse:
			[roundToUpper := (truncatedBits bitAnd: 16r10000000) isZero not
				and: [(mantissa bitAnd: 16r1) isZero not
					or: [(truncatedBits bitAnd: 16r0FFFFFFF) isZero not]]
			].
		
	"adjust mantissa and exponent due to IEEE rounding mode"
	roundToUpper
		ifTrue:
			[mantissa := mantissa + 1.
			mantissa > 16r7FFFFF
				ifTrue:
					[mantissa := 0.
					exponent := exponent+1]].

	exponent > 254 ifTrue: ["Overflow"
		exponent := 255.
		self isNaN
			ifTrue: [mantissa isZero
				ifTrue: ["BEWARE: do not convert a NaN to infinity due to truncatedBits"
					mantissa := 1]]
			ifFalse: [mantissa := 0]].
		
	"Encode the word"
	destWord := (sign bitOr: ((exponent max: 0) bitShift: 23)) bitOr: mantissa.
	^ destWord
]

{ #category : 'converting' }
Float >> asIEEE64BitWord [
	"Convert the receiver into a 64 bit Integer value representing the same number in IEEE 64 bit format.
	 Used for conversion in FloatArrays."
	
	^((self basicAt: 1) bitShift: 32) + (self basicAt: 2)
]

{ #category : 'converting' }
Float >> asMatching32BitSingleDecimalRepresentation [
	"Answer a Float that prints the same as the 32 bit single precision that best represents us.
	This is not just us with a reduced mantissa.
	Compare the following:

	0.1 print. 										'Regular 64 bit Double (Float). Works as expected'.
	(Float32Array with: 0.1) first print. 										'64 bits Double built from 32 bit Single Precision. Annoying to most users.'.
	(Float32Array with: 0.1) first asMatching32BitSingleDecimalRepresentation print.
		'Last is the 64 bits Double that would have been built from user input if no intermediate 32 bits Single was involved' print.

	Float pi print. 										'Regular 64 bit Double (Float). Works as expected'.
	(Float32Array with: Float pi) first print. 										'64 bits Double built from 32 bit Single Precision. Annoying to most users.'.
	(Float32Array with: Float pi) first asMatching32BitSingleDecimalRepresentation print.
		'Last is the 64 bits Double that would have been built from user input if no intermediate 32 bits Single was involved' print.
	"

	| mantissaBitsToUse answerAbs printed |
	mantissaBitsToUse := 24.
	self isNaN ifTrue: [ ^self ].
	self isInfinite ifTrue: [ ^self ].
	self = 0.0 ifTrue: [ ^self ].
	printed := String streamContents: [ :strm |
		(self > 0 ifTrue: [ self ] ifFalse: [ self negated ])
			absPrintOn: strm base: 10 mantissaSignificantBits: mantissaBitsToUse ].
	answerAbs := Number readFrom: printed.
	^self > 0 ifTrue: [ answerAbs ] ifFalse: [ answerAbs negated ].
]

{ #category : 'converting' }
Float >> asTrueFraction [
	" Answer a fraction that EXACTLY represents self,
	  a double precision IEEE floating point number.
	  By David N. Smith with significant performance
	  improvements by Luciano Esteban Notarfrancesco.
	  (Version of 11April97).
	Refactoring and simplification by jmv"
	
	^self
		partValues: [ :sign :exponent :mantissa | | zeroBitsCount |
			" Prepare result. If exponent is greater than mantissa size, result is an integer"
			(exponent >= 52 or: [
					zeroBitsCount := mantissa lowBit - 1.
					exponent + zeroBitsCount >= 52 ])
				ifTrue: [
					"result is an integer number"
					 sign * mantissa bitShift: exponent - 52 ]
				ifFalse: [
					" This is the 'obvious' way. Better do Luciano's trick below:"
					"result := Fraction
						numerator: sign * mantissa
						denominator: (1 bitShift: 52 - exponent)."
					" Form the result. When exp>52, the exponent is adjusted by
					  the number of trailing zero bits in the mantissa to minimize
					  the (huge) time that could be spent in #gcd:. "
					Fraction
						numerator: (sign * (mantissa bitShift: 0 - zeroBitsCount))
						denominator: (1 bitShift: 52 - exponent - zeroBitsCount) ]
		]
		ifInfinite: [ self error: 'Cannot represent infinity as a fraction' ]
		ifNaN: [ self error: 'Cannot represent Not-a-Number as a fraction' ].
]

{ #category : 'accessing' }
Float >> at: index  [
	"Avoid primitive in Object>>at:"

	^self basicAt: index
]

{ #category : 'accessing' }
Float >> at: index put: value  [
	"Avoid primitive in Object>>at:put:"

	^self basicAt: index put: value
]

{ #category : 'accessing' }
Float >> basicAt: index [
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
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'accessing' }
Float >> basicAt: index put: value [
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
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'accessing' }
Float >> byteSize [
	^ 8
]

{ #category : 'mathematical functions' }
Float >> copySignTo: aNumber [
	"Return a number with same magnitude as aNumber and same sign as self.
	Implementation note: take care of Float negativeZero, which is considered as having a negative sign."

	self isNaN ifTrue: [ ^self ].
	(self > 0.0 or: [(self at: 1) = 0]) ifTrue: [^ aNumber abs].
	^aNumber withNegativeSign
]

{ #category : 'mathematical functions' }
Float >> cos [
	"Answer the cosine of the receiver taken as an angle in radians."

	^ (self + Halfpi) sin
]

{ #category : 'mathematical functions' }
Float >> cosh [
	"Answer receivers hyperbolic cosine."
	
	| ex |
	ex := self abs exp.
	^(ex + ex reciprocal) / 2
]

{ #category : 'mathematical functions' }
Float >> degreeCos [
	"If finite, allow for special values such as cos(60 degrees) = 1/2"
	
	self isFinite ifTrue: [^super degreeCos].
	^self degreesToRadians cos
]

{ #category : 'mathematical functions' }
Float >> degreeSin [
	"If finite, allow for special values such as cos(30 degrees) = 1/2"
	
	self isFinite ifTrue: [^super degreeSin].
	^self degreesToRadians sin
]

{ #category : 'converting' }
Float >> degreesToRadians [
	^self * RadiansPerDegree
]

{ #category : 'mathematical functions' }
Float >> expNonPrimitive [
	"Answer e raised to the receiver power."

	| base fract correction delta div |

	"Taylor series"
	"check the special cases"
	self < 0.0 ifTrue: [^ (self negated exp) reciprocal].
	self = 0.0 ifTrue: [^ 1].
	self abs > MaxValLn ifTrue: [self error: 'exp overflow'].

	"get first approximation by raising e to integer power"
	base := E raisedToInteger: (self truncated).

	"now compute the correction with a short Taylor series"
	"fract will be 0..1, so correction will be 1..E"
	"in the worst case, convergance time is logarithmic with 1/Epsilon"
	fract := self fractionPart.
	fract = 0.0 ifTrue: [ ^ base ].  "no correction required"

	correction := 1.0 + fract.
	delta := fract * fract / 2.0.
	div := 2.0.
	[delta >= base ulp] whileTrue: [
		correction := correction + delta.
		div := div + 1.0.
		delta := delta * fract / div].
	correction := correction + delta.
	^ base * correction
]

{ #category : 'accessing' }
Float >> exponent [
	"Usually run as a primitive in specific subclass.
	Consider the receiver to be represented as a power of two
	multiplied by a mantissa between one and two  or between -1 and -2 (#significand).
	 Answer with the SmallInteger to whose power two is raised.
	Note: invalid for infinities, NaN and zero.
	See also #signPart,  #significandAsInteger and #exponentPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. f significand . f exponent. f significand * (2 raisedToInteger: f exponent) = f } print ].
	"

	self = 0.0 ifTrue: [^MinValLogBase2-1].
	self isFinite ifFalse: [^Float emax+1].
	self isDenormalized ifTrue: [^MinValLogBase2 + self mantissaPart asFloat exponent].
	^self exponentPart
]

{ #category : 'accessing' }
Float >> exponentBits [
	"
	Actual bits for the exponent part of the floating point representation.
	Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f exponentBits. f exponentPart. f exponent } print ].
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits | exponentBits ]
]

{ #category : 'accessing' }
Float >> exponentPart [
	"
	Exponent part of the floating point representation.
	Valid for any floating point number (except zeros, infinities and NaNs).
	Includes correction of stored exponent bits for denormals (where it acts as a label, not a real exponent).
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self partValues: [ :sign :exponent :mantissa | exponent ]
]

{ #category : 'accessing' }
Float >> floatsAwayFrom: aFloat [

	| count2 count1 |
	(self isNaN or: [ aFloat isNaN ]) ifTrue: [ ^ Float nan ].
	count2 := self partBits: [:s :e :m | (e bitShift: self class precision - 1) + m * (s * -2 + 1)].
	count1 := aFloat partBits: [:s :e :m | (e bitShift: self class precision - 1) + m * (s * -2 + 1)].
	^count2 - count1
]

{ #category : 'mathematical functions' }
Float >> floorLog: radix [
	"Answer the floor of the log base radix of the receiver.
	The result may be off by one due to rounding errors, except in base 2."

	(radix = 2 and: [self > 0.0 and: [self isFinite]]) ifTrue: [^self exponent].
	^ (self log: radix) floor

]

{ #category : 'inspecting' }
Float >> hasContentsInExplorer [

	^false
]

{ #category : 'comparing' }
Float >> hash [
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
	   ((self basicAt: 2) bitShift: -4)
]

{ #category : 'printing' }
Float >> hex [
	^ String streamContents: [ :strm | | word nibble |
		1 to: 2 do: [ :i |
			word := self at: i.
			1 to: 8 do: [ :s |
				nibble := (word bitShift: -8+s*4) bitAnd: 16rF.
				strm nextPut: ('0123456789ABCDEF' at: nibble+1)]]]
"
(-2.0 to: 2.0) collect: [:f | f hex]
"
]

{ #category : 'converting' }
Float >> inMinusPiToPiRange [
	"For angles in radians. Add or remove whole turns until we get to the (-Pi .. +Pi] range"
	| answer |
	answer := self \\ Twopi.
	answer > Pi ifTrue: [
		answer := answer - Twopi ].
	^ answer
]

{ #category : 'accessing' }
Float >> integerPart [
	"Answer with a new Float whose value is the receiver's truncated value."

	^self - self fractionPart
]

{ #category : 'testing' }
Float >> isDenormalized [
	"Denormalized numbers are only represented as BoxedFloat64"

	^ false
]

{ #category : 'testing' }
Float >> isFinite [
	"Infinities and Not a Number are only represented as BoxedFloat64"

	^ true
]

{ #category : 'testing' }
Float >> isFloat [
	^ true
]

{ #category : 'testing' }
Float >> isFloatOrFloatComplex [
	^ true
]

{ #category : 'testing' }
Float >> isInfinite [
	"Infinities are only represented as BoxedFloat64"

	^ false

]

{ #category : 'testing' }
Float >> isLiteral [
	"There is no literal representation of NaN.
	However, there are literal representations of Infinity, like 1.0e1000.
	But since they are not able to print properly, only case of finite Float is considered."
	
	^self isFinite
]

{ #category : 'testing' }
Float >> isNaN [
	"Not a Number are only represented as BoxedFloat64"

	^ false
]

{ #category : 'comparing' }
Float >> isWithin: anInteger floatsFrom: aNumber [

	^(self floatsAwayFrom: aNumber) abs <= anInteger
]

{ #category : 'testing' }
Float >> isZero [
	^self = 0.0
]

{ #category : 'comparing' }
Float >> literalEqual: aFloat [
	"Two float literals can be replaced by a single one only if their representation have the same bits.
	For example, zero and negativeZero are equal, but not literally equal."

	^self class == aFloat class and: [(self at: 1) = (aFloat at: 1) and: [(self at: 2) = (aFloat at: 2)]]
]

{ #category : 'mathematical functions' }
Float >> lnNonPrimitive [
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

	"Float e ln 1.0"
]

{ #category : 'mathematical functions' }
Float >> log [
	"Answer the base 10 logarithm of the receiver."

	^ self ln / Ln10
]

{ #category : 'mathematical functions' }
Float >> log: aNumber [
	"Answer the log base aNumber of the receiver."

	^ self ln / aNumber ln
]

{ #category : 'mathematical functions' }
Float >> log2 [
	"Answer the base 2 logarithm of the receiver.
	Arrange to answer exact result in case of exact power of 2."

	|  s  |
	 s := self significand.
	^s > 1.3333333333333333
		ifTrue: [(0.5 * s) ln / Ln2 + (1 + self exponent)]
		ifFalse: [s ln / Ln2 + self exponent]
]

{ #category : 'accessing' }
Float >> mantissaBits [
	"
	Actual bits for the mantissa part of the floating point representation.
	Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f mantissaBits. f mantissaPart. f significand. } print ].
	"
	^ self partBits: [ :signBit :exponentBits :mantissaBits | mantissaBits ]
]

{ #category : 'accessing' }
Float >> mantissaPart [
	"Equivalent to #significandAsInteger."
	"The actual bits in the mantissa of the receiver, as an integer, including the implicit leading 1 if appropriate.
	Does not include the sign.
	See #exponentPart and #signPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f mantissaPart * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self partValues: [ :sign :exponent :mantissa | mantissa ]
]

{ #category : 'arithmetic' }
Float >> negated [
	"Answer a Number that is the negation of the receiver.
	Implementation note: this version cares of negativeZero."

	^-1.0 * self
]

{ #category : 'accessing' }
Float >> nextAwayFromZero [
	"Answer the Float with smallest magnitude but larger than ours, with the same sign
	Only for finite numbers."

	^ self partValues: [ :sign :myExponent :myMantissa |
		| exponent mantissa |
		myMantissa = 16r1FFFFFFFFFFFFF
			ifTrue: [
				mantissa := 16r10000000000000.
				exponent := myExponent +1 ]
			ifFalse: [
				mantissa := myMantissa+1.
				exponent := myExponent ].
		Float signPart: sign mantissaPart: mantissa exponentPart:  exponent ]
]

{ #category : 'accessing' }
Float >> nextTowardsZero [
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
		Float signPart: sign mantissaPart: mantissa exponentPart:  exponent ]
]

{ #category : 'mathematical functions' }
Float >> nthRoot: aPositiveInteger [
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
		ifFalse: [ self raisedTo: 1.0 / aPositiveInteger ]
]

{ #category : 'converting' }
Float >> partBits: aThreeArgumentBlock [
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
	^aThreeArgumentBlock value: signBit value: exponentBits value: mantissaBits
]

{ #category : 'converting' }
Float >> partValues: aThreeArgumentBlock [
	^ self
		partValues: aThreeArgumentBlock
		ifInfinite: [ self error: 'Can not handle infinity' ]
		ifNaN: [ self error: 'Can not handle Not-a-Number' ].
]

{ #category : 'converting' }
Float >> partValues: aThreeArgumentBlock ifInfinite: infinityBlock ifNaN: nanBlock [
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
	]
]

{ #category : 'accessing' }
Float >> predecessor [
	"Answer the largest Float smaller than self"

	self isFinite ifFalse: [
		(self isNaN or: [self negative]) ifTrue: [^self].
		^Float fmax].
	self signBit = 1 ifTrue: [ "Negative or -0.0"
		^ self nextAwayFromZero ].
	self = 0.0 ifTrue: [
		^ -0.0 ].
	^ self nextTowardsZero.
]

{ #category : 'printing' }
Float >> printAsIEEE32BitPrecisionFloatOn: aStream base: base [
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:mantissaSignificantBits:
	Print as a 32 bit Float" 

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base mantissaSignificantBits: 24]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self = 0.0
				ifTrue: [aStream nextPutAll: '0.0'. ^ self]
				ifFalse: [self negated absPrintOn: aStream base: base mantissaSignificantBits: 24]]
]

{ #category : 'printing' }
Float >> printAsLiteralOn: aStream [
	^self storeOn: aStream base: 10
]

{ #category : 'printing' }
Float >> printOn: aStream base: base [
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:" 

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self = 0.0
				ifTrue: [aStream nextPutAll: '0.0']
				ifFalse: [self negated absPrintOn: aStream base: base]]
]

{ #category : 'printing' }
Float >> printOn: aStream fractionDigits: placesDesired [
	"This implementation avoids rounding errors doue to #rounded or #roundTo:
	Round to a suitable integer and insert the decimal point in the appropriately between the digits."

	| i s scaled |
	self isFinite ifFalse: [ ^self printOn: aStream ].

	placesDesired > 0 ifFalse: [
		^self rounded printOn: aStream ].

	scaled := self * (10 raisedTo: placesDesired).
	"If rounding could possibly print a sequence that is read back as a different float, then go the more expensive Fraction way.
	If the following line is commented, #testPrintShowingDecimalPlaces4 will fail!"
	scaled ulp > 1 ifTrue: [
		^ self asTrueFraction printOn: aStream fractionDigits: placesDesired ].
	i := scaled rounded.
	i negative ifTrue: [
		aStream nextPut: $-.
		i := i negated ].
	s := i printString.
	placesDesired + 1 > s size
		ifTrue: [
			aStream nextPutAll: '0.'.
			placesDesired - s size timesRepeat: [ aStream nextPut: $0 ].
			aStream nextPutAll: s ]
		ifFalse: [
			aStream
				nextPutAll: (s copyFrom: 1 to: s size-placesDesired);
				nextPut: $.;
				nextPutAll: (s copyFrom: s size-placesDesired+1 to: s size) ]
]

{ #category : 'converting' }
Float >> radiansToDegrees [
	^self / RadiansPerDegree
]

{ #category : 'mathematical functions' }
Float >> reciprocal [
	^ 1.0 / self
]

{ #category : 'mathematical functions' }
Float >> reciprocalFloorLog: radix  [
	"Quick computation of (self log: radix) floor, when self < 1.0.
	Avoids infinite recursion problems with denormalized numbers"

	| adjust scale n |
	adjust := 0.
	scale := 1.0.
	[(n := radix / (self * scale)) isInfinite]
		whileTrue:
			[scale := scale * radix.
			adjust := adjust + 1].
	^ ((n floorLog: radix) + adjust) negated
]

{ #category : 'mathematical functions' }
Float >> reciprocalLogBase2 [
	"optimized for self = 10, for use in conversion for printing"

	^ self = 10.0
		ifTrue: [Ln2 / Ln10]
		ifFalse: [Ln2 / self ln]
]

{ #category : 'private' }
Float >> replaceWordsFrom: start to: stop with: replacement startingAt: repStart [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	self primitiveFailed
]

{ #category : 'accessing' }
Float >> roundedHAFZ [
	"Answer the integer nearest the receiver.
	Use the rounding rule commonly taught in school and most familiar to most people.
		-1.5 roundedHAFZ. -2 .
		-2.5 roundedHAFZ. -3 .
		1.5 roundedHAFZ. 2 .
		2.5 roundedHAFZ.  3 .
	See https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero

	Extract from Wikipedia as of Nov 27, 2024:

This method, also known as commercial rounding, treats positive and negative values symmetrically, and therefore is free of overall positive/negative bias if the original numbers are positive or negative with equal probability. It does, however, still have bias away from zero. 

	See #rounded"

	self fractionPart abs < 0.5
		ifTrue: [^self truncated]
		ifFalse: [^self truncated + self sign].
]

{ #category : 'accessing' }
Float >> roundedHTE [
	"Answer the integer nearest the receiver.
		-1.5 roundedHTE. -2 .
		-2.5 roundedHTE. -2 .
		1.5 roundedHTE. 2 .
		2.5 roundedHTE. 2 .
	See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even
	Extract from Wikipedia as of Nov 27, 2024:
		Rounding half to even

		This is a tie-breaking rule without positive/negative bias and without bias toward/away from zero. By this convention, if the fractional part of x is 0.5, then y is the even integer nearest to x. Thus, for example, 23.5 becomes 24, as does 24.5; however, −23.5 becomes −24, as does −24.5. This function minimizes the expected error when summing over rounded figures, even when the inputs are mostly positive or mostly negative, provided they are neither mostly even nor mostly odd.

		This variant is also called convergent rounding, statistician's rounding, Dutch rounding, Gaussian rounding, odd–even rounding, or bankers' rounding.

		This is the default rounding mode used in IEEE 754 operations for results in binary floating-point formats.

		By eliminating bias, repeated addition or subtraction of independent numbers, as in a one-dimensional random walk, will give a rounded result with an error that tends to grow in proportion to the square root of the number of operations rather than linearly.

		However, this rule distorts the distribution by increasing the probability of evens relative to odds. Typically this is less important than the biases that are eliminated by this method. 
	"

	| truncated fractionPart |
	truncated := self truncated.
	fractionPart := self - truncated.
	(fractionPart > -0.5 and: [fractionPart < 0.5])
		ifTrue: [^truncated].
	fractionPart > 0.5
		ifTrue: [ ^truncated + 1 ].
	fractionPart < -0.5
		ifTrue: [ ^truncated - 1 ].
	truncated even ifTrue: [^truncated].
	self > 0
		ifTrue: [ ^truncated + 1 ]
		ifFalse: [ ^truncated - 1 ]
]

{ #category : 'mathematical functions' }
Float >> safeArcCos [
	"Answer the angle in radians."
	(self between: -1.0 and: 1.0)
		ifTrue: [^ self arcCos]
		ifFalse: [^ self sign arcCos]
]

{ #category : 'accessing' }
Float >> sign [
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0.
	Handle IEEE754 negative-zero by reporting a sign of -1
	Warning! This makes Float negativeZero the only number in the system such that
		x sign negated = x negated sign
	evaluates to false!
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
	^ 1
]

{ #category : 'accessing' }
Float >> signBit [
	"
	Actual sigh bit part of the floating point representation.
	0 means positive number or 0.0
	1 means negative number or -0.0
	Just extract the bit. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0. 0.0. -0.0} do: [ :f |
		{ f. f signBit. f signPart. f sign } print ].
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits | signBit ]
]

{ #category : 'accessing' }
Float >> signPart [
	"The sign of the mantissa.
	1 means positive number or 0.0
	-1 means negative number or -0.0
	See #mantissaPart and #exponentPart"
	"
	| f |
	f := -2.0.
	(f signPart * f mantissaPart * (2 raisedToInteger: f exponentPart-52)) asFloat.
	"
	^self partValues: [ :sign :exponent :mantissa | sign ]
]

{ #category : 'accessing' }
Float >> significand [
	"Answers mantissa as a Float between one and two (or between -1 and -2). See #exponent.
	See also #signPart,  #significandAsInteger and #exponentPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. f significand * (2 raisedToInteger: f exponent) = f } print ].
	"
	^ self timesTwoPower: (self exponent negated)
]

{ #category : 'accessing' }
Float >> significandAsInteger [
	"The actual bits in the mantissa of the receiver, as an integer, including the implicit leading 1 if appropriate.
	See #mantissaPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self mantissaPart
]

{ #category : 'mathematical functions' }
Float >> sinNonPrimitive [
	"Answer the sine of the receiver taken as an angle in radians."

	| sum delta self2 i |

	"Taylor series"
	"normalize to the range [0..Pi/2]"
	self < 0.0 ifTrue: [^ (0.0 - ((0.0 - self) sin))].
	self > Twopi ifTrue: [^ (self \\ Twopi) sin].
	self > Pi ifTrue: [^ (0.0 - (self - Pi) sin)].
	self > Halfpi ifTrue: [^ (Pi - self) sin].

	"unroll loop to avoid use of abs"
	sum := delta := self.
	self2 := 0.0 - (self * self).
	i := 2.0.
	[delta >= sum ulp] whileTrue: [
		"once"
		delta := (delta * self2) / (i * (i + 1.0)).
		i := i + 2.0.
		sum := sum + delta.
		"twice"
		delta := (delta * self2) / (i * (i + 1.0)).
		i := i + 2.0.
		sum := sum + delta].
	^ sum
]

{ #category : 'mathematical functions' }
Float >> sinh [
	"Answer receivers hyperbolic sine"
	
	| ex |
	ex := self abs exp.
	^ (ex - ex reciprocal) / 2 * self sign
]

{ #category : 'mathematical functions' }
Float >> smoothIsAbsBelow: threshold [
	"A Function that is 1 at zero, 0 outside a bounded interval of size 4*threshold, and is continuous and differentiable.
	
	It is a 'smooth' version of an #isAbsBelow: function: x abs < threshold.
	Useful when we need to 'count' stuff, but still want continuous and differentiable stuff."
	"
Compiler evaluate: '
	| g |
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < Float halfPi ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothIsAbsBelow: Float halfPi ] color: Color red.
	(g embeddedInMorphicWindowLabeled: ''graph'') openInWorld'
	"
	| scaled |
	scaled := self / threshold.
	^ scaled abs < 2 ifTrue: [ (scaled * Float halfPi) cos + 1.0 * 0.5 ] ifFalse: [ 0 ]
]

{ #category : 'mathematical functions' }
Float >> smoothStep [
	"A Function that is
		x <= -1 	-> 		1
		-1 < x < 1 	->		smooth differentiable transition from 1 to 0
		1 <= x 		-> 		0
	
	It is a 'smooth' version of an #negative
	Has countless applications. For example in image and signal processing, but also in other fields."
	"
Compiler evaluate: '
	| g |
	Feature require: ''Morphic-Widgets-Extras''.
	g := FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < 1 ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothStep ] color: Color red.
	(g embeddedInMorphicWindowLabeled: ''graph'') openInWorld'
	"
	self isNaN ifTrue: [ ^self ].
	self > -1 ifFalse: [ ^ 1 ].
	1 > self ifFalse: [ ^ 0 ].
	^ (self +2 * Float halfPi) sin * 0.5 + 0.5
]

{ #category : 'mathematical functions' }
Float >> sqrt [
	"Answer the square root of the receiver. 
	 Use primitive if available, or Smalltalk code if primitive is unavailable or fails."

	| prim |
	prim := self primSqrt.
	prim isNaN ifFalse: [ ^prim ].

	^ self sqrtNonPrimitive
]

{ #category : 'mathematical functions' }
Float >> sqrtNonPrimitive [
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
	exp := self exponent // 2.
	guess := self timesTwoPower: 0 - exp.
	[
		delta := self - (guess * guess) / (guess * 2.0).
		delta abs >= guess ulp ]
	whileTrue: [
		guess := guess + delta ].
	^ guess
]

{ #category : 'printing' }
Float >> storeOn: aStream base: base  [
	
	"Print the Number exactly so it can be interpreted back unchanged"
	
	self sign = -1 ifTrue: [aStream nextPutAll: '-'].
	base = 10 ifFalse: [aStream print: base; nextPut: $r].
	self = 0.0
		ifTrue: [aStream nextPutAll: '0.0']
		ifFalse: [self abs absPrintExactlyOn: aStream base: base]
]

{ #category : 'accessing' }
Float >> successor [
	"Answer the smallest Float greater than self"

	self isFinite ifFalse: [
		(self isNaN or: [self positive]) ifTrue: [^self].
		^Float fmax negated].
	self signBit = 0 ifTrue: [
		^ self nextAwayFromZero ].
	self = -0.0 ifTrue: [
		^ 0.0 ].
	^ self nextTowardsZero.
]

{ #category : 'mathematical functions' }
Float >> tan [
	"Answer the tangent of the receiver taken as an angle in radians."

	^ self sin / self cos
]

{ #category : 'mathematical functions' }
Float >> tanh [
	"Answer hyperbolic tangent of receiver.
	Trivial implementation is:
		^self sinh/self cosh
	This implementation takes care not to overflow."

	| ex emx |
	self = 0.0 ifTrue: [^self].	"Handle negativeZero"
	self > 20.0 ifTrue: [^1.0].
	self < -20.0 ifTrue: [^-1.0].
	ex := self exp.
	emx := ex reciprocal.
	^(ex - emx) / (ex + emx)
]

{ #category : 'intervals' }
Float >> to: stop count: n do: aBlock [
	"Quite like doing 
		self to: stop by: (stop-self)/(n-1) do: aBlock
	but ensuring that the last time, the block is evaluated exactly for stop, i.e. no rounding error is introduced
	This means that
		0.0 to: 1.0 by: 1.0/9.0 do: [ :each | (each@(each = 1.0)) print ]
	will print false for all  (nine!!!) values, and
		0.0 to: 1.0 by: 0.111111 do: [ :each | (each@(each = 1.0)) print ]
	will print false for all  (ten!!!) values, but
		0.0 to: 1.0 count: 10 do: [ :each | (each@(each = 1.0)) print ]
	will print true in the last iteration
	"
	self to: stop hops: n-1 do: aBlock
]

{ #category : 'intervals' }
Float >> to: stop hops: n do: aBlock [
	"Quite like doing 
		self to: stop by: (stop-self)/n do: aBlock
	but ensuring that the last time, the block is evaluated exactly for stop, i.e. no rounding error is introduced.
	Besides, the way for calculating steps inside the interval minimizes rounding error (wrt to incrementing and accumulator)
	This means that
		0.0 to: 1.0 by: 0.1 do: [ :each | (each@(each = 1.0)) print ]
	will print false for all values, but
		0.0 to: 1.0 hops: 10 do: [ :each | (each@(each = 1.0)) print ]
	will print true in the last iteration
	Warning: Having 10 hops means evaluating aBlock 11 times!
	"
"	self to: stop by: (stop-self)/n do: aBlock"

	| factor |
	factor := (stop - self) / n.
	aBlock value: self.
	1 to: n-1 do: [ :i |
		aBlock value: i * factor + self ].
	aBlock value: stop
]

{ #category : 'accessing' }
Float >> ulp [
	"Answer the unit of least precision of the receiver.
	Follow John Harrison's definition as described at
	https://en.wikipedia.org/wiki/Unit_in_the_last_place"
	
	self isFinite ifFalse: [^self abs].
	self = 0.0 ifTrue: [^0.0 nextAwayFromZero].
	^ (self - self nextTowardsZero) abs
]

{ #category : 'converting' }
Float >> withNegativeSign [
	"Same as super, but handle the subtle case of Float negativeZero"
	
	self = 0.0 ifTrue: [^self class negativeZero].  
	^super withNegativeSign
]
