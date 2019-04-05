'From Cuis 5.0 of 7 November 2016 [latest update: #3690] on 5 April 2019 at 5:29:12 pm'!

!Float commentStamp: 'jmv 4/5/2019 17:21:41' prior: 0!
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

Floats are (conceptually) approximate real numbers. That's why trig and other trascendental functions always answer Floats. That's why it is ok to round the result of operations. That's why Float is considered more general than Fraction in ST-80 and most Smalltalks. So, when we have a Float value, we must not think about it as a Rational but as a Real (actually as some unknown Real that could hopefully be close to the Rational we can actually represent). Keep this in mind when dealing with Floats, and especially avoid comparing them for equality.

When doing mixed operations with Floats and Fractions, Cuis, as most other Smalltalks, converts all values to Floats. Some other systems, including Pharo Smalltalk, Scheme and Lisp have two rules: when the answer is a Number, they convert to Float. But when the answer is a boolean (#<, #=, #<=, etc.) they convert to Fraction. We think this is a mistake. There should never be implicit conversions from Float to Fraction. Fractions are to hold exact values, and people expect Fractions to be exact. On the other hand, Floats are to hold approximations (and people should be aware of that!!). But an implicit conversion from Float to Fraction would give a Fraction that should not be considered an exact value (the value comes from an inexact Float), but that knowledge is lost, as it is an instance of Fraction.

If you want exact arithmetic, usual mathematical properties (like transitivity of equality), can live in the limited world of Rational numbers, and can afford a slight performance penalty, use Fraction instead. Avoid trascendental functions and never convert to Float.

In any case, most numeric computation is done on Float numbers. There are good reasons for that. One is that in most cases we don't need an exact answer. And in many cases we can't really have it: the inputs to algorithms already have a limited precision, or they use trascendental functions. And even when exact arithmetic possible, if we are doing sound synthesis, 24 bits of resolution is enough. For image processing and graphics, the result is never more than 16 bits per channel. So, these fields don't really need 64 bit Doubles. 32 bit Floats are enough. Other fields do need 64 bit Doubles, like physics simulations and geometry. Games usually prefer special, faster 32 bit Float operations in GPUs that have greater errors but are faster.

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
http://www.ima.umn.edu/2010-2011/W1.10-14.11/activities/Leeser-Miriam/Leeser-GPU-IMA-Jan2011.pdf!


!Number methodsFor: 'testing' stamp: 'jmv 4/5/2019 10:02:36'!
isFinite
	^ true! !


!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 09:09:44'!
complexConjugate
	"Return the complex conjugate of this complex number."

	^self class real: real imaginary: imaginary negated! !


!Float methodsFor: 'truncation and round off' stamp: 'jmv 4/5/2019 16:19:44'!
predecessor
	"Answer the largest Float smaller than self"

	self isFinite ifFalse: [
		(self isNaN or: [self negative]) ifTrue: [^self].
		^Float fmax].
	self signBit = 1 ifTrue: [ "Negative or -0.0"
		^ self nextAwayFromZero ].
	self = 0.0 ifTrue: [
		^ -0.0 ].
	^ self nextTowardsZero.! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 4/5/2019 16:11:39'!
signBit
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

	^ self partBits: [ :signBit :exponentBits :mantissaBits | signBit ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 4/5/2019 16:14:21'!
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

!Float methodsFor: 'truncation and round off' stamp: 'jmv 4/5/2019 16:18:03'!
successor
	"Answer the smallest Float greater than self"

	self isFinite ifFalse: [
		(self isNaN or: [self positive]) ifTrue: [^self].
		^Float fmax negated].
	self signBit = 0 ifTrue: [
		^ self nextAwayFromZero ].
	self = -0.0 ifTrue: [
		^ 0.0 ].
	^ self nextTowardsZero.! !

!Float methodsFor: 'testing' stamp: 'jmv 4/5/2019 16:14:46'!
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


!Float class methodsFor: 'instance creation' stamp: 'jmv 4/5/2019 17:23:54'!
fromIEEE32Bit: word
  	"Convert the given 32 bit word (which is supposed to be a positive 32bit value) from
	a 32bit IEEE floating point representation into an actual Float object (being
	64bit wide). Should only be used for conversion in FloatArrays or likewise objects."
  	
 	| sign exponent mantissa exponentBits fractionBits answerFractionBits delta signBit answerExponent |
  	word negative ifTrue: [ ^ self error: 'Cannot deal with negative numbers' ].
  	word = 0 ifTrue: [ ^ Float zero ].
 	word = 16r80000000 ifTrue: [ ^Float negativeZero ].
  	
	signBit _ word bitAnd: 16r80000000.
	sign _ (word bitShift: -31) = 0 ifTrue: [1] ifFalse: [-1].
  	exponentBits _ (word bitShift: -23) bitAnd: 16rFF.
  	fractionBits _ word bitAnd: 16r7FFFFF.
  
	" Special cases: infinites and NaN"
	exponentBits = 16rFF ifTrue: [
  		fractionBits = 0 ifFalse: [ ^ Float nan ].
  		^ sign positive
  			ifTrue: [ Float infinity ]
  			ifFalse: [ Float negativeInfinity ]].

	" Unbias exponent: 16r3FF is bias"
	exponent _ exponentBits - 16r7F.

"Older version."
false ifTrue: [
	" Replace omitted leading 1 in fraction if appropriate"
	"If expPart = 0, I am +/-zero or a denormal value. In such cases, no implicit leading bit in mantissa"
 	exponentBits = 0
 		ifTrue: [
 			"gradual underflow (denormalized number)
 			There is no implied one, but the exponent is -126"
			mantissa _ fractionBits.
			answerExponent _ exponent + 1 ]
 		ifFalse: [
			mantissa _ fractionBits + 16r800000.
			answerExponent _ exponent ].
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
			answerFractionBits _ fractionBits.
			answerExponent _ exponent ].
  
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
"! !

!Float class methodsFor: 'constants' stamp: 'jmv 4/5/2019 17:28:20'!
denormalized
	"Answer whether implementation supports denormalized numbers.
	Denormalized numbers guarantees that the result x - y is non-zero when x !!= y."
	
	^true! !

!Float class methodsFor: 'constants' stamp: 'jmv 4/5/2019 17:28:36'!
fminDenormalized
	"Answer the minimum denormalized value representable.
	Denormalized numbers guarantees that the result x - y is non-zero when x !!= y.
	"
	
	^1.0 timesTwoPower: MinValLogBase2! !

!Float class methodsFor: 'constants' stamp: 'jmv 4/5/2019 16:58:02'!
negativeZero
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

	^ NegativeZero! !


!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 4/5/2019 16:32:47'!
primSqrt
	"Answer the square root of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive.
	Note: 
	-0.0 primSqrt 
	-0.0 sqrt
	both evaluate to -0.0
	"

	<primitive: 55>
	^Float nan! !


!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 09:48:03'!
* aNumber
	"Answer the result of multiplying the receiver by aNumber."
	| c d newReal newImaginary |
	aNumber isComplex
		ifTrue: [
			c _ aNumber real.
			d _ aNumber imaginary.
			newReal _ (real * c) - (imaginary * d).
			newImaginary _ (real * d) + (imaginary * c) ]
		ifFalse: [
			newReal _ real * aNumber.
			newImaginary _ imaginary * aNumber ].
	^ Complex real: newReal imaginary: newImaginary! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 09:48:36'!
+ aNumber
	"Answer the sum of the receiver and aNumber."
	^ Complex
		real: real + aNumber real
		imaginary: imaginary + aNumber imaginary.! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 09:48:52'!
- aNumber
	"Answer the difference between the receiver and aNumber."
	^ Complex
		real: real - aNumber real
		imaginary: imaginary - aNumber imaginary.! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 10:34:02'!
/ aNumber
	"Answer the result of dividing receiver by aNumber"
	| c d newReal newImaginary s e f |
	aNumber isComplex
		ifTrue: [
			c _ aNumber real.
			d _ aNumber imaginary.
			e _ (real * c) + (imaginary * d).
			e isFinite ifFalse: [ ^ self divideFastAndSecureBy: aNumber ].
			f _ (imaginary * c) - (real * d).
			s _ (c * c) + (d * d).
			(e isFloat and: [ s  = 0.0 ]) ifTrue: [ ^ self divideFastAndSecureBy: aNumber ].
			newReal _ e / s.
			newImaginary _ f / s ]
		ifFalse: [
			newReal _ real / aNumber.
			newImaginary _ imaginary / aNumber ].
	^ Complex real: newReal imaginary: newImaginary! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 09:58:22'!
divideFastAndSecureBy: aComplex
	"Answer the result of dividing receiver by aNumber"
	" Both operands are scaled to avoid arithmetic overflow. 
	  This algorithm works for a wide range of values, and it needs only three divisions."
	| r d newReal newImaginary |
	aComplex real abs > aComplex imaginary abs
		ifTrue: [
			r _ aComplex imaginary / aComplex real.
			d _ r * aComplex imaginary + aComplex real.
			newReal _ r * imaginary + real / d.
			newImaginary _ r negated * real + imaginary / d ]
		ifFalse: [
			r _ aComplex real / aComplex imaginary.
			d _ r * aComplex real + aComplex imaginary.
			newReal _ r * real + imaginary / d.
			newImaginary _ r * imaginary - real / d ].
	^ Complex
		real: newReal
		imaginary: newImaginary.! !

!Complex methodsFor: 'comparing' stamp: 'jmv 4/5/2019 08:37:31'!
= anObject
	self == anObject ifTrue: [ ^ true ].
	anObject isNumber ifFalse: [^false].
	^real = anObject real and: [ imaginary = anObject imaginary ]! !


!Transcript class methodsFor: 'private' stamp: 'jmv 4/5/2019 16:11:01'!
finishEntry
	| newEntry |
	self unfinishedEntrySize > 0 ifTrue: [
		newEntry _ unfinishedEntry contents.
		unfinishedEntry reset.
		lastDisplayPosition _ 0.
		self addEntry: newEntry.
		self display ].! !

!methodRemoval: Complex #conjugated!
Complex removeSelector: #conjugated!
!methodRemoval: Complex #divideSecureBy:!
Complex removeSelector: #divideSecureBy:!
!methodRemoval: Number #adaptToComplex:andSend:!
Number removeSelector: #adaptToComplex:andSend:!
