'From Cuis 4.2 of 25 July 2013 [latest update: #2770] on 18 May 2016 at 10:52:35.505007 am'!

!Behavior methodsFor: 'private' stamp: 'jmv 8/28/2011 22:07'!
spaceUsed
	"Answer a rough estimate of number of bytes used by this class and its metaclass. Does not include space used by class variables."

	| space |
	space _ 0.
	self selectorsDo: [ :sel | | method |
		space _ space + 16.  "dict and org'n space"
		method _ self compiledMethodAt: sel.
		space _ space + (method size + 6 "hdr + avg pad").
		method literalsDo: [ :lit |
			(lit isMemberOf: Array) ifTrue: [ space _ space + ((lit size + 1) * 4)].
			(lit isMemberOf: Float) ifTrue: [ space _ space + 12].
			(lit isMemberOf: String) ifTrue: [ space _ space + (lit size + 6)].
			(lit isMemberOf: LargeNegativeInteger) ifTrue: [ space _ space + ((lit size + 1) * 4)].
			(lit isMemberOf: LargePositiveInteger) ifTrue: [ space _ space + ((lit size + 1) * 4)]]].
	^ space! !


!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 4/2/2013 14:02'!
doubleAt: index bigEndian: bool 
	"Return a 64 bit float starting from the given byte index"
	| w1 w2 dbl |
	w1 := self unsignedLongAt: index bigEndian: bool.
	w2 := self unsignedLongAt: index + 4 bigEndian: bool.
	dbl := Float new. 
	bool
		ifTrue: [
			dbl basicAt: 1 put: w1.
			dbl basicAt: 2 put: w2]
		ifFalse: [
			dbl basicAt: 1 put: w2.
			dbl basicAt: 2 put: w1].
	^ dbl! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmv 5/18/2016 10:49'!
floatAt: index bigEndian: bool 
	"Return a 32 bit float starting from the given byte index.
	We answer an instance of Float, i.e. 64 bits, but holding the 32 bit Float."
	| w |
	w := self unsignedLongAt: index bigEndian: bool.
	^ Float fromIEEE32Bit: w! !


!Float64Array methodsFor: 'accessing' stamp: 'jmv 5/18/2016 10:50'!
floatAt: index
	"Return the element (e.g., 64 bit Float) at the given index.
	Use the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits.
	Allow subclasses to redefine #at:"
	| answer |

	answer _ Float new.
	answer replaceWordsFrom: 1 to: 2 with: self startingAt: index * 2 - 1.
	^answer! !


!FloatArray methodsFor: 'accessing' stamp: 'ar 2/2/2001 15:47'!
at: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 4/18/2011 14:41'!
macroBenchmark3   "Smalltalk macroBenchmark3"
	| testBlock tallies prev receiver |
	"Runs the stepping simulator with the messageTally tree (like tallySends)."
	testBlock _
		['Running the context step simulator'
			displayProgressAt: Sensor mousePoint
			from: 0 to: 200
			during:
				[:bar |
				1 to: 200 do:
				[:x | bar value: x.
				Float pi printString.
				15 factorial printString]]].
	tallies _ MessageTally new class: testBlock receiver class
							method: testBlock method.
	receiver _ nil.
	prev _ testBlock.
	thisContext sender
		runSimulated: testBlock
		contextAtEachStep:
			[:current |
			current == prev ifFalse: [
				"call or return"
				prev sender ifNotNil: [
					"call only"
					(receiver == nil or: [current receiver == receiver])
						ifTrue: [tallies tally: current by: 1]].
				prev _ current]].
! !


!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/24/2011 22:44'!
arCosh
	"Answer receiver's area hyperbolic cosine.
	That is the inverse function of cosh.
	Some possible implementations:
	^imaginary > 0 
		ifTrue: [(self + (self * self - 1) sqrt) ln]
		ifFalse: [(self + (self * self - 1) sqrt) ln negated]
	^self arcCos i
	This implementation provides an answer with a positive real part.
	It also avoids creating intermediate Complex."
	
	| x y tmp sh2x shx delta ch2x chx |
	imaginary = 0 ifTrue: [real abs > 1
			ifTrue: 
				[y := real < 0
					ifTrue: [Float pi]
					ifFalse: [0].
				x := real abs arCosh.
				^self class real: x imaginary: y]
			ifFalse: [^self class real: 0 imaginary: real arcCos]].
	tmp := self squaredNorm - 1 / 2.
	delta := tmp squared + imaginary squared.
	sh2x := tmp + delta sqrt.
	shx := sh2x sqrt.
	ch2x := 1 + sh2x.
	chx := ch2x sqrt.
	x := shx arSinh.
	y := imaginary copySignTo: (real / chx) arcCos.
	^self class real: x imaginary: y! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/24/2011 21:31'!
arcCos
	"Answer the arc cosine of the receiver.
	This is the inverse function of cos."

	| x y tmp sh2y shy delta ch2y chy |
	imaginary = 0 ifTrue: [real abs > 1
			ifTrue: 
				[x := real < 0
					ifTrue: [Float pi]
					ifFalse: [0].
				y := real copySignTo: real abs arCosh.
				^self class real: x imaginary: y]
			ifFalse: [^self class real: real arcCos imaginary: 0]].
	tmp := self squaredNorm - 1 / 2.
	delta := tmp squared + imaginary squared.
	sh2y := tmp + delta sqrt.
	shy := sh2y sqrt.
	ch2y := 1 + sh2y.
	chy := ch2y sqrt.
	y := imaginary copySignTo: shy arSinh.
	x := (real / chy) arcCos.
	^self class real: x imaginary: y negated! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/24/2011 22:46'!
arcSin
	"Answer the arc sine of the receiver.
	This is the inverse function of sin."

	| x y tmp delta sh2y shy ch2y chy |
	imaginary = 0 
		ifTrue: 
			[real abs > 1 
				ifTrue: 
					[x := Float pi / 2 * real sign.
					y := (real copySignTo: real abs arCosh) negated.
					^self class real: x imaginary: y]
				ifFalse: [^self class real: real arcSin imaginary: 0]].
	tmp := (self squaredNorm - 1) / 2.
	delta := tmp squared + imaginary squared.
	sh2y := tmp + delta sqrt.
	shy := sh2y sqrt.
	ch2y := 1 + sh2y.
	chy := ch2y sqrt.
	y := imaginary copySignTo: shy arSinh.
	x := (real / chy) arcSin.
	^self class real: x imaginary: y! !

!Complex methodsFor: 'mathematical functions' stamp: 'nice 4/23/2011 01:34'!
arcTan: denominator 
	"Answer the  four quadrants arc tangent of receiver over denominator."

	^denominator isZero 
		ifTrue: 
			[self isZero 
				ifTrue: 
					["shouldn't it be an error ? ^DomainError signal: '0 arcTan: 0'"
					^self class real: 0 imaginary: 0]
				ifFalse: 
					[self class
						real: Float pi / (real copySignTo: 2)
						imaginary: 0]]
		ifFalse: 
			[| res |
			res := (self / denominator) arcTan.
			denominator real < 0 ifTrue: [res := res + Float pi].
			res real > Float pi 
				ifTrue: [res := res - (Float pi * 2)].
			res]! !


!AffineTransformation methodsFor: 'accessing' stamp: 'jmv 1/14/2015 14:19'!
at: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !


!Number methodsFor: 'arithmetic' stamp: 'mk 10/27/2003 21:00'!
arg
	"Answer the argument of the receiver (see Complex | arg)."
	
	self isZero ifTrue: [self error: 'Zero (0 + 0 i) does not have an argument.'].
	0 < self
		ifTrue: [^ 0]
		ifFalse: [^ Float pi]! !


!Number class methodsFor: 'instance creation' stamp: 'jmv 2/12/2010 10:32'!
readFrom: stringOrStream 
	"Answer a number as described on aStream.  The number may
	include a leading radix specification, as in 16rFADE"
	| value base aStream sign |
	aStream _ (stringOrStream isMemberOf: String)
		ifTrue: [ReadStream on: stringOrStream]
		ifFalse: [stringOrStream].
	(aStream nextMatchAll: 'NaN') ifTrue: [^ Float nan].
	sign _ (aStream peekFor: $-) ifTrue: [-1] ifFalse: [1].
	(aStream nextMatchAll: 'Infinity') ifTrue: [^ Float infinity * sign].
	base _ 10.
	value _ Integer readFrom: aStream base: base.
	(aStream peekFor: $r)
		ifTrue: [
			"<base>r<integer>"
			(base _ value) < 2 ifTrue: [
				base = 1 ifTrue: [ ^Integer readBaseOneFrom: aStream ].
				^self error: 'Invalid radix'].
			(aStream peekFor: $-) ifTrue: [sign _ sign negated].
			value _ Integer readFrom: aStream base: base].
	^ self readRemainderOf: value from: aStream base: base withSign: sign.! !

!Number class methodsFor: 'instance creation' stamp: 'jmv 9/1/2010 14:10'!
readRemainderOf: integerPart from: aStream base: base withSign: sign
	"Read optional fractional part and exponent, and return the final result"
	| value fraction fracpos peekChar |

	value := integerPart.
	(aStream peekFor: $.)
		ifTrue: [		 "<integer>.<fraction>"
			(aStream atEnd not and: [ aStream peek digitValue between: 0 and: base - 1 ])
				ifTrue:  [
					fracpos := aStream position.
					fraction := Integer readFrom: aStream base: base.
					fraction := fraction asFloat / (base raisedTo: aStream position - fracpos).
					value := value asFloat + fraction]
				ifFalse: [
					"oops - just <integer>."
					aStream skip: -1.
					"un-gobble the period"
					^ value * sign"Number readFrom: '3r-22.2'"]].
	peekChar := aStream peek.
	peekChar = $e | (peekChar = $d) | (peekChar = $q)
		ifTrue: [			"<number>(e|d|q)<exponent>>"
			aStream next.
			(aStream atEnd not and: [ (aStream peek digitValue between: 0 and: 9) or: [ aStream peek = $- ]])
				ifTrue:  [
					value := value * (base raisedTo: (Integer readFrom: aStream)) ]
				ifFalse: [
					"oops - just <number>."
					aStream skip: -1.]].
	^(value isFloat
		and: [ value = 0.0 and: [ sign = -1 ]])
			ifTrue: [ Float negativeZero ]
			ifFalse: [ value * sign ]! !


!Float methodsFor: 'mathematical functions' stamp: 'nice 5/4/2011 21:29'!
arTanh
	"Answer receiver's area hyperbolic tangent.
	That is the inverse function of tanh."

	self = 0.0 ifTrue: [^self].	"Handle negativeZero"
	self abs = 1 ifTrue: [^self copySignTo: Float infinity].
	self abs > 1 
		ifTrue: 
			[^DomainError signal: 'Receiver must be between -1.0 and 1.0'].
	^((1 + self) / (1 - self)) ln / 2! !

!Float methodsFor: 'mathematical functions' stamp: 'nice 9/21/2010 23:30'!
sign: aNumber
	"Return a Number with the same sign as aNumber and same magnitude as self.
	Implementation is different from super to handle the special case of Float negativeZero."

	(self = 0.0 and: [aNumber sign negative]) ifTrue: [^Float negativeZero].
	^aNumber copySignTo: self! !

!Float methodsFor: 'converting' stamp: 'jmv 12/1/2015 13:02'!
asIEEE32BitPrecisionFloat
	"Answer a 64-bit Float, but using only the precision of a 32-bit Float, as used in FloatArray and 'float' in the C world."
	
	^ Float fromIEEE32Bit: self asIEEE32BitWord! !

!Float methodsFor: 'truncation and round off' stamp: 'nice 12/2/2009 17:35'!
predecessor
	| ulp |
	self isFinite ifFalse: [
		(self isNaN or: [self negative]) ifTrue: [^self].
		^Float fmax].
	self = 0.0 ifTrue: [^Float fmin negated].
	ulp := self ulp.
	^self - (0.5 * ulp) = self
		ifTrue: [self - ulp]
		ifFalse: [self - (0.5 * ulp)]! !

!Float methodsFor: 'truncation and round off' stamp: 'nice 12/2/2009 17:35'!
successor
	| ulp |
	self isFinite ifFalse: [
		(self isNaN or: [self positive]) ifTrue: [^self].
		^Float fmax negated].
	self = 0.0 ifTrue: [^Float fmin].
	ulp := self ulp.
	^self + (0.5 * ulp) = self
		ifTrue: [self + ulp]
		ifFalse: [self + (0.5 * ulp)]! !

!Float methodsFor: 'truncation and round off' stamp: 'nice 5/4/2011 21:45'!
ulp
	"Answer the unit of least precision of self (the power of two corresponding to last bit of mantissa)"
	
	| exponent |
	self isFinite ifFalse: [^self abs].
	self = 0.0 ifTrue: [^Float fmin].
	exponent := self exponent.
	^exponent < self class emin
		ifTrue: [Float fminDenormalized]
 		ifFalse: [Float epsilon timesTwoPower: exponent]! !


!Float class methodsFor: 'instance creation' stamp: 'jmv 1/2/2015 10:00'!
fromIEEE32Bit: word
  	"Convert the given 32 bit word (which is supposed to be a positive 32bit value) from a 32bit IEEE floating point representation into an actual Squeak float object (being 64bit wide). Should only be used for conversion in FloatArrays or likewise objects."
  	
 	| sign exponent mantissa exponentBits fractionBits |
  	word negative ifTrue: [ ^ self error: 'Cannot deal with negative numbers' ].
  	word = 0 ifTrue: [ ^ Float zero ].
 	word = 16r80000000 ifTrue: [ ^Float negativeZero ].
  	
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
 
	" Replace omitted leading 1 in fraction if appropriate"
	"If expPart = 0, I am +/-zero or a denormal value. In such cases, no implicit leading bit in mantissa"	
 	exponentBits = 0
 		ifTrue: [
 			"gradual underflow (denormalized number)
 			There is no implied one, but the exponent is -126"
			mantissa _ fractionBits.
			exponent _ exponent + 1 ]
 		ifFalse: [
			mantissa _ fractionBits + 16r800000 ].
  	
  	"Create new float"
 	^(sign * mantissa)
 			asFloat timesTwoPower: exponent - 23! !


!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'nice 2/12/2015 02:48'!
timesTwoPower: anInteger 
	"Primitive. Answer with the receiver multiplied by 2.0 raised
	to the power of the argument.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 54>
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
	
	^ self * (2.0 raisedToInteger: anInteger)! !


!Fraction methodsFor: 'converting' stamp: 'nice 9/25/2011 12:47'!
asFloat
	"Answer a Float that closely approximates the value of the receiver.
	This implementation will answer the closest floating point number to the receiver.
	In case of a tie, it will use the IEEE 754 round to nearest even mode.
	In case of overflow, it will answer +/- Float infinity."

	| a b mantissa exponent hasTruncatedBits lostBit n ha hb hm |
	a := numerator abs.
	b := denominator.	"denominator is always positive"
	ha := a highBit.
	hb := b highBit.
	
	"Number of bits to keep in mantissa plus one to handle rounding."
	n := 1 + Float precision.

	"If both numerator and denominator are represented exactly in floating point number,
	then fastest thing to do is to use hardwired float division."
	(ha < n and: [hb < n]) ifTrue: [^numerator asFloat / denominator asFloat].

	"Shift the fraction by a power of two exponent so as to obtain a mantissa with n bits.
	First guess is rough, the mantissa might have n+1 bits."
	exponent := ha - hb - n.
	exponent >= 0
		ifTrue: [b := b bitShift: exponent]
		ifFalse: [a := a bitShift: exponent negated].
	mantissa := a quo: b.
	hasTruncatedBits := a > (mantissa * b).
	hm := mantissa highBit.
	
	"Check for gradual underflow, in which case the mantissa will loose bits.
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
	The case of tie (mantissa odd & hasTruncatedBits not)
	will be handled by Integer>>asFloat."
	(hasTruncatedBits and: [mantissa odd])
		ifTrue: [mantissa := mantissa + 1].

	^ (self positive
			ifTrue: [mantissa asFloat]
			ifFalse: [mantissa asFloat negated])
		timesTwoPower: exponent! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 10/19/2011 22:39'!
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver.
	See #nthRootAlt: for an alternative implementation."

	| selfAsFloat floatResult guess delta higher lower raised |
	selfAsFloat _ self asFloat.

	"If we can't do Float arithmetic because we are too big, then look for an exact answer in exact arithmetic"
	selfAsFloat isInfinite ifTrue: [
		guess _ self nthRootTruncated: aPositiveInteger.
		(guess raisedToInteger: aPositiveInteger) = self
			ifTrue: [ ^ guess ].
		"Nothing else can be done. No exact answer means answer must be a Float.
		Answer the best we have."
		^guess asFloat ].

	floatResult _ selfAsFloat nthRoot: aPositiveInteger.
	guess _ floatResult rounded.

	"If got an exact answer, answer it."
	raised _ guess raisedToInteger: aPositiveInteger.
	raised = self
		ifTrue: [ ^ guess ].

	"In this case, maybe it failed because we are such a big integer that the Float
	method gets inexact, even if we are a whole square number.
	Note 1(jmv): This algorithm is faster than #nthRootTruncated: for big n (aPositiveInteger)
	but fails if self asFloat isInfinite.
	Note 2(jmv): The algorithms I found for computing the nthRoot would havily use
	very large fractions. I wrote this one, that doesn't create fractions."
	selfAsFloat abs >= (Float maxExactInteger asFloat raisedToInteger: aPositiveInteger)
		ifTrue: [
			raised > self
				ifTrue: [
					higher _ guess.
					delta _  floatResult predecessor - floatResult.
					[
						floatResult _ floatResult + delta.
						lower _ floatResult rounded.
						(lower raisedToInteger: aPositiveInteger) > self ] whileTrue: [
							delta _ delta * 2.
							higher _ lower ] ]
				ifFalse: [
					lower _ guess.
					delta _  floatResult successor - floatResult.
					[
						floatResult _ floatResult + delta.
						higher _ floatResult rounded.
						(higher raisedToInteger: aPositiveInteger) < self ] whileTrue: [
							delta _ delta * 2.
							lower _ higher ]].
			[ higher - lower > 1 ] whileTrue: [
				guess _ lower + higher // 2.
				raised _ guess raisedToInteger: aPositiveInteger.
				raised = self
					ifTrue: [
						^ guess ].
				raised > self
					ifTrue: [ higher _ guess ]
					ifFalse: [ lower _ guess ]]].

	"We need an approximate result"
	^floatResult! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 10/29/2011 14:27'!
sqrt
	"Answer the square root of the receiver."

	| selfAsFloat floatResult guess |
	selfAsFloat _ self asFloat.
	floatResult _ selfAsFloat sqrt.

	floatResult isInfinite ifFalse: [
		guess _ floatResult truncated.

		"If got an exact answer, answer it. Otherwise answer float approximate answer."
		guess squared = self
			ifTrue: [ ^ guess ]].

	"In this case, maybe it failed because we are such a big integer that the Float method becomes
	inexact, even if we are a whole square number. So, try the slower but more general method."
	selfAsFloat >= Float maxExactInteger asFloat squared
		ifTrue: [
			guess _ self sqrtFloor.
			guess squared = self
				ifTrue: [ ^guess ].
			
			"Nothing else can be done. No exact answer means answer must be a Float.
			Answer the best we can which is the rounded sqrt."
			guess _ (self * 4) sqrtFloor.
			^(guess // 2 + (guess \\ 2)) asFloat ].

	"We need an approximate result"
	^floatResult! !


!LargePositiveInteger methodsFor: 'converting' stamp: 'nice 9/22/2011 01:53'!
asFloat
	"Answer a Float that best approximates the value of the receiver.
	This algorithm is optimized to process only the significant digits of a LargeInteger.
	And it does honour IEEE 754 round to nearest even mode in case of excess precision (see details below)."
	
	"How numbers are rounded in IEEE 754 default rounding mode:
	A shift is applied so that the highest 53 bits are placed before the floating point to form a mantissa.
	The trailing bits form the fraction part placed after the floating point.
	This fractional number must be rounded to the nearest integer.
	If fraction part is 2r0.1, exactly between two consecutive integers, there is a tie.
	The nearest even integer is chosen in this case.
	Examples (First 52bits of mantissa are omitted for brevity):
	2r0.00001 is rounded downward to 2r0
	2r1.00001 is rounded downward to 2r1
	2r0.1 is a tie and rounded to 2r0 (nearest even)
	2r1.1 is a tie and rounded to 2r10 (nearest even)
	2r0.10001 is rounded upward to 2r1
	2r1.10001 is rounded upward to 2r10
	Thus, if the next bit after floating point is 0, the mantissa is left unchanged.
	If next bit after floating point is 1, an odd mantissa is always rounded upper.
	An even mantissa is rounded upper only if the fraction part is not a tie."
	
	"Algorihm details:
	Floating point hardware will correctly handle the rounding by itself with a single inexact operation if mantissa has one excess bit of precision.
	Except in the last case when extra bits are present after an even mantissa, we must round upper by ourselves.
	Note 1: the inexact flag in floating point hardware must not be trusted because it won't take into account the bits we truncated by ourselves.
	Note 2: the floating point hardware is presumed configured in default rounding mode."
	
	| mantissa shift sum excess |

	"Check how many bits excess the maximum precision of a Float mantissa."
	excess := self highBitOfMagnitude - Float precision.
	excess > 1
		ifTrue:
			["Remove the excess bits but one."
			mantissa := self bitShift: 1 - excess.
			shift := excess - 1.
			"Handle the case of extra bits truncated after an even mantissa."
			((mantissa bitAnd: 2r11) = 2r01 and: [self anyBitOfMagnitudeFrom: 1 to: shift])
				ifTrue: [mantissa := mantissa + 1]]
		ifFalse:
			[mantissa := self.
			shift := 0].

	"Now that mantissa has at most 1 excess bit of precision, let floating point operations perform the final rounding."
	sum := 0.0.
	1 to: mantissa digitLength do:
		[:byteIndex | 
		sum := sum + ((mantissa digitAt: byteIndex) asFloat timesTwoPower: shift).
		shift := shift + 8].
	^sum! !


!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 5/18/2016 10:51'!
initCachedState
	"TypeMap maps Smalltalk classes to type ID numbers which identify the data stream primitive formats.  nextPut: writes these IDs to the data stream.  NOTE: Changing these type ID numbers will invalidate all extant data stream files.  Adding new ones is OK.  
	Classes named here have special formats in the file.  If such a class has a subclass, it will use type 9 and write correctly.  It will just be slow.  (Later write the class name in the special format, then subclasses can use the type also.)
	 See nextPut:, next, typeIDFor:, & ReferenceStream>>isAReferenceType:"
	"
	DataStream initCachedState
	"

	| refTypes t |
	refTypes _ OrderedCollection new.
	t _ TypeMap _ Dictionary new: 80. "sparse for fast hashing"

	t at: UndefinedObject put: 1.   refTypes add: 0.
	t at: True put: 2.   refTypes add: 0.
	t at: False put: 3.   refTypes add: 0.
	t at: SmallInteger put: 4.	 refTypes add: 0.
	t at: String put: 5.   refTypes add: 1.
	t at: Symbol put: 6.   refTypes add: 1.
	t at: ByteArray put: 7.   refTypes add: 1.
	t at: Array put: 8.   refTypes add: 1.
	"(type ID 9 is for arbitrary instances of any class, cf. typeIDFor:)"
		refTypes add: 1.
	"(type ID 10 is for references, cf. ReferenceStream>>tryToPutReference:)"
		refTypes add: 0.
	t at: Bitmap put: 11.   refTypes add: 1.
	t at: Metaclass put: 12.   refTypes add: 0.
	"Type ID 13 is used for HyperSqueak User classes that must be reconstructed."
		refTypes add: 1.
	t at: Float put: 14.  refTypes add: 1.
	"Deprecated compact Rects."
		refTypes add: 1.
	"type ID 16 is an instance with short header.  See beginInstance:size:"
		refTypes add: 1.
	t at: String put: 17.   refTypes add: 1.	"new String format, 1 or 4 bytes of length"
	t at: WordArray put: 18.  refTypes add: 1.	"bitmap-like"
	"t at: WordArrayForSegment put: 19.  refTypes add: 1."		"bitmap-like"
	t at: FloatArray put: 20.  refTypes add: 1.	"And all other word arrays, both 16-bit and 32-bit.  See methods in ArrayedCollection.  Overridden in SoundBuffer."
	"For all classes that are like WordArrays, store them the way ColorArray is stored.  As bits, and able to change endianness."
	Smalltalk do: [:cls |
		cls isInMemory ifTrue: [
			cls isBehavior ifTrue: [
				cls isPointers not & cls isVariable & cls isWords ifTrue: [
					(t includesKey: cls) ifFalse: [t at: cls put: 20]]]]].

	t at: CompiledMethod put: 21.  refTypes add: 1.	"special creation method"
	"t at:  put: 22.  refTypes add: 0."
	ReferenceTypes _ refTypes.		"save it"! !

