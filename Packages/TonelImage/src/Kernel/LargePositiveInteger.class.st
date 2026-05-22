"
I represent positive integers beyond the range of SmallInteger. They and are encoded here as an array of 8-bit digits.  Care must be taken, when new values are computed, that any result that COULD BE a SmallInteger MUST BE a SmallInteger (see normalize).

Note that the bit manipulation primitives, bitAnd:, bitShift:, etc., = and ~= run without failure (and therefore fast) if the value fits in 32 bits.  This is a great help to the simulator.
"
Class {
	#name : 'LargePositiveInteger',
	#superclass : 'Integer',
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'testing' }
LargePositiveInteger class >> initializedInstance [
	^ 12345678901234567 copy
]

{ #category : 'arithmetic' }
LargePositiveInteger >> * anInteger [
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 29>
	^super * anInteger.
]

{ #category : 'arithmetic' }
LargePositiveInteger >> + anInteger [
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 21>
	^super + anInteger.
]

{ #category : 'arithmetic' }
LargePositiveInteger >> - anInteger [
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 22>
	^super - anInteger.
]

{ #category : 'arithmetic' }
LargePositiveInteger >> / anInteger [
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 30>
	^super / anInteger.
]

{ #category : 'arithmetic' }
LargePositiveInteger >> // anInteger [
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 32>
	^super // anInteger.
]

{ #category : 'comparing' }
LargePositiveInteger >> < anInteger [
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 23>
	^super < anInteger.
]

{ #category : 'comparing' }
LargePositiveInteger >> <= anInteger [
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 25>
	^super <= anInteger.
]

{ #category : 'comparing' }
LargePositiveInteger >> > anInteger [
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 24>
	^super > anInteger.
]

{ #category : 'comparing' }
LargePositiveInteger >> >= anInteger [
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 26>
	^super >= anInteger.
]

{ #category : 'arithmetic' }
LargePositiveInteger >> \\ anInteger [
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 31>
	^super \\ anInteger.
]

{ #category : 'arithmetic' }
LargePositiveInteger >> abs [

]

{ #category : 'converting' }
LargePositiveInteger >> asFloat [
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
	^sum
]

{ #category : 'bit manipulation' }
LargePositiveInteger >> bitAt: anInteger [
	"Optimize super algorithm to avoid long bit operations.
	Instead work on digits which are known to be SmallInteger and fast.
	Note that this algorithm does not work for negative integers."
	
	| digitIndex bitIndex |
	digitIndex := anInteger - 1 // 8 + 1.
	digitIndex > self digitLength ifTrue: [^0].
	bitIndex := (anInteger - 1 bitAnd: 2r111) + 1.
	^(self digitAt: digitIndex) bitAt: bitIndex
]

{ #category : 'bit manipulation' }
LargePositiveInteger >> bitReverse: highBit  [
	"This implementation is faster than super"
	
	| digitSize reversed adjust |
	highBit < self highBit ifTrue: [ self error: 'Not enough bits.' ].
	digitSize := highBit + 7 // 8.
	reversed := self class new: digitSize.
	1 to: self digitLength do: [:i |
		reversed digitAt: digitSize + 1 - i put: (self digitAt: i) byteReversed].
	adjust := highBit - (digitSize * 8).
	^adjust = 0
		ifTrue: [reversed normalize]
		ifFalse: [reversed bitShift: adjust]
]

{ #category : 'bit manipulation' }
LargePositiveInteger >> bitXor: arg [
	"Primitive 36 deals with only 64-bit values (up to 8 byte LargeIntegers).
	The inherited <primitive: 'primDigitBitXor' module:'LargeIntegers'> deals with 
	arbitrary sized large integers, but is much slower.
	This method gives a performance improvement for integers using 32 to 64 bits on 32 bit VMs,
	but only for 62 to 64 bits on 64 bits VMs.
	See http://forum.world.st/Integer-arithmetic-and-bit-operations-in-Squeak-and-Pharo-32bit-amp-64bit-tc4928994.html#none
	"

    <primitive:36>
    ^super bitXor: arg
]

{ #category : 'object serialization' }
LargePositiveInteger >> comeFullyUpOnReload: smartRefStream [
	"Squeak may serialize SmallIntegers as LargeIntegers. Convert them back to SmallInteger if needed."
         ^self normalize
]

{ #category : 'system primitives' }
LargePositiveInteger >> digitAt: index  [
	"Primitive. Answer the value of an indexable field in the receiver.   LargePositiveInteger uses bytes of base two number, and each is a 'digit' base 256.  Fail if the argument (the index) is not an Integer or is out of bounds. Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 60>
	self digitLength < index
		ifTrue: [^0]
		ifFalse: [^super at: index]
]

{ #category : 'system primitives' }
LargePositiveInteger >> digitAt: index put: value  [
	"Store the second argument (value) in the indexable field of the receiver 
	indicated by index.  Fail if the value is negative or is larger than 255.  Fail if the 
	index is not an Integer or is out of bounds.  Answer with the value that was 
	stored.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 61>
	^super at: index put: value
]

{ #category : 'system primitives' }
LargePositiveInteger >> digitLength [
	"Answer with the number of indexable fields in the receiver.  This value is the
	same as the largest legal subscript.  Essential.  See Object documentation
	whatIsAPrimitive."

	<primitive: 62>
	self primitiveFailed
]

{ #category : 'inspecting' }
LargePositiveInteger >> hasContentsInExplorer [
	^true
]

{ #category : 'comparing' }
LargePositiveInteger >> hash [

	self < `(2 raisedToInteger: Float emax+1)` ifFalse: [
		^ `Float infinity hash` ].

	self < Float maxExactInteger ifFalse: [
		"Will always enter here for 64 bits images.
		See Float>>#hash, Integer>>#hash and LargeNegativeInteger>>#hash"
		^ self asFloat hash ].

	"May normally only reach here in 32 bit images"

	"If could be a SmallInteger (regardless of the current word size, we want consistency between 32/64 bit systems)"
	self digitLength <= 8 ifTrue: [
		^ self hashMultiply ].

	^ByteArray
		hashBytes: self
		startingWith: self species hash
]

{ #category : 'bit manipulation' }
LargePositiveInteger >> hashMultiply [
	"Truncate to 28 bits and try again"

	^(self bitAnd: 16rFFFFFFF) hashMultiply
]

{ #category : 'bit manipulation' }
LargePositiveInteger >> highBit [
	"Answer the position of the leading bit or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to  
	get the highest bit of the magnitude."
	^ self highBitOfMagnitude
]

{ #category : 'bit manipulation' }
LargePositiveInteger >> highBitOfMagnitude [
	"Answer the position of the leading bit or zero if the  
	receiver is zero. 
	This method is used for LargeNegativeIntegers as well,  
	since Squeak's LargeIntegers are sign/magnitude."
	| byteIndex msByte |
	byteIndex := self digitLength.
	[byteIndex > 0] whileTrue:
		[
			msByte  := self at: byteIndex.
			msByte  > 0 ifTrue: [^byteIndex - 1 * 8 + msByte  highBit].
			byteIndex := byteIndex - 1
		].
	^0
]

{ #category : 'testing' }
LargePositiveInteger >> isLarge [
	^true
]

{ #category : 'testing' }
LargePositiveInteger >> isPowerOfTwo [
	"Return true if the receiver is an integral power of two. Optimized version."
	
	| size |
	1 to: (size := self digitLength) do: [ :index |
		| digit |
		(digit := self digitAt: index) = 0 ifFalse: [ 
			^size = index and: [ digit isPowerOfTwo ] ] ].
	^false
]

{ #category : 'mathematical functions' }
LargePositiveInteger >> ln [
	"This function is defined because super ln might overflow."
	| res h |
	res := super ln.
	res isFinite ifTrue: [^res].
	h := self highBit.
	^2 ln * h + (self / (1 << h)) asFloat ln
]

{ #category : 'mathematical functions' }
LargePositiveInteger >> log2 [
	"This function is defined because super log2 might overflow."
	| res h |
	res := super log2.
	res isFinite ifTrue: [^res].
	h := self highBit.
	^h + (self / (1 << h)) asFloat log2
]

{ #category : 'mathematical functions' }
LargePositiveInteger >> mightBeASquare [
	"In base 16, a square number can end only with 0,1,4 or 9 and
	- in case 0, only 0,1,4,9 can precede it,
	- in case 4, only even numbers can precede it.
	See http://en.wikipedia.org/wiki/Square_number
	So, in hex, the last byte must be one of:
		00
		10
		40
		90
		x1
		e4
		x9
	where x is any hex digit and e is any even digit
	
	Also, the receiver must be an aven power of two.
	This needs additional testing in the 00 suffix,
	as it implied in all the other conditions"
	| lsb |
	lsb := self digitAt: 1.
	^(lsb = 0 and: [ self lowBit odd ])		"00 (and even power of 2)"
		or: [ lsb = 16r40					"40"
		or: [ (lsb bitAnd: 16r7) = 1			"any|1 or any|9"
		or: [ (lsb bitAnd: 16r1F) = 4			"even|4"
		or: [ (lsb bitAnd: 16r7F) = 16 ]]]]	"10 or 90"
]

{ #category : 'arithmetic' }
LargePositiveInteger >> negated  [
	^ (self copyto: (LargeNegativeInteger new: self digitLength))
		normalize  "Need to normalize to catch SmallInteger minVal"
]

{ #category : 'testing' }
LargePositiveInteger >> negative [
	"Answer whether the receiver is mathematically negative."

	^ false
]

{ #category : 'converting' }
LargePositiveInteger >> normalize [
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen |
	<primitive: 'primNormalizePositive' module:'LargeIntegers'>
	"First establish len = significant length"
	len := oldLen := self digitLength.
	[len = 0 ifTrue: [^0].
	(self digitAt: len) = 0]
		whileTrue: [len := len - 1].

	"Now check if in SmallInteger range.  Fast compute SmallInteger maxVal digitLength"
	sLen := SmallInteger maxVal > 16r7FFFFFFF "we're definitely on 64bit if we are larger than (2 raisedTo: 31) - 1"
				ifTrue: [8]
				ifFalse: [4].
	(len <= sLen
	 and: [(self digitAt: sLen) <= (SmallInteger maxVal digitAt: sLen)])
		ifTrue: ["If so, return its SmallInt value"
				val := 0.
				len to: 1 by: -1 do:
					[:i | val := (val *256) + (self digitAt: i)].
				^ val].

	"Return self, or a shortened copy"
	len < oldLen
		ifTrue: [^ self growto: len]
		ifFalse: [^ self]
]

{ #category : 'testing' }
LargePositiveInteger >> positive [
	"Answer whether the receiver is positive or equal to 0. (ST-80 protocol).
	See also strictlyPositive"

	^ true
]

{ #category : 'printing' }
LargePositiveInteger >> printOn: aStream base: b [
	"Append a representation of this number in base b on aStream.
	In order to reduce cost of LargePositiveInteger ops, split the number in approximately two equal parts in number of digits."
	
	| halfDigits halfPower head tail nDigitsUnderestimate |
	"Don't engage any arithmetic if not normalized"
	(self digitLength = 0 or: [(self digitAt: self digitLength) = 0]) ifTrue: [^self normalize printOn: aStream base: b].
	
	nDigitsUnderestimate := b = 10
		ifTrue: [((self highBit - 1) * 1233 >> 12) + 1. "This is because (2 log)/(10 log)*4096 is slightly greater than 1233"]
		ifFalse: [self highBit quo: b highBit].
		
	"splitting digits with a whole power of two is more efficient"
	halfDigits := 1 bitShift: nDigitsUnderestimate highBit - 2.
	
	halfDigits <= 1
		ifTrue: ["Hmmm, this could happen only in case of a huge base b... Let lower level fail"
			^self printOn: aStream base: b nDigits: (self numberOfDigitsInBase: b)].
	
	"Separate in two halves, head and tail"
	halfPower := b raisedToInteger: halfDigits.
	head := self quo: halfPower.
	tail := self - (head * halfPower).
	
	"print head"
	head printOn: aStream base: b.
	
	"print tail without the overhead to count the digits"
	tail printOn: aStream base: b nDigits: halfDigits
]

{ #category : 'printing' }
LargePositiveInteger >> printOn: aStream base: b nDigits: n [
	"Append a representation of this number in base b on aStream using n digits.
	In order to reduce cost of LargePositiveInteger ops, split the number of digts approximatily in two
	Should be invoked with: 0 <= self < (b raisedToInteger: n)"
	
	| halfPower half head tail |
	n <= 1 ifTrue: [
		n <= 0 ifTrue: [self error: 'Number of digits n should be > 0'].
		
		"Note: this is to stop an infinite loop if one ever attempts to print with a huge base
		This can happen because choice was to not hardcode any limit for base b
		We let Character>>#digitValue: fail"
		^aStream nextPut: (Character digitValue: self) ].
	halfPower := n bitShift: -1.
	half := b raisedToInteger: halfPower.
	head := self quo: half.
	tail := self - (head * half).
	head printOn: aStream base: b nDigits: n - halfPower.
	tail printOn: aStream base: b nDigits: halfPower
]

{ #category : 'arithmetic' }
LargePositiveInteger >> quo: anInteger [
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 33>
	^super quo: anInteger.
]

{ #category : 'system primitives' }
LargePositiveInteger >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]

{ #category : 'testing' }
LargePositiveInteger >> sign [
	"Optimization. Answer 1 since receiver is greater than 0."

	^ 1

]

{ #category : 'mathematical functions' }
LargePositiveInteger >> sqrt [
	"If we know for sure no exact solution exists, and we have a reasonable  float approximation,
	then answer it without wasting time."
	| selfAsFloat |
	self mightBeASquare ifFalse: [
		selfAsFloat := self asFloat.
		selfAsFloat isFinite ifTrue: [
			^self asFloat sqrt ]].

	"If some exact solution might exist, or self asFloat isInfinite, call potentially expensive super"
	^super sqrt
]

{ #category : 'testing' }
LargePositiveInteger >> strictlyPositive [
	"Answer whether the receiver is mathematically positive."

	^ true
]

{ #category : 'converting' }
LargePositiveInteger >> withAtLeastNDigits: desiredLength [

	| new |

	self size >= desiredLength ifTrue: [^self].
	new := self class new: desiredLength.
	new
		replaceFrom: 1 
		to: self size 
		with: self 
		startingAt: 1.
	^new
]
