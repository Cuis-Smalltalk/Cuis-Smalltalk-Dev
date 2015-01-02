'From Cuis 4.2 of 25 July 2013 [latest update: #2142] on 2 January 2015 at 10:00:21.278324 am'!

!Float methodsFor: 'converting' stamp: 'jmv 1/2/2015 09:54'!
partValues: aThreeArgumentBlock ifInfinite: aZeroOrOneArgBlock ifNaN: otherZeroOrOneOrTwoArgBlock
	"
	Float pi hex print
	Float pi partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	0.0 partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	For 0.0, exponent will be the minimum possible, i.e.  -1023, and mantissa will be 0.
	"
	| allBits sign exponent mantissa exponentBits fractionBits |

	" Extract the bits of an IEEE double float "
	allBits _ ((self basicAt: 1) bitShift: 32) + (self basicAt: 2).

	" Extract the sign and the biased exponent "
	sign _ (allBits bitShift: -63) = 0 ifTrue: [1] ifFalse: [-1].
	exponentBits _ (allBits bitShift: -52) bitAnd: 16r7FF.

	" Extract fractional part "
	fractionBits _ allBits bitAnd: 16r000FFFFFFFFFFFFF.

	" Special cases: infinites and NaN"
	exponentBits = 16r7FF ifTrue: [
		^fractionBits = 0
			ifTrue: [ aZeroOrOneArgBlock valueWithPossibleArgument: self ]
			ifFalse: [ otherZeroOrOneOrTwoArgBlock valueWithPossibleArgument: self and: fractionBits ]].

	" Unbias exponent: 16r3FF is bias"
	exponent _ exponentBits - 16r3FF.

	" Replace omitted leading 1 in fraction if appropriate"
	"If expPart = 0, I am +/-zero or a denormal value. In such cases, no implicit leading bit in mantissa"	
	exponentBits = 0
		ifTrue: [
			mantissa _ fractionBits.
			exponent _ exponent + 1 ]
		ifFalse: [
			mantissa _ fractionBits bitOr: 16r0010000000000000 ].

	"Evaluate the block"
	^aThreeArgumentBlock value: sign value: exponent value: mantissa! !


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

