'From Cuis7.9 [latest update: #8308] on 2 November 2026 at 1:16:26 pm'!

!SmallInteger methodsFor: 'bit manipulation' stamp: 'nice 30/Aug/2019, 6:04:00 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
highBit
	"Answer the index of the high order bit of the receiver, or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to 
	get the highest bit of the magnitude."
	<primitive: 575>
	^super highBit! !

!SmallInteger methodsFor: 'printing' stamp: 'jmv 2/Nov/2026, 3:48:49 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
printOn: aStream
	"Simplified version for SmallInteger and base 10.
	Needed for #MinimalKernel01, but doesn't hurt in the full system,
	as behavior is the same as inherited from Number."

	| base n divisor |
	self < 0 ifTrue: [
		"Special case: SmallInteger minVal negated is not a SmallInteger."
		self = -1152921504606846976 ifTrue: [
			aStream nextPutAll: '-1152921504606846976'.
			^self ].
		self = -1073741824 ifTrue: [ "Same, but for 32 bit systems. Doesn't hurt on 64 bits."
			aStream nextPutAll: '-1073741824'.
			^self ].
		aStream nextPut: $- .
		self negated printOn: aStream.
		^self ].
	base := 10.
	n := self.
	divisor := (base raisedToInteger: n decimalDigitLength - 1).
	[ divisor > 0 ] whileTrue: [
		| digit |
		digit := n // divisor.
		aStream nextPut: ('0123456789' at: digit + 1).
		n := n - (digit * divisor).
		divisor := divisor // base ].! !

!SmallInteger methodsFor: 'printing' stamp: 'jmv 2/Nov/2026, 3:49:50 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
printString
	"Highly optimized version for base 10
	and that we know it is a SmallInteger."
	
	| integer next result len |
	self = 0 ifTrue: [^'0'].
	self < 0 ifTrue: [
		"Special case: SmallInteger minVal negated is not a SmallInteger."
		self = -1152921504606846976 ifTrue: [
			^ '-1152921504606846976' ].
		self = -1073741824 ifTrue: [ "Same, but for 32 bit systems. Doesn't hurt on 64 bits."
			^ '-1073741824' ].
		^'-', self negated printString ].
	len := self decimalDigitLength.
	result := ByteArray new: len.
	integer := self.
	len to: 1 by: -1 do: [:i |
		next := integer // 10.
		result at: i put: 48 + (integer - (next * 10)).
		integer := next ].
	^result asString! !


!SmallInteger class methodsFor: 'constants' stamp: 'jmv 17/Sep/2025, 11:08:33 am (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
maxVal
	"Answer the maximum value for a SmallInteger."
	maxVal ifNil: [
		"Don't create any LargeInteger!!"
		maxVal := -1 - self minVal ].
	^maxVal.! !

