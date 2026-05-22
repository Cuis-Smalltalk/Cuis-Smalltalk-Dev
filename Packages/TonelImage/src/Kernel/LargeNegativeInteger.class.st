"
Just like LargePositiveInteger, but represents a negative number.
"
Class {
	#name : 'LargeNegativeInteger',
	#superclass : 'LargePositiveInteger',
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'as yet unclassified' }
LargeNegativeInteger class >> initializedInstance [
	^ -9876543210987654321 copy
]

{ #category : 'arithmetic' }
LargeNegativeInteger >> abs [
	^self negated
]

{ #category : 'converting' }
LargeNegativeInteger >> asFloat [
	^self negated asFloat negated
]

{ #category : 'bit manipulation' }
LargeNegativeInteger >> bitAt: anInteger [
	"super would not work because we have to pretend we are in two-complement.
	this has to be tricky..."
	
	| digitIndex bitIndex i |
	digitIndex := anInteger - 1 // 8 + 1.
	digitIndex > self digitLength ifTrue: [^1].
	bitIndex := (anInteger - 1 bitAnd: 2r111) + 1.

	i := 1.
	[i = digitIndex
		ifTrue:
			["evaluate two complement (bitInvert + 1) on the digit :
			(if digitIndex > 1, we must still add 1 due to the carry).
			but x bitInvert is -1-x, bitInvert+1 is just x negated..."
			^(self digitAt: digitIndex) negated bitAt: bitIndex].
	(self digitAt: i) = 0]
		whileTrue: [
			"two complement (bitInvert + 1) raises a carry:
			0 bitInvert -> 2r11111111.  2r11111111 + 1 -> 0 with carry...
			Thus we must inquire one digit forward"
			i := i + 1].
	
	"We escaped the while loop, because there is no more carry.
	Do a simple bitInvert without a carry"
	^1 - ((self digitAt: digitIndex) bitAt: bitIndex)
]

{ #category : 'comparing' }
LargeNegativeInteger >> hash [

	self > `(2 raisedToInteger: Float emax+1) negated` ifFalse: [
		^ `Float negativeInfinity hash` ].

	self > Float maxExactInteger negated ifFalse: [
		"Will always enter here for 64 bits images.
		See Float>>#hash, Integer>>#hash and LargePositiveInteger>>#hash"
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
LargeNegativeInteger >> highBit [
	"Answer the position of the leading bit or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to  
	get the highest bit of the magnitude."

	^self error: 'highBit is not defined for negative integers'
]

{ #category : 'testing' }
LargeNegativeInteger >> isPowerOfTwo [
	"Return true if the receiver is an integral power of two. Optimized version."
	
	^false
]

{ #category : 'mathematical functions' }
LargeNegativeInteger >> ln [
	^DomainError signal: 'ln is only defined for x > 0'
]

{ #category : 'mathematical functions' }
LargeNegativeInteger >> log2 [
	^DomainError signal: 'log2 is only defined for x > 0'
]

{ #category : 'arithmetic' }
LargeNegativeInteger >> negated [
	^self copyto: (LargePositiveInteger new: self digitLength)
]

{ #category : 'testing' }
LargeNegativeInteger >> negative [
	"Answer whether the receiver is mathematically negative."

	^ true
]

{ #category : 'converting' }
LargeNegativeInteger >> normalize [
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen minVal |
	<primitive: 'primNormalizeNegative' module: 'LargeIntegers'>
	"First establish len = significant length"
	len := oldLen := self digitLength.
	[len = 0 ifTrue: [^0].
	(self digitAt: len) = 0]
		whileTrue: [len := len - 1].

	"Now check if in SmallInteger range.
	 Fast compute SmallInteger minVal digitLength"
	sLen := SmallInteger minVal < -16r80000000 "we're definitely on 64bit if we are smaller than (-2 raisedTo: 31)"
				ifTrue: [8]
				ifFalse: [4].
	len <= sLen ifTrue:
		[minVal := SmallInteger minVal.
		(len < sLen
		 or: [(self digitAt: sLen) < minVal lastDigit])
			ifTrue: ["If high digit less, then can be small"
					val := 0.
					len to: 1 by: -1 do:
						[:i | val := (val *256) - (self digitAt: i)].
					^ val].
		1 to: sLen do:  "If all digits same, then = minVal"
			[:i | (self digitAt: i) = (minVal digitAt: i)
					ifFalse: ["Not so; return self shortened"
							len < oldLen
								ifTrue: [^ self growto: len]
								ifFalse: [^ self]]].
		^ minVal].

	"Return self, or a shortened copy"
	len < oldLen
		ifTrue: [^ self growto: len]
		ifFalse: [^ self]
]

{ #category : 'testing' }
LargeNegativeInteger >> positive [
	"Answer whether the receiver is positive or equal to 0. (ST-80 protocol).
	See also strictlyPositive"

	^ false
]

{ #category : 'printing' }
LargeNegativeInteger >> printOn: aStream base: b [
	"Append a representation of this number in base b on aStream."
	
	aStream nextPut: $-.
	self abs printOn: aStream base: b
]

{ #category : 'printing' }
LargeNegativeInteger >> printOn: aStream base: b nDigits: n [
	"See comment at LargePositiveInteger."

	self shouldNotImplement.
]

{ #category : 'testing' }
LargeNegativeInteger >> sign [
	"Optimization. Answer -1 since receiver is less than 0."

	^ -1

]

{ #category : 'mathematical functions' }
LargeNegativeInteger >> sqrt [
	"Answer the square root of the receiver."
	^NegativePowerError new signalReceiver: self selector: #sqrt arguments: {}
]

{ #category : 'testing' }
LargeNegativeInteger >> strictlyPositive [
	"Answer whether the receiver is mathematically positive."

	^ false
]
