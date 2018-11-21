'From Cuis 5.0 of 7 November 2016 [latest update: #3494] on 21 November 2018 at 5:40:44 pm'!

!Float methodsFor: 'comparing' stamp: 'jmv 11/21/2018 17:04:24'!
hash
	"Hash is reimplemented because = is implemented."

	| asInteger |
	self isFinite ifTrue: [
		asInteger _ self truncated.
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


!LargePositiveInteger methodsFor: 'comparing' stamp: 'jmv 11/21/2018 17:32:14'!
hash

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
		startingWith: self species hash! !


!LargeNegativeInteger methodsFor: 'comparing' stamp: 'jmv 11/21/2018 17:32:25'!
hash

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
		startingWith: self species hash! !

