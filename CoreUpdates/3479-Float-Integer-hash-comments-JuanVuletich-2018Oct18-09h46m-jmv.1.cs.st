'From Cuis 5.0 of 7 November 2016 [latest update: #3478] on 18 October 2018 at 9:52:42 am'!

!Float methodsFor: 'comparing' stamp: 'jmv 10/18/2018 09:52:13'!
hash
	"Hash is reimplemented because = is implemented. Both words of the float are used. (The bitShift:'s ensure that the intermediate results do not become a large integer.) Care is taken to answer same hash as an equal Integer."

	| asInteger |
	self isFinite ifTrue: [
		asInteger _ self truncated.
		"See Integer>>#hash, LargePositiveInteger>>#hash and LargeNegativeInteger>>#hash"
		"Very big integers will answer true when asked #= with their own conversion to Float,
		But that Float will #asInteger to a different integer. Use Float hash in those cases.
		In addition, there is a performance improvement: the hash in LargeIntegers could
		get very slow for very big numbers"
		(asInteger = self and: [ asInteger abs < Float maxExactInteger ]) ifTrue: [
			^ asInteger hash ]].
	^ ((self basicAt: 1) bitShift: -4) +
	   ((self basicAt: 2) bitShift: -4)! !


!Integer methodsFor: 'comparing' stamp: 'jmv 10/18/2018 09:48:12'!
hash
	"Hash is reimplemented because = is implemented.
	| s |
	s _ (1 to: 10000) asSet.
	[s includes: 123456] bench
	"

	"Can only happen in 64 bits images...
	See Float>>#hash, LargePositiveInteger>>#hash and LargeNegativeInteger>>#hash"
	self abs < Float maxExactInteger ifFalse: [
		^ self asFloat hash ].

	"In 32 bit image it will always go this way"
	^self hashMultiply! !


!LargePositiveInteger methodsFor: 'comparing' stamp: 'jmv 10/18/2018 09:48:28'!
hash

	self < `(2 raisedToInteger: Float emax+1)` ifFalse: [
		^ `Float infinity hash` ].

	self < Float maxExactInteger ifFalse: [
		"Will always enter here for 64 bits images.
		See Float>>#hash, Integer>>#hash and LargeNegativeInteger>>#hash"
		^ self asFloat hash ].

	"May only reach here in 32 bit images"
	^ByteArray
		hashBytes: self
		startingWith: self species hash! !


!LargeNegativeInteger methodsFor: 'comparing' stamp: 'jmv 10/18/2018 09:48:53'!
hash

	self > `(2 raisedToInteger: Float emax+1) negated` ifFalse: [
		^ `Float negativeInfinity hash` ].

	self > Float maxExactInteger negated ifFalse: [
		"Will always enter here for 64 bits images.
		See Float>>#hash, Integer>>#hash and LargePositiveInteger>>#hash"
		^ self asFloat hash ].

	"May only reach here in 32 bit images"
	^ByteArray
		hashBytes: self
		startingWith: self species hash! !

