'From Cuis 5.0 of 7 November 2016 [latest update: #3476] on 10 October 2018 at 3:49:14 pm'!

!Integer methodsFor: 'comparing' stamp: 'jmv 10/10/2018 15:44:51'!
hash
	"Hash is reimplemented because = is implemented.
	| s |
	s _ (1 to: 10000) asSet.
	[s includes: 123456] bench
	"

	"Can only happen in 64 bits images..."
	self abs < Float maxExactInteger ifFalse: [
		^ self asFloat hash ].

	"In 32 bit image it will always go this way"
	^self hashMultiply! !


!LargePositiveInteger methodsFor: 'comparing' stamp: 'jmv 10/10/2018 15:44:55'!
hash

	self < `(2 raisedToInteger: Float emax+1)` ifFalse: [
		^ `Float infinity hash` ].

	self < Float maxExactInteger ifFalse: [
		"Will always enter here for 64 bits images."
		^ self asFloat hash ].

	"May only reach here in 32 bit images"
	^ByteArray
		hashBytes: self
		startingWith: self species hash! !


!LargeNegativeInteger methodsFor: 'comparing' stamp: 'jmv 10/10/2018 15:45:00'!
hash

	self > `(2 raisedToInteger: Float emax+1) negated` ifFalse: [
		^ `Float negativeInfinity hash` ].

	self > Float maxExactInteger negated ifFalse: [
		"Will always enter here for 64 bits images."
		^ self asFloat hash ].

	"May only reach here in 32 bit images"
	^ByteArray
		hashBytes: self
		startingWith: self species hash! !

