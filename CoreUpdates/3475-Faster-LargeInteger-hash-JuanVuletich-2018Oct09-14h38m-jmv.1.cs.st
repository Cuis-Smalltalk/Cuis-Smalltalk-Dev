'From Cuis 5.0 of 7 November 2016 [latest update: #3474] on 9 October 2018 at 2:51:00 pm'!

!LargePositiveInteger methodsFor: 'comparing' stamp: 'jmv 10/9/2018 14:49:10'!
hash
	self < `(2 raisedToInteger: Float emax+1)` ifFalse: [
		^ `Float infinity hash` ].
	self < Float maxExactInteger ifFalse: [
		^ self asFloat hash ].
	^ByteArray
		hashBytes: self
		startingWith: self species hash! !


!LargeNegativeInteger methodsFor: 'comparing' stamp: 'jmv 10/9/2018 14:50:07'!
hash

	self > `(2 raisedToInteger: Float emax+1) negated` ifFalse: [
		^ `Float negativeInfinity hash` ].
	self > Float maxExactInteger negated ifFalse: [
		^ self asFloat hash ].
	^ByteArray
		hashBytes: self
		startingWith: self species hash! !

