'From Cuis 5.0 of 7 November 2016 [latest update: #3471] on 9 October 2018 at 2:29:53 pm'!

!LargeNegativeInteger methodsFor: 'comparing' stamp: 'jmv 10/9/2018 14:20:27'!
hash

	self > Float maxExactInteger negated ifFalse: [
		^ self asFloat hash ].
	^ByteArray
		hashBytes: self
		startingWith: self species hash! !


!Float methodsFor: 'comparing' stamp: 'jmv 10/9/2018 12:30:01'!
hash
	"Hash is reimplemented because = is implemented. Both words of the float are used. (The bitShift:'s ensure that the intermediate results do not become a large integer.) Care is taken to answer same hash as an equal Integer."

	| asInteger |
	self isFinite ifTrue: [
		asInteger _ self truncated.
		asInteger abs < Float maxExactInteger ifTrue: [
			^ asInteger hash ]].
	^ ((self basicAt: 1) bitShift: -4) +
	   ((self basicAt: 2) bitShift: -4)! !


!Fraction methodsFor: 'comparing' stamp: 'jmv 10/8/2018 16:30:04'!
hash
	"Hash is reimplemented because = is implemented.
	Care is taken that a Fraction equal to a Float also have an equal hash"

	^ self asFloat hash! !


!Integer methodsFor: 'comparing' stamp: 'jmv 10/9/2018 12:30:19'!
hash
	"Hash is reimplemented because = is implemented.
	| s |
	s _ (1 to: 10000) asSet.
	[s includes: 123456] bench
	"

	self abs < Float maxExactInteger ifFalse: [
		^ self asFloat hash ].
	^self hashMultiply! !


!LargePositiveInteger methodsFor: 'comparing' stamp: 'jmv 10/9/2018 14:20:30'!
hash

	self < Float maxExactInteger ifFalse: [
		^ self asFloat hash ].
	^ByteArray
		hashBytes: self
		startingWith: self species hash! !


!LargeNegativeInteger reorganize!
('arithmetic' abs negated)
('bit manipulation' bitAt: highBit)
('comparing' hash)
('converting' asFloat normalize)
('mathematical functions' sqrt)
('printing' printOn:base:)
('testing' negative positive sign strictlyPositive)
!

