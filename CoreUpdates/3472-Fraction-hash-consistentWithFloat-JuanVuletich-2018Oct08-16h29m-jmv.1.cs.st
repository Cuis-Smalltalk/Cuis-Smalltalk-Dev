'From Cuis 5.0 of 7 November 2016 [latest update: #3470] on 8 October 2018 at 4:36:36 pm'!

!Float methodsFor: 'comparing' stamp: 'jmv 10/8/2018 16:29:40'!
hash
	"Hash is reimplemented because = is implemented. Both words of the float are used. (The bitShift:'s ensure that the intermediate results do not become a large integer.) Care is taken to answer same hash as an equal Integer."

	| asInteger |
	self isFinite ifTrue: [
		asInteger _ self truncated.
		asInteger = self ifTrue: [
			^ asInteger hash ]].
	^ ((self basicAt: 1) bitShift: -4) +
	   ((self basicAt: 2) bitShift: -4)! !


!Fraction methodsFor: 'comparing' stamp: 'jmv 10/8/2018 16:30:04'!
hash
	"Hash is reimplemented because = is implemented.
	Care is taken that a Fraction equal to a Float also have an equal hash"

	^ self asFloat hash! !

