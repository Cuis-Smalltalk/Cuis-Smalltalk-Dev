'From Cuis 5.0 of 7 November 2016 [latest update: #3464] on 3 October 2018 at 3:23:41 pm'!

!Float methodsFor: 'truncation and round off' stamp: 'nice 3/1/2014 00:32'!
successor
	| ulp |
	self isFinite ifFalse: [
		(self isNaN or: [self positive]) ifTrue: [^self].
		^Float fmax negated].
	ulp := self ulp.
	^self + (0.5 * ulp) = self
		ifTrue: [self * -1.0 - ulp * -1.0 "This trick is for obtaining a negativeZero"]
		ifFalse: [self + (0.5 * ulp)]! !

