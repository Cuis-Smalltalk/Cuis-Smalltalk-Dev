'From Cuis 5.0 of 7 November 2016 [latest update: #3476] on 9 October 2018 at 4:31:26 pm'!

!Number methodsFor: 'truncation and round off' stamp: 'jmv 10/9/2018 16:30:05'!
rounded
	"Answer the integer nearest the receiver."
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even"

	| truncated fractionPartAbs |
	truncated _ self truncated.
	fractionPartAbs _ (self-truncated) abs.
	fractionPartAbs = (1/2)
		ifTrue: [ truncated even ifTrue: [^truncated] ifFalse: [^truncated + self sign]].
	fractionPartAbs < (1/2)
		ifTrue: [^ truncated]
		ifFalse: [^ truncated + self sign]! !

!methodRemoval: Float #rounded!
Float removeSelector: #rounded!
