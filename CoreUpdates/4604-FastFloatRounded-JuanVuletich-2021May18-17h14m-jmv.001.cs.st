'From Cuis 5.0 [latest update: #4603] on 18 May 2021 at 5:32:21 pm'!

!Float methodsFor: 'truncation and round off' stamp: 'jmv 5/18/2021 17:27:09'!
rounded
	"Answer the integer nearest the receiver."
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even"

	| truncated fractionPart |
	truncated _ self truncated.
	fractionPart _ self - truncated.
	(fractionPart > -0.5 and: [fractionPart < 0.5])
		ifTrue: [^truncated].
	fractionPart > 0.5
		ifTrue: [ ^truncated + 1 ].
	fractionPart < -0.5
		ifTrue: [ ^truncated - 1 ].
	truncated even ifTrue: [^truncated].
	self > 0
		ifTrue: [ ^truncated + 1 ]
		ifFalse: [ ^truncated - 1 ]! !

