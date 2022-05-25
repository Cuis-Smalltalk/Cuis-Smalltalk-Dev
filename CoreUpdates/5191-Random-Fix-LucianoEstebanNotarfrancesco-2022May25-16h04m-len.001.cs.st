'From Cuis 6.0 [latest update: #5189] on 25 May 2022 at 4:04:33 pm'!

!Random methodsFor: 'accessing' stamp: 'len 5/25/2022 15:55:44'!
nextBits: anInteger
	"Answer a random integer in the interval [0, 2^anInteger - 1]"

	| remainder answer |
	anInteger negative ifTrue: [self error: 'invalid interval'].
	remainder _ anInteger \\ self nextChunkSize.
	answer _ remainder > 0
		ifTrue: [self nextChunkBits bitShift: remainder - self nextChunkSize]
		ifFalse: [0].
	anInteger // self nextChunkSize timesRepeat:
		[answer _ answer bitShift: self nextChunkSize :: bitXor: self nextChunkBits].
	^answer! !

