'From Cuis 5.0 of 7 November 2016 [latest update: #3785] on 7 June 2019 at 9:38:59 am'!

!Random methodsFor: 'accessing' stamp: 'HAW 6/7/2019 09:34:08'!
nextBits: anInteger
	"Answer a random integer in the interval [0, 2^anInteger - 1]"

	| toGo remainder answer |
	
	anInteger negative ifTrue: [self error: 'invalid interval'].
	remainder := anInteger \\ self nextChunkSize.
	answer _ remainder > 0
		ifTrue: [self nextChunkBits bitShift: remainder - self nextChunkSize]
		ifFalse: [0].
	toGo := anInteger - self nextChunkSize.
	[toGo > 0] whileTrue:
		[
			answer _ answer bitShift: self nextChunkSize :: bitXor: self nextChunkBits.
			toGo _ toGo - self nextChunkSize
		].
	^answer! !

!Random methodsFor: 'accessing' stamp: 'HAW 6/7/2019 09:37:25'!
nextInteger: anInteger
	"Answer a random integer in the interval [1, anInteger]"

	| answer |
	
	anInteger strictlyPositive ifFalse: [self error: 'invalid interval'].
	[(answer _ self nextBits: anInteger highBit) >= anInteger] whileTrue.

	^ answer + 1! !


!ParkMiller88Random methodsFor: 'private' stamp: 'HAW 6/7/2019 09:33:01'!
seed: anInteger

	seed _ anInteger - 1 \\  (self m - 1) truncated + 1.
	! !

