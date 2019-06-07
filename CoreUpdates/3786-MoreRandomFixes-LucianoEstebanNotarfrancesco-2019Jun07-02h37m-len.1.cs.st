'From Cuis 5.0 of 7 November 2016 [latest update: #3784] on 7 June 2019 at 3:27:40 am'!

!Random methodsFor: 'accessing' stamp: 'len 5/13/2019 10:04:43'!
nextBoolean
	^ (self nextBits: 1) = 1! !


!Random methodsFor: 'accessing' stamp: 'len 6/7/2019 02:40:48'!
nextBits: anInteger
	"Answer a random integer in the interval [0, 2^anInteger - 1]"

	| toGo remainder answer |
	anInteger < 0 ifTrue: [self error: 'invalid interval'].
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

!Random methodsFor: 'accessing' stamp: 'len 6/7/2019 02:40:59'!
nextInteger: anInteger
	"Answer a random integer in the interval [1, anInteger]"

	| answer |
	anInteger >= 1 ifFalse: [self error: 'invalid interval'].
	[(answer _ self nextBits: anInteger highBit) >= anInteger] whileTrue.
	^ answer + 1! !


!LaggedFibonacciRandom methodsFor: 'private' stamp: 'len 6/7/2019 03:23:02'!
seed: anInteger

	| random |
	random _ ParkMiller93Random seed: anInteger.
	self initializeRingWith: random.
	self last: 1! !


!ParkMiller88Random methodsFor: 'private' stamp: 'len 6/6/2019 05:48:02'!
seed: anInteger
	seed _ anInteger - 1 \\  (self m - 1) truncated + 1.
"	(seed between: 1 and: self m - 1) ifFalse: [self error: 'Seed out of range']"! !

