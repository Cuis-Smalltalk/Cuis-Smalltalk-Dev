"
This class implements a two-tap Lagged Fibonacci random number generator, with lags 83 and 258.
"
Class {
	#name : 'LaggedFibonacciRandom',
	#superclass : 'Random',
	#instVars : [
		'ring',
		'last'
	],
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'accessing-private' }
LaggedFibonacciRandom >> clampRingIndex: anInteger [

	^anInteger < 1
		ifTrue: [anInteger + self majorLag]
		ifFalse: [anInteger]
]

{ #category : 'private' }
LaggedFibonacciRandom >> initializeRingWith: aRandom [
	"The odds aRandom will produce self majorLag even integers in a row should be almost zero.
	So, if that effectively impossible event does happen, fail"

	| anyOdd |
	anyOdd := false.
	1 to: self ring size do:
		[:eachIndex |
			| nextInteger nextDouble |
			nextInteger := aRandom nextBits: self nextChunkSize.
			anyOdd := anyOdd or: [nextInteger odd].
			nextDouble := nextInteger asFloat timesTwoPower: 0 - self nextChunkSize.
			self ring at: eachIndex put: nextDouble
		].
	anyOdd ifFalse: [self error: 'Initialization failed']
]

{ #category : 'accessing-private' }
LaggedFibonacciRandom >> last [

	^last
]

{ #category : 'accessing-private' }
LaggedFibonacciRandom >> last: anInteger [

	last := anInteger
]

{ #category : 'private' }
LaggedFibonacciRandom >> majorLag [

	^258
]

{ #category : 'private' }
LaggedFibonacciRandom >> minorLag [

	^83
]

{ #category : 'private' }
LaggedFibonacciRandom >> newRing [

	^Array new: self majorLag
]

{ #category : 'accessing' }
LaggedFibonacciRandom >> next [
	"Answer a random Float in the interval [0, 1)"

	| answer tap1 tap2 |
	tap1 := self ringAt: self last.
	tap2 := self ringAt: self last - self minorLag + 1.
	answer := tap2 - tap1.
	answer < 0.0 ifTrue: [answer := 1.0 + answer].
	self ringAt: self last put: answer.
	self setLast: self last + 1.
	^answer
]

{ #category : 'private' }
LaggedFibonacciRandom >> nextChunkBits [
	"This method generates random instances of Integer in the interval
	0 to 16r1FFFFFFFFFFFFF (53 bits)."
	
	^(self next timesTwoPower: self nextChunkSize) truncated
]

{ #category : 'private' }
LaggedFibonacciRandom >> nextChunkSize [
	"Recall double precision floating point numbers in this
	generator's output range have an implicit mantissa bit"

	^53
]

{ #category : 'accessing-private' }
LaggedFibonacciRandom >> ring [

	^ring
]

{ #category : 'accessing-private' }
LaggedFibonacciRandom >> ring: aRing [

	ring := aRing
]

{ #category : 'accessing-private' }
LaggedFibonacciRandom >> ringAt: anInteger [

	| index |
	index := self clampRingIndex: anInteger.
	^self ring at: index
]

{ #category : 'accessing-private' }
LaggedFibonacciRandom >> ringAt: anInteger put: anObject [

	| index |
	index := self clampRingIndex: anInteger.
	self ring at: index put: anObject
]

{ #category : 'private' }
LaggedFibonacciRandom >> seed: anInteger [
	ring isNil ifTrue: [ring := self newRing].
	self last: 1.
	self initializeRingWith: (ParkMiller93Random seed: anInteger)
]

{ #category : 'accessing-private' }
LaggedFibonacciRandom >> setLast: anInteger [

	anInteger > self majorLag
		ifTrue: [self last: anInteger - self majorLag]
		ifFalse: [self last: anInteger]
]
