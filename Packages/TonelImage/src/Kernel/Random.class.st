"
The abstract class for all random number generators.

If you just want a quick random integer, use:
		10 atRandom
Every integer interval can give a random number:
		(6 to: 12) atRandom
SequenceableCollections can give randomly selected elements:
		'pick one of these letters randomly' atRandom
SequenceableCollections also respond to shuffled, as in:
		($A to: $Z) shuffled

The correct way to use class Random is to store one in an instance or class variable:
		myGenerator := Random new.
Then use it every time you need another Float in the interval [0, 1)
		myGenerator next
You can also generate a positive integer
		myGenerator nextInt: 10
"
Class {
	#name : 'Random',
	#superclass : 'Object',
	#classVars : [
		'DefaultInstance',
		'MutexForDefaultInstance'
	],
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'instance creation' }
Random class >> new [
	^ self seed: Time millisecondClockValue
]

{ #category : 'services' }
Random class >> next [
	"Answer a Float in [0.0 .. 1.0)
	Random next
	"
	
	^self withDefaultDo: [ :random | random next ]
]

{ #category : 'cached state access' }
Random class >> releaseClassCachedState [
	DefaultInstance := nil.
	MutexForDefaultInstance := nil
]

{ #category : 'instance creation' }
Random class >> seed: anInteger [
	^ (self = Random ifTrue: [ParkMiller93Random] ifFalse: [self]) basicNew seed: anInteger
]

{ #category : 'default instance' }
Random class >> withDefaultDo: aBlock [
	"
	Random withDefaultDo: [ :r | r next ]
	"
	DefaultInstance ifNil: [
		DefaultInstance := Random new.
		MutexForDefaultInstance := Semaphore forMutualExclusion ].
	^MutexForDefaultInstance critical: [
		aBlock value: DefaultInstance ]
]

{ #category : 'services' }
Random class >> withProbabilities: probabilities pickFrom: elements [
	"probabilities and elements must be of same size.
	probabilities sum = 1.0"

	| r |
	r := self next.
	probabilities with: elements do: [ :p :e |
		r <= p ifTrue: [ ^e ].
		r := r - p ].
	
	self error: [ 'Probabilities do not add up to 1.0' ].
]

{ #category : 'accessing' }
Random >> next [
	"Answer a random Float in the interval [0, 1)"

	self subclassResponsibility
]

{ #category : 'accessing' }
Random >> next: numberOfElements [
	| elements |
	elements := Array new: numberOfElements.
	1 to: numberOfElements do:
		[:i| elements at: i put: self next].
	^elements
]

{ #category : 'accessing' }
Random >> nextBits: anInteger [
	"Answer a random integer in the interval [0, 2^anInteger - 1]"

	| remainder answer |
	anInteger negative ifTrue: [self error: 'invalid interval'].
	remainder := anInteger \\ self nextChunkSize.
	answer := remainder > 0
		ifTrue: [self nextChunkBits bitShift: remainder - self nextChunkSize]
		ifFalse: [0].
	anInteger // self nextChunkSize timesRepeat:
		[answer := (answer bitShift: self nextChunkSize) bitXor: self nextChunkBits].
	^answer
]

{ #category : 'accessing' }
Random >> nextBoolean [
	^ (self nextBits: 1) = 1
]

{ #category : 'private' }
Random >> nextChunkBits [

	self subclassResponsibility
]

{ #category : 'private' }
Random >> nextChunkSize [

	self subclassResponsibility
]

{ #category : 'accessing' }
Random >> nextInteger: anInteger [
	"Answer a random integer in the interval [1, anInteger]"

	| answer |
	
	anInteger strictlyPositive ifFalse: [self error: 'invalid interval'].
	[(answer := self nextBits: anInteger highBit) >= anInteger] whileTrue.

	^ answer + 1
]

{ #category : 'private' }
Random >> seed: anObject [

	self subclassResponsibility
]
