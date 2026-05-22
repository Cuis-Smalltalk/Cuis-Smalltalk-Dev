"
This Random Number Generator graciously contributed by David N. Smith.  It is an adaptation of the Park-Miller RNG which uses Floats to avoid the need for LargeInteger arithmetic.  The algorithm is described in detail in

	Random Number Generators: Good Ones Are Hard To Find
	Stephen K. Park, Keith W. Miller
	(Communications of the ACM, 31(10):1192--1201, 1988)

	http://www.firstpr.com.au/dsp/rand31/p1192-park.pdf

See also https://en.wikipedia.org/wiki/Lehmer_random_number_generator.
"
Class {
	#name : 'ParkMiller88Random',
	#superclass : 'Random',
	#instVars : [
		'seed'
	],
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'private' }
ParkMiller88Random >> a [
	"See https://en.wikipedia.org/wiki/Lehmer_random_number_generator"

	^16807.0
]

{ #category : 'private' }
ParkMiller88Random >> m [

	^2147483647.0
]

{ #category : 'accessing' }
ParkMiller88Random >> next [
	"Answer a random Float in the interval [0, 1).
	NOTE: this is not a uniformly distributed random Float, since only uses 31 bits out of 53 bits mantissa.
	Additionally, #privateNextChunk can not produce all zeros or all ones.

	A higher quality Float RNG is LaggedFibonacciRandom"

	^ self privateNextChunk asFloat / self m
	
	"In any case, an alternative, slower but better answer could be:
	
	^(self nextBits: 53) asFloat  timesTwoPower: -53"
]

{ #category : 'private' }
ParkMiller88Random >> nextChunkBits [
	"This method generates random instances of Integer in the interval
	0 to 16r3FFFFFFF (30 bits)."

	| answer |
	[
		answer := self privateNextChunk.
		answer > 16r40000000
	] whileTrue.
	^answer - 1
]

{ #category : 'private' }
ParkMiller88Random >> nextChunkSize [
	"#privateNextChunk generates almost 31 bits.
	#nextChunkBits generates all of just 30 bits."

	^30
]

{ #category : 'private' }
ParkMiller88Random >> privateNextChunk [
	"This method generates random instances of Integer in the interval
	1 to 16r7FFFFFFF (almost 31 bits).  Note the calculations cannot
	result in seed = 0 because a is a primitive generator of the integers
	modulo m."

	seed := (seed asFloat * self a \\ self m) truncated.
	^seed
]

{ #category : 'private' }
ParkMiller88Random >> seed: anInteger [

	seed := anInteger - 1 \\  (self m - 1) truncated + 1.
	
]
