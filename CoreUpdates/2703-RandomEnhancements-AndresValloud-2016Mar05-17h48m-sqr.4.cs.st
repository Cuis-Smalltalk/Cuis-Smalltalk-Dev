'From Cuis 4.2 of 25 July 2013 [latest update: #2701] on 5 March 2016 at 8:52:10.500337 pm'!

!Random commentStamp: 'jmv 2/28/2016 16:34' prior: 0!
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
		myGenerator _ Random new.
Then use it every time you need another Float in the interval [0, 1)
		myGenerator next
You can also generate a positive integer
		myGenerator nextInt: 10!

Smalltalk renameClassNamed: #ParkMillerRandom as: #ParkMiller88Random!
!classDefinition: #ParkMiller88Random category: #'Kernel-Numbers'!
Random subclass: #ParkMiller88Random
	instanceVariableNames: 'seed a m q r '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!ParkMiller88Random commentStamp: '<historical>' prior: 0!
This Random Number Generator graciously contributed by David N. Smith.  It is an adaptation of the Park-Miller RNG which uses Floats to avoid the need for LargeInteger arithmetic.  The algorithm is described in detail in

	Random Number Generators: Good Ones Are Hard To Find
	Stephen K. Park, Keith W. Miller
	(Communications of the ACM, 31(10):1192--1201, 1988)

	http://www.firstpr.com.au/dsp/rand31/p1192-park.pdf

See also https://en.wikipedia.org/wiki/Lehmer_random_number_generator.!


!ParkMiller88Random commentStamp: '<historical>' prior: 0!
This Random Number Generator graciously contributed by David N. Smith.  It is an adaptation of the Park-Miller RNG which uses Floats to avoid the need for LargeInteger arithmetic.  The algorithm is described in detail in

	Random Number Generators: Good Ones Are Hard To Find
	Stephen K. Park, Keith W. Miller
	(Communications of the ACM, 31(10):1192--1201, 1988)

	http://www.firstpr.com.au/dsp/rand31/p1192-park.pdf

See also https://en.wikipedia.org/wiki/Lehmer_random_number_generator.!

!classDefinition: #ParkMiller93Random category: #'Kernel-Numbers'!
ParkMiller88Random subclass: #ParkMiller93Random
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!ParkMiller93Random commentStamp: '<historical>' prior: 0!
The 1993 version of the Park-Miller RNG.!


!ParkMiller88Random methodsFor: 'private' stamp: 'sqr 3/5/2016 20:31'!
a
	"See https://en.wikipedia.org/wiki/Lehmer_random_number_generator"

	^16807.0! !

!ParkMiller88Random methodsFor: 'private' stamp: 'sqr 3/5/2016 20:40'!
m

	^2147483647.0! !

!ParkMiller88Random methodsFor: 'private' stamp: 'sqr 3/5/2016 20:39'!
privateNextChunk
	"This method generates random instances of Integer in the interval
	1 to 16r7FFFFFFF (almost 31 bits).  Note the calculations cannot
	result in seed = 0 because a is a primitive generator of the integers
	modulo m."

	seed _ seed asFloat * self a \\ self m :: truncated.
	^seed! !


!ParkMiller93Random methodsFor: 'private' stamp: 'sqr 3/5/2016 20:33'!
a
	"See https://en.wikipedia.org/wiki/Lehmer_random_number_generator"

	^48271.0! !


!Random methodsFor: 'private' stamp: 'sqr 3/5/2016 19:30'!
nextChunkBits

	self subclassResponsibility! !


!Random class methodsFor: 'instance creation' stamp: 'sqr 3/5/2016 19:56'!
concreteRandomClass

	ConcreteSubclass ifNil: [
		ConcreteSubclass _ ParkMiller93Random ].
	^ConcreteSubclass! !


!LaggedFibonacciRandom methodsFor: 'private' stamp: 'sqr 3/5/2016 19:56'!
initialize

	self ring: self newRing.
	self last: 1.
	self initializeRingWith: ParkMiller93Random new! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'sqr 3/5/2016 19:35'!
initializeRingWith: aRandom
	"The odds aRandom will produce self majorLag even integers in a row should be almost zero.
	So, if that effectively impossible event does happen, fail"

	| anyOdd |
	anyOdd := false.
	1 to: self ring size do:
		[:eachIndex |
			| nextInteger nextDouble |
			nextInteger _ aRandom nextBits: self nextChunkSize.
			anyOdd _ anyOdd or: [nextInteger odd].
			nextDouble _ nextInteger asFloat timesTwoPower: 0 - self nextChunkSize.
			self ring at: eachIndex put: nextDouble
		].
	anyOdd ifFalse: [self error: 'Initialization failed']! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'sqr 3/5/2016 19:29'!
nextChunkBits
	"This method generates random instances of Integer in the interval
	0 to 16r1FFFFFFFFFFFFF (53 bits)."
	
	^self next timesTwoPower: self nextChunkSize :: truncated! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'sqr 3/5/2016 19:34'!
nextChunkSize
	"Recall double precision floating point numbers in this
	generator's output range have an implicit mantissa bit"

	^53! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'sqr 3/5/2016 19:56'!
seed: anInteger

	| random |
	random _ ParkMiller93Random seed: anInteger.
	self initializeRingWith: random! !


!ParkMiller88Random methodsFor: 'accessing' stamp: 'sqr 3/5/2016 20:33'!
next
	"Answer a random Float in the interval [0, 1).
	NOTE: this is not a uniformly distributed random Float, since only uses 31 bits out of 53 bits mantissa.
	Additionally, #privateNextChunk can not produce all zeros or all ones.

	A higher quality Float RNG is LaggedFibonacciRandom"

	^ self privateNextChunk asFloat / self m
	
	"In any case, an alternative, slower but better answer could be:
	
	^(self nextBits: 53) asFloat  timesTwoPower: -53"! !

!ParkMiller88Random methodsFor: 'private' stamp: 'sqr 3/5/2016 20:38'!
initialize
	"Output stabilization is the user's responsibility"

	[
		seed _ Time localMillisecondClock + self identityHash \\ self m.
		seed = 0  "zero seeds are unacceptable"
	] whileTrue.
	seed _ seed asFloat! !

!ParkMiller88Random methodsFor: 'private' stamp: 'sqr 3/5/2016 20:39'!
nextChunkBits
	"This method generates random instances of Integer in the interval
	0 to 16r3FFFFFFF (30 bits)."

	| answer |
	[
		answer _ self privateNextChunk.
		answer > 16r40000000
	] whileTrue.
	^answer - 1! !

!ParkMiller88Random methodsFor: 'private' stamp: 'sqr 3/5/2016 20:39'!
nextChunkSize
	"#privateNextChunk generates almost 31 bits.
	#nextChunkBits generates all of just 30 bits."

	^30! !

!ParkMiller88Random methodsFor: 'private' stamp: 'sqr 3/5/2016 20:36'!
seed: anInteger

	(anInteger between: 1 and: self m - 1) ifFalse: [self error: 'Seed out of range'].
	seed _ anInteger! !

!methodRemoval: ParkMiller93Random #initialA!
ParkMiller93Random removeSelector: #initialA!
!methodRemoval: ParkMiller88Random #initialA!
ParkMiller88Random removeSelector: #initialA!
!methodRemoval: Random class #bucketTest:!
Random class removeSelector: #bucketTest:!
!methodRemoval: Random class #theItsCompletelyBrokenTest!
Random class removeSelector: #theItsCompletelyBrokenTest!
!classDefinition: #ParkMiller88Random category: #'Kernel-Numbers'!
Random subclass: #ParkMiller88Random
	instanceVariableNames: 'seed'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!ParkMiller93Random reorganize!
('private' a)
!

