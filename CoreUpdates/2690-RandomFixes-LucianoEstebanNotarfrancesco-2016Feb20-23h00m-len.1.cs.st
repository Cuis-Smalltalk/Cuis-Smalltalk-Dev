'From Cuis 4.2 of 25 July 2013 [latest update: #2658] on 20 February 2016 at 11:10:19.699084 pm'!
!classDefinition: #Random category: #'Kernel-Numbers'!
Object subclass: #Random
	instanceVariableNames: ' '
	classVariableNames: 'ConcreteSubclass DefaultInstance MutexForDefaultInstance '
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!Random commentStamp: '<historical>' prior: 0!
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
Then use it every time you need another number between 0.0 and 1.0 (excluding)
		myGenerator next
You can also generate a positive integer
		myGenerator nextInt: 10!

!classDefinition: #LaggedFibonacciRandom category: #'Kernel-Numbers'!
Random subclass: #LaggedFibonacciRandom
	instanceVariableNames: 'ring last'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!LaggedFibonacciRandom commentStamp: '<historical>' prior: 0!
This class implements a two-tap Lagged Fibonacci random number generator, with lags 83 and 258.!

!classDefinition: #ParkMillerRandom category: #'Kernel-Numbers'!
Random subclass: #ParkMillerRandom
	instanceVariableNames: 'seed m a q r '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!ParkMillerRandom commentStamp: '<historical>' prior: 0!
This Random Number Generator graciously contributed by David N. Smith.  It is an adaptation of the Park-Miller RNG which uses Floats to avoid the need for LargeInteger arithmetic.!

!Random methodsFor: 'accessing' stamp: 'sqr 2/19/2016 20:31'!
nextBits: anInteger
	"Answer a random integer in the interval [0, 2^anInteger - 1]"

	| toGo remainder answer |
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

!Random methodsFor: 'accessing' stamp: 'sqr 2/19/2016 20:32'!
nextInteger: anInteger
	"Answer a random integer in the interval [1, anInteger]"

	| answer |
	[(answer _ self nextBits: anInteger highBit) >= anInteger] whileTrue.
	^ answer + 1! !

!Random methodsFor: 'private' stamp: 'len 2/20/2016 21:56'!
nextChunkBits
	^ self subclassResponsibility! !

!Random methodsFor: 'private' stamp: 'sqr 2/19/2016 20:30'!
nextChunkSize

	self subclassResponsibility! !


!Random class methodsFor: 'instance creation' stamp: 'sqr 2/19/2016 20:43'!
newDefault

	^self concreteRandomClass new! !


!LaggedFibonacciRandom methodsFor: 'accessing' stamp: 'sqr 2/19/2016 21:08'!
next
	"Answer a random Float in the interval [0, 1)"

	| answer tap1 tap2 |
	tap1 _ self ringAt: self last.
	tap2 _ self ringAt: self last - self minorLag + 1.
	answer _ tap2 - tap1.
	answer < 0.0 ifTrue: [answer _ 1.0 + answer].
	self ringAt: self last put: answer.
	self setLast: self last + 1.
	^answer! !

!LaggedFibonacciRandom methodsFor: 'accessing-private' stamp: 'sqr 2/19/2016 20:51'!
clampRingIndex: anInteger

	^anInteger < 1
		ifTrue: [anInteger + self majorLag]
		ifFalse: [anInteger]! !

!LaggedFibonacciRandom methodsFor: 'accessing-private' stamp: 'sqr 2/19/2016 21:01'!
last

	^last! !

!LaggedFibonacciRandom methodsFor: 'accessing-private' stamp: 'sqr 2/19/2016 21:01'!
last: anInteger

	last _ anInteger! !

!LaggedFibonacciRandom methodsFor: 'accessing-private' stamp: 'sqr 2/19/2016 20:20'!
ring

	^ring! !

!LaggedFibonacciRandom methodsFor: 'accessing-private' stamp: 'sqr 2/19/2016 20:20'!
ring: aRing

	ring _ aRing! !

!LaggedFibonacciRandom methodsFor: 'accessing-private' stamp: 'sqr 2/19/2016 20:52'!
ringAt: anInteger

	| index |
	index := self clampRingIndex: anInteger.
	^self ring at: index! !

!LaggedFibonacciRandom methodsFor: 'accessing-private' stamp: 'sqr 2/19/2016 20:52'!
ringAt: anInteger put: anObject

	| index |
	index := self clampRingIndex: anInteger.
	self ring at: index put: anObject! !

!LaggedFibonacciRandom methodsFor: 'accessing-private' stamp: 'sqr 2/19/2016 21:01'!
setLast: anInteger

	anInteger > self majorLag
		ifTrue: [self last: anInteger - self majorLag]
		ifFalse: [self last: anInteger]! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'sqr 2/19/2016 21:02'!
initialize

	self ring: self newRing.
	self last: 1.
	self initializeRingWith: ParkMillerRandom new! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'sqr 2/19/2016 20:39'!
initializeRingWith: aRandom
	"The odds aRandom will produce self majorLag even integers in a row should be almost zero.
	So, if that effectively impossible event does happen, fail"

	| anyOdd |
	anyOdd := false.
	1 to: self ring size do:
		[:eachIndex |
			| nextInteger nextDouble |
			nextInteger _ aRandom nextBits: 53.
			anyOdd _ anyOdd or: [nextInteger odd].
			nextDouble _ nextInteger asFloat timesTwoPower: -53.
			self ring at: eachIndex put: nextDouble
		].
	anyOdd ifFalse: [self error: 'Initialization failed']! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'sqr 2/19/2016 20:19'!
majorLag

	^258! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'sqr 2/19/2016 20:19'!
minorLag

	^83! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'sqr 2/19/2016 20:20'!
newRing

	^Array new: self majorLag! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'len 2/20/2016 22:04'!
nextChunkBits
	^ self next mantissaPart bitAnd: 1 << 52 - 1! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'len 2/20/2016 22:03'!
nextChunkSize
	^ 52! !

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'sqr 2/19/2016 20:26'!
seed: anInteger

	| random |
	random _ ParkMillerRandom seed: anInteger.
	self initializeRingWith: random! !


!ParkMillerRandom methodsFor: 'accessing' stamp: 'sqr 2/19/2016 20:31'!
next
	"Answer a random Float in the interval [0, 1).
	NOTE: this is not a uniformly distributed random Float, since only uses 31 bits out of 53 bits mantissa."

	^ (seed _ self nextChunkBits) asFloat / m! !

!ParkMillerRandom methodsFor: 'private' stamp: 'len 2/20/2016 23:08'!
initialize
	" Set a reasonable Park-Miller starting seed "
	[seed _ (Time localMillisecondClock bitAnd: 16r3FFFFFFF) bitXor: self hash.
	seed = 0] whileTrue. 	"Try again if ever get a seed = 0"

	seed _ seed asFloat.
	a _ 16r000041A7 asFloat.	" magic constant = 16807 "
	m _ 16r7FFFFFFF asFloat.	" magic constant = 2147483647 "
	q _ m quo: a.
	r _ m rem: a.

	80 timesRepeat: [self next] "in order to mix the internal state, otherwise successive evaluations of Random new next don't look randoml at all"! !

!ParkMillerRandom methodsFor: 'private' stamp: 'len 2/20/2016 22:16'!
nextChunkBits
	"This method generates random instances of Integer in the interval
	0 to 16r7FFFFFFF (31 bits).
	The algorithm is described in detail in 'Random Number Generators: 
	Good Ones Are Hard to Find' by Stephen K. Park and Keith W. Miller 
	(Comm. Asso. Comp. Mach., 31(10):1192--1201, 1988)."
	| lo hi aLoRHi |
	hi _ seed quo: q.
	lo _ seed - (hi * q).  " = seed rem: q"  
	aLoRHi _ (a * lo) - (r * hi).
	^ (seed _ aLoRHi > 0.0 ifTrue: [aLoRHi] ifFalse: [aLoRHi + m]) truncated! !

!ParkMillerRandom methodsFor: 'private' stamp: 'sqr 2/19/2016 20:30'!
nextChunkSize

	^31! !

!ParkMillerRandom methodsFor: 'private' stamp: 'sqr 2/19/2016 20:13'!
seed: anInteger
	anInteger = 0 ifTrue: [self error: 'The seed should never be zero'].
	seed _ anInteger! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'len 2/16/2016 17:12'!
atRandom: aGenerator
	"Answer a random element of the receiver.  Uses aGenerator which
	should be kept by the user in a variable and used every time. Use
	this instead of #atRandom for better uniformity of random numbers 
	because only you use the generator.  Causes an error if self has no 
	elements."

	^ self at: (self size atRandom: aGenerator)! !


!Set methodsFor: 'accessing' stamp: 'len 2/16/2016 17:07'!
atRandom: aGenerator 
	"Answer a random element of the receiver. Uses aGenerator which 
	should be kept by the user in a variable and used every time. Use 
	this instead of #atRandom for better uniformity of random numbers  
	because only you use the generator. Causes an error if self has no  
	elements."
	self emptyCheck.
	[(array atRandom: aGenerator) ifNotNil: [:element| ^ element]] repeat! !


!Integer methodsFor: 'truncation and round off' stamp: 'len 2/16/2016 17:11'!
atRandom: aGenerator
	"Answer a random integer from 1 to self picked from aGenerator."

	^ aGenerator nextInteger: self! !


!Random methodsFor: 'accessing' stamp: 'sqr 2/19/2016 20:14'!
next
	"Answer a random Float in the interval [0, 1)"

	self subclassResponsibility! !

!Random methodsFor: 'private' stamp: 'sqr 2/19/2016 20:16'!
seed: anObject

	self subclassResponsibility! !


!Random class methodsFor: 'instance creation' stamp: 'jmv 5/2/2015 16:24'!
concreteRandomClass

	ConcreteSubclass ifNil: [
		ConcreteSubclass _ ParkMillerRandom ].
	^ConcreteSubclass! !

!Random class methodsFor: 'instance creation' stamp: 'sqr 2/19/2016 20:46'!
new

	^self = Random
		ifTrue: [self concreteRandomClass basicNew initialize]
		ifFalse: [super new]! !

!Random class methodsFor: 'instance creation' stamp: 'jmv 12/2/2011 09:43'!
seed: anInteger
	^ self new seed: anInteger! !

!Random class methodsFor: 'testing' stamp: 'jmv 4/4/2015 20:01'!
bucketTest: randy
	"Execute this:
	Random bucketTest: Random new
	"
	" A quick-and-dirty bucket test. Prints nbuckets values on the Transcript.
	  Each should be 'near' the value of ntries. Any run with any value 'far' from ntries
	  indicates something is very wrong. Each run generates different values.
	  For a slightly better test, try values of nbuckets of 200-1000 or more; go get coffee.
	  This is a poor test; see Knuth.   Some 'OK' runs:
		1000 1023 998 969 997 1018 1030 1019 1054 985 1003
		1011 987 982 980 982 974 968 1044 976
		1029 1011 1025 1016 997 1019 991 954 968 999 991
		978 1035 995 988 1038 1009 988 993 976
"
	| nbuckets buckets ntrys slot |
	nbuckets := 20.
	buckets := Array new: nbuckets.
	buckets atAllPut: 0.
	ntrys :=  100.
	ntrys*nbuckets timesRepeat: [
		slot := (randy next * nbuckets) floor + 1.
		buckets at: slot put: (buckets at: slot) + 1 ].
	Transcript newLine.
	1 to: nbuckets do: [ :nb |
		Transcript show: (buckets at: nb) printString, ' ' ]! !

!Random class methodsFor: 'default instance' stamp: 'jmv 5/2/2015 16:25'!
withDefaultDo: aBlock
	"
	Random withDefaultDo: [ :r | r next ]
	"
	DefaultInstance ifNil: [
		DefaultInstance _ Random new.
		MutexForDefaultInstance _ Semaphore forMutualExclusion ].
	^MutexForDefaultInstance critical: [
		aBlock value: DefaultInstance ]! !

!Random class methodsFor: 'cached state access' stamp: 'jmv 5/2/2015 16:25'!
releaseClassCachedState

	ConcreteSubclass _ nil.
	DefaultInstance _ nil.
	MutexForDefaultInstance _ nil! !

!methodRemoval: ParkMillerRandom #seed!
ParkMillerRandom removeSelector: #seed!
!classDefinition: #ParkMillerRandom category: #'Kernel-Numbers'!
Random subclass: #ParkMillerRandom
	instanceVariableNames: 'seed a m q r'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!
!methodRemoval: Random #initialize!
Random removeSelector: #initialize!
!methodRemoval: Random #nextInt:!
Random removeSelector: #nextInt:!
!methodRemoval: Random #nextValue!
Random removeSelector: #nextValue!
!methodRemoval: Random #seed!
Random removeSelector: #seed!
!classDefinition: #Random category: #'Kernel-Numbers'!
Object subclass: #Random
	instanceVariableNames: ''
	classVariableNames: 'ConcreteSubclass DefaultInstance MutexForDefaultInstance'
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!Random reorganize!
('accessing' next nextBits: nextInteger:)
('private' nextChunkBits nextChunkSize seed:)
!

