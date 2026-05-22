Class {
	#name : 'RandomTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Numbers',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Numbers'
}

{ #category : 'testing' }
RandomTest class >> isAbstract [

	^self subclasses notEmpty
]

{ #category : 'setup' }
RandomTest >> byteBitCounts [

	| tablePower answer |
	tablePower _ 8.
	answer _ ByteArray new: (1 bitShift: tablePower).
	0 to: tablePower - 1 do:
		[:eachPowerIndex |
			| powerOfTwo |
			powerOfTwo _ 1 bitShift: eachPowerIndex.
			1 + powerOfTwo to: answer size by: powerOfTwo * 2 do:
				[:eachPivot |
					0 to: powerOfTwo - 1 do:
						[:eachIndex |
							answer
								at: eachPivot + eachIndex
								put: (answer at: eachPivot + eachIndex) + 1
						]
				]
		].
	^answer
]

{ #category : 'setup' }
RandomTest >> nSamplesByDefault [

	^1000
]

{ #category : 'setup' }
RandomTest >> rng [

	^self rngClass new
]

{ #category : 'setup' }
RandomTest >> rngClass [

	self subclassResponsibility
]

{ #category : 'tests' }
RandomTest >> testNext [
	"Generate some float values, and see if they are in the [0,1) interval. Also check that the smallest and the largest values are small/large enough."

	| random min max |
	min := Float infinity.
	max := Float negativeInfinity.
	random := Random seed: 112629.
	100000 timesRepeat: [
		| next | 
		next := random next.
		next < min ifTrue: [ min := next ].
		next > max ifTrue: [ max := next ].
		self assert: next >= 0.0 description: [ 'Generated value ', next asString, ' should be non-negative.' ].
		self assert: next < 1.0 description: [ 'Generated value ', next asString, ' should be less than 1.0.' ] ].
	self assert: max > 0.9999 description: 'The largest generated value should be greater than 0.9999.'.
	self assert: min < 0.0001 description: 'The smallest generated value should be less than 0.0001.'
]

{ #category : 'tests' }
RandomTest >> testNextBits [
	"Test randomBits: returns integers in the correct range."
	| random |
	random _ self rng.
	0 to: 10 do: [:n| 100 timesRepeat: [self assert: ((random nextBits: n) between: 0 and: 2^n - 1)]].
	0 to: 100 do: [:n| self assert: ((random nextBits: n) between: 0 and: 2^n - 1)]
]

{ #category : 'tests' }
RandomTest >> testNextBitsInvalid [
	"Requesting a negative amount of random bits should raise an error."
	| random |
	random _ self rng.
	self should: [random nextBits: -1] raise: Error.
	self should: [random nextBits: -100] raise: Error
]

{ #category : 'tests' }
RandomTest >> testNextInteger [
	"Test nextInteger: returns integers in the correct range."
	| random n |
	random _ self rng.
	1 to: 5 do: [:i| 100 timesRepeat: [self assert: ((random nextInteger: i) between: 1 and: i)]].
	n _ 1<<1000.
	self assert: ((random nextInteger: n) between: 1 and: n)
]

{ #category : 'tests' }
RandomTest >> testNextIntegerInvalid [
	| random |
	random _ self rng.
	self should: [random nextInteger: -1] raise: Error.
	self should: [random nextInteger: -100] raise: Error.
	self should: [random nextInteger: 0] raise: Error
]

{ #category : 'tests' }
RandomTest >> testRandomLargeInteger1 [

	| random nSamples highBound odds |
	random _ self rng.
	nSamples _ self nSamplesByDefault.
	highBound _ 1 << 64.
	odds _ 0.
	nSamples timesRepeat: [odds _ highBound atRandom: random :: bitAnd: 1 :: + odds].
	self assert: (odds * 2 - nSamples * 5) abs < nSamples  "10% max deviation"
]

{ #category : 'tests' }
RandomTest >> testRandomLargeInteger2 [

	| random nSamples bitCounts bitsPerRandom highBound ones |
	random _ self rng.
	nSamples _ self nSamplesByDefault.
	bitCounts _ self byteBitCounts.
	bitsPerRandom _ 1000.
	highBound _ 1 << bitsPerRandom.
	ones _ 0.
	nSamples timesRepeat:
		[
			| next |
			next _ highBound atRandom: random.
			1 to: next basicSize do:
				[:eachIndex | ones _ bitCounts at: (next basicAt: eachIndex) + 1 :: + ones].
		].
	self assert: ones - (nSamples * bitsPerRandom / 2) abs / 10 < nSamples "1% max deviation"
]

{ #category : 'tests' }
RandomTest >> testSeedAndRepeatability [
	| random seed a b c |
	random := self rng.
	seed _ ((1 << 100) negated to: 1 << 100) atRandom.
	random seed: seed.
	a _ random next.
	b _ random next.
	c _ random nextBits: 100.
	random seed: seed.
	self assert: random next = a.
	self assert: random next = b.
	self assert: (random nextBits: 100) = c
]

{ #category : 'tests' }
RandomTest >> testSetAtRandom [

	| random set nSamples sum |
	random _ self rng.
	set _ Set with: 0 with: 1.
	nSamples _ self nSamplesByDefault.
	sum _ 0.
	nSamples timesRepeat: [sum _ set atRandom: random :: + sum].
	self assert: (sum / nSamples - 0.5) abs < 0.1
]

{ #category : 'tests' }
RandomTest >> testSimpleBuckets [
	"This is a poor test, see Knuth's TAOCP"

	| nbuckets buckets nSamples random slot |
	nbuckets _ 1000.
	buckets _ Array new: nbuckets.
	buckets atAllPut: 0.
	nSamples _ 1000.
	random _ self rng.
	nSamples * nbuckets timesRepeat:
		[
			slot := (random next * nbuckets) floor + 1.
			buckets at: slot put: (buckets at: slot) + 1
		].
	buckets do:
		[:each |
			"max ~17% deviation"
			self assert: (each - nSamples * 6) abs < nSamples
		]
]
