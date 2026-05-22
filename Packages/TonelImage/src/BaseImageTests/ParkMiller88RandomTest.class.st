Class {
	#name : 'ParkMiller88RandomTest',
	#superclass : 'RandomTest',
	#category : 'BaseImageTests-Kernel-Numbers',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Numbers'
}

{ #category : 'setup' }
ParkMiller88RandomTest >> rngClass [

	^ParkMiller88Random
]

{ #category : 'tests' }
ParkMiller88RandomTest >> testParkMillerCorrectness [
	"The correctness test suggested by the authors"

	| random next |
	random _ self rng seed: 1.
	10000 timesRepeat: [next _ random next].
	self assert: next * 16r7FFFFFFF = 1043618065.0
]

{ #category : 'tests' }
ParkMiller88RandomTest >> testParkMillerInitialValues [

	| nSamples random samples |
	nSamples _ self nSamplesByDefault.
	random _ self rng.
	samples _ 1 to: nSamples :: collect: [:each | random next].
	self assert: (samples average - 0.5) abs < 0.1.
	self assert: samples size * 10 > nSamples
]

{ #category : 'tests' }
ParkMiller88RandomTest >> testParkMillerSeedCornerCases [
	"Test the insernal state of the Park-Miller generator is correctly set, including corner cases."
	| random m |
	m _ 2147483647.
	{1. 2. (m-1) atRandom. m-1. m-2}
		do: [:each|
			random _ self rng seed: each.
			self assert: (random instVarNamed: #seed) = each]
]

{ #category : 'tests' }
ParkMiller88RandomTest >> testParkMillerSeedRange [
	"Test the insernal state of the Park-Miller generator is correctly set, even for arbitrary user-provided seeds."
	| random m |
	m _ 2147483647.
	{1. 1000. 10000000000000000000000. -100. 0. -1. m. m-1. m-2. m+1}
		do: [:each|
			random _ self rng seed: each.
			self assert: ((random instVarNamed: #seed) between: 1 and: m - 1)]
]
