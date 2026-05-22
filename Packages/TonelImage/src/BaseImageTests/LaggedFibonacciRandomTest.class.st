Class {
	#name : 'LaggedFibonacciRandomTest',
	#superclass : 'RandomTest',
	#category : 'BaseImageTests-Kernel-Numbers',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Numbers'
}

{ #category : 'setup' }
LaggedFibonacciRandomTest >> nSamplesByDefault [

	^self rng majorLag * 100
]

{ #category : 'setup' }
LaggedFibonacciRandomTest >> rngClass [

	^LaggedFibonacciRandom
]
