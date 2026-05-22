Class {
	#name : 'BagTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'tests' }
BagTest >> testProduct [

	| bag |	
	bag _ Bag new.
	bag add: 3; add: 4; add: 4; add: 5.
	self assert: (bag product: [ :v | v ]) = 240.
	self assert: (bag product: [ :v | v-1 ]) = 72.
]

{ #category : 'tests' }
BagTest >> testSumIfEmptyEvalsIfEmptyBlockWhenEmpty [

	self assert: (Bag new sum: [:each | 1 ] ifEmpty: [ 'empty' ]) equals: 'empty'.
]

{ #category : 'tests' }
BagTest >> testSumIfEmptyEvalsSumBlockForEachElementSumingTheResult [

	| bag |
	
	bag := Bag new.
	bag add: 'hello'; add: 'hello'.
	bag add: 'bye'.
	
	self assert: (bag sum: [ :aString | aString = 'hello' ifTrue: [ 2 ] ifFalse: [ -1 ]] ifEmpty: [ self fail ]) equals: 3.
]
