Class {
	#name : 'BinarySearchTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'testing' }
BinarySearchTest >> testquickFindFirst [

	| collection |
	collection := 1000 to: 2100.
	collection withIndexDo:
		[:eachElement :eachIndex |
			self assert: (collection quickFindFirst: [:one | one >= eachElement]) = eachIndex.
			self assert: (collection quickFindFirst: [:one | one >= (eachElement - 0.5)]) = eachIndex
		].
	self assert: (collection quickFindFirst: [:one | one > 2101]) = 0
]

{ #category : 'testing' }
BinarySearchTest >> testquickFindLast [

	| collection |
	collection := 1000 to: 2100.
	collection withIndexDo:
		[:eachElement :eachIndex |
			self assert: (collection quickFindLast: [:one | one <= eachElement]) = eachIndex.
			self assert: (collection quickFindLast: [:one | one <= (eachElement + 0.5)]) = eachIndex
		].
	self assert: (collection quickFindLast: [:one | one < 0]) = 0
]

{ #category : 'testing' }
BinarySearchTest >> testquickIndexOf [

	| collection |
	collection := 1000 to: 2100.
	collection withIndexDo:
		[:eachElement :eachIndex |
			self assert: (collection quickIndexOf: eachElement) = eachIndex
		].
	self assert: (collection quickIndexOf: 999) = 0.
	self assert: (collection quickIndexOf: 2101) = 0
]

{ #category : 'testing' }
BinarySearchTest >> testquickIndexOfWithSemistableOrder [

	| collection |
	collection := SortedCollection
		sortBlock: [:x :y | x key <= y key].
	1 to: 1000 do: [:each | collection add: each -> each].
	1 to: 50 do: [:each | collection add: 42 -> each].
	self assert: (collection quickIndexOf: 42 -> 6) = 48
]
