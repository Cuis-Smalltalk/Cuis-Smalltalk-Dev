Class {
	#name : 'SortedCollectionTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'tests-many' }
SortedCollectionTest >> testAddManyMaintainsSortOrder [
	"Adding elements in any order keeps collection sorted ascending"
	| sc |
	sc := SortedCollection new.
	sc add: 5.
	sc add: 2.
	sc add: 8.
	sc add: 1.
	sc add: 4.
	self assert: sc size = 5.
	self assert: sc first = 1.
	self assert: sc last = 8.
	self assert: (sc at: 1) = 1.
	self assert: (sc at: 2) = 2.
	self assert: (sc at: 3) = 4
]

{ #category : 'tests-one' }
SortedCollectionTest >> testAddOneElement [
	"Adding one element sets size to 1 and first=last=that element"
	| sc |
	sc := SortedCollection new.
	sc add: 42.
	self assert: sc size = 1.
	self assert: sc first = 42.
	self assert: sc last = 42
]

{ #category : 'tests-exception' }
SortedCollectionTest >> testAtPutRaisesError [
	"at:put: should not be implemented on SortedCollection"
	| sc |
	sc := SortedCollection withAll: #(1 2 3).
	self should: [sc at: 1 put: 99] raise: Error
]

{ #category : 'tests-boundary' }
SortedCollectionTest >> testCustomSortBlockDescending [
	"Custom sort block gives descending order"
	| sc |
	sc := SortedCollection sortBlock: [:a :b | a > b].
	sc add: 3. sc add: 1. sc add: 5. sc add: 2.
	self assert: sc first = 5.
	self assert: sc last = 1
]

{ #category : 'tests-many' }
SortedCollectionTest >> testDoIteratesInSortedOrder [
	"do: iterates elements in ascending sorted order"
	| sc result |
	sc := SortedCollection new.
	sc add: 3. sc add: 1. sc add: 2.
	result := OrderedCollection new.
	sc do: [:each | result add: each].
	self assert: result = #(1 2 3) asOrderedCollection
]

{ #category : 'tests-zero' }
SortedCollectionTest >> testNewIsEmpty [
	"A new SortedCollection is empty"
	| sc |
	sc := SortedCollection new.
	self assert: sc isEmpty.
	self assert: sc size = 0
]

{ #category : 'tests-boundary' }
SortedCollectionTest >> testSelectReturnsSortedCollection [
	"select: on a SortedCollection returns a SortedCollection"
	| sc result |
	sc := SortedCollection withAll: #(1 2 3 4 5 6).
	result := sc select: [:each | each even].
	self assert: (result isKindOf: SortedCollection).
	self assert: result size = 3.
	self assert: result first = 2.
	self assert: result last = 6
]

{ #category : 'tests-many' }
SortedCollectionTest >> testWithAllSortsElements [
	"withAll: should produce a sorted collection regardless of input order"
	| sc |
	sc := SortedCollection withAll: #(5 3 1 4 2).
	self assert: sc size = 5.
	self assert: sc asArray = #(1 2 3 4 5)
]
