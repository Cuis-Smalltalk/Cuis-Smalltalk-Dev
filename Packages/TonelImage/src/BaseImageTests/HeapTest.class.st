Class {
	#name : 'HeapTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'tests-many' }
HeapTest >> testAddManyPreservesMinAtTop [
	"Default sort: smallest element should always be first"
	| h |
	h := Heap new.
	h add: 5.
	h add: 1.
	h add: 3.
	h add: 2.
	h add: 4.
	self assert: h size = 5.
	self assert: h first = 1
]

{ #category : 'tests-one' }
HeapTest >> testAddOneElement [
	"Adding one element: size=1, peek returns it, not empty"
	| h |
	h := Heap new.
	h add: 42.
	self assert: h size = 1.
	self deny: h isEmpty.
	self assert: h first = 42
]

{ #category : 'tests-boundary' }
HeapTest >> testCustomSortBlockMaxHeap [
	"A max-heap with sort block [:a :b | a >= b] has largest at first"
	| h |
	h := Heap sortBlock: [:a :b | a >= b].
	h add: 3.
	h add: 7.
	h add: 1.
	self assert: h first = 7
]

{ #category : 'tests-boundary' }
HeapTest >> testDoEnumeratesAllElements [
	"do: iterates all elements (not necessarily sorted)"
	| h result |
	h := Heap withAll: #(3 1 2).
	result := OrderedCollection new.
	h do: [:each | result add: each].
	self assert: result size = 3.
	self assert: (result asSortedCollection asArray) = #(1 2 3)
]

{ #category : 'tests-zero' }
HeapTest >> testNewIsEmpty [
	"A new Heap is empty"
	| h |
	h := Heap new.
	self assert: h isEmpty.
	self assert: h size = 0
]

{ #category : 'tests-many' }
HeapTest >> testRemoveFirstExtracts [
	"removeFirst extracts in ascending order for default (min) heap"
	| h result |
	h := Heap withAll: #(3 1 4 1 5 9 2 6).
	result := OrderedCollection new.
	[h isEmpty] whileFalse: [result add: h removeFirst].
	self assert: result = #(1 1 2 3 4 5 6 9) asOrderedCollection
]

{ #category : 'tests-one' }
HeapTest >> testRemoveFirstFromSingleElement [
	"removeFirst on a single-element heap leaves it empty"
	| h removed |
	h := Heap new.
	h add: 99.
	removed := h removeFirst.
	self assert: removed = 99.
	self assert: h isEmpty
]

{ #category : 'tests-exception' }
HeapTest >> testRemoveFirstOnEmptyRaisesError [
	"removeFirst on empty heap should raise an error"
	| h |
	h := Heap new.
	self should: [h removeFirst] raise: Error
]

{ #category : 'tests-many' }
HeapTest >> testSizeAfterAddAndRemove [
	"Size tracks adds and removes correctly"
	| h |
	h := Heap new.
	h add: 1. h add: 2. h add: 3.
	self assert: h size = 3.
	h removeFirst.
	self assert: h size = 2.
	h removeFirst. h removeFirst.
	self assert: h size = 0.
	self assert: h isEmpty
]
