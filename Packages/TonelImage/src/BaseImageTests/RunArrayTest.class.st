Class {
	#name : 'RunArrayTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'tests-many' }
RunArrayTest >> testAddLastCoalescesSameValues [
	"Adding the same value extends the current run rather than creating a new one"
	| ra |
	ra := RunArray new.
	ra addLast: $a.
	ra addLast: $a.
	ra addLast: $a.
	self assert: ra size = 3.
	self assert: ra runs size = 1.
	self assert: (ra runs at: 1) = 3
]

{ #category : 'tests-many' }
RunArrayTest >> testAddLastMultipleDistinctValues [
	"Adding distinct values creates separate runs"
	| ra |
	ra := RunArray new.
	ra addLast: $a.
	ra addLast: $b.
	ra addLast: $c.
	self assert: ra size = 3.
	self assert: ra runs size = 3.
	self assert: (ra at: 1) = $a.
	self assert: (ra at: 2) = $b.
	self assert: (ra at: 3) = $c
]

{ #category : 'tests-one' }
RunArrayTest >> testAddLastOneElement [
	"Adding one element creates a run of 1"
	| ra |
	ra := RunArray new.
	ra addLast: $a.
	self assert: ra size = 1.
	self assert: (ra at: 1) = $a
]

{ #category : 'tests-boundary' }
RunArrayTest >> testAtBoundaryToleranceIsIntentional [
	"at: tolerates index=0 and index=size+1 for copyReplace: compatibility.
	This is NOT an error condition for RunArray — it returns the nearest value."
	| ra |
	ra := RunArray new: 3 withAll: $a.
	"index=0 and index=size+1 return the first/last value without error"
	self assert: (ra at: 0) = $a.
	self assert: (ra at: 4) = $a
]

{ #category : 'tests-boundary' }
RunArrayTest >> testDoIteratesAllElements [
	"do: should visit each logical element, expanding runs"
	| ra result |
	ra := RunArray new: 3 withAll: $x.
	ra addLast: $y.
	result := OrderedCollection new.
	ra do: [:each | result add: each].
	self assert: result size = 4.
	self assert: (result at: 1) = $x.
	self assert: (result at: 4) = $y
]

{ #category : 'tests-zero' }
RunArrayTest >> testNewIsEmpty [
	"RunArray new should be empty with size 0"
	| ra |
	ra := RunArray new.
	self assert: ra isEmpty.
	self assert: ra size = 0
]

{ #category : 'tests-one' }
RunArrayTest >> testNewWithAllCreatesOneRun [
	"new:withAll: with size>0 creates a single run"
	| ra |
	ra := RunArray new: 5 withAll: 42.
	self assert: ra size = 5.
	self assert: ra runs size = 1.
	1 to: 5 do: [:i | self assert: (ra at: i) = 42]
]

{ #category : 'tests-zero' }
RunArrayTest >> testNewWithAllZeroSizeIsEmpty [
	"new:withAll: with size 0 should return an empty RunArray"
	| ra |
	ra := RunArray new: 0 withAll: $a.
	self assert: ra isEmpty.
	self assert: ra size = 0
]

{ #category : 'tests-boundary' }
RunArrayTest >> testRunsAndValuesDo [
	"runsAndValuesDo: should yield run lengths and values"
	| ra runs values |
	ra := RunArray new: 3 withAll: $x.
	ra addLast: $y.
	ra addLast: $y.
	runs := OrderedCollection new.
	values := OrderedCollection new.
	ra runsAndValuesDo: [:run :val | runs add: run. values add: val].
	self assert: runs size = 2.
	self assert: (runs at: 1) = 3.
	self assert: (runs at: 2) = 2.
	self assert: (values at: 1) = $x.
	self assert: (values at: 2) = $y
]
