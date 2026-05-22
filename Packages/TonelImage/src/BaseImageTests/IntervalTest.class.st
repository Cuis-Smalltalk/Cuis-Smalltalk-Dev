Class {
	#name : 'IntervalTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections',
	#package : 'BaseImageTests',
	#tag : 'Collections'
}

{ #category : 'testing' }
IntervalTest >> testAt [
	"
	IntervalTest new testAt
	"
	self assert: (2 to: 5 by: 2) first = 2.
	self assert: ((2 to: 5 by: 2) at: 1) = 2.
	self assert: ((2 to: 5 by: 2) at: 2) = 4.
	self assert: (2 to: 5 by: 2) last = 4.

	self assert: (0 to: 2.4 by: 0.1) first = 0.
	self assert: ((0 to: 2.4 by: 0.1) at: 1) = 0.
	self assert: (((0 to: 2.4 by: 0.1) at: 2) isWithin: 1 floatsFrom: 0.1).
	self assert: (((0 to: 2.4 by: 0.1) at: 3) isWithin: 1 floatsFrom: 0.2).
	self assert: (((0 to: 2.4 by: 0.1) at: 23) isWithin: 1 floatsFrom: 2.2).
	self assert: ((0 to: 2.4 by: 0.1) at: 24) = 2.3.
	self assert: ((0 to: 2.4 by: 0.1) at: 25) = 2.4.
	self assert: (0 to: 2.4 by: 0.1) last = 2.4.
]

{ #category : 'test - includes' }
IntervalTest >> testDoesNotIncludeNumbersOutsideTheInInterval [

	| from1To10 |
	
	from1To10 := 1 to: 10.
	
	self deny: (from1To10 includes: 0).
	self deny: (from1To10 includes: 11)
]

{ #category : 'test - includes' }
IntervalTest >> testDoesNotIncludeObjectsThatAreNotNumbers [

	self deny: ((1 to: 10) includes: $a)
]

{ #category : 'test - includes' }
IntervalTest >> testDoesNotIncludedNumbersThatAreInRangeButNotReachedByTheStep [

	| from1To10By2 |
	
	from1To10By2 := 1 to: 10 by: 2.
	
	self deny: (from1To10By2 includes: 0).
	self deny: (from1To10By2 includes: 2).
	self deny: (from1To10By2 includes: 6).
	self deny: (from1To10By2 includes: 10).
]

{ #category : 'test - includes' }
IntervalTest >> testDoesNotIncludedNumbersThatAreInRangeButNotReachedByTheStepWithReversedIntervals [

	| from10To1ByMinus2 |
	
	from10To1ByMinus2 := 10 to: 1 by: -2.
	
	self deny: (from10To1ByMinus2 includes: 0).
	self deny: (from10To1ByMinus2 includes: 1).
	self deny: (from10To1ByMinus2 includes: 5).
	self deny: (from10To1ByMinus2 includes: 9).
]

{ #category : 'test - intersectsRange' }
IntervalTest >> testDoesNotIntersectRangeWhen_FirstAfter_LastAfter [

	self deny: ((5 to: 10) intersectsRange: (11 to: 12))
]

{ #category : 'test - intersectsRange' }
IntervalTest >> testDoesNotIntersectRangeWhen_FirstBefore_LastBefore [

	self deny: ((5 to: 10) intersectsRange: (1 to: 4))
]

{ #category : 'testing' }
IntervalTest >> testFloatInterval [
	"
	IntervalTest new testFloatInterval
	"
	
	self assert: (0 to: 2.4 by: 0.1) size = 25.
	self assert: (0 to: 2.4 by: 0.1) first = 0.
	self assert: (0 to: 2.4 by: 0.1) last = 2.4.

	2 to: 200 do: [ :n |
		self assert: (0.0 to: 1.0 count: n) size = n.
		self assert: (0.0 to: 1.0 count: n) first = 0.0.
		self assert: ((0.0 to: 1.0 count: n) last isWithin: 1 floatsFrom: 1.0)].
	2 to: 200 do: [ :n |
		self assert: (1.0 to: 0.0 count: n) size = n.
		self assert: (1.0 to: 0.0 count: n) first = 1.0.
		self assert: ((1.0 to: 0.0 count: n) last isWithin: 1 floatsFrom: 0.0)].
]

{ #category : 'testing' }
IntervalTest >> testFloatToByDo [
	"
	IntervalTest new testFloatToByDo
	"
	| interval toByDo |

	"See comment at #to:by:do:"
	self shouldFail: [
		interval _ 0 to: 2.4 by: 0.1.
		toByDo _ Array streamContents: [ :strm |
			0 to: 2.4 by: 0.1 do: [ :each |
				strm nextPut: each ]].
		self assert: toByDo size = interval size.
		1 to: toByDo size do: [ :i |
			self assert: (toByDo at: i) = (interval at: i) ]]
]

{ #category : 'test - includes' }
IntervalTest >> testIncludedNumbersRespectAStepDifferentToOne [

	| from1To10By2 |
	
	from1To10By2 := 1 to: 10 by: 2.
	
	self assert: (from1To10By2 includes: 1).
	self assert: (from1To10By2 includes: 5).
	self assert: (from1To10By2 includes: 9).
]

{ #category : 'test - includes' }
IntervalTest >> testIncludedNumbersRespectNegativeSteps [

	| from10To1ByMinus2 |
	
	from10To1ByMinus2 := 10 to: 1 by: -2.
	
	self assert: (from10To1ByMinus2 includes: 10).
	self assert: (from10To1ByMinus2 includes: 6).
	self assert: (from10To1ByMinus2 includes: 2).
]

{ #category : 'tests - includesAll' }
IntervalTest >> testIncludesAllOfFailsWhenFirstNotIncluded [

	self deny: ((3 to: 9 by: 2) includesAllOf: (1 to: 9 by: 2))
]

{ #category : 'tests - includesAll' }
IntervalTest >> testIncludesAllOfFailsWhenLastNotIncluded [

	self deny: ((1 to: 7 by: 2) includesAllOf: (1 to: 9 by: 2))
]

{ #category : 'tests - includesAll' }
IntervalTest >> testIncludesAllOfFailsWithOtherTypeOfCollectionWhenElementNotIncluded [

	self deny: ((1 to: 9 by: 2) includesAllOf: {1. 3. 5. 6. 7. 9})
]

{ #category : 'tests - includesAll' }
IntervalTest >> testIncludesAllOfIntervalWithIncludedFirstLastAndDifferentIncrement [

	self assert: ((1 to: 9 by: 2) includesAllOf: (3 to: 7 by: 4))
]

{ #category : 'tests - includesAll' }
IntervalTest >> testIncludesAllOfIntervalWithIncludedFirstSameLastAndIncrement [

	self assert: ((1 to: 10 by: 2) includesAllOf: (3 to: 10 by: 2))
]

{ #category : 'tests - includesAll' }
IntervalTest >> testIncludesAllOfIntervalWithSameFirstIncrementAndIncludedLast [

	self assert: ((1 to: 10 by: 2) includesAllOf: (1 to: 7 by: 2))
]

{ #category : 'tests - includesAll' }
IntervalTest >> testIncludesAllOfIntervalWithSameFirstLastAndIncrement [

	self assert: ((1 to: 10 by: 2) includesAllOf: (1 to: 10 by: 2))
]

{ #category : 'tests - includesAll' }
IntervalTest >> testIncludesAllOfOtherTypeOfCollection [

	self assert: ((1 to: 9 by: 2) includesAllOf: {1. 3. 5. 7. 9})
]

{ #category : 'test - includes' }
IntervalTest >> testIncludesNumbersInsideTheInterval [

	| from1To10 |
	
	from1To10 := 1 to: 10.
	
	self assert: (from1To10 includes: 1).
	self assert: (from1To10 includes: 5).
	self assert: (from1To10 includes: 10).
]

{ #category : 'test - includes' }
IntervalTest >> testIncludesNumbersInsideTheIntervalEvenForReversedIntervals [

	| from10To1 |
	
	from10To1 := 10 to: 1 by: -1.
	
	self assert: (from10To1 includes: 1).
	self assert: (from10To1 includes: 5).
	self assert: (from10To1 includes: 10).
]

{ #category : 'testing' }
IntervalTest >> testIntegerInterval [
	"
	IntervalTest new testIntegerInterval
	"
	| z |
	self assert: (2 to: 5 by: 2) size = 2.
	self assert: (2 to: 5 by: 2) last = 4.
	z _ 0.
	(2 to: 5 by: 2) do: [ :i | z _ i ].
	self assert: z = 4
]

{ #category : 'testing' }
IntervalTest >> testIntegerToByDo [
	"
	IntervalTest new testIntegerToByDo
	"
	| interval toByDo |
	interval _ 2 to: 5 by: 2.
	toByDo _ Array streamContents: [ :strm |
		2 to: 5 by: 2 do: [ :each |
			strm nextPut: each ]].
	self assert: toByDo size = interval size.
	1 to: toByDo size do: [ :i |
		self assert: (toByDo at: i) = (interval at: i) ]
]

{ #category : 'test - intersectsRange' }
IntervalTest >> testIntersectsRangeWhen_FirstBefore_LastAfter [

	self assert: ((5 to: 10) intersectsRange: (4 to: 11))
]

{ #category : 'test - intersectsRange' }
IntervalTest >> testIntersectsRangeWhen_FirstBefore_LastIncluded [

	self assert: ((5 to: 10) intersectsRange: (1 to: 5))
]

{ #category : 'test - intersectsRange' }
IntervalTest >> testIntersectsRangeWhen_FirstIncluded_LastAfter [

	self assert: ((5 to: 10) intersectsRange: (5 to: 11))
]

{ #category : 'test - intersectsRange' }
IntervalTest >> testIntersectsRangeWhen_FirstIncluded_LastIncluded [

	self assert: ((5 to: 10) intersectsRange: (5 to: 10))
]

{ #category : 'test - includes' }
IntervalTest >> testIntervalWithOneNumberAndNegativeStepBiggerThanOneIncludeOnlyThatNumber [

	| from1To1ByMinus2 |
	
	from1To1ByMinus2 := 1 to: 1 by: -2.
	
	self assert: (from1To1ByMinus2 includes: 1).
	self deny: (from1To1ByMinus2 includes: -1).
	self deny: (from1To1ByMinus2 includes: 3).
]

{ #category : 'test - includes' }
IntervalTest >> testIntervalWithOneNumberAndNegativeStepIncludeOnlyThatNumber [

	| from1To1ByMinus1 |
	
	from1To1ByMinus1 := 1 to: 1 by: -1.
	
	self assert: (from1To1ByMinus1 includes: 1).
	self deny: (from1To1ByMinus1 includes: 0).
	self deny: (from1To1ByMinus1 includes: 2).
]

{ #category : 'test - includes' }
IntervalTest >> testIntervalWithOneNumberAndStepBiggerThanOneIncludesOnlyTheNumber [

	| from1To1By2 |
	
	from1To1By2 := 1 to: 1 by: 2.
	
	self assert: (from1To1By2 includes: 1).
	self deny: (from1To1By2 includes: -1).
	self deny: (from1To1By2 includes: 3).
]

{ #category : 'test - includes' }
IntervalTest >> testIntervalWithOneNumberIncludesOnlyTheNumber [

	| from1To1 |
	
	from1To1 := 1 to: 1.
	
	self assert: (from1To1 includes: 1).
	self deny: (from1To1 includes: 0).
	self deny: (from1To1 includes: 2).
]
