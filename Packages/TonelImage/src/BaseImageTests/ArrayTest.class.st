Class {
	#name : 'ArrayTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections-Arrayed',
	#package : 'BaseImageTests',
	#tag : 'Collections-Arrayed'
}

{ #category : 'test - accessing' }
ArrayTest >> testAntepenultimateIfAbsentReturnsAntepenultimateIfExists [

	self assert: (#(1 2 3) antepenultimateIfAbsent: [ self fail ]) equals: 1.
]

{ #category : 'test - accessing' }
ArrayTest >> testAntepenultimateIfAbsentValuesIfAbsentBlockWhenNoAntepenultimate [

	self assert: (#(1 2) antepenultimateIfAbsent: [ 0 ]) equals: 0.
]

{ #category : 'test - accessing' }
ArrayTest >> testAntepenultimateReturnsAntepenultimateIfExists [

	self assert: #(1 2 3) antepenultimate equals: 1.
]

{ #category : 'test - accessing' }
ArrayTest >> testAntepenultimateSignalsErrorWhenCollectionIsNotBigEnough [

	self
		should: [ #(1 2) antepenultimate ]
		raise: Error
		withMessageText: Collection collectionTooSmallDescription
]

{ #category : 'test - insert:ShiftingRight:' }
ArrayTest >> testCanNotInsertShiftingRightWhenInsertionPointIsBiggerThanSize [

	| arrayToInsertTo invalidIndex |
	
	arrayToInsertTo := #(1 2 3) copy.
	invalidIndex := arrayToInsertTo size + 1.
	
	self 
		should: [ arrayToInsertTo insert: 0 shiftingRightAt: invalidIndex ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: anError messageText equals: (Object errorDescriptionForSubcriptBounds: invalidIndex).
			self assert: arrayToInsertTo equals: #(1 2 3) ].
]

{ #category : 'test - insert:ShiftingRight:' }
ArrayTest >> testCanNotInsertShiftingRightWhenInsertionPointIsLessThanOne [

	| arrayToInsertTo invalidIndex |
	
	arrayToInsertTo := #(1 2 3) copy.
	invalidIndex := 0.
	
	self 
		should: [ arrayToInsertTo insert: 0 shiftingRightAt: invalidIndex ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: anError messageText equals: (Object errorDescriptionForSubcriptBounds: invalidIndex).
			self assert: arrayToInsertTo equals: #(1 2 3) ].
]

{ #category : 'test - insert:ShiftingRight:' }
ArrayTest >> testInsertShiftingRightLoosesLastElementAndKeepsPreviousOnes [

	self assert: (#(1 3 4 5) copy insert: 2 shiftingRightAt: 2) equals: #(1 2 3 4)
]

{ #category : 'test - insert:ShiftingRight:' }
ArrayTest >> testInsertShiftingRightReplacesElementWhenSizeIsOne [

	self assert: (#(1) copy insert: 2 shiftingRightAt: 1) equals: #(2)
]

{ #category : 'test - insert:ShiftingRight:' }
ArrayTest >> testInsertShiftingRightReplacesLastElementWhenInsertingAtLastIndex [

	self assert: (#(1 3 4 5) copy insert: 2 shiftingRightAt: 4) equals: #(1 3 4 2)
]

{ #category : 'test - testing' }
ArrayTest >> testIsArray [
	self assert: (#(1 2 3) is: #Array).
	self assert: (#() is: #Array).
]

{ #category : 'test - testing' }
ArrayTest >> testIsCollection [
	self assert: (#(1 2 3) is: #Collection).
	self assert: (#() is: #Collection).
]

{ #category : 'test - accessing' }
ArrayTest >> testLastIfEmptyReturnsLastWhenExist [

	self assert: (#(1) lastIfEmpty: [ self fail ]) equals: 1.
]

{ #category : 'test - accessing' }
ArrayTest >> testLastIfEmptyValuesEmptyBlockWhenEmpty [

	self assert: (#() lastIfEmpty: [ 1 ]) equals: 1.
]

{ #category : 'test - accessing' }
ArrayTest >> testLastSignalsErrorWhenEmpty [

	self
		should: [ #() last ]
		raise: Error
		withMessageText: Collection emptyCollectionDescription
]

{ #category : 'test - accessing' }
ArrayTest >> testPenultimateIfAbsentReturnsPenultimateIfExists [

	self assert: (#(2 3) penultimateIfAbsent: [ self fail ]) equals: 2.
]

{ #category : 'test - accessing' }
ArrayTest >> testPenultimateIfAbsentValuesIfAbsentBlockWhenNoPenultimate [

	self assert: (#(1) penultimateIfAbsent: [ 2 ]) equals: 2.
]

{ #category : 'test - accessing' }
ArrayTest >> testPenultimateReturnsPenultimateIfExists [

	self assert: #(2 3) penultimate equals: 2.
]

{ #category : 'test - accessing' }
ArrayTest >> testPenultimateSignalsErrorWhenCollectionIsNotBigEnough [

	self
		should: [ #(1) penultimate ]
		raise: Error
		withMessageText: Collection collectionTooSmallDescription
]
