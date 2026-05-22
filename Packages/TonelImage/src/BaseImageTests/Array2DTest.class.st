Class {
	#name : 'Array2DTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections-Arrayed',
	#package : 'BaseImageTests',
	#tag : 'Collections-Arrayed'
}

{ #category : 'tests-boundary' }
Array2DTest >> testAtWithPoint [
	"at: with a Point uses x as column (j) and y as row (i)"
	| a |
	a := Array2D height: 3 width: 3.
	a i: 2 j: 3 put: 42.
	self assert: (a at: 3 @ 2) = 42
]

{ #category : 'tests-boundary' }
Array2DTest >> testDoVisitsAllElements [
	"do: visits all height*width elements"
	| a count |
	a := Array2D height: 3 width: 4.
	count := 0.
	a do: [:each | count := count + 1].
	self assert: count = 12
]

{ #category : 'tests-scenario' }
Array2DTest >> testEquality [
	"Two Array2D with same dimensions and contents are equal"
	| a b |
	a := Array2D fromArrayOfArrays: #(#(1 2) #(3 4)).
	b := Array2D fromArrayOfArrays: #(#(1 2) #(3 4)).
	self assert: a = b
]

{ #category : 'tests-boundary' }
Array2DTest >> testExtent [
	"extent returns a Point with x=width, y=height"
	| a |
	a := Array2D height: 4 width: 6.
	self assert: a extent = (6 @ 4)
]

{ #category : 'tests-scenario' }
Array2DTest >> testFromArrayOfArrays [
	"fromArrayOfArrays: fills the Array2D from nested arrays"
	| a |
	a := Array2D fromArrayOfArrays: #(#(1 2 3) #(4 5 6)).
	self assert: a height = 2.
	self assert: a width = 3.
	self assert: (a i: 1 j: 1) = 1.
	self assert: (a i: 1 j: 3) = 3.
	self assert: (a i: 2 j: 1) = 4.
	self assert: (a i: 2 j: 3) = 6
]

{ #category : 'tests-zero' }
Array2DTest >> testNewElementsAreNil [
	"A new Array2D has all nil elements"
	| a |
	a := Array2D height: 2 width: 3.
	self assert: a height = 2.
	self assert: a width = 3.
	1 to: 2 do: [:i |
		1 to: 3 do: [:j |
			self assert: (a i: i j: j) isNil]]
]

{ #category : 'tests-one' }
Array2DTest >> testOneByOneAccess [
	"A 1x1 Array2D stores and retrieves a single element"
	| a |
	a := Array2D height: 1 width: 1.
	a i: 1 j: 1 put: 99.
	self assert: (a i: 1 j: 1) = 99
]

{ #category : 'tests-exception' }
Array2DTest >> testOutOfBoundsColumnRaisesError [
	"Accessing out-of-bounds column should raise an error"
	| a |
	a := Array2D height: 2 width: 2.
	self should: [a i: 1 j: 3] raise: Error
]

{ #category : 'tests-many' }
Array2DTest >> testPutAndGetElements [
	"i:j:put: stores elements retrievable by i:j:"
	| a |
	a := Array2D height: 3 width: 3.
	a i: 1 j: 1 put: 1.
	a i: 2 j: 2 put: 5.
	a i: 3 j: 3 put: 9.
	self assert: (a i: 1 j: 1) = 1.
	self assert: (a i: 2 j: 2) = 5.
	self assert: (a i: 3 j: 3) = 9.
	self assert: (a i: 1 j: 3) isNil
]
