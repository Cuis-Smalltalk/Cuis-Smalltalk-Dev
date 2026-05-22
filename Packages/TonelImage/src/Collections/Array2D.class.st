"
My instances are two dimensional arrays, providing basic accessors and some convenience methods. Specialized subclasses provide additional services. See packages LinearAlgebra and SignalProcessing.
"
Class {
	#name : 'Array2D',
	#superclass : 'Object',
	#instVars : [
		'width',
		'height',
		'elements'
	],
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'instance creation' }
Array2D class >> extent: aPoint [
	^self basicNew initHeight: aPoint y width: aPoint x
]

{ #category : 'instance creation' }
Array2D class >> fromArrayOfArrays: anArray [
	"Example:
	Array2D fromArrayOfArrays: #(
		#(1 2 0 0 0 0 3) 
		#(2 4 1 0 0 -4 3) 
		#(0 1 1 1 0 0 3)
		#(0 0 1 1 1 0 3)
		#(0 0 0 1 1 1 3)
		#(0 1 0 0 1 1 3)
	).
	"

	| result |
	result := self height: anArray size width: anArray first size.
	result fillWithArrayOfArrays: anArray.
	^result

]

{ #category : 'instance creation' }
Array2D class >> height: h width: w [
	^self basicNew initHeight: h width: w
]

{ #category : 'instance creation' }
Array2D class >> new [
	^ self shouldNotImplement
]

{ #category : 'instance creation' }
Array2D class >> newSize: aPoint [

	^ self width: aPoint x height: aPoint y 
]

{ #category : 'instance creation' }
Array2D class >> width: w height: h [
	^self basicNew initHeight: h width: w
]

{ #category : 'comparing' }
Array2D >> = another [
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^false ].
	width = another width ifFalse: [ ^false ].
	height = another height ifFalse: [ ^false ].
	^elements = another elements
]

{ #category : 'accessing' }
Array2D >> at: aPointOrIndex [
	"If aPointOrIndex is a Number, the receiver must be a vector (either row or column).
	Indexes goes from 1 to size (width or height)"

	| i j |
	aPointOrIndex isPoint
		ifTrue: [ i := aPointOrIndex y. j := aPointOrIndex x ]
		ifFalse: [
			width = 1 ifTrue: [ i := aPointOrIndex. j := 1 ].
			height = 1 ifTrue: [ i := 1. j := aPointOrIndex. ]].
	^ self i: i j: j 
]

{ #category : 'accessing' }
Array2D >> at: aPointOrIndex put: aNumber [
	"If aPointOrIndex is a Number, the receiver must be a vector (either row or column).
	Indexes goes from 1 to size (width or height)"

	| i j |
	aPointOrIndex isPoint
		ifTrue: [ i := aPointOrIndex y. j := aPointOrIndex x ]
		ifFalse: [
			width = 1 ifTrue: [ i := aPointOrIndex. j := 1 ].
			height = 1 ifTrue: [ i := 1. j := aPointOrIndex ]].
	^ self i: i j: j put: aNumber
]

{ #category : 'iterating' }
Array2D >> do: aBlock [
	"Evaluate aBlock on each element"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: (self i: i j: j) ] ]
]

{ #category : 'accessing' }
Array2D >> elements [

	^elements
]

{ #category : 'private' }
Array2D >> elementsIndexForI: i j: j [
	"Indexes goes from 1 to size (width or height)"

	(j between: 1 and: width) ifFalse: [ self errorSubscriptBounds: j@i ].
	^ i-1*width+j
]

{ #category : 'accessing' }
Array2D >> extent [
	"The result is a point. So, x is our width and y is our height"

	^width @ height
]

{ #category : 'filling' }
Array2D >> fillWith: anArray2D [

	1 to: (height min: anArray2D height) do: [ :i |
		1 to: (width min: anArray2D width) do: [ :j |
			self i: i j: j put: (anArray2D i: i j: j) ] ]
]

{ #category : 'filling' }
Array2D >> fillWithArrayOfArrays: anArray [
	"Array2D fromArrayOfArrays: #(
		#(1 2 0 0 0 0) 
		#(2 4 1 0 0 -4) 
		#(0 1 1 1 0 0)
		#(0 0 1 1 1 0)
		#(0 0 0 1 1 1)
		#(0 1 0 0 1 1)
	)."

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			self i: i j: j put: ((anArray at: i) at: j) ] ]
]

{ #category : 'comparing' }
Array2D >> hash [
	^elements hash
]

{ #category : 'accessing' }
Array2D >> height [

	^height
]

{ #category : 'accessing' }
Array2D >> i: i j: j [

	"Answer element at row i, column j"

	^ elements at: (self elementsIndexForI: i j: j)
]

{ #category : 'accessing' }
Array2D >> i: i j: j put: anObject [
	"Store anObject as the element at row i, column j"

	elements at: (self elementsIndexForI: i j: j) put: anObject
]

{ #category : 'initialization' }
Array2D >> initHeight: h width: w [

	height := h.
	width := w.
	self initializeElements
]

{ #category : 'initialization' }
Array2D >> initializeElements [

	elements := Array new: height * width
]

{ #category : 'testing' }
Array2D >> isSquare [

	^height = width
]

{ #category : 'copying' }
Array2D >> postCopy [
	elements := elements copy
]

{ #category : 'printing' }
Array2D >> print [
	self printOn: Transcript.
	Transcript newLine
]

{ #category : 'printing' }
Array2D >> printOn: aStream [
	1 to: height do: [ :i |
		aStream nextPutAll: '| '.
		1 to: width do: [ :j |
			aStream print: (self i: i j: j).
			aStream nextPut: $  ].
		aStream nextPut: $|; newLine ]
]

{ #category : 'iterating' }
Array2D >> replaceValues: aBlock [
	"Replace each value with the result of evaluating aBlock, with i, j and the previous value as the arguments"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			self i: i j: j put:
				(aBlock value: i value: j value: (self i: i j: j)) ] ]
]

{ #category : 'accessing' }
Array2D >> size [
	"The result is a point. So, x is our width and y is our height"

	^width @ height
]

{ #category : 'accessing' }
Array2D >> width [

	^width
]

{ #category : 'iterating' }
Array2D >> with: otherImage do: aBlock [
	"Evaluate aBlock on each element"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: (self i: i j: j) value: (otherImage i: i j: j) ] ]
]

{ #category : 'iterating' }
Array2D >> withIndexesDo: aBlock [
	"Evaluate aBlock on each element, including i, j indexes also as arguments"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: i value: j value: (self i: i j: j) ] ]
]

{ #category : 'accessing' }
Array2D >> wrapI: i j: j [

	^ self i: i-1\\height+1 j: j-1\\width+1
]

{ #category : 'accessing' }
Array2D >> wrapI: i j: j put: anObject [

	^ self i: i-1\\height+1 j: j-1\\width+1 put: anObject
]

{ #category : 'accessing' }
Array2D >> x: x y: y [
	"Answer element at column x, row y
	x and y are integers in [1 .. width] and [1 .. height]"

	^ self i: y j: x
]

{ #category : 'accessing' }
Array2D >> x: x y: y put: aNumber [
	"Set the value at coordinates x@y.
	x and y are integers in [1 .. width] and [1 .. height]"

	^ self i: y j: x put: aNumber
]
