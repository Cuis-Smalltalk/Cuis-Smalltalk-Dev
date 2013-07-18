'From Cuis 4.1 of 12 December 2012 [latest update: #1728] on 16 June 2013 at 8:26:28.459346 pm'!
!classDefinition: #Array2D category: #'Collections-Multidimensional'!
Object subclass: #Array2D
	instanceVariableNames: 'width height elements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Multidimensional'!

!Array2D methodsFor: 'accessing' stamp: 'jmv 6/16/2013 20:02'!
at: aPointOrIndex
	"If aPointOrIndex is a Number, the receiver must be a vector (either row or column).
	Indexes goes from 1 to size (width or height)"

	| i j |
	aPointOrIndex isPoint
		ifTrue: [ i _ aPointOrIndex y. j _ aPointOrIndex x ]
		ifFalse: [
			width = 1 ifTrue: [ i _ aPointOrIndex. j _ 1 ].
			height = 1 ifTrue: [ i _ 1. j _ aPointOrIndex. ]].
	^ self i: i j: j ! !

!Array2D methodsFor: 'accessing' stamp: 'jmv 6/16/2013 20:03'!
at: aPointOrIndex put: aNumber
	"If aPointOrIndex is a Number, the receiver must be a vector (either row or column).
	Indexes goes from 1 to size (width or height)"

	| i j |
	aPointOrIndex isPoint
		ifTrue: [ i _ aPointOrIndex y. j _ aPointOrIndex x ]
		ifFalse: [
			width = 1 ifTrue: [ i _ aPointOrIndex. j _ 1 ].
			height = 1 ifTrue: [ i _ 1. j _ aPointOrIndex ]].
	^ self i: i j: j put: aNumber! !

!Array2D methodsFor: 'accessing' stamp: 'jmv 6/16/2013 20:04'!
height

	^height! !

!Array2D methodsFor: 'accessing' stamp: 'jmv 6/16/2013 20:03'!
i: i j: j

	"Answer element at row i, column j"

	^ elements at: (self elementsIndexForI: i j: j)! !

!Array2D methodsFor: 'accessing' stamp: 'jmv 6/16/2013 20:03'!
i: i j: j put: anObject
	"Store anObject as the element at row i, column j"

	elements at: (self elementsIndexForI: i j: j) put: anObject! !

!Array2D methodsFor: 'accessing' stamp: 'jmv 6/16/2013 20:04'!
size
	"The result is a point. So, x is our width and y is our height"

	^width @ height! !

!Array2D methodsFor: 'accessing' stamp: 'jmv 6/16/2013 20:04'!
width

	^width! !

!Array2D methodsFor: 'accessing' stamp: 'jmv 6/16/2013 20:04'!
x: x y: y
	"Answer element at column x, row y"
	^ self i: y j: x! !

!Array2D methodsFor: 'accessing' stamp: 'jmv 6/16/2013 20:04'!
x: x y: y put: aNumber
	"Answer element at column x, row y"
	^ self i: y j: x put: aNumber! !

!Array2D methodsFor: 'copying' stamp: 'jmv 6/16/2013 20:05'!
postCopy
	elements _ elements copy! !

!Array2D methodsFor: 'filling' stamp: 'jmv 6/16/2013 20:06'!
fillWith: anArray2D

	1 to: (height min: anArray2D height) do: [ :i |
		1 to: (width min: anArray2D width) do: [ :j |
			self i: i j: j put: (anArray2D i: i j: j) ] ]! !

!Array2D methodsFor: 'filling' stamp: 'jmv 6/16/2013 20:07'!
fillWithArrayOfArrays: anArray
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
			self i: i j: j put: ((anArray at: i) at: j) ] ]! !

!Array2D methodsFor: 'initialization' stamp: 'jmv 6/16/2013 20:08'!
initHeight: h width: w

	height _ h.
	width _ w.
	self initializeElements.! !

!Array2D methodsFor: 'initialization' stamp: 'jmv 6/16/2013 20:08'!
initializeElements

	elements _ Array new: height * width! !

!Array2D methodsFor: 'misc' stamp: 'jmv 6/16/2013 20:10'!
replaceValues: aBlock
	"Replace each value with the result of evaluating aBlock, with i, j and the previous value as the arguments"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			self i: i j: j put:
				(aBlock value: i value: j value: (self i: i j: j)) ] ]! !

!Array2D methodsFor: 'misc' stamp: 'jmv 6/16/2013 20:10'!
withIndexesDo: aBlock
	"Replace each value with the result of evaluating aBlock, with the previous value as the argument"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: i value: j value: (self i: i j: j) ] ]! !

!Array2D methodsFor: 'printing' stamp: 'jmv 6/16/2013 20:10'!
print
	self printOn: Transcript.
	Transcript newLine! !

!Array2D methodsFor: 'printing' stamp: 'jmv 6/16/2013 20:10'!
printOn: aStream
	1 to: height do: [ :i |
		aStream nextPutAll: '| '.
		1 to: width do: [ :j |
			aStream print: (self i: i j: j).
			aStream nextPut: $  ].
		aStream nextPut: $|; newLine ]! !

!Array2D methodsFor: 'private' stamp: 'jmv 6/16/2013 20:09'!
elementsIndexForI: i j: j
	"Indexes goes from 1 to size (width or height)"

	^ i-1*width+j! !

!Array2D methodsFor: 'testing' stamp: 'jmv 6/16/2013 20:11'!
isSquare

	^height = width! !


!Array2D class methodsFor: 'instance creation' stamp: 'jmv 6/16/2013 20:09'!
fromArrayOfArrays: anArray
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
	result _ self newHeight: anArray size width: anArray first size.
	result fillWithArrayOfArrays: anArray.
	^result
! !

!Array2D class methodsFor: 'instance creation' stamp: 'jmv 6/16/2013 20:07'!
newHeight: height width: width

	^self new initHeight: height width: width! !

!Array2D class methodsFor: 'instance creation' stamp: 'jmv 6/16/2013 20:12'!
newSize: aPoint

	^ self newHeight: aPoint y width: aPoint x! !


!DifferenceFinder methodsFor: 'private' stamp: 'jmv 6/16/2013 20:13'!
initializeMap
	map _ Array2D newHeight: x size width: y size! !

!DifferenceFinder methodsFor: 'private' stamp: 'jmv 6/16/2013 20:12'!
initializeMatrix
	matrix _ Array2D newHeight: x size width: y size! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 6/16/2013 20:13'!
computeMap
	| m |
	tally _ Array2D newHeight: x size width: y size.
	1 to: x size do: [ :i | 
		1 to: y size do: [ :j | 
			m _ self computeMapAt: i at: j.
			map i: i j: j put: m ]]! !

!methodRemoval: MatrixTransform2x3 #asMatrix!
MatrixTransform2x3 removeSelector: #asMatrix!

!MatrixTransform2x3 reorganize!
('converting coordinates' autoHint: externalizeDelta: externalizePosition: externalizeScalar: internalizeDelta: internalizePosition: internalizeScalar: inverseTransform: transform:)
('accessing' at: at:put: degrees inverseTransformation position radians scale translation)
('comparing' = hash)
('composing' composedWith: composedWith:into:)
('element access' a11 a11: a12 a12: a13 a13: a21 a21: a22 a22: a23 a23:)
('initialize' setIdentiy)
('objects from disk' byteSize bytesPerBasicElement restoreEndianness writeOn:)
('printing' matrixPrintString print printMatrixOn: printOn:)
('testing' doesNotRotate isIdentity isPureTranslation)
('private' setPosition: setRadians: setRadians:scale:position: setScale: setTranslation:)
('modifying' addOffset: rotateBy: scaleBy: scaleByNumber:rotateBy:)
('transforming rects' displayBoundsOfInverseTransformOf: displayBoundsOfInverseTransformOf:into: displayBoundsOfTransformOf: displayBoundsOfTransformOf:into: primDisplayBoundsOfInverseTransformOf:into: primDisplayBoundsOfTransformOf:into:)
('inspecting' customizeExplorerContents explorerContents inspectorClass)
('private - primitives' fixRoundingInPrimitives: floatInverseTransform: floatTransform: intInverseTransform: intTransform: primInverseTransform: primTransform:)
!

!classRemoval: #FloatBandMatrix!
Smalltalk removeClassNamed: #FloatBandMatrix!
!classRemoval: #FloatMatrix!
Smalltalk removeClassNamed: #FloatMatrix!
!classRemoval: #Matrix!
Smalltalk removeClassNamed: #Matrix!
