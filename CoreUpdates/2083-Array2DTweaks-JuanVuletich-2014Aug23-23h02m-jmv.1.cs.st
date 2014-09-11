'From Cuis 4.2 of 25 July 2013 [latest update: #2081] on 24 August 2014 at 12:19:08.944233 am'!

!Array2D commentStamp: '<historical>' prior: 0!
My instances are two dimensional arrays, providing basic accessors and some convenience methods. Specialized subclasses provide additional services. See packages LinearAlgebra and SignalProcessing.!


!Array2D class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:22'!
height: h width: w
	^self basicNew initHeight: h width: w! !

!Array2D class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:16'!
new
	^ self shouldNotImplement! !

!Array2D class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:17'!
width: w height: h
	^self basicNew initHeight: h width: w! !


!Array2D methodsFor: 'accessing' stamp: 'jmv 8/24/2014 00:16'!
x: x y: y
	"Answer element at column x, row y
	x and y are integers in [1 .. width] and [1 .. height]"

	^ self i: y j: x! !

!Array2D methodsFor: 'accessing' stamp: 'jmv 8/23/2014 23:52'!
x: x y: y put: aNumber
	"Set the value at coordinates x@y.
	x and y are integers in [1 .. width] and [1 .. height]"

	^ self i: y j: x put: aNumber! !

!Array2D methodsFor: 'initialization' stamp: 'jmv 8/23/2014 23:34'!
initHeight: h width: w

	height _ h.
	width _ w.
	self initializeElements! !


!Array2D class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:22'!
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
	result _ self height: anArray size width: anArray first size.
	result fillWithArrayOfArrays: anArray.
	^result
! !

!Array2D class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:23'!
newSize: aPoint

	^ self width: aPoint x height: aPoint y ! !


!DifferenceFinder methodsFor: 'private' stamp: 'jmv 8/23/2014 23:23'!
initializeMap
	map _ Array2D height: x size width: y size! !

!DifferenceFinder methodsFor: 'private' stamp: 'jmv 8/23/2014 23:23'!
initializeMatrix
	matrix _ Array2D height: x size width: y size! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 8/23/2014 23:23'!
computeMap
	| m |
	tally _ Array2D height: x size width: y size.
	1 to: x size do: [ :i | 
		1 to: y size do: [ :j | 
			m _ self computeMapAt: i at: j.
			map i: i j: j put: m ]]! !

!methodRemoval: Array2D class #newHeight:width:!
Array2D class removeSelector: #newHeight:width:!
