'From Cuis 4.2 of 25 July 2013 [latest update: #2963] on 1 November 2016 at 4:12:01 pm'!
'Description Please enter a description for this package '!
!provides: 'LinearAlgebra' 1 34!
!requires: 'Collections-CompactArrays' 1 2 nil!
!classDefinition: #Matrix category: #LinearAlgebra!
Array2D subclass: #Matrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LinearAlgebra'!
!classDefinition: 'Matrix class' category: #LinearAlgebra!
Matrix class
	instanceVariableNames: ''!

!classDefinition: #ByteMatrix category: #LinearAlgebra!
Matrix subclass: #ByteMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LinearAlgebra'!
!classDefinition: 'ByteMatrix class' category: #LinearAlgebra!
ByteMatrix class
	instanceVariableNames: ''!

!classDefinition: #FloatMatrix category: #LinearAlgebra!
Matrix subclass: #FloatMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LinearAlgebra'!
!classDefinition: 'FloatMatrix class' category: #LinearAlgebra!
FloatMatrix class
	instanceVariableNames: ''!

!classDefinition: #Float64Matrix category: #LinearAlgebra!
FloatMatrix subclass: #Float64Matrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LinearAlgebra'!
!classDefinition: 'Float64Matrix class' category: #LinearAlgebra!
Float64Matrix class
	instanceVariableNames: ''!

!classDefinition: #FloatBandMatrix category: #LinearAlgebra!
FloatMatrix subclass: #FloatBandMatrix
	instanceVariableNames: 'bandWidth'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LinearAlgebra'!
!classDefinition: 'FloatBandMatrix class' category: #LinearAlgebra!
FloatBandMatrix class
	instanceVariableNames: ''!

!classDefinition: #IntegerMatrix category: #LinearAlgebra!
Matrix subclass: #IntegerMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LinearAlgebra'!
!classDefinition: 'IntegerMatrix class' category: #LinearAlgebra!
IntegerMatrix class
	instanceVariableNames: ''!

!classDefinition: #ShortIntegerMatrix category: #LinearAlgebra!
Matrix subclass: #ShortIntegerMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LinearAlgebra'!
!classDefinition: 'ShortIntegerMatrix class' category: #LinearAlgebra!
ShortIntegerMatrix class
	instanceVariableNames: ''!

!classDefinition: #ShortWordMatrix category: #LinearAlgebra!
Matrix subclass: #ShortWordMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LinearAlgebra'!
!classDefinition: 'ShortWordMatrix class' category: #LinearAlgebra!
ShortWordMatrix class
	instanceVariableNames: ''!


!Matrix commentStamp: 'jmv 7/19/2012 11:52' prior: 0!
My instances are m x n matrices, where m is the height and n the width. A matrix is a kind of 2-d array. A Matrix whose width is 1 is called a "Column Vector". A Matrix whose height is 1 is called a "Row Vector".

My instances can hold elements of any kind, but include some methods that assume that the elements understand arithmetic operations. These are useful for Matrices whose elements are Fractions, LargeIntegers, Matrices and Vectors (column or row Matrices). They are also suitable for Complex numbers, but in such cases, an alternative is to use different FloatMatrices for Real and Imaginary parts.!

!ByteMatrix commentStamp: '<historical>' prior: 0!
Elements are integers between 0 and 255, i.e. bytes.

ByteMatrix fromArrayOfArrays: #(
	#[ 0 	255 	94   	]
	#[ 4 	127 	128 	]
	#[ 1 	254	3 		])!

!FloatMatrix commentStamp: '<historical>' prior: 0!
My instances are m x n matrices of Float values, where m is the height and n the width. Suitable for LinearAlgebra and SignalProcessing suff, among many other possible applications. It is possible to build VM plugins for fast processing.!

!FloatBandMatrix commentStamp: '<historical>' prior: 0!
My instances can have non-zero values only in the diagonal, and a band of certain width around it. Save space.!

!IntegerMatrix commentStamp: '<historical>' prior: 0!
Elements are integers between -16r80000000 and 16r7FFFFFFF, i.e. signed 32-bit words.

IntegerMatrix fromArrayOfArrays: {
	{ 0. 									1. 						-1 }.
	{ (2 raisedTo: 31) negated.	-10000.   				-16r80000000}.
	{ (2 raisedTo: 31) -1. 			12 factorial.			16r7FFFFFFF }}!

!ShortIntegerMatrix commentStamp: '<historical>' prior: 0!
Elements are integers between -32768 and 32767, i.e. signed 16-bit words.

ShortIntegerMatrix fromArrayOfArrays: #(
	#( 0 			255 		94)
	#(-4 			127 		128)
	#( -32768 	254		32767 ))!

!ShortWordMatrix commentStamp: '<historical>' prior: 0!
Elements are integers between 0 and 65535, i.e. unsigned 16-bit words.

ShortWordMatrix fromArrayOfArrays: #(
	#( 0 			255 		65500)
	#( 40000 		127 		128)
	#( 32768 		254		32767 ))!

!Form methodsFor: '*LinearAlgebra' stamp: 'jmv 5/21/2015 11:34'!
hackBitsForFloatMatrix: aFloatMatrix
	"Quite like #hackBits: but we know that FloatMatrixes use 32 bits per element"
	depth _ 32.
	width _ aFloatMatrix width.
	height _ aFloatMatrix height.
	bits _ aFloatMatrix elements! !

!AffineTransformation methodsFor: '*LinearAlgebra' stamp: 'jmv 8/23/2014 23:29'!
asMatrix
	"Answer a FloatMatrix."

	| answer |
	answer _ FloatMatrix height: 3 width: 3.
	answer i: 1 j: 1 put: self a11.
	answer i: 1 j: 2 put: self a12.
	answer i: 1 j: 3 put: self a13.
	answer i: 2 j: 1 put: self a21.
	answer i: 2 j: 2 put: self a22.
	answer i: 2 j: 3 put: self a23.
	answer i: 3 j: 3 put: 1.
	^answer! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 8/23/2014 23:27'!
* aMatrixOrNumber
	"Standard matrix multiplication"
	| result |
	(aMatrixOrNumber is: #Matrix) ifTrue: [
		width = aMatrixOrNumber height ifFalse: [ ^self error: 'Matrix sizes do not match' ].
		result _ self appropriateResultClass height: height width: aMatrixOrNumber width.
		result fillWith: self multipliedBy: aMatrixOrNumber ]
	ifFalse: [
		result _ self copy.
		result replaceValues: [ :i :j :value | value * aMatrixOrNumber ] ].
	^result
	! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 8/23/2014 23:27'!
+ aMatrixOrNumber
	"Add element by element"
	| result |
	(aMatrixOrNumber is: #Matrix) ifTrue: [
		(width = aMatrixOrNumber width) & (height = aMatrixOrNumber height)
			ifFalse: [ ^self error: 'Matrix sizes do not match' ].
		result _ self appropriateResultClass height: height width: width.
		result fillWith: self plus: aMatrixOrNumber ]
	ifFalse: [
		result _ self copy.
		result replaceValues: [ :i :j :value | value + aMatrixOrNumber ] ].
	^result
	! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 8/23/2014 23:27'!
- aMatrixOrNumber
	"Substract element by element"
	| result |
	(aMatrixOrNumber is: #Matrix) ifTrue: [
		(width = aMatrixOrNumber width) & (height = aMatrixOrNumber height)
			ifFalse: [ ^self error: 'Matrix sizes do not match' ].
		result _ self appropriateResultClass height: height width: width.
		result fillWith: self minus: aMatrixOrNumber ]
	ifFalse: [
		result _ self copy.
		result replaceValues: [ :i :j :value | value - aMatrixOrNumber ] ].
	^result
	! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 11/16/2012 12:15'!
/ aNumber
	"Divide by a scalar"
	| reciprocal result |
	reciprocal _ 1 / aNumber.
	result _ self copy.
	result replaceValues: [ :i :j :value | value * reciprocal ].
	^result
	! !

!Matrix methodsFor: 'misc' stamp: 'jmv 9/27/2007 23:35'!
appropriateResultClass
	"Some subclasses might need to redefine this method.
	For example to specify a more general class than themselves."

	^self class! !

!Matrix methodsFor: 'conversion' stamp: 'jmv 7/26/2016 11:29:44'!
as32bitFormEvaluating: aBlock
	"Fill 32bit Form with gray values."

	| answer bits word index |
	answer _ Form extent: width@height depth: 32.
	bits _ answer bits.
	word _ 16rFF000000 copy.
	index _ 1.
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: index value: word.
			bits at: index put: word.
			index _ index + 1 ]].
	^answer! !

!Matrix methodsFor: 'conversion' stamp: 'jmv 7/26/2016 11:30:05'!
as8bitFormEvaluating: aBlock
	"Fill an 8-bit ColorForm with gray values."

	| answer bits bitsIndex elementsIndex bytesPerRow |
	bytesPerRow _ width + 3 // 4 * 4.
	answer _ GrayForm extent: width@height.
	bits _ answer bits.
	elementsIndex _ 0.
	1 to: height do: [ :i |
		bitsIndex _ i-1 * bytesPerRow.
		1 to: width do: [ :j |
			elementsIndex _ elementsIndex + 1.
			bitsIndex _ bitsIndex + 1.
			bits byteAt: bitsIndex put: (aBlock value: elementsIndex) bigEndian: answer isBigEndian ]].
	^answer! !

!Matrix methodsFor: 'conversion' stamp: 'jmv 5/4/2016 11:18'!
asForm
	"Answer a Form with gray values.
	Map used range of pixel values to available gray values.
	Do not apply any gamma.
	
	Each subclass might do it differently:
	- mapping the used range of values or some arbitrary range
	- applying or not gamma correction (to make a linear range look linear to our eyes, using nonlinear mapping)"

	| max min |
	min _ self min.
	max _ self max.
	max > min ifFalse: [
		max _ min + 1 ].
	^self asFormMin: min max: max! !

!Matrix methodsFor: 'conversion' stamp: 'jmv 9/2/2014 12:07'!
asFormG: greenMatrix b: blueMatrix

	| min max |
	min _ (self min min: greenMatrix min) min: blueMatrix min.
	max _ (self max max: greenMatrix max) max: blueMatrix max.
	^self asFormG: greenMatrix b: blueMatrix min: min max: max! !

!Matrix methodsFor: 'conversion' stamp: 'jmv 4/28/2015 10:26'!
asFormG: greenMatrix b: blueMatrix min: min max: max
	| k r g b blueElements greenElements |
	k _ 255.0 / (max-min).
	greenElements _ greenMatrix elements.
	blueElements _ blueMatrix elements.
	^ self as32bitFormEvaluating: [ :index :word |
		r _ (((elements at: index) - min) * k + 0.5) truncated min: 255 max: 0.
		g _ (((greenElements at: index) - min) * k + 0.5) truncated min: 255 max: 0.
		b _ (((blueElements at: index) - min) * k + 0.5) truncated min: 255 max: 0.
		word at: 1 put: b; at: 2 put: g; at: 3 put: r ]! !

!Matrix methodsFor: 'conversion' stamp: 'jmv 5/4/2016 10:49'!
asFormMin: min max: max
	"Answer a 8bit Form with gray values.
	Map used range of pixel values to available gray values."

	| k |
	k _ 255.0 / (max-min).
	^ self as8bitFormEvaluating: [ :index |
		(((elements at: index) - min) * k + 0.5) truncated min: 255 max: 0 ]! !

!Matrix methodsFor: 'statistics' stamp: 'jmv 5/12/2015 11:56'!
average
	^self mean! !

!Matrix methodsFor: 'filling' stamp: 'jmv 7/26/2016 11:32:09'!
border: box with: value
	"box includes topLeft but doesn't include bottom right (as it is usual with Rectangles)
	Coordinates are 1-based"

	box top to: box bottom-1 do: [ :i |
		self i: i j: box left-1 put: value.
		self i: i j: box right-1+1 put: value ].
	box left to: box right-1 do: [ :j |
		self i: box top-1 j: j put: value.
		self i: box bottom-1+1 j: j put: value ]! !

!Matrix methodsFor: 'aux operations' stamp: 'jmv 7/19/2012 12:25'!
columnsWithoutPivot: count
	"Return a collection with the indexes of the columns that are closer to not having a pivot"

	| col col2 |
	col _ SortedCollection sortBlock: [ :a :b | a y < b y ].
	1 to: height do: [ :j |
		col2 _ OrderedCollection new.
		1 to: height do: [ :i |
			(j = 1 or: [(self i: i j: j-1) isZero]) ifTrue: [
				col2 add: (self i: i j: j) abs ] ].
		col add: (j @ col2 max) ].
	^((col copyFrom: 1 to: count) collect: [ :each | each x ] ) asSortedCollection! !

!Matrix methodsFor: 'operations' stamp: 'jmv 7/19/2012 11:20'!
determinant

	| aux rowPermutationCount result |
	self isSquare ifFalse: [ ^self error: 'Only for Square Matrices' ].
	aux _ self copy.
	rowPermutationCount _ aux triangleWithPartialPivoting.
	result _ -1 raisedToInteger: rowPermutationCount.
	1 to: width do: [ :i |
		result _ result * (aux i: i j: i) ].
	^result! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:18'!
diagonal

	| x |
	x _ self appropriateResultClass newVectorSize: height.
	1 to: height do: [ :i |
		x i: i j: 1 put: (self i: i j: i ) ].
	^x! !

!Matrix methodsFor: 'displaying' stamp: 'jmv 5/4/2016 10:55'!
display
	self displayAt: 0@0! !

!Matrix methodsFor: 'displaying' stamp: 'jmv 5/4/2016 11:16'!
displayAt: aPoint
	"Display receiver, using default conversion to Form.
	Each subclass might do it differently:
	- mapping the used range of values or some arbitrary range
	- applying or not gamma correction (to make a linear range look linear to our eyes, using nonlinear mapping)"

	self asForm displayAt: aPoint! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 10:25'!
elements
	^ elements! !

!Matrix methodsFor: 'private' stamp: 'jmv 7/19/2012 12:35'!
epsilon
	"Something better is needed. For example, taking into accoung the largest elements, or the norm, or something like that... In any case it is better to check senders, and use a different strategy for ending iterative methods or such...
	Check inheritance too"

	^self zeroElement! !

!Matrix methodsFor: 'filling' stamp: 'jmv 7/26/2016 11:32:38'!
fill: box with: value
	"box includes topLeft but doesn't include bottom right (as it is usual with Rectangles)
	Coordinates are 1-based"

	box top to: box bottom-1 do: [ :i |
		box left to: box right-1 do: [ :j |
			self i: i j: j put: value ]]! !

!Matrix methodsFor: 'filling' stamp: 'jmv 7/19/2012 11:21'!
fillDiagonalWith: aVectorOrArray

	1 to: (width min: height) do: [ :j |
		self i: j j: j put: (aVectorOrArray at: j) ].! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 17:41'!
fillEndsWith1Derivative
	"Fill one row and one column at each end with such values that the derivative is that at the ends of the inner submatrix.
	If the size is less than 4 in any direction, meaning we don't have enough elements to compute the derivatives, do nothing.
	Note: We could make derivatives zero, meaning to copy the values."

	| m n third second first antepenultimate penultimate last |
	m _ self m. n _ self n.
	n >= 4 ifTrue: [
		2 to: m-1 do: [ :i |
			second _ self i: i j: 2. third _ self i: i j: 3.
			first _ second * 2-third.
			self i: i j: 1 put: first.
			antepenultimate _ self i: i j: n-2.
			penultimate _ self i: i j: n-1.
			last  _ penultimate * 2 - antepenultimate.
			self i: i j: n put: last ]].
	m >= 4 ifTrue: [
		1 to: n do: [ :j |
			second _ self i: 2 j: j. third _ self i: 3 j: j.
			first _ second * 2-third.
			self i: 1 j: j put: first.
			antepenultimate _ self i: m-2 j: j.
			penultimate _ self i: m-1 j: j.
			last  _ penultimate * 2 - antepenultimate.
			self i: m j: j put: last ]]! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 17:42'!
fillEndsWith2Derivative2
	"Fill two rows and two columns at each end with such values that the second derivative is that at the ends of the inner submatrix.
	If the size is less than 7 in any direction, meaning we don't have enough elements to compute the derivatives, do nothing.
	Note: We could make derivatives zero, meaning to copy the values."

	| m n third fourth fifth second first fourthToEnd thirdToEnd antepenultimate penultimate last |
	m _ self m. n _ self n.
	n >= 7 ifTrue: [
		3 to: m-2 do: [ :i |
			third _ self i: i j: 3. fourth _ self i: i j: 4. fifth _ self i: i j: 5.
			second _ (third-fourth) * 3.0 + fifth.
			first _ (second-third) * 3.0 + fourth.
			self i: i j: 2 put: second.
			self i: i j: 1 put: first.
			fourthToEnd _ self i: i j: n-4.
			thirdToEnd _ self i: i j: n-3.
			antepenultimate _ self i: i j: n-2.
			penultimate _ (antepenultimate - thirdToEnd) * 3.0 + fourthToEnd.
			last _ (penultimate -antepenultimate) * 3.0 + thirdToEnd.
			self i: i j: n-1 put: penultimate.
			self i: i j: n put: last ]].
	m >= 7 ifTrue: [
		1 to: n do: [ :j |
			third _ self i: 3 j: j. fourth _ self i: 4 j: j. fifth _ self i: 5 j: j.
			second _ (third-fourth) * 3.0 + fifth.
			first _ (second-third) * 3.0 + fourth.
			self i: 2 j: j put: second.
			self i: 1 j: j put: first.
			fourthToEnd _ self i: m-4 j: j.
			thirdToEnd _ self i: m-3 j: j.
			antepenultimate _ self i: m-2 j: j.
			penultimate _ (antepenultimate - thirdToEnd) * 3.0 + fourthToEnd.
			last _ (penultimate -antepenultimate) * 3.0 + thirdToEnd.
			self i: m-1 j: j put: penultimate.
			self i: m j: j put: last ]]! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 7/19/2012 11:18'!
fillWith: aMatrix minus: bMatrix

	| elem |
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			elem _ ((aMatrix i: i j: j) - (bMatrix i: i j: j)).
			self i: i j: j put: elem ] ]! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 7/19/2012 12:36'!
fillWith: aMatrix multipliedBy: bMatrix

	| zeroElement elem |
	zeroElement _ self zeroElement.
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			elem _ zeroElement.
			1 to: aMatrix width do: [ :k |
				elem _ ((aMatrix i: i j: k) * (bMatrix i: k j: j)) + elem ].
			self i: i j: j put: elem ] ]! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 7/19/2012 11:18'!
fillWith: aMatrix plus: bMatrix

	| elem |
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			elem _ ((aMatrix i: i j: j) + (bMatrix i: i j: j)).
			self i: i j: j put: elem ] ]! !

!Matrix methodsFor: 'filling' stamp: 'jmv 9/28/2007 19:30'!
fillWithIdentity

	1 to: self width do: [ :i |
		self i: i j: i put: 1.
		i+1 to: self width do: [ :j |
			self i: i j: j put: 0.
			self i: j j: i put: 0 ] ].! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:22'!
i: i
	"Answers a row vector with the values at row y"

	| result |
	result _ self appropriateResultClass newRowVectorSize: width.
	1 to: width do: [ :j |
		result i: 1 j: j put: (self i: i j: j) ].
	^result! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:22'!
i: i put: row
	"aVector must be a row vector, i.e. a matrix with height 1"

	1 to: width do: [ :j |
		self i: i j: j put: (row at: j) ]! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 9/30/2007 09:31'!
i: i upTo: k
	"Answers a row vector with the values at row y, but only up to index w"

	| result |
	result _ self appropriateResultClass newRowVectorSize: k.
	1 to: k do: [ :j |
		result i: 1 j: j put: (self i: i j: j) ].
	^result! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:22'!
iExtended: i
	"Answers a row vector with the values at row y,
	but with 2 extra values at each end, such that the second derivative is that at the original ends"

	| result |
	result _ self appropriateResultClass newRowVectorSize: width + 4.
	1 to: width do: [ :j |
		result i: 1 j: j+2 put: (self i: i j: j) ].
	"The first available second derivative is
	(s5-s4)-(s4-s3)"
	"
Tomar esto para lo que realmente hace falta. Ver el otro browser
	second _ (third-fourth) * 3.0 + fifth.
	first _ (second-third) * 3.0 + fourth.
	penultimate _ (antepenultimate - thirdToEnd) * 3.0 + fourthToEnd.
	last _ (penultimate -antepenultimate) * 3.0 + thirdToEnd.
	"
	^result! !

!Matrix methodsFor: 'operations' stamp: 'jmv 8/23/2014 23:27'!
inverse
	"Answers the inverse matrix"

	| zeroElement bigMatrix result |
	zeroElement _ self zeroElement.
	self isSquare ifFalse: [ ^self error: 'Only for Square Matrices' ].
	result _ self appropriateResultClass height: height width: width.
	bigMatrix _ self appropriateResultClass height: height width: width + 1.
	1 to: width do: [ :i |
		bigMatrix fillWith: self.
		1 to: width do: [ :j | bigMatrix i: j j: width+1 put: zeroElement ].
		bigMatrix i: i j: width + 1 put: 1.
		result j: i put: bigMatrix solveLinearSystem ].
	^result! !

!Matrix methodsFor: 'testing' stamp: 'jmv 12/2/2010 08:38'!
is: aSymbol
	^ aSymbol == #Matrix or: [ super is: aSymbol ]! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:19'!
j: j
	"Answers a column vector with the values at column x"

	| result |
	result _ self appropriateResultClass newColumnVectorSize: height.
	1 to: height do: [ :i |
		result i: i j: 1 put: (self i: i j: j) ].
	^result! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 10/3/2007 06:19'!
j: j i: i
	"Convenience accessor"
	^self i: i j: j! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:19'!
j: j put: column
	"column can be a column vector (m x 1 matrix), or a kind of Array"

	1 to: height do: [ :i |
		self i: i j: j put: (column at: i) ]! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:27'!
m
	"Usual name in Linear Algebra"

	^height! !

!Matrix methodsFor: 'statistics' stamp: 'jmv 5/12/2015 11:52'!
max
	^ elements max! !

!Matrix methodsFor: 'statistics' stamp: 'jmv 5/12/2015 11:55'!
mean
	^elements mean! !

!Matrix methodsFor: 'statistics' stamp: 'jmv 5/12/2015 11:52'!
min
	^ elements min! !

!Matrix methodsFor: 'operations' stamp: 'jmv 7/19/2012 12:21'!
moveOriginToCenter: aBoolean
	"This method performs a circular shift of the both in horizontal and vertical direction. The magnitude of the shift is half our extent.
	This is useful, for example, to make a better visualization of convolution kernels:
	
	To be properly applied, the kernel is centered at 1@1 . For instance a simple low pass filter could be
	| 0.6   0.1   0.0   0.1 |
	| 0.1   0.0   0.0   0.0 |
	| 0.0   0.0   0.0   0.0 |
	| 0.1   0.0   0.0   0.0 |
	
	But it is much easier to 'see' it if the center of the filter is moved to the center of the matrix like this:
	| 0.0   0.0   0.0   0.0 |
	| 0.0   0.0   0.1   0.0 |
	| 0.0   0.1   0.6   0.1 |
	| 0.0   0.0   0.1   0.0 |
	
	This method takes the receiver and answers a new instance, transforming it from 1@1 to center if aBoolean is true, or backwards if false.
	(if the extent is even, then aBoolean makes no difference).
	
	((FloatMatrix fromArrayOfArrays: #(
		(0.6   0.1   0.0   0.1)
		(0.1   0.0   0.0   0.0)
		(0.0   0.0   0.0   0.0)
		(0.1   0.0   0.0   0.0)))
			moveOriginToCenter: true)
			print

	((Matrix fromArrayOfArrays: #(
		(0.6   0.1   0.0   0.0   0.1)
		(0.1   0.0   0.0   0.0   0.0)
		(0.0   0.0   0.0   0.0   0.0)
		(0.0   0.0   0.0   0.0   0.0)
		(0.1   0.0   0.0   0.0   0.0)))
			moveOriginToCenter: true)
			print
	"
	| answer deltaI deltaJ e ex ey |
	e _ self size.
	ey _ e y.
	ex _ e x.
	deltaI _ ey //2.
	deltaJ _ ex // 2.
	aBoolean ifFalse: [
		deltaI _ deltaI negated.
		deltaJ _ deltaJ negated ].
	answer _ self class newSize: e.
	0 to: ey-1 do: [ :i |
		0 to: ex-1 do: [ :j |
			answer i: i+deltaI\\ey+1 j: j+deltaJ\\ex+1 put: (self i: i+1 j: j+1) ]].
	^answer! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:27'!
n
	"Usual name in Linear Algebra"

	^width! !

!Matrix methodsFor: 'operations' stamp: 'jmv 7/19/2012 12:37'!
normalizeColumns

	| zeroElement normSquared |
	zeroElement _ self zeroElement.
	1 to: width do: [ :j |
		normSquared _ zeroElement.
		1 to: height do: [ :i |
			normSquared _(self i: i j: j) squared + normSquared ].
		1 to: height do: [ :i |
			self i: i j: j put: (self i: i j: j) / normSquared sqrt ] ]! !

!Matrix methodsFor: 'operations' stamp: 'jmv 10/7/2016 15:35:55'!
normalizeRows

	| zeroElement normSquared |
	zeroElement _ self zeroElement.
	1 to: height do: [ :i |
		normSquared _ zeroElement.
		1 to: width do: [ :j |
			normSquared _(self i: i j: j) squared + normSquared ].
		1 to: width do: [ :j |
			self i: i j: j put: (self i: i j: j) / normSquared sqrt ] ]! !

!Matrix methodsFor: 'operations' stamp: 'jmv 7/19/2012 12:17'!
permuteRow: i and: k

	| a b |
	i = k ifFalse: [
		1 to: width do: [ :j |
			a _ self i: k j: j.
			b _ self i: i j: j.
			self i: k j: j put: b.
			self i: i j: j put: a ] ]! !

!Matrix methodsFor: 'aux operations' stamp: 'jmv 7/19/2012 12:04'!
rowWithMaxInColumn: j startingAtRow: jStart

	| rowWithMax |
	rowWithMax _ jStart.
	jStart+1 to: height do: [ :i |
		(self i: i j: j) abs > (self i: rowWithMax j: j) abs ifTrue: [
			rowWithMax _ i ] ].
	^rowWithMax! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/4/2001 11:38'!
scalarSize
	"Specially useful for Vectors, but defined for matrices too."

	^height * width! !

!Matrix methodsFor: 'linear equation systems' stamp: 'jmv 7/19/2012 13:07'!
solveLinearSystem
	| m n x sum |
	"Solve the sistem A x = b where the receiver has this form | Ab |,
	the last column of the receiver contains the independent term b.
	(FloatMatrix fromArrayOfArrays: #(
		#(1 2 0 0 0 0 3) 
		#(2 4 1 0 0 -4 3) 
		#(0 1 1 1 0 0 3)
		#(0 0 1 1 1 0 3)
		#(0 0 0 1 1 1 3)
		#(0 1 0 0 1 1 3)
	)) solveLinearSystem
	(Matrix fromArrayOfArrays: {
		{1. 2. 0. 0. 0. 0. 3/2}.
		{2. 4. 1. 0. 0. -4. 3/2}. 
		{0. 1. 1. 1. 0. 0. 3/2}.
		{0. 0. 1. 1. 1. 0. 3/2}.
		{0. 0. 0. 1. 1. 1. 3/2}.
		{0. 1. 0. 0. 1. 1. 3/2}
	}) solveLinearSystem
	"

	m _ height.
	n _ width.
	x _ self appropriateResultClass newVectorSize: n-1.

	"Check we have enough equations"
	m < (n-1) ifTrue: [
		^self error: 'This system does not have a single solution' ].

	self triangleWithPartialPivoting.

	"Checks"
	"If the only coeficient of the last equation is zero, we have infinite solutions."
	((self i: n-1 j: n-1) abs <= self epsilon) & ((self i: n-1 j: n) abs <= self epsilon) ifTrue: [
		^self error: 'This system does not have a single solution' ].
	"If the only coeficient of the last equation is zero, but not the independent term,
	the system is incompatible."
	(self i: n-1 j: n-1) abs <= self epsilon ifTrue: [
		^self error: 'This system is incompatible (it does not have solution)' ].
	"We have too much equations, and they did not go away. Incompatible system."
	(m > (n-1) and: [ (self i: n j: n-1) abs > self epsilon ]) ifTrue: [
		^self error: 'This system does is incompatible (it does not have solution)' ].
	"We have too much equations, and they left their independent terms."
	((n to: m) inject: true into: [ :previousValue :k |
		previousValue & ((self i: k j: n) abs <= self epsilon ) ]) ifFalse: [
			^self error: 'This system does is incompatible (it does not have solution)' ].

	"Do backward substitution"
	n-1 to: 1 by: -1 do: [ :i |
		sum _ self i: i j: n.
		n-1 to: i+1 by: -1 do: [ :k |
			sum _ sum - ((self i: i j: k) * (x i: k j: 1)) ].
		x i: i j: 1 put: sum / (self i: i j: i) ].
	^x! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 9/30/2007 09:30'!
subMatrixTopLeft: aPoint bottomRight: bPoint

	| result |
	result _ self appropriateResultClass newSize: bPoint - aPoint + (1@1).
	1 to: result height do: [ :i |
		1 to: result width do: [ :j |
			result i: i j: j put:
				(self at: (j@i)+aPoint-(1@1)) ] ].
	^result! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/12/2011 17:49'!
subMatrixTopLeft: aPoint size: sizePoint
	^ self subMatrixTopLeft: aPoint bottomRight: aPoint + sizePoint - 1! !

!Matrix methodsFor: 'aux operations' stamp: 'jmv 10/28/2016 18:03:15'!
substractRow: i multipliedBy: factor to: k startingAtColumn: jStart

	self i: k j: jStart put: self zeroElement.
	jStart+1 to: width do: [ :j |
		self i: k j: j put:
			(self i: k j: j) - ((self i: i j: j) * factor) ]! !

!Matrix methodsFor: 'statistics' stamp: 'jmv 5/12/2015 11:56'!
sum
	^elements sum! !

!Matrix methodsFor: 'operations' stamp: 'jmv 10/6/2016 15:35:41'!
trace

	| trace |
	trace _ self zeroElement.
	1 to: height do: [ :i |
		trace _ trace + (self i: i j: i ) ].
	^trace! !

!Matrix methodsFor: 'operations' stamp: 'jmv 7/19/2012 12:17'!
transpose
	"Traspose the receiver. Modify it."

	| aux |
	1 to: width do: [ :ii |
		ii+1 to: width do: [ :jj |
			aux _ self i: ii j: jj.
			self i: ii j: jj put: (self i: jj j: ii).
			self i: jj j: ii put: (aux) ] ].! !

!Matrix methodsFor: 'operations' stamp: 'jmv 8/23/2014 23:27'!
transposed
	"Answer a new matrix, transposed."

	| result |
	result _ self appropriateResultClass height: width width: height.
	1 to: result height do: [ :i |
		1 to: result width do: [ :j |
			result i: i j: j put: (self i: j j: i) ] ].
	^result! !

!Matrix methodsFor: 'linear equation systems' stamp: 'jmv 7/19/2012 12:26'!
triangleWithPartialPivoting
	"Triangle self, and answer the row permutation count"

	| j jdelta k factor permutationCount |
	permutationCount _ 0.
	jdelta _ 0.
	1 to: height-1 do: [ :i |

		[
			j _ i + jdelta.
			j <= width ifFalse: [ ^self ].
 			"Look for the pivot for column j, from row i down. Call it row k"
			k _ self rowWithMaxInColumn: j startingAtRow: i.
			((self i: k j: j) isZero) & (j < width) ] whileTrue: [ jdelta _ jdelta + 1 ].

		"Permute rows k and i"
		i = k ifFalse: [
			self permuteRow: i and: k.
			permutationCount _ permutationCount + 1 ].

      	"Substract the row i to all the ones below it"
		i+1 to: height do: [ :ii |
			(self i: ii j: j) isZero ifFalse: [
				factor _ (self i: ii j: j) / (self i: i j: j).
				"Only after column j"
				self substractRow: i multipliedBy: factor to: ii startingAtColumn: j ] ]
	].
	^ permutationCount! !

!Matrix methodsFor: 'private' stamp: 'jmv 7/19/2012 12:35'!
zeroElement
	"We don't really restrict the kind of objects we might hold... Our best guess for a kind of null additive value is to just ask some element..."

	^elements first class zero! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:27'!
columnFrom: anArray	
"Example:
	FloatMatrix columnFrom: #(1 2 0 0 0 0 3)."

	| result |
	result _ self height: anArray size width: 1.
	1 to: result height do: [ :i |
		result i: i j: 1 put: (anArray at: i) ].
	^result! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 5/3/2016 17:55'!
from32BitColorForm: aForm component: aSymbol
	"aSymbol must be in #(red green blue alpha)"

	| instance |
	instance _ self width: aForm width height: aForm height.
	instance fillFrom32BitForm: aForm component: aSymbol.
	^instance! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 6/2/2016 20:18'!
fromGrayForm: aGrayForm
	"aGrayForm can be an instance of GrayForm or a ColorForm with a colormap with 256 gray levels, going from black to white.
	This is what we get, for instance, when reading a gray 8 bit bmp file"

	| instance |
	instance _ self width: aGrayForm width height: aGrayForm height.
	instance fillFromGrayForm: aGrayForm.
	^instance! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 11/16/2014 19:56'!
hilbert: n
"
(Matrix hilbert: 15) inverse inverse
(FloatMatrix hilbert: 15) inverse inverse
"
	| result |
	result _ self height: n width: n.
	1 to: n do: [ :i |
		1 to: n do: [ :j |
			result i: j j: i put: 1/(i+j+1) ] ].
	^result! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:28'!
identity: size

	| result |
	result _ self height: size width: size.
	result fillWithIdentity.
	^result
! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 6/2/2016 20:34'!
lena
	"
	(Base64MimeConverter mimeEncode: ((FileStream readOnlyFileNamed: 'Lena.png') binary)) upToEnd
Form lena display.
FloatImage lena display.
ByteMatrix lena display.
	"
	| form |
	form _ Form lena.
	^self fromGrayForm: form! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 9/2/2014 10:10'!
m: height n: width

	^self basicNew initHeight: height width: width! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:28'!
newColumnVectorSize: size
	"A column vector is just a matrix with width 1"

	^self height: size width: 1! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:28'!
newIdentity: size

	| result |
	result _ self height: size width: size.
	result fillWithIdentity.
	^result
! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:28'!
newRowVectorSize: size
	"A row vector is just a matrix with height 1"

	^self height: 1 width: size! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 7/3/2001 19:41'!
newVectorSize: size
	"Vectors are by default Column Vectors"

	^self newColumnVectorSize: size! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:28'!
rowFrom: anArray	
"Example:
	FloatMatrix rowFrom: #(1 2 0 0 0 0 3)."

	| result |
	result _ self height: 1 width: anArray size.
	1 to: result width do: [ :j |
		result i: 1 j: j put: (anArray at: j) ].
	^result! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 10/7/2016 15:00:01'!
withColumns: aCollectionOfColumns
	"Example:
	FloatMatrix withColumns: #(
		#(1
			2
			3)
		#( 4
			5
			6)
		#( 7
			8
			9)
	).
	"
	| width height result |
	width _ aCollectionOfColumns size.
	height _ aCollectionOfColumns first size.
	result _ self width: width height: height.
	1 to: width do: [ :j |
		1 to: height do: [ :i |
			result i: i j: j put: ((aCollectionOfColumns at: j) at: i) ] ].
	^result! !

!Matrix class methodsFor: 'instance creation' stamp: 'jmv 10/7/2016 14:59:14'!
withRows: aCollectionOfRows
	"Example:
	FloatMatrix withRows: #(
		#(1 2 0 0 0 0 3) 
		#(2 4 1 0 0 -4 3) 
		#(0 1 1 1 0 0 3)
		#(0 0 1 1 1 0 3)
		#(0 0 0 1 1 1 3)
		#(0 1 0 0 1 1 3)
	).
	"
	^self fromArrayOfArrays: aCollectionOfRows! !

!ByteMatrix methodsFor: 'conversion' stamp: 'jmv 5/4/2016 10:47'!
asForm
	"Our pixel values are already constrained to 0..255.
	Converting to an 8bit gray Form is straightforward."

	^ self as8bitFormEvaluating: [ :index | elements at: index ]! !

!ByteMatrix methodsFor: 'conversion' stamp: 'jmv 5/12/2016 18:30'!
fillFrom32BitForm: aForm component: aSymbol
	"aSymbol must be in #(red green blue alpha) or nil"

	| hack map componentIndex mask shift |
	hack _ GrayForm extent: width@height bits: elements.
	map _ aSymbol ifNotNil: [
		componentIndex _ #(alpha red green blue) indexOf: aSymbol.
		shift _ #(-24 -16 -8 0) at: componentIndex.
		mask _ #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
		ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 } ].
	(BitBlt toForm: hack)
		sourceForm: aForm;
		combinationRule: Form over;
		colorMap: map;
		sourceRect: (0@0 extent: width@height);
		destOrigin: 0@0;
		copyBits! !

!ByteMatrix methodsFor: 'conversion' stamp: 'jmv 6/2/2016 20:17'!
fillFromGrayForm: aGrayForm
	"aGrayForm can be an instance of GrayForm or a ColorForm with a colormap with 256 gray levels, going from black to white.
	This is what we get, for instance, when reading a gray 8 bit bmp file"

	| hack |
	hack _ GrayForm extent: width@height bits: elements.
	(BitBlt toForm: hack)
		sourceForm: aGrayForm;
		combinationRule: Form over;
		colorMap: nil;
		sourceRect: (0@0 extent: width@height);
		destOrigin: 0@0;
		copyBits! !

!ByteMatrix methodsFor: 'initialization' stamp: 'jmv 5/18/2015 14:27'!
initializeElements

	elements _ ByteArray new: height * width! !

!FloatMatrix methodsFor: 'conversion' stamp: 'jmv 5/4/2016 11:07'!
asGammaFormFullRange
	"See comment at #fullRangeDisplayAt:"

	| min scale |
	min _ elements min.
	scale _ elements max-min.
	scale _ scale > 0.0
		ifTrue: [ 1.0 / scale ]
		ifFalse: [ 1.0 ].
	^self as8bitFormEvaluating: [ :index | | value |
		value _ Color linearTosRGBGamma: (elements at: index)-min * scale.
		(value * 255 + 0.5) truncated ]! !

!FloatMatrix methodsFor: 'conversion' stamp: 'jmv 5/4/2016 11:09'!
asGammaFormFullRangeG: greenImage b: blueImage
	"Assume all three images are exactly the same size.
	See comment at #fullRangeDisplayAt:"

	| answer bits rElements rMin rScale gElements gMin gScale bElements bMin bScale b g r word |
	answer _ Form extent: width@height depth: 32.
	bits _ answer bits.
	rElements _ self elements.
	rMin _ rElements min.
	rScale _ rElements max - rMin. rScale _ rScale > 0.0 ifTrue: [ 1.0 / rScale ] ifFalse: [ 0.0 ].
	gElements _ greenImage elements.
	gMin _ gElements min.
	gScale _ gElements max - gMin. gScale _ gScale > 0.0 ifTrue: [ 1.0 / gScale ] ifFalse: [ 0.0 ].
	gScale print.
	bElements _ blueImage elements.
	bMin _ bElements min.
	bScale _ bElements max - bMin. bScale _ bScale > 0.0 ifTrue: [ 1.0 / bScale ] ifFalse: [ 0.0 ].
	1 to: elements size do: [ :i |
		r _ ((Color linearTosRGBGamma: (rElements at: i)-rMin * rScale) * 255 + 0.5) truncated.
		g _ ((Color linearTosRGBGamma: (gElements at: i)-gMin * gScale) * 255 + 0.5) truncated.
		b _ ((Color linearTosRGBGamma: (bElements at: i)-bMin * bScale) * 255 + 0.5) truncated.
		word _ (r bitShift: 16) + (g bitShift: 8) + b.
		bits integerAt: i put: word ].
	^answer! !

!FloatMatrix methodsFor: 'norm and condition number' stamp: 'jmv 7/19/2012 10:57'!
conditionNumber
	^self norm * self inverse norm! !

!FloatMatrix methodsFor: 'copying' stamp: 'jmv 5/22/2015 12:21'!
copy: aRectangle
	"
	| i |
	i _ FloatMatrix width: 10 height: 10.
	i x: 10 y: 10 put: Float pi.
	i copy: (1@1 extent: 10@10)
	"
	| answer |
	answer _ self appropriateResultClass width: aRectangle width height: aRectangle height.
	self copyToTile: answer position: aRectangle origin.
	^answer! !

!FloatMatrix methodsFor: 'copying' stamp: 'jmv 5/22/2015 13:24'!
copyFromTile: aFloatMatrix position: aPoint
	"
	aPoint is the position in self where the contents of aFloatMatrix are to be stored
	As in all Matrix protocol, aPoint is 1-based
	"
	| blitter formArgument formSelf |
	formSelf _ Form new hackBitsForFloatMatrix: self.
	formArgument _ Form new hackBitsForFloatMatrix: aFloatMatrix.
	blitter _ BitBlt new.
	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: aPoint-1 sourceOrigin: 0@0.
	blitter copyBits! !

!FloatMatrix methodsFor: 'copying' stamp: 'jmv 5/22/2015 13:25'!
copyFromTile: aFloatMatrix position: aPoint guardZone: borderWidth
	"
	aPoint is the position in self where the contents of aFloatMatrix are to be stored
	As in all Matrix protocol, aPoint is 1-based
	Exclude a border of width borderWidth from the copy

	| i t |
	t _ FloatImage from32BitColorForm: (Display copy: (100@100 extent: 100@100)) component: #red.
	i _ FloatImage extent: 256@256.
	i copyFromTile: t position: 10@10 guardZone: 10.
	i display.
	"

	| blitter formArgument formSelf |
	formSelf _ Form new hackBitsForFloatMatrix: self.
	formArgument _ Form new hackBitsForFloatMatrix: aFloatMatrix.
	blitter _ BitBlt new.
	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: aPoint-1 + borderWidth sourceOrigin: borderWidth@borderWidth
		extent: (formArgument width - (2*borderWidth))@(formArgument height - (2*borderWidth)).
	blitter copyBits! !

!FloatMatrix methodsFor: 'copying' stamp: 'jmv 5/22/2015 13:24'!
copyToTile: aFloatMatrix position: aPoint
	"
	aPoint is the position in self where the contents of aFloatMatrix are to be taken from
	As in all Matrix protocol, aPoint is 1-based

	| i t |
	i _ FloatImage from32BitColorForm: (Display copy: (1@1 extent: 100@100)) component: #red.
	t _ FloatImage extent: 256@256.
	i copyToTile: t position: -10@0.
	t display.
	"
	| blitter formArgument formSelf |
	formSelf _ Form new hackBitsForFloatMatrix: self.
	formArgument _ Form new hackBitsForFloatMatrix: aFloatMatrix.
	blitter _ BitBlt new.
	blitter
		setDestForm: formArgument sourceForm: formSelf
		destOrigin: 0@0 sourceOrigin: aPoint-1.
	blitter copyBits! !

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 10/21/2016 15:56:49'!
eigenvalues
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(6 4 4 1) 
		#(4 6 1 4) 
		#(4 1 6 4)
		#(1 4 4 6)
	)) eigenvalues
	"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(1 2 3 4) 
		#(5 6 7 8) 
		#(9 10 11 12)
		#(13 14 15 16)
	)) eigenvalues
	"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(6 4 4 1) 
		#(4 6 1 4) 
		#(4 1 6 4)
		#(1 4 4 6)
	)) eigenvalues 
	"

	^[ self eigenvaluesByQR ] on: Error do: [ self eigenvaluesByJacobi ]! !

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 10/7/2016 15:31:23'!
eigenvaluesAndEigenvectors
	"Answers a transposed vector (a row matrix) of eigenvalues and a matrix of eigenvectors.
	Each eigenvector is a column of the eigenvector matrix. Column k is the eigenvector for the
	eigenvalue at column k in the eigenvalue vector.
	The returned object is a Dictionary, with keys #eigenvalues and #eigenvectors.
	This seems to be a bad way. Numerical Recipes in C has a couple of algorithms that should be studied!!"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(6 4 4 1) 
		#(4 6 1 4) 
		#(4 1 6 4)
		#(1 4 4 6)
	)) eigenvaluesAndEigenvectors
	"

	| result eigenvalues eigenvectorSystem eigenvaluesVector resultIndex resultEigenvalues resultEigenvectors rows columns |
	result _ Dictionary new.
	eigenvaluesVector _ self eigenvalues.
	eigenvaluesVector round.	"This is necesary to consider repeated eigenvalues this way, and not as different ones (that would lead to repeated eigenvectors)"
	eigenvalues _ Bag new.
	1 to: eigenvaluesVector height do: [ :j |
		eigenvalues add: (eigenvaluesVector i: j j: 1) ].
	eigenvectorSystem _ self appropriateResultClass height: height width: width+1.
	resultIndex _ 1.
	resultEigenvalues _ self appropriateResultClass newRowVectorSize: width.
	resultEigenvectors _ self appropriateResultClass height: height width: width.
	eigenvalues asSet do: [ :eigenvalue |
		"For each occurrence of an eigenvalue find a different eigenvector.
		This part could be optimized. The correct way would be to implement a method
		that finds a base of the row space. In single eigenvectors it would be a space 
		of dimension one. In eigenvalues with k occurrences, it would be a space of
		dimension k. This is a less elegant solution."
		1 to: (eigenvalues occurrencesOf: eigenvalue) do: [ :iteration |
			eigenvectorSystem fillWith: self.
			1 to: height do: [ :j |
				eigenvectorSystem i: j j: j put:
					(eigenvectorSystem i: j j: j) - eigenvalue.
				eigenvectorSystem i: j j: width+1 put: 0 ].
			eigenvectorSystem triangleWithPartialPivoting.
"This is WRONG. the iteration doesn't make sense"
			"This second phase completes the linear system so it will have just one solution.
			The added equations are changed for each occurrence of the eigenvalue, to get
			a different eigenvector."
			rows _ eigenvectorSystem rowsCloserToZero: (eigenvalues occurrencesOf: eigenvalue).
			columns _ eigenvectorSystem columnsWithoutPivot: (eigenvalues occurrencesOf: eigenvalue).
			1 to: rows size do: [ :j |
				eigenvectorSystem i: (rows at: j) j: (columns at: j) put: 1.
				iteration = j ifTrue: [
					eigenvectorSystem i: (rows at: j) j: width+1 put: 1 ] ].

			resultEigenvalues i: 1 j: resultIndex put: eigenvalue.
			resultEigenvectors j: resultIndex put: eigenvectorSystem solveLinearSystem.
			resultIndex _ resultIndex + 1 ] ].
	result at: #eigenvalues put: resultEigenvalues.
	result at: #eigenvectors put: resultEigenvectors.
	^result! !

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 10/7/2016 15:17:18'!
eigenvaluesByJacobi
"Needs rework. Optimization. Cleanup"
"la traza, suma de la diagonal es tambien la suma de los autovalores...
puedo tirar bocha de vectores random para estimar radio espectral (mayor autovalor...). incluso ir girando para ver adonde aumenta la norma del resultado, etc"
	"This implementation only works if all the eigenvalues are real."
"SOLO para matrices SIMETRICAS"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(6 4 4 1) 
		#(4 6 1 4) 
		#(4 1 6 4)
		#(1 4 4 6)
	)) eigenvaluesByJacobi
	"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(-1.510 -1.312 0.000 0.000) 
		#(-1.312 1.510 -0.000 -0.000) 
		#(-0.000 -0.000 -1.800 -0.872)
		#(0.000 0.000 -0.872 1.800)
	)) eigenvaluesByJacobi
	"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(-1.510 -1.312 ) 
		#(-1.312 1.510 ) 
	)) eigenvaluesByJacobi
	"

	| h loopCount j aux c a_jj a_kk k p_i a_jk b aux2 maxE |
	h _ self copy.
	loopCount _ 0.
	[ h isZeroBelowDiagonal ] whileFalse: [
		maxE _ nil.
		1 to: height-1 do: [ :ii |
			ii+1 to: width do: [ :jj |
				(maxE isNil or: [ (h i: ii j: jj) abs > maxE ]) ifTrue: [
					maxE _ (h i: ii j: jj) abs.
					j _ ii.
					k _ jj.
					]]].
		p_i _ self class identity: width.
		a_jj _ h i: j j: j.
		a_kk _ h i: k j: k.
		a_jj = a_kk ifTrue: [
			p_i i: j j: j put: 2.0 sqrt / 2.
			p_i i: k j: k put: 2.0 sqrt / 2.
			p_i i: k j: j put: 2.0 sqrt / 2.
			p_i i: j j: k put: 0-(2.0 sqrt / 2).
"			p_i print."
			]
		ifFalse: [
			a_jk _ h i: j j: k.
			c _ 2 * a_jk * (a_jj - a_kk) sign.		"ok?"
			b _ (a_jj - a_kk) abs.
			aux _ ((b / ((b*b)+(c*c)) sqrt) + 1 / 2) sqrt.
			p_i i: j j: j put: aux.
			p_i i: k j: k put: aux.
			aux2 _ c / (2 * aux * ((b*b)+(c*c))sqrt).
			aux2 _ aux arcCos sin.
			p_i i: k j: j put: aux2.
			p_i i: j j: k put: 0 - aux2.
"			p_i print.
			{aux arcCos. aux2 arcSin} print."
			].
		h _ p_i * h * p_i transposed.
"probablemente valga la pena optimizar y no crear todo asi... O simplemente reusar la instancia. No se."
		loopCount _ loopCount + 1.
		loopCount > 200 ifTrue: [^self error: 'Could not find real eigenvalues'] ].
"	h print."
	^h diagonal! !

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 10/7/2016 15:17:29'!
eigenvaluesByQR
	"This implementation only works if all the eigenvalues are real, and if there is a single one with absolute value greater than all the rest."
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(6 4 4 1) 
		#(4 6 1 4) 
		#(4 1 6 4)
		#(1 4 4 6)
	)) eigenvaluesByQR
	"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(1 2 3 4) 
		#(5 6 7 8) 
		#(9 10 11 12)
		#(13 14 15 16)
	)) eigenvaluesByQR
	"

	| h q r loopCount |
	h _ self hessembergSuperior.
	q _ self appropriateResultClass height: height width: width.
	r _ self appropriateResultClass height: height width: width.
	loopCount _ 0.
	[ h isFirstSubdiagonalZero ] whileFalse: [
		h storeQRDecompositionOfHessembergSuperiorOnQ: q r: r.
		h fillWith: r multipliedBy: q.
		loopCount _ loopCount + 1.
		loopCount > 200 ifTrue: [^self error: 'Could not find real eigenvalues'] ].
	^h diagonal! !

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 11/1/2016 16:10:22'!
eigenvectorForEigenvalue: eigenvalue
	"Answers a transposed vector (a row matrix) of eigenvalues and a matrix of eigenvectors.
	Each eigenvector is a column of the eigenvector matrix. Column k is the eigenvector for the
	eigenvalue at column k in the eigenvalue vector.
	The returned object is a Dictionary, with keys #eigenvalues and #eigenvectors.
	This seems to be a bad way. Numerical Recipes in C has a couple of algorithms that should be studied!!"

"SOLO para matrices SIMETRICAS"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(6 4 4 1) 
		#(4 6 1 4) 
		#(4 1 6 4)
		#(1 4 4 6)
	)) eigenvectorForLargestEigenvalue
	"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(-1.510 -1.312 0.000 0.000) 
		#(-1.312 1.510 -0.000 -0.000) 
		#(-0.000 -0.000 -1.800 -0.872)
		#(0.000 0.000 -0.872 1.800)
	)) eigenvectorForLargestEigenvalue
	"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(-1.510 -1.312 ) 
		#(-1.312 1.510 ) 
	)) eigenvectorForLargestEigenvalue
	"
	"
	(Float64Matrix withRows: #(
		( -1.509566546224207 1.311895552824272 0.01142904475547099 -6.28247456846262e-5)
		( 1.311895552824272 1.509567264852017 -5.931237097779896e-5 0.01129103744090236)
		( 0.01142904475547099 -5.931237097779896e-5 1.509205244817257 -1.312314005942332)
		( -6.28247456846262e-5 0.01129103744090236 -1.312314005942332 -1.509205963445068)))
	eigenvectorForLargestEigenvalue elements normalize
	"

	| eigenvectorSystem |
	eigenvectorSystem _ self appropriateResultClass height: height width: width+1.

	eigenvectorSystem fillWith: self.
	1 to: height do: [ :i |
		eigenvectorSystem i: i j: i put:
			(eigenvectorSystem i: i j: i) - eigenvalue.
		eigenvectorSystem i: i j: width+1 put: 0 ].
	eigenvectorSystem triangleWithPartialPivoting.

	"This second phase completes the linear system so it will have just one solution."
	"The danger here is that epsilon is too small and the 'zero' is not replaced. 
	Then, the only solution found is the vector zero, as if argument wasn't an eigenvalue.
	If such is the case, we'd retry with a slightly larger epsilon..."
	1 to: height do: [ :i |
		(eigenvectorSystem i: i j: i) abs < self epsilon"1e-4" ifTrue: [
			eigenvectorSystem i: i j: i put: 1.
			eigenvectorSystem i: i j: width+1  put: 1 ]].

	^eigenvectorSystem solveLinearSystem! !

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 10/26/2016 09:54:03'!
eigenvectorForLargestEigenvalue
	"Answers a transposed vector (a row matrix) of eigenvalues and a matrix of eigenvectors.
	Each eigenvector is a column of the eigenvector matrix. Column k is the eigenvector for the
	eigenvalue at column k in the eigenvalue vector.
	The returned object is a Dictionary, with keys #eigenvalues and #eigenvectors.
	This seems to be a bad way. Numerical Recipes in C has a couple of algorithms that should be studied!!"

"SOLO para matrices SIMETRICAS"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(6 4 4 1) 
		#(4 6 1 4) 
		#(4 1 6 4)
		#(1 4 4 6)
	)) eigenvectorForLargestEigenvalue
	"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(-1.510 -1.312 0.000 0.000) 
		#(-1.312 1.510 -0.000 -0.000) 
		#(-0.000 -0.000 -1.800 -0.872)
		#(0.000 0.000 -0.872 1.800)
	)) eigenvectorForLargestEigenvalue
	"
	"
	(FloatMatrix fromArrayOfArrays: #(
		#(-1.510 -1.312 ) 
		#(-1.312 1.510 ) 
	)) eigenvectorForLargestEigenvalue
	"
	"
	(Float64Matrix withRows: #(
		( -1.509566546224207 1.311895552824272 0.01142904475547099 -6.28247456846262e-5)
		( 1.311895552824272 1.509567264852017 -5.931237097779896e-5 0.01129103744090236)
		( 0.01142904475547099 -5.931237097779896e-5 1.509205244817257 -1.312314005942332)
		( -6.28247456846262e-5 0.01129103744090236 -1.312314005942332 -1.509205963445068)))
	eigenvectorForLargestEigenvalue elements normalize
	"

	| largestEigenvalue |
	largestEigenvalue _ self eigenvaluesByJacobi elements max.
	^ self eigenvectorForEigenvalue: largestEigenvalue! !

!FloatMatrix methodsFor: 'private' stamp: 'jmv 10/7/2016 15:10:39'!
epsilon
	"Something better is needed. For example, taking into accoung the largest elements, or the norm, or something like that... In any case it is better to check senders, and use a different strategy for ending iterative methods or such..."

	^1e-7 asFloat! !

!FloatMatrix methodsFor: 'copying' stamp: 'jmv 5/22/2015 12:23'!
expandTo: newExtent
	"
	| i |
	i _ FloatMatrix width: 5 height: 5.
	i x: 5 y: 5 put: Float pi.
	i expandTo:10@10
	"
	| answer |
	answer _ self appropriateResultClass width: newExtent x height: newExtent y.
	self copyToTile: answer position: 1@1.
	^answer! !

!FloatMatrix methodsFor: 'conversion' stamp: 'jmv 7/18/2016 11:12:10'!
fillFrom32BitForm: aForm component: aSymbol
	"aSymbol must be in #(red green blue alpha) or nil
	Result values v
		0.0 <= v <= (255/256.0)
	This means that #asForm must multiply by 256, to have byte values [0..255]"

	| componentIndex shift mask magic bb |
	magic _ 128.0.
	"If aSymbol is not supplied, just take blue.
	This is for consistency with ByteMatrix, where no map takes blue, and is actually faster."
	componentIndex _ aSymbol ifNil: [4] ifNotNil: [#(alpha red green blue) indexOf: aSymbol].
	shift _ #(-16 -8 0 8 ) at: componentIndex.
	mask _ #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
	bb _ BitBlt toForm: (Form new hackBitsForFloatMatrix: self).
	bb sourceForm: aForm.
	bb combinationRule: 7. "or"
	bb sourceX: 0; sourceY: 0.
	bb destX: 0 destY: 0 width: width height: height.
	bb colorMap: (ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 }).

	elements *= 0.
	elements += magic.
	bb copyBits.
	elements -= magic! !

!FloatMatrix methodsFor: 'conversion' stamp: 'jmv 7/18/2016 11:13:03'!
fillFromGrayForm: aGrayForm
	"aGrayForm can be 
		- an instance of GrayForm 
					(for example, from a grayscale png or jpg file)
		- a ColorForm with a colormap with 256 gray levels, going from black to white.
					(This is what we get, for instance, when reading a gray 8 bit bmp file)
	Result values v
		0.0 <= v <= (255/256.0)
	This means that #asForm must multiply by 256, to have byte values [0..255]"

	| magic bb |
	magic _ 32768.0.
	bb _ BitBlt toForm: (Form new hackBitsForFloatMatrix: self).
	bb sourceForm: aGrayForm.
	bb combinationRule: 7. "or"
	bb sourceX: 0; sourceY: 0.
	bb destX: 0 destY: 0 width: width height: height.
	bb colorMap: nil.

	elements *= 0.
	elements += magic.
	bb copyBits.
	elements -= magic! !

!FloatMatrix methodsFor: 'copying' stamp: 'jmv 8/28/2015 11:29'!
fillLowFFTFrequenciesWith: aFloatMatrix
	"
	Assume self extent > argument extent.
	Assume frequency zero is at i=1;j=1 (topLeft) as #fftImaginary:forward: leaves them
	fill low frequencies (i.e. areas closer to the 4 corners) with argument.
	The effect is to zero-pad higher frequecies.

	| i1 i2 |
	i1 := FloatImage lena.
	i2 := FloatImage extent: i1 extent * 2.
	i2 += 0.5.
	i1 displayAt: 0@0.
	i2 fillLowFFTFrequenciesWith: i1.
	i2 displayAt: 256@0.

	| i1 i2 |
	i1 := FloatImage lena.
	i2 := FloatImage extent: i1 extent * 3//4.
	i2 += 0.5.
	i1 displayAt: 0@0.
	i2 fillLowFFTFrequenciesWith: i1.
	i2 displayAt: 256@0.
	"
	
	| blitter formArgument formSelf deltaI deltaJ quarterExtent |
	deltaI _ height // 2.
	deltaJ _ width // 2.
	quarterExtent _ aFloatMatrix extent // 2 min: deltaJ@deltaI.
	formSelf _ Form new hackBitsForFloatMatrix: self.
	formArgument _ Form new hackBitsForFloatMatrix: aFloatMatrix.
	blitter _ BitBlt new.

	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: 0@0 sourceOrigin: 0@0
		extent: quarterExtent.
	blitter copyBits.
	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: width-quarterExtent x@0 sourceOrigin: aFloatMatrix width - quarterExtent x@0
		extent: quarterExtent.
	blitter copyBits.
	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: 0@(height-quarterExtent) y sourceOrigin: 0@(aFloatMatrix height - quarterExtent y)
		extent: quarterExtent.
	blitter copyBits.
	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: width@height-quarterExtent sourceOrigin: aFloatMatrix extent -quarterExtent
		extent: quarterExtent.
	blitter copyBits.! !

!FloatMatrix methodsFor: 'copying' stamp: 'jmv 5/22/2015 12:17'!
fillWith: aFloatMatrix
	| blitter formArgument formSelf |

	"Use inherited, more general and slower code if needed."
	(aFloatMatrix is: #FloatMatrix) ifFalse: [
		^super fillWith: aFloatMatrix ].

	formSelf _ Form new hackBitsForFloatMatrix: self.
	formArgument _ Form new hackBitsForFloatMatrix: aFloatMatrix.
	blitter _ BitBlt new.
	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: 0@0 sourceOrigin: 0@0.
	blitter copyBits! !

!FloatMatrix methodsFor: 'copying' stamp: 'jmv 5/22/2015 13:14'!
fillWith: aFloatMatrix displaced: deltaPoint
	"
	Assume self and argument are of the same extent
	Do a  simple displacement. Leave unaffected areas unchanged.
	deltaPoint is integer. Can be negative. (no resampling or interpolation)

	| i1 i2 |
	i1 _ FloatImage lena.
	i2 _ FloatImage extent: i1 extent.
	i1 displayAt: 0@0.
	i2 fillWith: i1 displaced: 40@-30.
	i2 displayAt: 256@0.
	"
	
	| blitter formArgument formSelf |
	formSelf _ Form new hackBitsForFloatMatrix: self.
	formArgument _ Form new hackBitsForFloatMatrix: aFloatMatrix.
	blitter _ BitBlt new.
	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: deltaPoint sourceOrigin: 0@0.
	blitter copyBits.! !

!FloatMatrix methodsFor: 'copying' stamp: 'jmv 5/22/2015 13:12'!
fillWith: aFloatMatrix displacedCircular: deltaPoint
	"
	Assume self and argument are of the same extent
	Do a 'circular' displacement. deltaPoint is integer. Can be negative. (no resampling or interpolation)
	| i1 i2 |
	i1 _ FloatImage lena.
	i2 _ FloatImage extent: i1 extent.
	i1 displayAt: 0@0.
	i2 fillWith: i1 displacedCircular: 120@140.
	i2 displayAt: 256@0.
	"
	
	| blitter formArgument formSelf deltaI deltaJ |
	deltaI _ deltaPoint y \\ height.
	deltaJ _ deltaPoint x \\ width.
	formSelf _ Form new hackBitsForFloatMatrix: self.
	formArgument _ Form new hackBitsForFloatMatrix: aFloatMatrix.
	blitter _ BitBlt new.
	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: deltaJ@deltaI sourceOrigin: 0@0.
	blitter copyBits.
	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: deltaJ-width@deltaI sourceOrigin: 0@0.
	blitter copyBits.
	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: deltaJ@(deltaI-height) sourceOrigin: 0@0.
	blitter copyBits.
	blitter
		setDestForm: formSelf sourceForm: formArgument
		destOrigin: deltaJ-width@(deltaI-height) sourceOrigin: 0@0.
	blitter copyBits.! !

!FloatMatrix methodsFor: 'filling' stamp: 'jmv 7/19/2012 10:46'!
fillWithIdentity

	self fillWithZeroes.
	1 to: (width min: height) do: [ :j |
		self i: j j: j put: 1 ]! !

!FloatMatrix methodsFor: 'filling' stamp: 'jmv 8/4/2014 16:03'!
fillWithOnes
	self fillWithValue: 1.0! !

!FloatMatrix methodsFor: 'filling' stamp: 'jmv 8/4/2014 16:05'!
fillWithValue: value
	elements *= 0.0.
	elements += value! !

!FloatMatrix methodsFor: 'accessing' stamp: 'jmv 9/17/2007 12:43'!
fillWithZeroes
	elements *= 0.0! !

!FloatMatrix methodsFor: 'displaying' stamp: 'jmv 5/4/2016 11:05'!
fullRangeDisplayAt: aPoint
	"Assume the values are not normalized in the scale 0.0 to 1.0.
	Apply appropriate scaling to use viewable dynamic range to show receiver actual range of values.
	Assume receiver is in linear scale.
	Apply appropriate gamma encoding to make good use of pixel dynamic range.
	(See #linearSpaceToGammaEncoded and #linearSpaceToGammaEncoded)"

	self asGammaFormFullRange displayAt: aPoint! !

!FloatMatrix methodsFor: 'displaying' stamp: 'jmv 5/4/2016 11:09'!
fullRangeG: greenImage b: blueImage displayAt: aPoint
	"Assume the values are not normalized in the scale 0.0 to 1.0.
	Apply appropriate scaling to use viewable dynamic range to show receiver actual range of values.
	Assume receiver is in linear scale.
	Apply appropriate gamma encoding to make good use of pixel dynamic range.
	(See #linearSpaceToGammaEncoded and #linearSpaceToGammaEncoded)"

	(self asGammaFormFullRangeG: greenImage b: blueImage) displayAt: aPoint! !

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 10/6/2016 22:28:26'!
hessembergSuperior
	"Answers a new matrix that is the Hessemberg Superior transform of the receiver.
	A Hessemberg Superior matrix has zero entries below the first subdiagonal.
	The new matrix has the same eigenvalues as the receiver."

	| result u |
	self isSquare ifFalse: [ ^self error: 'Only for Square Matrices' ].
	result _ self copy.
	u _ self appropriateResultClass height: height width: width.
	1 to: width-2 do: [ :j |
		(result storeHouseholderTransformOn: u column: j forQR: false) ifTrue: [
			result _ u * result * u.
		].
	].
	^ result
! !

!FloatMatrix methodsFor: 'accessing' stamp: 'jmv 4/27/2015 10:09'!
i: i
	"Answers a row vector with the values at row y
	Optimized for large Matrices
	|m w h |
	w _ 8. h _ 8.
	m _ FloatMatrix extent: w@h. m elements += (1 to: w*h).
	m i: 3
	"

	| result |
	result _ self appropriateResultClass newRowVectorSize: width.
	self i: i copyTo: result.
	^result! !

!FloatMatrix methodsFor: 'private' stamp: 'jmv 4/27/2015 10:21'!
i: i copyTo: row
	"Assumes row is of of apropriate size"
	| ss |
	ss _ i-1 * width + 1.
	row elements replaceFrom: 1 to: width with: elements startingAt: ss! !

!FloatMatrix methodsFor: 'accessing' stamp: 'jmv 4/27/2015 10:01'!
i: i put: row
	"aVector must be a row vector, i.e. a matrix with height 1.
	Optimized for large Matrices
	|m w h |
	w _ 8. h _ 8.
	m _ FloatMatrix extent: w@h. m elements += (1 to: w*h).
	m i: 7 put: (m i: 2).
	m print.
	"

	| ss |
	ss _ i-1 * width + 1.
	elements replaceFrom: ss to: ss+width-1 with: row elements startingAt: 1! !

!FloatMatrix methodsFor: 'aritmethic - complex' stamp: 'jmv 7/19/2012 11:15'!
imaginary: imaginaryPart elementDivideBy: operandRealPart imaginary: operandImaginaryPart
	"The receiver and imaginaryPart conform a complex matrix.
	operandRealPart and operandImaginaryPart conform another complex matrix.
	fill the receiver (and imaginaryPart) with the result of complex division with operand, element by element.
	I.e., at each position of the matrix, compute (e+if) = (a+ib) / (c+id)

	| r1 i1 r2 i2 |
	r1 _ FloatMatrix fromArrayOfArrays: #(
		#( 1 2 3)
		#( 4 5 6)
		#( 1 2 3)).
	i1 _ FloatMatrix fromArrayOfArrays: #(
		#( 1 1 0)
		#( 0 0 1)
		#( 0 0 0)).
	r2 _ FloatMatrix fromArrayOfArrays: #(
		#( 2 4 6)
		#( 4 5 6)
		#( 1 2 3)).
	i2 _ FloatMatrix fromArrayOfArrays: #(
		#( 5 5 5)
		#( 5 5 5)
		#( 7 8 9)).
	Transcript clear.
	r1 print. i1 print.
	r1 imaginary: i1 elementMultiplyBy: r2 imaginary: i2.
	r1 print. i1 print.
	r1 imaginary: i1 elementDivideBy: r2 imaginary: i2.
	r1 print. i1 print
	"

	| a b c d e f operandSquaredNorm |
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			a _ self i: i j: j.
			b _ imaginaryPart i: i j: j.
			c _ operandRealPart i: i j: j.
			d _ operandImaginaryPart i: i j: j.
			operandSquaredNorm _ c * c + (d * d).
			operandSquaredNorm = 0.0
				ifFalse: [
					e _ a * c + (b * d) / operandSquaredNorm.
					f _ b * c - (a * d) / operandSquaredNorm ]
				ifTrue: [ e _ f _ 0.0 ].
			self i: i j: j put: e.
			imaginaryPart i: i j: j put: f ]]! !

!FloatMatrix methodsFor: 'aritmethic - complex' stamp: 'jmv 7/19/2012 11:15'!
imaginary: imaginaryPart elementDivideBy: operandRealPart imaginary: operandImaginaryPart threshold: threshold
	"The receiver and imaginaryPart conform a complex matrix.
	operandRealPart and operandImaginaryPart conform another complex matrix.
	fill the receiver (and imaginaryPart) with the result of complex division with operand, element by element.
	I.e., at each position of the matrix, compute (e+if) = (a+ib) / (c+id)

	| r1 i1 r2 i2 |
	r1 _ FloatMatrix fromArrayOfArrays: #(
		#( 1 2 3)
		#( 4 5 6)
		#( 1 2 3)).
	i1 _ FloatMatrix fromArrayOfArrays: #(
		#( 1 1 0)
		#( 0 0 1)
		#( 0 0 0)).
	r2 _ FloatMatrix fromArrayOfArrays: #(
		#( 2 4 6)
		#( 4 5 6)
		#( 1 2 3)).
	i2 _ FloatMatrix fromArrayOfArrays: #(
		#( 5 5 5)
		#( 5 5 5)
		#( 7 8 9)).
	Transcript clear.
	r1 print. i1 print.
	r1 imaginary: i1 elementMultiplyBy: r2 imaginary: i2.
	r1 print. i1 print.
	r1 imaginary: i1 elementDivideBy: r2 imaginary: i2.
	r1 print. i1 print
	"

	| a b c d e f operandSquaredNorm |
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			a _ self i: i j: j.
			b _ imaginaryPart i: i j: j.
			c _ operandRealPart i: i j: j.
			d _ operandImaginaryPart i: i j: j.
			operandSquaredNorm _ c * c + (d * d).
			operandSquaredNorm >= threshold
				ifTrue: [
					e _ a * c + (b * d) / operandSquaredNorm.
					f _ b * c - (a * d) / operandSquaredNorm ]
				ifFalse: [ e _ f _ 0.0 ].
			self i: i j: j put: e.
			imaginaryPart i: i j: j put: f ]]! !

!FloatMatrix methodsFor: 'aritmethic - complex' stamp: 'jmv 4/27/2015 12:26'!
imaginary: imaginaryPart elementMultiplyBy: operandRealPart imaginary: operandImaginaryPart
	"The receiver and imaginaryPart conform a complex matrix.
	operandRealPart and operandImaginaryPart conform another complex matrix.
	fill the receiver (and imaginaryPart) with the result of complex multiplication with operand, element by element.
	I.e., at each position of the matrix, compute (e+if) = (a+ib) * (c+id)"

"
	| a b c d e f |
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			a _ self i: i j: j.
			b _ imaginaryPart i: i j: j.
			c _ operandRealPart i: i j: j.
			d _ operandImaginaryPart i: i j: j.
			e _ a * c - (b * d).
			f _ b * c + (a * d).
			self i: i j: j put: e.
			imaginaryPart i: i j: j put: f ]]
"
	| c d aByC bByC aByD bByD e f |
	c _ FloatMatrix newRowVectorSize: width.
	d _ FloatMatrix newRowVectorSize: width.
	aByC _ FloatMatrix newRowVectorSize: width.
	bByC _ FloatMatrix newRowVectorSize: width.
	aByD _ FloatMatrix newRowVectorSize: width.
	bByD _ FloatMatrix newRowVectorSize: width.
	1 to: height do: [ :i |
		
		" Compute:
		e _ a * c - (b * d).
		f _ b * c + (a * d)."
		
		operandRealPart i: i copyTo: c.
		self i: i copyTo: aByC.
		aByC elements *= c elements.
		imaginaryPart i: i copyTo: bByC.
		bByC elements *= c elements.
		
		operandImaginaryPart i: i copyTo: d.
		self i: i copyTo: aByD.
		aByD elements *= d elements.
		imaginaryPart i: i copyTo: bByD.
		bByD elements *= d elements.
		
		e _ aByC.
		e elements -= bByD elements.
		f _ bByC.
		f elements += aByD elements.

		self i: i put: e.
		imaginaryPart i: i put: f ]! !

!FloatMatrix methodsFor: 'initialization' stamp: 'jmv 6/18/2001 19:10'!
initializeElements

	elements _ FloatArray new: height * width! !

!FloatMatrix methodsFor: 'testing' stamp: 'jmv 5/22/2015 12:16'!
is: aSymbol
	^ aSymbol == #FloatMatrix or: [ super is: aSymbol ]! !

!FloatMatrix methodsFor: 'testing' stamp: 'jmv 7/19/2012 13:06'!
isFirstSubdiagonalZero

	^(1 to: width-1) inject: true into: [ :previousValue :each |
		previousValue and: [ (self i: each + 1 j: each) abs <= self epsilon ] ]! !

!FloatMatrix methodsFor: 'testing' stamp: 'jmv 10/6/2016 23:32:51'!
isZeroBelowDiagonal

	2 to: height do: [ :i |
		1 to: i-1 do: [ :j |
			(self i: i j: j) abs > self epsilon ifTrue: [ ^false ]]].
	^true! !

!FloatMatrix methodsFor: 'accessing' stamp: 'jmv 4/27/2015 10:11'!
j: j
	"Answers a column vector with the values at column x.
	Optimized for large Matrices
	|m w h |
	w _ 8. h _ 8.
	m _ FloatMatrix extent: w@h. m elements += (1 to: w*h).
	m j: 3
	"

	| result |
	result _ self appropriateResultClass newColumnVectorSize: height.
	self j: j copyTo: result.
	^result! !

!FloatMatrix methodsFor: 'private' stamp: 'jmv 5/21/2015 12:44'!
j: j copyTo: column
	"Assumes column is of of apropriate size"
	self copyToTile: column position: j@1! !

!FloatMatrix methodsFor: 'accessing' stamp: 'jmv 5/21/2015 12:45'!
j: j put: column
	"column can be a column vector (m x 1 matrix), or a kind of Array.
	Optimized for large Matrices
	| m w h |
	w _ 8. h _ 8.
	m _ FloatMatrix extent: w@h. m elements += (1 to: w*h).
	m j: 3 put: (m j: 1).
	m print.
	"

	self copyFromTile: column position: j@1! !

!FloatMatrix methodsFor: 'linear equation systems' stamp: 'jmv 7/19/2012 11:16'!
linesLost

	| result |
	result _ 0.
	[
		(1 to: width) inject: true into: [ :prev :each |
			prev & ((self i: height-result j: each ) = 0.0) ]
	] whileTrue: [ result _ result + 1 ].
	^result! !

!FloatMatrix methodsFor: 'aritmethic - complex' stamp: 'jmv 4/24/2015 10:27'!
magnitudeWithImaginary: imaginaryPart
	"answer a new matrix"
	| result a b magnitude |
	result _ self appropriateResultClass newSize: self size.
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			a _ self i: i j: j.
			b _ imaginaryPart i: i j: j.
			magnitude _ (a * a + (b * b)) sqrt.
			result i: i j: j put: magnitude ]].
	^result! !

!FloatMatrix methodsFor: 'norm and condition number' stamp: 'jmv 10/7/2016 14:44:28'!
norm
	"
	(FloatMatrix identity: 5) norm
	"
	^(self transposed * self) eigenvalues elements max sqrt! !

!FloatMatrix methodsFor: 'norm and condition number' stamp: 'jmv 7/19/2012 11:06'!
norm2

	^self norm2Squared sqrt! !

!FloatMatrix methodsFor: 'norm and condition number' stamp: 'jmv 7/19/2012 11:16'!
norm2Squared

	| result |
	width > 1 & (height > 1) ifTrue: [ ^self error: 'right now, only for vectors' ].
	result _ 0.0.
	width = 1 ifTrue: [
		1 to: height do: [ :i |
			result _ result + (self i: i j: 1) squared ] ]
	ifFalse: [
		1 to: width do: [ :i |
			result _ result + (self i: 1 j: i) squared ] ].
	^result! !

!FloatMatrix methodsFor: 'aux operations' stamp: 'jmv 7/19/2012 12:02'!
preMultiplyByGivensRotationRowI: i rowK: k titaCosine: c titaSine: s
	"Modify the receiver, doing a premultiplication by a Givens rotation of angle tita,
	affecting rows i and k."

	| elemI elemK |
	1 to: width do: [ :j |
		elemI _ self i: i j: j.
		elemK _ self i: k j: j.
		self i: i j: j put: c*elemI - (s*elemK).
		self i: k j: j put: s*elemI + (c*elemK) ].! !

!FloatMatrix methodsFor: 'printing' stamp: 'jmv 1/9/2014 21:28'!
printOn: aStream
	aStream newLine.
	1 to: height do: [ :i |
		aStream nextPutAll: '| '.
		1 to: width do: [ :j |
			(self i: i j: j) printOn: aStream integerDigits: 1 fractionDigits: 3.
			aStream space ].
		aStream nextPut: $|; newLine ]! !

!FloatMatrix methodsFor: 'operations' stamp: 'jmv 9/2/2014 11:28'!
round

	| a b |
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			(self i: i j: j) = 0.0 ifFalse: [
				a _ self i: i j: j.
				b _ (10.0 raisedTo: a abs log truncated - 5).
				self i: i j: j put: (a roundTo: b) ] ] ]! !

!FloatMatrix methodsFor: 'aux operations' stamp: 'jmv 7/19/2012 12:01'!
rowsCloserToZero: count
	"Return a collection with the indexes of the rows that are closer to being all zeroes"

	^(((((1 to: height) collect: [ :i |
		i @ (self i: i) norm2Squared ])
			asSortedCollection: [ :a :b | a y < b y ])
				copyFrom: 1 to: count) collect: [ :each | each x ] ) asSortedCollection! !

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 7/19/2012 11:17'!
storeHouseholderTransformOn: aMatrix column: j forQR: forQR
	"Modifies entries on aMatrix to make it the Householder transforms
	that puts zeroes at column i of the receiver. If forQR is false, the
	product of aMatrix * self is Hessemberg superior, otherways its a
	triangular matrix. "

	| x xNorm2Squared v vNorm2Squared element i |
	i _ j - (forQR ifTrue: [ 1 ] ifFalse: [ 0 ]).
	x _ self appropriateResultClass newVectorSize: height-i.
	xNorm2Squared _ 0.
	1 to: x height do: [ :ii |
		element _ self i: ii+i j: j.
		xNorm2Squared _ xNorm2Squared + element squared.
		x i: ii j: 1 put: element ].
	v _ x.
	"If column already has zeros, do nothing"
	xNorm2Squared = 0.0 ifTrue: [ ^false ].
	"If column already has zeros, do nothing. If forQR = false, then the first element in x
	could not be zero, and anyway there's nothing to do"
	(forQR not and: [ xNorm2Squared = (x i: 1 j: 1) squared ]) ifTrue: [ ^false ].

	v i: 1 j: 1 put: (v i: 1 j: 1) + xNorm2Squared sqrt.
	vNorm2Squared _ v norm2Squared.

	1 to: i do: [ :ii |
		aMatrix i: ii j: ii put: 1.
		ii+1 to: aMatrix width do: [ :jj |
			aMatrix i: ii j: jj put: 0.
			aMatrix i: jj j: ii put: 0 ] ].
	1 to: x height do: [ :ii |
		1 to: x height do: [ :jj |
			aMatrix i: ii+i j: jj+i put: 
				(ii=jj ifTrue:[1] ifFalse:[0]) - 
				(2.0 / vNorm2Squared * (v i: ii j: 1) * (v i: jj j: 1) ) ] ].
	^true! !

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 10/6/2016 22:27:34'!
storeQRDecompositionOfHessembergSuperiorOnQ: q r: r
	"Works only if the receiver is a Hessemberg superior matrix."

	| elementAtJ elementBelowJ aux c s |
	q fillWithIdentity.
	r fillWith: self.
	1 to: width-1 do: [ :j |
		elementAtJ _ r i: j j: j.
		elementBelowJ _ r i: j+1 j: j.
		elementBelowJ = 0 ifFalse: [
			aux _ ((elementAtJ*elementAtJ) + (elementBelowJ*elementBelowJ)) sqrt.
			c _ elementAtJ / aux.
			s _ 0 - elementBelowJ / aux.
			r preMultiplyByGivensRotationRowI: j rowK: j+1 titaCosine: c titaSine: s.
			q preMultiplyByGivensRotationRowI: j rowK: j+1 titaCosine: c titaSine: s ] ].
	q transpose.! !

!FloatMatrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:26'!
form: aForm color: aSymbol
	| h w answer color |
	h _ aForm height.
	w _ aForm width.
	answer _ self height: h width: w.
	1 to: h do: [ :i |
		1 to: w do: [ :j |
			color _ aForm colorAt: (j-1@(i-1)).
			answer i: i j: j put: (color perform: aSymbol)]].
	^answer! !

!Float64Matrix methodsFor: 'yet unimplemented' stamp: 'jmv 10/11/2016 10:52:14'!
copyFromTile: aFloatMatrix position: aPoint
	self halt! !

!Float64Matrix methodsFor: 'yet unimplemented' stamp: 'jmv 10/11/2016 10:52:17'!
copyFromTile: aFloatMatrix position: aPoint guardZone: borderWidth
	self halt! !

!Float64Matrix methodsFor: 'yet unimplemented' stamp: 'jmv 10/11/2016 10:52:20'!
copyToTile: aFloatMatrix position: aPoint
	self halt! !

!Float64Matrix methodsFor: 'private' stamp: 'jmv 10/11/2016 10:51:49'!
epsilon
	"Something better is needed. For example, taking into accoung the largest elements, or the norm, or something like that... In any case it is better to check senders, and use a different strategy for ending iterative methods or such..."

	^1e-8 asFloat! !

!Float64Matrix methodsFor: 'yet unimplemented' stamp: 'jmv 10/11/2016 10:52:24'!
fillFrom32BitForm: aForm component: aSymbol
	self halt! !

!Float64Matrix methodsFor: 'copying' stamp: 'jmv 10/11/2016 10:52:42'!
fillWith: aFloatMatrix
	1 to: (height min: aFloatMatrix height) do: [ :i |
		1 to: (width min: aFloatMatrix width) do: [ :j |
			self i: i j: j put: (aFloatMatrix i: i j: j) ] ]! !

!Float64Matrix methodsFor: 'copying' stamp: 'jmv 10/11/2016 10:52:55'!
fillWith: aFloatMatrix displaced: deltaPoint
	1 to: aFloatMatrix height do: [ :i |
		1 to: aFloatMatrix width do: [ :j |
			self i: i+deltaPoint y j: j+deltaPoint x put: (aFloatMatrix i: i j: j) ]]! !

!Float64Matrix methodsFor: 'yet unimplemented' stamp: 'jmv 10/11/2016 10:53:08'!
fillWith: aFloatMatrix displacedCircular: deltaPoint
	self halt! !

!Float64Matrix methodsFor: 'yet unimplemented' stamp: 'jmv 10/11/2016 10:53:11'!
imaginary: imaginaryPart elementMultiplyBy: operandRealPart imaginary: operandImaginaryPart
	self halt! !

!Float64Matrix methodsFor: 'initialization' stamp: 'jmv 10/6/2016 22:02:07'!
initializeElements

	elements _ Float64Array new: height * width! !

!Float64Matrix methodsFor: 'accessing' stamp: 'jmv 10/11/2016 10:53:24'!
j: j
	"Answers a column vector with the values at column x"

	| result |
	result _ self appropriateResultClass newColumnVectorSize: height.
	1 to: height do: [ :i |
		result i: i j: 1 put: (self i: i j: j) ].
	^result! !

!Float64Matrix methodsFor: 'accessing' stamp: 'jmv 10/11/2016 10:53:26'!
j: j put: column
	"column can be a column vector (m x 1 matrix), or a kind of Array"

	1 to: height do: [ :i |
		self i: i j: j put: (column at: i) ]! !

!FloatBandMatrix methodsFor: 'misc' stamp: 'jmv 9/27/2007 23:38'!
appropriateResultClass
	"Many operations will result in a general (non band) matrix."

	^FloatMatrix! !

!FloatBandMatrix methodsFor: 'accessing' stamp: 'jmv 9/28/2007 20:06'!
elementsIndexForI: i j: j
	"Band matrices use this special representation.
	Answer nil if outside the band"

	^ (j-i) abs * 2 < bandWidth
		ifTrue: [ i*bandWidth + j-i ]
		ifFalse: [ nil ]! !

!FloatBandMatrix methodsFor: 'accessing' stamp: 'jmv 9/2/2014 10:11'!
i: i j: j
	"Answer element at row i, column j"

	^(self elementsIndexForI: i j: j)
		ifNotNil: [ :position | elements at: position ]
		ifNil: [ 0.0 ]! !

!FloatBandMatrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 10:26'!
i: i j: j put: aNumber
	"Store aNumber as the element at row i, column j"

	(self elementsIndexForI: i j: j)
		ifNotNil: [ :position | elements at: position put: aNumber ]
		ifNil: [ self error: 'Can not store outside band' ]! !

!FloatBandMatrix methodsFor: 'initialization' stamp: 'jmv 7/3/2001 17:44'!
initHeightAndWidth: size bandWidth: bw

	width _ size.
	height _ size.
	bandWidth _ bw.
	self initializeElements.! !

!FloatBandMatrix methodsFor: 'initialization' stamp: 'jmv 7/3/2001 17:47'!
initializeElements

	elements _ FloatArray new: height * bandWidth! !

!FloatBandMatrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:33'!
height: height width: width

	^self error: 'Please call #heightAndWidth:bandWidth: for Band Matrices'! !

!FloatBandMatrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:32'!
heightAndWidth: aNumber bandWidth: bw
	"Must be square!!"

	^self basicNew initHeightAndWidth: aNumber bandWidth: bw! !

!FloatBandMatrix class methodsFor: 'instance creation' stamp: 'jmv 9/2/2014 10:10'!
m: size bandWidth: bw

	^self basicNew initHeightAndWidth: size bandWidth: bw! !

!FloatBandMatrix class methodsFor: 'instance creation' stamp: 'jmv 9/30/2007 19:12'!
m: height n: width

	^self error: 'Please call #m:bandWidth: for Band Matrices'! !

!FloatBandMatrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:32'!
newIdentity: size

	| result |
	result _ self heightAndWidth: size bandWidth: 1.
	result fillWithIdentity.
	^result
! !

!FloatBandMatrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:33'!
newWithDiagonalValues: aVectorOrArray

	| size result |
	size _ aVectorOrArray size.
	size isPoint ifTrue: [ size _ size x max: size y ].
	result _ self heightAndWidth: size bandWidth: 1.
	result fillDiagonalWith: aVectorOrArray.
	^result
! !

!FloatBandMatrix class methodsFor: 'instance creation' stamp: 'jmv 8/23/2014 23:35'!
width: w height: h

	^self error: 'Please call #heightAndWidth:bandWidth: for Band Matrices'! !

!IntegerMatrix methodsFor: 'accessing' stamp: 'jmv 9/4/2015 10:59'!
at: aPointOrIndex put: aNumber
	
	(aNumber between: -16r80000000 and: 16r7FFFFFFF)
		ifTrue: [ self errorImproperStore ].

	^super at: aPointOrIndex put: aNumber! !

!IntegerMatrix methodsFor: 'initialization' stamp: 'jmv 9/4/2015 10:36'!
initializeElements

	elements _ IntegerArray new: height * width! !

!ShortIntegerMatrix methodsFor: 'initialization' stamp: 'jmv 9/4/2015 13:12'!
initializeElements

	| n |
	n _ height * width.
	n even ifFalse: [
		n _ n + 1 ].
	elements _ ShortIntegerArray new: n! !

!ShortWordMatrix methodsFor: 'initialization' stamp: 'jmv 7/26/2016 12:37:58'!
initializeElements

	| n |
	n _ height * width.
	n even ifFalse: [
		n _ n + 1 ].
	elements _ ShortWordArray new: n! !
