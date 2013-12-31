'From Cuis 4.0 of 21 April 2012 [latest update: #1315] on 19 July 2012 at 1:08:02 pm'!
!classDefinition: #Matrix category: #LinearAlgebra!
Object subclass: #Matrix
	instanceVariableNames: 'width height elements '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LinearAlgebra'!

!Matrix commentStamp: 'jmv 7/19/2012 11:52' prior: 0!
My instances are m x n matrices, where m is the height and n the width. A matrix is a kind of 2-d array. A Matrix whose width is 1 is called a "Column Vector". A Matrix whose height is 1 is called a "Row Vector".

My instances can hold elements of any kind, but include some methods that assume that the elements understand arithmetic operations. These are useful for Matrices whose elements are Fractions, LargeIntegers, Matrices and Vectors (column or row Matrices). They are also suitable for Complex numbers, but in such cases, an alternative is to use different FloatMatrices for Real and Imaginary parts.!


!FloatMatrix commentStamp: '<historical>' prior: 0!
My instances are m x n matrices of Float values, where m is the height and n the width. Suitable for LinearAlgebra and SignalProcessing suff, among many other possible applications. It is possible to build VM plugins for fast processing.!


!FloatBandMatrix commentStamp: '<historical>' prior: 0!
My instances can have non-zero values only in the diagonal, and a band of certain width around it. Save space.!


!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 10:25'!
elements
	^ elements! !

!Matrix methodsFor: 'private' stamp: 'jmv 7/19/2012 11:09'!
elementsIndexForI: i j: j
	"Indexes goes from 1 to size (width or height)"

	^ i-1*width+j! !

!Matrix methodsFor: 'private' stamp: 'jmv 7/19/2012 12:35'!
zeroElement
	"We don't really restrict the kind of objects we might hold... Our best guess for a kind of null additive value is to just ask some element..."

	^elements first class zero! !


!FloatMatrix methodsFor: 'filling' stamp: 'jmv 7/19/2012 10:46'!
fillWithIdentity

	self fillWithZeroes.
	1 to: (width min: height) do: [ :j |
		self i: j j: j put: 1 ]! !

!FloatMatrix methodsFor: 'linear equation systems' stamp: 'jmv 7/19/2012 11:16'!
linesLost

	| result |
	result _ 0.
	[
		(1 to: width) inject: true into: [ :prev :each |
			prev & ((self i: height-result j: each ) = 0.0) ]
	] whileTrue: [ result _ result + 1 ].
	^result! !

!FloatMatrix methodsFor: 'norm and condition number' stamp: 'jmv 7/19/2012 10:57'!
conditionNumber
	^self norm * self inverse norm! !

!FloatMatrix methodsFor: 'norm and condition number' stamp: 'jmv 7/19/2012 10:57'!
norm
	"
	(FloatMatrix identity: 5) norm
	"
	^(self transposed * self) eigenvaluesByQR elements max sqrt! !

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

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 7/19/2012 10:56'!
eigenvaluesAndEigenvectors
	"Answers a transposed vector (a row matrix) of eigenvalues and a matrix of eigenvectors.
	Each eigenvector is a column of the eigenvector matrix. Column k is the eigenvector for the
	eigenvalue at column k in the eigenvalue vector.
	The returned object is a Dictionary, with keys #eigenvalues and #eigenvectors."

	^ self eigenvaluesAndEigenvectorsByQR! !

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 7/19/2012 11:17'!
eigenvaluesAndEigenvectorsByQR
	"Answers a transposed vector (a row matrix) of eigenvalues and a matrix of eigenvectors.
	Each eigenvector is a column of the eigenvector matrix. Column k is the eigenvector for the
	eigenvalue at column k in the eigenvalue vector.
	The returned object is a Dictionary, with keys #eigenvalues and #eigenvectors.
	This seems to be a bad way. Numerical Recipes in C has a couple of algorithms that should be studied!!"

	| result eigenvalues eigenvectorSystem eigenvaluesVector resultIndex resultEigenvalues resultEigenvectors rows columns |
	result _ Dictionary new.
	eigenvaluesVector _ self eigenvaluesByQR.
	eigenvaluesVector round.	"This is necesary to consider repeated eigenvalues this way, and not as different ones (that would lead to repeated eigenvectors)"
	eigenvalues _ Bag new.
	1 to: eigenvaluesVector height do: [ :j |
		eigenvalues add: (eigenvaluesVector i: j j: 1) ].
	eigenvectorSystem _ self appropriateResultClass newHeight: height width: width+1.
	resultIndex _ 1.
	resultEigenvalues _ self appropriateResultClass newRowVectorSize: width.
	resultEigenvectors _ self appropriateResultClass newHeight: height width: width.
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

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 7/19/2012 11:15'!
eigenvaluesByQR
	"This implementation only works if all the eigenvalues are real."
	"(FloatMatrix fromArrayOfArrays: #(
		#(6 4 4 1) 
		#(4 6 1 4) 
		#(4 1 6 4)
		#(1 4 4 6)
	)) eigenvaluesByQR"

	| h q r loopCount |
	h _ self hessembergSuperior.
	q _ self appropriateResultClass newHeight: height width: width.
	r _ self appropriateResultClass newHeight: height width: width.
	loopCount _ 0.
	[ h isFirstSubdiagonalZero ] whileFalse: [
		h storeQRDecompositionOfHessemberSuperiorOnQ: q r: r.
		h fillWith: r multipliedBy: q.
		loopCount _ loopCount + 1.
		loopCount > 200 ifTrue: [^self error: 'Could not find real eigenvalues'] ].
	^h diagonal! !

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 7/19/2012 11:15'!
hessembergSuperior
	"Answers a new matrix that is the Hessember Superior transform of the receiver.
	A Hessember Superior matrix has zero entries below the first subdiagonal.
	The new matrix has the same eigenvalues as the receiver."

	| result u |
	self isSquare ifFalse: [ ^self error: 'Only for Square Matrices' ].
	result _ self copy.
	u _ self appropriateResultClass newHeight: height width: width.
	1 to: width-2 do: [ :j |
		(result storeHouseholderTransformOn: u column: j forQR: false) ifTrue: [
			result _ u * result * u.
		].
	].
	^ result
! !

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

!FloatMatrix methodsFor: 'eigenvalues and eigenvectors' stamp: 'jmv 7/19/2012 12:00'!
storeQRDecompositionOfHessemberSuperiorOnQ: q r: r
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

!FloatMatrix methodsFor: 'operations' stamp: 'jmv 7/19/2012 12:19'!
round

	| a b |
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			(self i: i j: j) = 0.0 ifFalse: [
				a _ self i: i j: j.
				b _ (10.0 raisedTo: a abs log truncated - 5).
				self i: i j: j put: (a / b) rounded * b ] ] ]! !

!FloatMatrix methodsFor: 'printing' stamp: 'jmv 7/19/2012 12:10'!
printOn: aStream
	1 to: height do: [ :i |
		aStream nextPutAll: '| '.
		1 to: width do: [ :j |
			aStream nextPutAll: ((self i: i j: j) printPaddedLeft: 1 decimalPlaces: 2).
			"aStream print: ((self i: i j: j) roundTo: 0.001)."
			aStream nextPut: $  ].
		aStream nextPut: $|; newLine ]! !

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

!FloatMatrix methodsFor: 'aux operations' stamp: 'jmv 7/19/2012 12:01'!
rowsCloserToZero: count
	"Return a collection with the indexes of the rows that are closer to being all zeroes"

	^(((((1 to: height) collect: [ :i |
		i @ (self i: i) norm2Squared ])
			asSortedCollection: [ :a :b | a y < b y ])
				copyFrom: 1 to: count) collect: [ :each | each x ] ) asSortedCollection! !

!FloatMatrix methodsFor: 'private' stamp: 'jmv 7/19/2012 12:18'!
epsilon
	"Something better is needed. For example, taking into accoung the largest elements, or the norm, or something like that... In any case it is better to check senders, and use a different strategy for ending iterative methods or such..."

	^1e-3 asFloat! !

!FloatMatrix methodsFor: 'testing' stamp: 'jmv 7/19/2012 13:06'!
isFirstSubdiagonalZero

	^(1 to: width-1) inject: true into: [ :previousValue :each |
		previousValue and: [ (self i: each + 1 j: each) abs <= self epsilon ] ]! !


!Number class methodsFor: 'constants' stamp: 'jmv 7/19/2012 12:31'!
zero

	^0! !


!Float class methodsFor: 'constants' stamp: 'jmv 7/19/2012 12:32'!
zero

	^0.0! !


!Collection methodsFor: 'testing' stamp: 'jmv 7/19/2012 12:29'!
isZero
	"Answer whether the receiver is zero"
	Transcript newLine.
	#isZero print.
	thisContext printStack: 5.
	self flag: #jmv. "Remove this crap!!"

	^ false! !


!DifferenceFinder methodsFor: 'private' stamp: 'jmv 7/19/2012 10:33'!
initializeMatrix
	matrix _ Matrix newHeight: x size width: y size! !


!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:18'!
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

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:18'!
at: aPointOrIndex put: aNumber
	"If aPointOrIndex is a Number, the receiver must be a vector (either row or column).
	Indexes goes from 1 to size (width or height)"

	
| i j |
	aPointOrIndex isPoint
		ifTrue: [ i _ aPointOrIndex y. j _ aPointOrIndex x ]
		ifFalse: [
			width = 1 ifTrue: [ i _ aPointOrIndex. j _ 1 ].
			height = 1 ifTrue: [ i _ 1. j _ aPointOrIndex. ]].
	^ self i: i j: j put: aNumber! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:18'!
diagonal

	| x |
	x _ self appropriateResultClass newVectorSize: height.
	1 to: height do: [ :i |
		x i: i j: 1 put: (self i: i j: i ) ].
	^x! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:22'!
i: i
	"Answers a row vector with the values at row y"

	| result |
	result _ self appropriateResultClass newRowVectorSize: width.
	1 to: width do: [ :j |
		result i: 1 j: j put: (self i: i j: j) ].
	^result! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 10:27'!
i: i j: j

	"Answer element at row i, column j"

	^ elements at: (self elementsIndexForI: i j: j)! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 10:25'!
i: i j: j put: anObject
	"Store anObject as the element at row i, column j"

	elements at: (self elementsIndexForI: i j: j) put: anObject! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:22'!
i: i put: row
	"aVector must be a row vector, i.e. a matrix with height 1"

	1 to: width do: [ :j |
		self i: i j: j put: (row at: j) ]! !

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

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:19'!
j: j
	"Answers a column vector with the values at column x"

	| result |
	result _ self appropriateResultClass newColumnVectorSize: height.
	1 to: height do: [ :i |
		result i: i j: 1 put: (self i: i j: j) ].
	^result! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:19'!
j: j put: column
	"column can be a column vector (m x 1 matrix), or a kind of Array"

	1 to: height do: [ :i |
		self i: i j: j put: (column at: i) ]! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:27'!
m
	"Usual name in Linear Algebra"

	^height! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 11:27'!
n
	"Usual name in Linear Algebra"

	^width! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 7/19/2012 11:20'!
* aMatrixOrNumber
	"Standard matrix multiplication"
	| result |
	(aMatrixOrNumber is: #Matrix) ifTrue: [
		width = aMatrixOrNumber height ifFalse: [ ^self error: 'Matrix sizes do not match' ].
		result _ self appropriateResultClass newHeight: height width: aMatrixOrNumber width.
		result fillWith: self multipliedBy: aMatrixOrNumber ]
	ifFalse: [
		result _ self copy.
		result replaceValues: [ :value | value * aMatrixOrNumber ] ].
	^result
	! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 7/19/2012 11:20'!
+ aMatrixOrNumber
	"Add element by element"
	| result |
	(aMatrixOrNumber is: #Matrix) ifTrue: [
		(width = aMatrixOrNumber width) & (height = aMatrixOrNumber height)
			ifFalse: [ ^self error: 'Matrix sizes do not match' ].
		result _ self appropriateResultClass newHeight: height width: width.
		result fillWith: self plus: aMatrixOrNumber ]
	ifFalse: [
		result _ self copy.
		result replaceValues: [ :value | value + aMatrixOrNumber ] ].
	^result
	! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 7/19/2012 11:20'!
- aMatrixOrNumber
	"Substract element by element"
	| result |
	(aMatrixOrNumber is: #Matrix) ifTrue: [
		(width = aMatrixOrNumber width) & (height = aMatrixOrNumber height)
			ifFalse: [ ^self error: 'Matrix sizes do not match' ].
		result _ self appropriateResultClass newHeight: height width: width.
		result fillWith: self minus: aMatrixOrNumber ]
	ifFalse: [
		result _ self copy.
		result replaceValues: [ :value | value - aMatrixOrNumber ] ].
	^result
	! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 7/19/2012 12:06'!
/ aNumber
	"Divide by a scalar"
	| reciprocal result |
	reciprocal _ 1 / aNumber.
	result _ self copy.
	result replaceValues: [ :value | value * reciprocal ].
	^result
	! !

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

!Matrix methodsFor: 'copying' stamp: 'jmv 7/19/2012 10:33'!
copy
	"Don't share the elements collection, but use a new copy of it"

	^self copyTwoLevel! !

!Matrix methodsFor: 'filling' stamp: 'jmv 7/19/2012 11:21'!
fillDiagonalWith: aVectorOrArray

	1 to: (width min: height) do: [ :j |
		self i: j j: j put: (aVectorOrArray at: j) ].! !

!Matrix methodsFor: 'filling' stamp: 'jmv 7/19/2012 11:18'!
fillWith: aMatrix

	1 to: (height min: aMatrix height) do: [ :i |
		1 to: (width min: aMatrix width) do: [ :j |
			self i: i j: j put: (aMatrix i: i j: j) ] ]! !

!Matrix methodsFor: 'filling' stamp: 'jmv 7/19/2012 11:19'!
fillWithArrayOfArrays: anArray
	"FloatMatrix fromArrayOfArrays: #(
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

!Matrix methodsFor: 'initialization' stamp: 'jmv 7/19/2012 10:29'!
initializeElements

	elements _ Array new: height * width! !

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

!Matrix methodsFor: 'printing' stamp: 'jmv 7/19/2012 12:11'!
printOn: aStream
	1 to: height do: [ :i |
		aStream nextPutAll: '| '.
		1 to: width do: [ :j |
			aStream print: (self i: i j: j).
			aStream nextPut: $  ].
		aStream nextPut: $|; newLine ]! !

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

!Matrix methodsFor: 'operations' stamp: 'jmv 7/19/2012 12:36'!
inverse
	"Answers the inverse matrix"

	| zeroElement bigMatrix result |
	zeroElement _ self zeroElement.
	self isSquare ifFalse: [ ^self error: 'Only for Square Matrices' ].
	result _ self appropriateResultClass newHeight: height width: width.
	bigMatrix _ self appropriateResultClass newHeight: height width: width + 1.
	1 to: width do: [ :i |
		bigMatrix fillWith: self.
		1 to: width do: [ :j | bigMatrix i: j j: width+1 put: zeroElement ].
		bigMatrix i: i j: width + 1 put: 1.
		result j: i put: bigMatrix solveLinearSystem ].
	^result! !

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

!Matrix methodsFor: 'operations' stamp: 'jmv 7/19/2012 12:17'!
permuteRow: i and: k

	| a b |
	i = k ifFalse: [
		1 to: width do: [ :j |
			a _ self i: k j: j.
			b _ self i: i j: j.
			self i: k j: j put: b.
			self i: i j: j put: a ] ]! !

!Matrix methodsFor: 'operations' stamp: 'jmv 7/19/2012 12:17'!
transpose
	"Traspose the receiver. Modify it."

	| aux |
	1 to: width do: [ :ii |
		ii+1 to: width do: [ :jj |
			aux _ self i: ii j: jj.
			self i: ii j: jj put: (self i: jj j: ii).
			self i: jj j: ii put: (aux) ] ].! !

!Matrix methodsFor: 'operations' stamp: 'jmv 7/19/2012 12:17'!
transposed
	"Answer a new matrix, transposed."

	| result |
	result _ self appropriateResultClass newHeight: width width: height.
	1 to: result height do: [ :i |
		1 to: result width do: [ :j |
			result i: i j: j put: (self i: j j: i) ] ].
	^result! !

!Matrix methodsFor: 'private' stamp: 'jmv 7/19/2012 12:35'!
epsilon
	"Something better is needed. For example, taking into accoung the largest elements, or the norm, or something like that... In any case it is better to check senders, and use a different strategy for ending iterative methods or such...
	Check inheritance too"

	^self zeroElement! !

!Matrix methodsFor: 'testing' stamp: 'jmv 7/19/2012 11:26'!
isSquare

	^height = width! !

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

!Matrix methodsFor: 'aux operations' stamp: 'jmv 7/19/2012 12:04'!
rowWithMaxInColumn: j startingAtRow: jStart

	| rowWithMax |
	rowWithMax _ jStart.
	jStart+1 to: height do: [ :i |
		(self i: i j: j) abs > (self i: rowWithMax j: j) abs ifTrue: [
			rowWithMax _ i ] ].
	^rowWithMax! !

!Matrix methodsFor: 'aux operations' stamp: 'jmv 7/19/2012 12:37'!
substractRow: i multipliedBy: factor to: k startingAtColumn: jStart

	self i: k j: jStart put: self zeroElement..
	jStart+1 to: width do: [ :j |
		self i: k j: j put:
			(self i: k j: j) - ((self i: i j: j) * factor) ]! !


!FloatMatrix methodsFor: 'signal processing' stamp: 'jmv 7/19/2012 11:14'!
convolutionLinearWith: aForm
	"Linear convolution. Warning: Slow.
	It is usually preferred to do a circular convolution via FFT and take care of border effects."
	"
	| f m |
	f _ Form fromUser.
	m _ FloatMatrix m: 3 n: 3.
	m fillGaussian.
	(m convolutionWith: f) display
	"

	| resultWidth resultHeight result xx yy blue c green red k |
	resultWidth _ width + aForm width - 1.
	resultHeight _ height + aForm height - 1.
	k _ 1.0 / elements sum.
	result _ Form extent: resultWidth@resultHeight depth: 32.
	1 to: resultHeight do: [ :y |
		1 to: resultWidth do: [ :x |
			red _ 0.0.
			green _ 0.0.
			blue _ 0.0.
			1 to: height do: [ :j |
				1 to: width do: [ :i |
					xx _ x-i+1.
					yy _ y-j+1.
					(xx > 0) & (xx <= aForm width) & (yy > 0) & (yy <= aForm height)
						ifTrue: [
							c _ aForm colorAt: xx@yy.
							red _ c red * (self i: i j: j) + red.
							green _ c green * (self i: i j: j) + green.
							blue _ c blue * (self i: i j: j) + blue ]]].
			red _ red * k min: 1.0 max: 0.0.
			green _ green * k min: 1.0 max: 0.0.
			blue _ blue * k min: 1.0 max: 0.0.
			result colorAt: x@y put:
				(Color r: red g: green b: blue).
		].
	].
	^result! !

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

!FloatMatrix methodsFor: 'aritmethic - complex' stamp: 'jmv 7/19/2012 11:15'!
imaginary: imaginaryPart elementMultiplyBy: operandRealPart imaginary: operandImaginaryPart
	"The receiver and imaginaryPart conform a complex matrix.
	operandRealPart and operandImaginaryPart conform another complex matrix.
	fill the receiver (and imaginaryPart) with the result of complex multiplication with operand, element by element.
	I.e., at each position of the matrix, compute (e+if) = (a+ib) * (c+id)"

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
			imaginaryPart i: i j: j put: f ]]! !

!FloatMatrix methodsFor: 'aritmethic - complex' stamp: 'jmv 7/19/2012 11:16'!
magnitudeWithImaginary: imaginaryPart
	"answer a new matrix"
	| result a b magnitude |
	result _ FloatMatrix newSize: self size.
	1 to: height do: [ :i |
		1 to: width do: [ :j |
			a _ self i: i j: j.
			b _ imaginaryPart i: i j: j.
			magnitude _ (a * a + (b * b)) sqrt.
			result i: i j: j put: magnitude ]].
	^result! !


!FloatBandMatrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 10:27'!
i: i j: j
	"Answer element at row i, column j"

	(self elementsIndexForI: i j: j)
		ifNotNil: [ :position | elements at: position ]
		ifNil: [ 0.0 ]! !

!FloatBandMatrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 10:26'!
i: i j: j put: aNumber
	"Store aNumber as the element at row i, column j"

	(self elementsIndexForI: i j: j)
		ifNotNil: [ :position | elements at: position put: aNumber ]
		ifNil: [ self error: 'Can not store outside band' ]! !


!Rectangle methodsFor: 'testing' stamp: 'jmv 7/19/2012 12:30'!
isZero
	"Answer whether the receiver is zero"
	Transcript newLine.
	#isZero print.
	thisContext printStack: 5.
	self flag: #jmv. "is this method needed at all?"

	^origin isZero and:[corner isZero]! !

!methodRemoval: Number #isNonZero!
Number removeSelector: #isNonZero!
!methodRemoval: FloatBandMatrix #fillWithIdentity!
FloatBandMatrix removeSelector: #fillWithIdentity!
!methodRemoval: FloatMatrix #copy!
FloatMatrix removeSelector: #copy!
!methodRemoval: FloatMatrix #elements!
FloatMatrix removeSelector: #elements!
!methodRemoval: FloatMatrix #elementsIndexForI:j:!
FloatMatrix removeSelector: #elementsIndexForI:j:!
!methodRemoval: FloatMatrix #i:j:!
FloatMatrix removeSelector: #i:j:!
!methodRemoval: FloatMatrix #i:j:put:!
FloatMatrix removeSelector: #i:j:put:!
!methodRemoval: Matrix #conditionNumber!
Matrix removeSelector: #conditionNumber!
!methodRemoval: Matrix #eigenvaluesAndEigenvectors!
Matrix removeSelector: #eigenvaluesAndEigenvectors!
!methodRemoval: Matrix #eigenvaluesAndEigenvectorsByQR!
Matrix removeSelector: #eigenvaluesAndEigenvectorsByQR!
!methodRemoval: Matrix #eigenvaluesByQR!
Matrix removeSelector: #eigenvaluesByQR!
!methodRemoval: Matrix #hessembergSuperior!
Matrix removeSelector: #hessembergSuperior!
!methodRemoval: Matrix #isFirstSubdiagonalZero!
Matrix removeSelector: #isFirstSubdiagonalZero!
!methodRemoval: Matrix #linesLost!
Matrix removeSelector: #linesLost!
!methodRemoval: Matrix #norm!
Matrix removeSelector: #norm!
!methodRemoval: Matrix #norm2!
Matrix removeSelector: #norm2!
!methodRemoval: Matrix #norm2Squared!
Matrix removeSelector: #norm2Squared!
!methodRemoval: Matrix #preMultiplyByGivensRotationRowI:rowK:titaCosine:titaSine:!
Matrix removeSelector: #preMultiplyByGivensRotationRowI:rowK:titaCosine:titaSine:!
!methodRemoval: Matrix #round!
Matrix removeSelector: #round!
!methodRemoval: Matrix #rowsCloserToZero:!
Matrix removeSelector: #rowsCloserToZero:!
!methodRemoval: Matrix #storeHouseholderTransformOn:column:forQR:!
Matrix removeSelector: #storeHouseholderTransformOn:column:forQR:!
!methodRemoval: Matrix #storeQRDecompositionOfHessemberSuperiorOnQ:r:!
Matrix removeSelector: #storeQRDecompositionOfHessemberSuperiorOnQ:r:!
!classDefinition: #Matrix category: #LinearAlgebra!
Object subclass: #Matrix
	instanceVariableNames: 'width height elements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LinearAlgebra'!

!Matrix reorganize!
('accessing' at: at:put: diagonal elements fillEndsWith1Derivative fillEndsWith2Derivative2 height i: i:j: i:j:put: i:put: i:upTo: iExtended: j: j:i: j:put: m n scalarSize size subMatrixTopLeft:bottomRight: subMatrixTopLeft:size: width)
('aritmethic' * + - / fillWith:minus: fillWith:multipliedBy: fillWith:plus:)
('copying' copy)
('filling' fillDiagonalWith: fillWith: fillWithArrayOfArrays: fillWithIdentity)
('initialization' initHeight:width: initializeElements)
('linear equation systems' solveLinearSystem triangleWithPartialPivoting)
('misc' appropriateResultClass replaceValues: withIndexesDo:)
('printing' print printOn:)
('rounding')
('operations' determinant inverse moveOriginToCenter: normalizeColumns permuteRow:and: transpose transposed)
('conversion' asForm asFormG:b: asFormG:b:min:max: asFormMin:max:)
('private' elementsIndexForI:j: epsilon zeroElement)
('testing' isSquare)
('aux operations' columnsWithoutPivot: rowWithMaxInColumn:startingAtRow: substractRow:multipliedBy:to:startingAtColumn:)
!

!classRemoval: #ObjectMatrix!
Smalltalk removeClassNamed: #ObjectMatrix!
