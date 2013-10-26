'From Cuis 4.0 of 21 April 2012 [latest update: #1485] on 16 November 2012 at 4:08:21 pm'!

!Matrix methodsFor: 'accessing' stamp: 'jmv 11/16/2012 15:30'!
x: x y: y
	"Answer element at column x, row y"
	^ self i: y j: x! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 11/16/2012 15:31'!
x: x y: y put: aNumber
	"Answer element at column x, row y"
	^ self i: y j: x put: aNumber! !


!FFT2D methodsFor: 'computing' stamp: 'jmv 11/16/2012 15:33'!
transformColumns: forward
	| transform height realColumn imagColumn |
	height _ realMatrix height.
	transform _ FFT new: height.
	realColumn _ FloatArray new: height.
	imagColumn _ FloatArray new: height.
	1 to: realMatrix width do: [ :x |
		1 to: height do: [ :y |
			realColumn at: y put: (realMatrix x: x y: y)].
		imagMatrix ifNotNil: [
			1 to: height do: [ :y |
				imagColumn at: y put: (imagMatrix x: x y: y)]].
		transform realData: realColumn imagData: imagColumn.
		transform transformForward: forward.
		1 to: height do: [ :y |
			realMatrix x: x y: y put: (realColumn at: y)].
		imagMatrix ifNotNil: [
			1 to: height do: [ :y |
				imagMatrix x: x y: y put: (imagColumn at: y)]]]! !

!FFT2D methodsFor: 'computing' stamp: 'jmv 11/16/2012 15:35'!
transformRows: forward
	| transform width realRow imagRow |
	width _ realMatrix width.
	transform _ FFT new: width.
	realRow _ FloatArray new: width.
	imagRow _ FloatArray new: width.
	1 to: realMatrix height do: [ :y |
		1 to: width do: [ :x |
			realRow at: x put: (realMatrix x: x y: y)].
		imagMatrix ifNotNil: [
			1 to: width do: [ :x |
				imagRow at: x put: (imagMatrix x: x y: y)]].
		transform realData: realRow imagData: imagRow.
		transform transformForward: forward.
		1 to: width do: [ :x |
			realMatrix x: x y: y put: (realRow at: x)].
		imagMatrix ifNotNil: [
			1 to: width do: [ :x |
				imagMatrix x: x y: y put: (imagRow at: x)]]]! !


!Matrix methodsFor: 'aritmethic' stamp: 'jmv 11/16/2012 12:14'!
* aMatrixOrNumber
	"Standard matrix multiplication"
	| result |
	(aMatrixOrNumber is: #Matrix) ifTrue: [
		width = aMatrixOrNumber height ifFalse: [ ^self error: 'Matrix sizes do not match' ].
		result _ self appropriateResultClass newHeight: height width: aMatrixOrNumber width.
		result fillWith: self multipliedBy: aMatrixOrNumber ]
	ifFalse: [
		result _ self copy.
		result replaceValues: [ :i :j :value | value * aMatrixOrNumber ] ].
	^result
	! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 11/16/2012 12:14'!
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
		result replaceValues: [ :i :j :value | value + aMatrixOrNumber ] ].
	^result
	! !

!Matrix methodsFor: 'aritmethic' stamp: 'jmv 11/16/2012 12:15'!
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

!Matrix methodsFor: 'misc' stamp: 'jmv 11/16/2012 12:14'!
replaceValues: aBlock
	"Replace each value with the result of evaluating aBlock, with i, j and the previous value as the arguments"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			self i: i j: j put:
				(aBlock value: i value: j value: (self i: i j: j)) ] ]! !


!FloatMatrix methodsFor: 'misc' stamp: 'jmv 11/16/2012 12:14'!
addNormalNoise: standardDeviation seed: randomSeed
	| random |
	random _ NormalRandom new seed: randomSeed.
	self replaceValues: [ :i :j :v | random next * standardDeviation + v ]! !

