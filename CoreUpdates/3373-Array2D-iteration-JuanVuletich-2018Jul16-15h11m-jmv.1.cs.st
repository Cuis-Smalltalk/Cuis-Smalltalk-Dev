'From Cuis 5.0 of 7 November 2016 [latest update: #3372] on 16 July 2018 at 4:24:19 pm'!

!Array2D methodsFor: 'iterating' stamp: 'jmv 7/16/2018 15:12:46'!
do: aBlock
	"Evaluate aBlock on each element"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: (self i: i j: j) ] ]! !

!Array2D methodsFor: 'iterating' stamp: 'jmv 7/16/2018 16:14:30'!
with: otherImage do: aBlock
	"Evaluate aBlock on each element"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: (self i: i j: j) value: (otherImage i: i j: j) ] ]! !


!Array2D methodsFor: 'iterating' stamp: 'jmv 7/16/2018 15:11:48'!
withIndexesDo: aBlock
	"Evaluate aBlock on each element, including i, j indexes also as arguments"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: i value: j value: (self i: i j: j) ] ]! !

