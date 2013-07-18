'From Cuis 4.1 of 12 December 2012 [latest update: #1650] on 1 April 2013 at 2:05:22 pm'!

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/1/2013 14:02'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	if DoIntegerTransformations, answer rounded integer values, for better performance. "

	DoIntegerTransformations ifTrue: [
		(self intInverseTransform: aPoint) ifNotNil: [ :answer |
			answer > (0@0)
				ifTrue: [ ^answer ]].
		^ (self floatInverseTransform: aPoint) rounded ].

	^ self floatInverseTransform: aPoint! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 4/1/2013 14:02'!
transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	if DoIntegerTransformations, answer rounded integer values, for better performance."

	DoIntegerTransformations ifTrue: [
		(self intTransform: aPoint) ifNotNil: [ :answer |
			answer > (0@0)
				ifTrue: [ ^answer ]].
		^ (self floatTransform: aPoint) rounded ].
	
	^ self floatTransform: aPoint! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 4/1/2013 14:04'!
intInverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	MatrixTransform2x3 identity intInverseTransform: (-2@ 2)
	"

	<primitive: 'primitiveInvertPoint' module: 'Matrix2x3Plugin'>

	^nil! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 4/1/2013 14:03'!
intTransform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Primitive rounds and answers integers.
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	MatrixTransform2x3 identity intTransform: (-2@ 2)
	"

	<primitive: 'primitiveTransformPoint' module: 'Matrix2x3Plugin'>

	^nil! !

