'From Cuis 4.1 of 12 December 2012 [latest update: #1642] on 24 March 2013 at 10:53:16 am'!
!classDefinition: #MatrixTransform2x3 category: #'Morphic-Kernel'!
Object variableWordSubclass: #MatrixTransform2x3
	instanceVariableNames: ''
	classVariableNames: 'DoIntegerTransformations '
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 3/24/2013 10:47'!
floatInverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Smalltalk code for Float conversion."
	| x y det a11 a12 a21 a22 detX detY |

	x _ aPoint x asFloat - (self a13).
	y _ aPoint y asFloat - (self a23).
	a11 _ self a11.
	a12 _ self a12.
	a21 _ self a21.
	a22 _ self a22.
	det _ (a11 * a22) - (a12 * a21).
	det = 0.0 ifTrue: [ ^0@0 ].		"So we have at least a valid result"
	det _ 1.0 / det.
	detX _ (x * a22) - (a12 * y).
	detY _ (a11 * y) - (x * a21).
	^(detX * det) @ (detY * det)! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 3/24/2013 10:47'!
floatTransform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Smalltalk code for Float conversion."

	| x y |
	x _ (aPoint x * self a11) + (aPoint y * self a12) + self a13.
	y _ (aPoint x * self a21) + (aPoint y * self a22) + self a23.
	^x @ y! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 3/24/2013 10:48'!
intInverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Primitive rounds and answers integers. Do the same in Smalltalk fallback code"

	<primitive: 'primitiveInvertPoint' module: 'Matrix2x3Plugin'>

	^(self floatInverseTransform: aPoint) rounded! !

!MatrixTransform2x3 methodsFor: 'private - primitives' stamp: 'jmv 3/24/2013 10:49'!
intTransform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Primitive rounds and answers integers. Do the same in Smalltalk fallback code"

	<primitive: 'primitiveTransformPoint' module: 'Matrix2x3Plugin'>

	^(self floatTransform: aPoint) rounded! !


!MatrixTransform2x3 class methodsFor: 'class initialization' stamp: 'jmv 3/24/2013 10:52'!
initialize
	DoIntegerTransformations _ true! !


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 3/24/2013 10:51'!
inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint."

	DoIntegerTransformations ifNil: [ DoIntegerTransformations _ false ].
	^DoIntegerTransformations
		ifTrue: [ self intInverseTransform: aPoint ]
		ifFalse: [ self floatInverseTransform: aPoint ]! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 3/24/2013 10:51'!
transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint."

	DoIntegerTransformations ifNil: [ DoIntegerTransformations _ false ].
	^DoIntegerTransformations
		ifTrue: [ self intTransform: aPoint ]
		ifFalse: [ self floatTransform: aPoint ]! !

MatrixTransform2x3 initialize!

!MatrixTransform2x3 class reorganize!
('instance creation' forScaleFactor:originalExtent: identity new newFromStream: transformFromLocal:toExternal: withPosition: withRadians: withRadians:scale:position: withScale: withTranslation:)
('class initialization' initialize)
!

!classDefinition: #MatrixTransform2x3 category: #'Morphic-Kernel'!
Object variableWordSubclass: #MatrixTransform2x3
	instanceVariableNames: ''
	classVariableNames: 'DoIntegerTransformations'
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
