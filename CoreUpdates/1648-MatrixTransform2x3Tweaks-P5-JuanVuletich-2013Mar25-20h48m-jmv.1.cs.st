'From Cuis 4.1 of 12 December 2012 [latest update: #1647] on 26 March 2013 at 12:21:18 am'!
!classDefinition: #MatrixTransform2x3 category: #'Morphic-Kernel'!
Object variableWordSubclass: #MatrixTransform2x3
	instanceVariableNames: ''
	classVariableNames: 'DoIntegerTransformations DoAutoHint '
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 3/26/2013 00:07'!
autoHint: p
	"Round p for text autoHinting in Morphic 3.
	Make y an integer + 0.5. This is because our fonts (and maybe most of them)
	use fills with zero width borders.
	Leave x alone."

	| answerY |
	answerY _ (p y - 0.5) rounded + 0.5.
	^p x @ answerY! !

!MatrixTransform2x3 methodsFor: 'testing' stamp: 'jmv 3/25/2013 23:25'!
doesNotRotate
	"Answer true if #radians would answer 0.0. Performance optimization."

	^self a21 = 0.0 and: [ self a11 sign >= 0 ]! !


!MatrixTransform2x3 class methodsFor: 'class initialization' stamp: 'jmv 3/25/2013 20:55'!
doAutoHinting: aBoolean

	DoAutoHint _ aBoolean! !


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 3/25/2013 20:54'!
externalizePosition: aPoint
	"Answer coordinates for aPoint in the space we are in.
	 aPoint is expressed in us."

	^ DoAutoHint
		ifTrue: [ self autoHint:( self transform: aPoint) ]
		ifFalse: [ self transform: aPoint ]! !


!MatrixTransform2x3 class methodsFor: 'class initialization' stamp: 'jmv 3/25/2013 20:50'!
initialize
	DoIntegerTransformations _ true.
	DoAutoHint _ false! !

MatrixTransform2x3 initialize!
!classDefinition: #MatrixTransform2x3 category: #'Morphic-Kernel'!
Object variableWordSubclass: #MatrixTransform2x3
	instanceVariableNames: ''
	classVariableNames: 'DoAutoHint DoIntegerTransformations'
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
