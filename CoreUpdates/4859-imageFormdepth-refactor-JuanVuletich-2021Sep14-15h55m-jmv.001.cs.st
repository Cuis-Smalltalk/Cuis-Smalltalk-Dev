'From Cuis 5.0 [latest update: #4858] on 14 September 2021 at 3:57:49 pm'!

!WidgetMorph methodsFor: 'drawing' stamp: 'jmv 9/13/2021 17:26:04'!
imageForm: extentOrNil depth: depth

	| answerExtent answer auxCanvas |
	self requiresVectorCanvas ifFalse: [	
		answerExtent _ extent.
		extentOrNil ifNotNil: [ answerExtent _ answerExtent min: extentOrNil * 4 ].
		auxCanvas _ MorphicCanvas depth: depth over: (self morphPosition floor extent: answerExtent ceiling).
		auxCanvas fullDraw: self.
		answer _ auxCanvas form divideByAlpha.
		extentOrNil ifNotNil: [ answer _ answer magnifyTo: extentOrNil ].
		^answer ].
	^super imageForm: extentOrNil depth: depth.! !


!Morph methodsFor: 'drawing' stamp: 'jmv 9/13/2021 17:26:23'!
imageForm: extentOrNil depth: depth

	self subclassResponsibility! !

!methodRemoval: MovableMorph #privateLocation: stamp: 'jmv 9/14/2021 15:55:23'!
MovableMorph removeSelector: #privateLocation:!
!methodRemoval: Morph #privateLocation: stamp: 'jmv 9/14/2021 15:55:22'!
Morph removeSelector: #privateLocation:!
