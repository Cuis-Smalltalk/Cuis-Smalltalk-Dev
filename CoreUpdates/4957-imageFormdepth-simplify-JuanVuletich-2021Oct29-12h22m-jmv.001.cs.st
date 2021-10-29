'From Cuis 5.0 [latest update: #4956] on 29 October 2021 at 12:23:47 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 10/29/2021 12:22:53'!
imageForm: extentOrNil depth: depth

	| answerExtent answer auxCanvas |
	self requiresVectorCanvas ifTrue: [	
		^ self imageFormVG: extentOrNil depth: depth ].

	answerExtent _ self findFullBoundsInOwner extent.
	extentOrNil ifNotNil: [ answerExtent _ answerExtent min: extentOrNil * 4 ].
	auxCanvas _ MorphicCanvas depth: depth over: (self morphPosition floor extent: answerExtent ceiling).
	auxCanvas fullDraw: self.
	answer _ auxCanvas form divideByAlpha.
	extentOrNil ifNotNil: [ answer _ answer magnifyTo: extentOrNil ].
	^answer! !

!methodRemoval: WidgetMorph #imageForm:depth: stamp: 'jmv 10/29/2021 12:23:11'!
WidgetMorph removeSelector: #imageForm:depth:!
!methodRemoval: KernelMorph #imageForm:depth: stamp: 'jmv 10/29/2021 12:23:09'!
KernelMorph removeSelector: #imageForm:depth:!
