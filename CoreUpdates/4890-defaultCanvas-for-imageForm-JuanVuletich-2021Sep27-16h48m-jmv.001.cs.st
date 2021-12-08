'From Cuis 5.0 [latest update: #4889] on 27 September 2021 at 5:22:48 pm'!

!KernelMorph methodsFor: 'drawing' stamp: 'jmv 9/27/2021 17:20:50'!
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


!WidgetMorph methodsFor: 'drawing' stamp: 'jmv 9/27/2021 17:20:55'!
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

