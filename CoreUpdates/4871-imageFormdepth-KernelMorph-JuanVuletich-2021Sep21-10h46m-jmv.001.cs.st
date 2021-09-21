'From Cuis 5.0 [latest update: #4870] on 21 September 2021 at 10:47:45 am'!

!KernelMorph methodsFor: 'drawing' stamp: 'jmv 9/21/2021 10:47:14'!
imageForm: extentOrNil depth: depth

	| answerExtent answer auxCanvas |
	self requiresVectorCanvas ifFalse: [	
		answerExtent _ extent.
		extentOrNil ifNotNil: [ answerExtent _ answerExtent min: extentOrNil * 4 ].
		auxCanvas _ BitBltCanvas depth: depth over: (self morphPosition floor extent: answerExtent ceiling).
		auxCanvas fullDraw: self.
		answer _ auxCanvas form divideByAlpha.
		extentOrNil ifNotNil: [ answer _ answer magnifyTo: extentOrNil ].
		^answer ].
	^super imageForm: extentOrNil depth: depth.! !

