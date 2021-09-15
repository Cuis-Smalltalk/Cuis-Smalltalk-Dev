'From Cuis 5.0 [latest update: #4861] on 15 September 2021 at 9:07:29 am'!

!WidgetMorph methodsFor: 'drawing' stamp: 'jmv 9/15/2021 08:48:04'!
imageForm: extentOrNil depth: depth

	| answerExtent answer auxCanvas |
	self requiresVectorCanvas ifFalse: [	
		answerExtent _ extent.
		extentOrNil ifNotNil: [ answerExtent _ answerExtent min: extentOrNil * 4 ].
		"To avoid slower Smalltalk VG engine just because of window decorations"
		auxCanvas _ BitBltCanvas depth: depth over: (self morphPosition floor extent: answerExtent ceiling).
		auxCanvas fullDraw: self.
		answer _ auxCanvas form divideByAlpha.
		extentOrNil ifNotNil: [ answer _ answer magnifyTo: extentOrNil ].
		^answer ].
	^super imageForm: extentOrNil depth: depth.! !

