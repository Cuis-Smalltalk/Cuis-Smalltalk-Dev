'From Cuis7.3 [latest update: #7131] on 11 April 2025 at 3:11:56 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 4/11/2025 15:10:16'!
imageForm: extentOrNil depth: depth

	| answerExtent answer auxCanvas bounds |
	self ensureMinimumExtent.

	self requiresVectorCanvas ifTrue: [	
		^ self imageFormVG: extentOrNil depth: depth ].

	bounds := self fullBoundsInOwner.
	answerExtent := bounds extent.
	extentOrNil ifNotNil: [ answerExtent := answerExtent min: extentOrNil * 4 ].
	auxCanvas := MorphicCanvas depth: depth over: (bounds origin extent: answerExtent ceiling).
	auxCanvas fullDraw: self.
	answer := auxCanvas form.
	"answer divideByAlpha."
	answer makeAllPixelsOpaque.
	extentOrNil ifNotNil: [ answer := answer magnifyTo: extentOrNil ].
	^answer! !

