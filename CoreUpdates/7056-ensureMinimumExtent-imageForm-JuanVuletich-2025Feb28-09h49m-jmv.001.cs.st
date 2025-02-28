'From Cuis7.3 [latest update: #7055] on 28 February 2025 at 9:50:24 am'!

!Morph methodsFor: 'drawing' stamp: 'jmv 2/28/2025 09:32:42'!
imageForm
	^self imageForm: 32! !

!Morph methodsFor: 'geometry' stamp: 'jmv 2/28/2025 09:28:15'!
ensureMinimumExtent
	"We can't change extent. NOP here.
	See inheritance."! !


!BoxMorph methodsFor: 'geometry' stamp: 'jmv 2/28/2025 09:47:42'!
ensureMinimumExtent
	"Make our extent no smaller than #minimumExtent"

	self morphExtent:
		(extent
			ifNotNil: [extent max: self minimumExtent]
			ifNil: [self minimumExtent]).! !


!Morph methodsFor: 'drawing' stamp: 'jmv 2/28/2025 09:31:12'!
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
	answer := auxCanvas form divideByAlpha.
	extentOrNil ifNotNil: [ answer := answer magnifyTo: extentOrNil ].
	^answer! !

