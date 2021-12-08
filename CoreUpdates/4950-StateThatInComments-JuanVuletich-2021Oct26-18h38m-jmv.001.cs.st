'From Cuis 5.0 [latest update: #4948] on 26 October 2021 at 6:44:05 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 10/26/2021 18:41:50'!
fullBoundsInOwner
	"Answer fullBoundsInOwner. Cache the found bounds.
	Note: Answers an integer rectangle"

	(self valueOfProperty: #fullBoundsInOwner) ifNotNil: [ :fullBoundsInOwner |
		^fullBoundsInOwner ].

	^self setProperty: #fullBoundsInOwner toValue: self findFullBoundsInOwner.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/26/2021 18:42:22'!
morphExtentInOwner
	"Note: Answers an integer point"

	^self fullBoundsInOwner extent! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/26/2021 18:43:05'!
topLeftInOwner
	"Note: Answers an integer rectangle"

	^self fullBoundsInOwner origin! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 10/26/2021 18:41:42'!
findFullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!!
	Note: Answers an integer rectangle"

	self requiresVectorCanvas ifTrue: [
		^super findFullBoundsInOwner ].
	
	"#morphExtent exists, and it is also valid in owner, because #requiresVectorCanvas is false."
	^(self morphPosition extent: self morphExtent) encompassingIntegerRectangle.! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 10/26/2021 18:41:46'!
findFullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!!
	Note: Answers an integer rectangle"

	self requiresVectorCanvas ifTrue: [
		^super findFullBoundsInOwner ].
	
	"#morphExtent exists, and it is also valid in owner, because #requiresVectorCanvas is false."
	^(self morphPosition extent: self morphExtent) encompassingIntegerRectangle.! !

