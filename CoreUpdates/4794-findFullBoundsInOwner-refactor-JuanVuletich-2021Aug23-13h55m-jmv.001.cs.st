'From Cuis 5.0 [latest update: #4793] on 23 August 2021 at 1:58:19 pm'!

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/23/2021 13:55:11'!
findFullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!!"

	self requiresVectorCanvas ifTrue: [
		^super findFullBoundsInOwner ].
	
	"#morphExtent exists, and it is also valid in owner, because #requiresVectorCanvas is false."
	^(self morphPosition extent: self morphExtent) encompassingIntegerRectangle.! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/23/2021 13:55:25'!
findFullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!!"

	self requiresVectorCanvas ifTrue: [
		^super findFullBoundsInOwner ].
	
	"#morphExtent exists, and it is also valid in owner, because #requiresVectorCanvas is false."
	^(self morphPosition extent: self morphExtent) encompassingIntegerRectangle.! !


!Morph methodsFor: 'geometry' stamp: 'jmv 8/23/2021 13:57:28'!
findFullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!!
	Subclasses need to redefine this method.
	Alternatively, VectorGraphics provides a working implementation as an override."

	self subclassResponsibility.! !

