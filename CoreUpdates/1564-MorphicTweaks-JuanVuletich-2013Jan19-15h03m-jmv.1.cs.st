'From Cuis 4.1 of 12 December 2012 [latest update: #1563] on 19 January 2013 at 3:06:02 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 1/19/2013 15:04'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"

	"Draw receiver itself"
	(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [
		aCanvas clipBy: self morphBoundsInWorld during: [ :c | self drawOn: c ]].	"Needs bounds 'in owner' if inside a scroller"
	self drawSubmorphsOn: aCanvas.
	self drawMouseDownHighlightOn: aCanvas! !

!methodRemoval: Morph #drawDropHighlightOn:!
Morph removeSelector: #drawDropHighlightOn:!
!methodRemoval: Morph #highlightForDrop!
Morph removeSelector: #highlightForDrop!
!methodRemoval: Morph #highlightForDrop:!
Morph removeSelector: #highlightForDrop:!
!methodRemoval: Morph #highlightedForDrop!
Morph removeSelector: #highlightedForDrop!
!methodRemoval: Morph #resetHighlightForDrop!
Morph removeSelector: #resetHighlightForDrop!
