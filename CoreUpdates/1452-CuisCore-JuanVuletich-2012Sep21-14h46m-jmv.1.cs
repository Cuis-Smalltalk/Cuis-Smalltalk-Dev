'From Cuis 4.0 of 21 April 2012 [latest update: #1450] on 21 September 2012 at 2:53:57 pm'!
!classDefinition: #HandleMorph category: #'Morphic-Widgets'!
EllipseMorph subclass: #HandleMorph
	instanceVariableNames: 'pointBlock lastPointBlock '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/21/2012 14:49'!
addNameBeneath: outerRectangle string: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition w nameBackground |
	w _ self world ifNil: [ target world ].
	nameBackground _ RectangleLikeMorph new
		color: (Color lightBlue alpha: 0.9).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color magenta.
	self addMorph: nameBackground.
	self addMorph: nameMorph.
	namePosition _ outerRectangle bottomCenter - ((nameMorph morphWidth // 2) @ (self handleSize negated // 2 - 1)).
	nameMorph morphPosition: (namePosition min: w viewBox bottomRight - nameMorph morphHeight + 5).
	nameBackground morphPositionInOwner: nameMorph morphPositionInOwner - 2.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	^nameMorph! !

!methodRemoval: HandleMorph #justDroppedInto:event:!
HandleMorph removeSelector: #justDroppedInto:event:!
!classDefinition: #HandleMorph category: #'Morphic-Widgets'!
EllipseMorph subclass: #HandleMorph
	instanceVariableNames: 'pointBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!HandleMorph reorganize!
('events' keyStroke:)
('initialization' initialize)
('initialize' forEachPointDo:)
('stepping and presenter' step)
('testing' stepTime)
!

