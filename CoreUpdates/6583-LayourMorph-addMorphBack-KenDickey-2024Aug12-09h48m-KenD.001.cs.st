'From Cuis7.1 [latest update: #6582] on 12 August 2024 at 9:49:09 am'!

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'KenD 8/11/2024 07:22:55'!
addMorphBack: aMorph
	"Add a submorph, at the top or left, with a default LayoutSpec if none was provided."

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec.
	super addMorphBack: aMorph.! !


!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/12/2024 09:46:40'!
addMorph: aMorph layoutSpec: aLayoutSpec
	"Add a submorph, at the bottom or right, with aLayoutSpec"

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec: aLayoutSpec.
	self addMorphFront: aMorph.! !

