'From Cuis 6.0 [latest update: #5981] on 17 August 2023 at 9:47:38 am'!

!Morph methodsFor: 'testing' stamp: 'jmv 8/17/2023 09:31:53'!
isPlacedMorph
	"Meaning that:
	- we can moved around by the user or by code
	- we can be scaled
	- maybe we can be resized
	This all means, for instance, that we can be added to LayoutMorphs."

	^false! !


!PlacedMorph methodsFor: 'testing' stamp: 'jmv 8/17/2023 09:32:04'!
isPlacedMorph
	"Meaning that:
	- we can moved around by the user or by code
	- we can be scaled
	- maybe we can be resized
	This all means, for instance, that we can be added to LayoutMorphs."

	^true! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 8/17/2023 09:35:25'!
addMorphFrontFromWorldPosition: aMorph

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec.
	self addMorphFront: aMorph.
	self layoutSubmorphs.
! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/17/2023 09:34:39'!
addMorph: aMorph
	"Add a submorph, at the bottom or right, with a default LayoutSpec if none was provided."

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec.
	super addMorphFront: aMorph! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/17/2023 09:39:56'!
addMorph: aMorph layoutSpec: aLayoutSpec
	"Add a submorph, at the bottom or right, with aLayoutSpec"

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec: aLayoutSpec.
	super addMorphFront: aMorph.! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/17/2023 09:35:21'!
addMorphFront: aMorph
	"Add a submorph, at the bottom or right, with a default LayoutSpec if none was provided."

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec.
	super addMorphFront: aMorph! !

!LayoutMorph methodsFor: 'private' stamp: 'jmv 8/17/2023 09:37:31'!
privateAddAllMorphs: aCollection atIndex: index

	aCollection do: [ :m |
		m isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
		m layoutSpec ].
	^ super privateAddAllMorphs: aCollection atIndex: index.! !

!LayoutMorph methodsFor: 'private' stamp: 'jmv 8/17/2023 09:38:32'!
privateAddMorph: aMorph atIndex: index position: aPoint

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec.
	^ super privateAddMorph: aMorph atIndex: index position: aPoint.! !


!InnerTextMorph methodsFor: 'anchors' stamp: 'jmv 8/17/2023 09:44:45'!
anchorMorph: aMorph at: aPoint
	| relPt |
	aMorph owner == self ifTrue: [ self removeMorph: aMorph ].
	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Texts' ].
	self addMorphFront: aMorph.
	relPt := aPoint - self morphPositionInWorld.
	editor insertMorph: aMorph at: relPt.
	self fit.! !

