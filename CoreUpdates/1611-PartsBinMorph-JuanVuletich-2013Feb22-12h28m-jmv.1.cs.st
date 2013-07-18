'From Cuis 4.1 of 12 December 2012 [latest update: #1610] on 22 February 2013 at 12:51:41 pm'!
!classDefinition: #PartsBinMorph category: #'Morphic-Widgets'!
DragAndDropAreaMorph subclass: #PartsBinMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PartsBinMorph commentStamp: '<historical>' prior: 0!
Like DragAndDropAreaMorph, but when submorphs are dragged, get a copy and not the original one.

| m |
m _ PartsBinMorph new.
m openInWorld.
m addMorph: EllipseMorph new.
m morphExtent: 120@240!


!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 2/22/2013 12:48'!
aboutToGrab: submorph
	"submorph is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."

	^submorph "Grab it"! !


!PartsBinMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 2/22/2013 12:49'!
aboutToGrab: aMorph
	"submorph is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."

	^aMorph copy! !


!HandMorph methodsFor: 'meta-actions' stamp: 'jmv 2/22/2013 12:49'!
grabMorph: aMorph
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."
	| grabbed |
	self releaseMouseFocus. "Break focus"
	grabbed _ aMorph aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^self ].
	aMorph owner ifNotNil: [ :o | grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^self ].
	^self grabMorph: grabbed from: grabbed owner! !


!PartsBinMorph reorganize!
('dropping/grabbing' aboutToGrab:)
!

