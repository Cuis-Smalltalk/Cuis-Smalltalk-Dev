'From Cuis 4.1 of 12 December 2012 [latest update: #1613] on 23 February 2013 at 10:02:55 pm'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 2/23/2013 21:57'!
grabMorph: aMorph from: formerOwner at: aPoint
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	self releaseMouseFocus. "Break focus"
	self addMorphBack: aMorph.
	aMorph morphPosition: aPoint.
	aMorph justGrabbedFrom: formerOwner.! !


!Morph methodsFor: 'meta-actions' stamp: 'jmv 2/23/2013 22:00'!
duplicateMorph: evt
	"Make and return a duplicate of the receiver's argument"
	| dup |
	dup _ self duplicate.
	evt hand grabMorph: dup from: owner at: (self morphPositionInWorld - self morphPositionInWorld).
	^dup! !


!HandMorph methodsFor: 'meta-actions' stamp: 'jmv 2/23/2013 21:58'!
grabMorph: aMorph
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."
	| grabbed |
	self releaseMouseFocus. "Break focus"
	grabbed _ aMorph aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^self ].
	aMorph owner ifNotNil: [ :o | grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^self ].
	^self grabMorph: grabbed from: grabbed owner at: (aMorph morphPositionInWorld - self morphPositionInWorld)! !


!MenuItemMorph methodsFor: 'grabbing' stamp: 'jmv 2/23/2013 21:59'!
duplicateMorph: evt
	"Make and return a duplicate of the receiver's argument"
	| dup menu |
	dup _ self duplicate isSelected: false.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: dup.
	menu morphBoundsInWorld: self morphBoundsInWorld.
	menu stayUp.
	evt hand grabMorph: menu from: owner at: (menu morphPositionInWorld - self morphPositionInWorld).
	^menu! !

!methodRemoval: HandMorph #grabMorph:from:!
HandMorph removeSelector: #grabMorph:from:!
