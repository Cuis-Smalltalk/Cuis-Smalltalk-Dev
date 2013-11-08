'From Cuis 4.2 of 25 July 2013 [latest update: #1862] on 5 November 2013 at 2:36:20.80602 pm'!

!Morph methodsFor: 'meta-actions' stamp: 'jmv 11/5/2013 14:34'!
duplicateMorph: evt
	"Make and return a duplicate of the receiver's argument"
	| dup h |
	dup _ self duplicate.
	h _ evt hand.
	h
		grabMorph: dup
		from: owner
		at: self morphPositionInWorld - h morphPositionInWorld.
	^ dup! !


!HandMorph methodsFor: 'meta-actions' stamp: 'jmv 11/5/2013 14:35'!
grabMorph: aMorph
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."
	| grabbed |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	aMorph owner ifNotNil: [ :o |
		grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	^ self
		grabMorph: grabbed
		from: grabbed owner
		at: aMorph morphPositionInWorld - self morphPositionInWorld! !


!MenuItemMorph methodsFor: 'grabbing' stamp: 'jmv 11/5/2013 14:35'!
duplicateMorph: evt
	"Make and return a duplicate of the receiver's argument"
	| dup menu |
	dup _ self duplicate isSelected: false.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: dup.
	menu morphBoundsInWorld: self morphBoundsInWorld.
	menu stayUp.
	evt hand
		grabMorph: menu
		from: owner
		at: menu morphPositionInWorld - self morphPositionInWorld.
	^ menu! !

