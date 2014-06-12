'From Cuis 4.2 of 25 July 2013 [latest update: #2017] on 8 June 2014 at 7:34:36.600455 pm'!

!Object methodsFor: 'morphic' stamp: 'jmv 6/8/2014 18:27'!
activeHand
	"Answer a hand for the morphic world that is the current UI focus.
	This is the UI root animated by the active Process.
	This method could answer nil, if not in an UI process!!"

	^self runningWorld ifNotNil: [ :w | w activeHand ]! !


!PasteUpMorph methodsFor: 'object serialization' stamp: 'jmv 6/8/2014 18:28'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	| dp |
	dp _ DiskProxy global: #Smalltalk selector: #runningWorld args: #().
	refStrm replace: self with: dp.
	^ dp! !


!HandMorph methodsFor: 'meta-actions' stamp: 'jmv 6/8/2014 19:11'!
grabMorph: aMorph positionInWorld: posOrNil
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand.
	Use posOrNil if specified."
	| grabbed pos |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	pos _ posOrNil ifNil: [ aMorph morphPositionInWorld ].
	aMorph owner ifNotNil: [ :o |
		grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	^ self
		grabMorph: grabbed
		from: aMorph owner
		at: pos-self morphPositionInWorld! !


!Morph methodsFor: 'meta-actions' stamp: 'jmv 6/8/2014 18:38'!
duplicateMorph: evt
	"Make and return a duplicate of the receiver's argument"

	^self duplicate! !


!HaloMorph methodsFor: 'accessing' stamp: 'jmv 6/8/2014 18:37'!
target: aMorph

	target _ aMorph.
	target ifNotNil: [ self addHandles ]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 6/8/2014 19:08'!
doDup: evt with: dupHandle 
	"Ask hand to duplicate my target."

	| h pos |
	h _ evt hand.
	h obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	pos _ target morphPositionInWorld.
	target _ target duplicateMorph: evt.
	h grabMorph: target positionInWorld: pos.
	self updatePositionAndExtent.	"update position if necessary"! !


!HandMorph methodsFor: 'meta-actions' stamp: 'jmv 6/8/2014 19:09'!
grabMorph: aMorph
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."
	
	self grabMorph: aMorph positionInWorld: nil! !

!HandMorph methodsFor: 'objects from disk' stamp: 'jmv 6/8/2014 18:28'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	| dp |
	dp _ DiskProxy global: #Smalltalk selector: #activeHand args: #().
	refStrm replace: self with: dp.
	^ dp! !


!MenuItemMorph methodsFor: 'grabbing' stamp: 'jmv 6/8/2014 19:27'!
aboutToBeGrabbedBy: aHand
	"Don't allow the receiver to act outside a Menu"
	| menu |
	self isSelected: false.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: self.
	menu adjustSubmorphsLayout.
	menu stayUp.
	^ menu! !

!MenuItemMorph methodsFor: 'grabbing' stamp: 'jmv 6/8/2014 19:27'!
duplicateMorph: evt
	"Don't allow the receiver to act outside a Menu"
	| dup menu |
	dup _ self duplicate isSelected: false.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: dup.
	menu adjustSubmorphsLayout.
	menu stayUp.
	^ menu! !

!methodRemoval: HaloMorph #setTarget:!
HaloMorph removeSelector: #setTarget:!
