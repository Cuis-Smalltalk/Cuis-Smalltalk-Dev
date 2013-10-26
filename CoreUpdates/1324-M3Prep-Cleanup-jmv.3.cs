'From Cuis 4.0 of 3 April 2012 [latest update: #1255] on 10 April 2012 at 3:16:49 pm'!

!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/16/2011 15:24'!
aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand."

	^self "Grab me"! !

!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/16/2011 15:23'!
justDroppedInto: aMorph event: anEvent 
	"This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph"

	aMorph activateWindow.
	self isInWorld  ifTrue: [
		self world startSteppingSubmorphsOf: self ].
	"Note an unhappy inefficiency here:  the startStepping... call will often have already been called in the sequence leading up to entry to this method, but unfortunately the isPartsDonor: call often will not have already happened, with the result that the startStepping... call will not have resulted in the startage of the steppage."! !

!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/16/2011 15:22'!
rejectDropMorphEvent: evt
	"The receiver has been rejected, and must be put back somewhere. 
	Just keep it in the hand"

	^evt hand grabMorph: self! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 12/16/2011 14:58'!
removeAllMorphs
	| oldMorphs |
	submorphs isEmpty ifTrue: [ ^self ].
	self redrawNeeded.
	submorphs do: [ :m |
		m privateOwner: nil ].
	oldMorphs _ submorphs.
	submorphs _ #().
	oldMorphs do: [ :m |
		self removedMorph: m ].
	self someSubmorphPositionOrExtentChanged! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 12/16/2011 15:27'!
removeAllMorphsIn: aCollection
	"greatly speeds up the removal of *lots* of submorphs"
	| set |
	aCollection isEmpty ifTrue: [ ^self ].
	set _ IdentitySet new: aCollection size * 4 // 3.
	aCollection do: [ :each | each owner == self ifTrue: [ set add: each ]].
	set isEmpty ifTrue: [ ^self ].
	self redrawNeeded.
	set do: [ :m | m privateOwner: nil ].
	submorphs _ submorphs reject: [ :each | set includes: each].
	set do: [ :m | self removedMorph: m ].
	self someSubmorphPositionOrExtentChanged! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/16/2011 15:24'!
attachMorph: m
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."
	| delta |
	self releaseMouseFocus. "Break focus"
	self addMorphBack: m.
	delta _ m bounds extent // 2.
	m morphPosition: (self morphPosition - delta).
	targetOffset _ m morphPosition - self morphPosition.! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/16/2011 15:20'!
dropMorph: aMorph event: anEvent
	"Drop the given morph which was carried by the hand"
	| event |
	(anEvent isMouseUp and:[aMorph shouldDropOnMouseUp not]) ifTrue: [ ^self ].
	event _ DropEvent new setPosition: self morphPosition contents: aMorph hand: self.
	self sendEvent: event.
	event wasHandled ifFalse: [ aMorph rejectDropMorphEvent: event ].
	self mouseOverHandler processMouseOver: anEvent! !


!MenuItemMorph methodsFor: 'grabbing' stamp: 'jmv 12/16/2011 15:24'!
aboutToBeGrabbedBy: aHand
	"Don't allow the receiver to act outside a Menu"
	| menu box |
	(owner notNil and: [ owner submorphs size = 1]) ifTrue:[
		"I am a lonely menuitem already; just grab my owner"
		owner stayUp.
		^owner aboutToBeGrabbedBy: aHand ].
	box _ bounds.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: self.
	menu bounds: box.
	menu stayUp.
	self isSelected: false.
	^menu! !

!methodRemoval: Morph #formerOwner!
Morph removeSelector: #formerOwner!
!methodRemoval: Morph #formerOwner:!
Morph removeSelector: #formerOwner:!
!methodRemoval: Morph #formerPosition!
Morph removeSelector: #formerPosition!
!methodRemoval: Morph #formerPosition:!
Morph removeSelector: #formerPosition:!
!methodRemoval: Morph #slideBackToFormerSituation:!
Morph removeSelector: #slideBackToFormerSituation:!
!methodRemoval: Morph #vanishAfterSlidingTo:event:!
Morph removeSelector: #vanishAfterSlidingTo:event:!
