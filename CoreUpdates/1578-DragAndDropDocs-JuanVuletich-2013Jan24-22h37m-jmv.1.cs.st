'From Cuis 4.1 of 12 December 2012 [latest update: #1577] on 24 January 2013 at 11:40:47 pm'!

!DragAndDropAreaMorph commentStamp: '<historical>' prior: 0!
A simple morph that allows dragging submorphs, and dropping other morphs on it. It is provided as an example, and also as a reference of the methods you might need to implement in your morphs to handle drag and drop.

| m |
m _ DragAndDropAreaMorph new.
m openInWorld.
m addMorph: EllipseMorph new.
m morphExtent: 320@240!


!DragAndDropAreaMorph methodsFor: 'drag and drop us' stamp: 'jmv 1/24/2013 22:53'!
aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."
	"This message is sent to the dragged morph, not to the owner.
	It is included here just for reference."

	^self "Grab me"! !

!DragAndDropAreaMorph methodsFor: 'drag and drop us' stamp: 'jmv 1/24/2013 22:53'!
justDroppedInto: newOwnerMorph event: anEvent 
	"This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph"
	"This message is sent to the dragged morph, not to the owner.
	It is included here just for reference."

	^super justDroppedInto: newOwnerMorph event: anEvent ! !

!DragAndDropAreaMorph methodsFor: 'drag and drop us' stamp: 'jmv 1/24/2013 22:53'!
justGrabbedFrom: formerOwner
	"The receiver was just grabbed from its former owner and is now attached to the hand."
	"This message is sent to the dragged morph, not to the owner.
	It is included here just for reference."! !

!DragAndDropAreaMorph methodsFor: 'drag and drop us' stamp: 'jmv 1/24/2013 22:52'!
shouldDropOnMouseUp
	"This message is sent to the dragged morph, not to the owner.
	It is included here just for reference."
	^super shouldDropOnMouseUp! !

!DragAndDropAreaMorph methodsFor: 'drag and drop us' stamp: 'jmv 1/24/2013 22:53'!
wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph. This check is symmetric to #wantsDroppedMorph:event: to give both parties a chance of figuring out whether they like each other."
	"This message is sent to the dragged morph, not to the owner.
	It is included here just for reference."

	^true! !


!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/24/2013 22:49'!
aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."

	^self "Grab me"! !

!Morph methodsFor: 'testing' stamp: 'jmv 1/24/2013 22:46'!
shouldDropOnMouseUp
	"Answer false if we allow grabbing us with click (i.e. button1Down + button1Up). Click again to drop us.
	Answer true if we require the mouse button 1 to be down while dragging (this is the regular behavior when dragging windows around in most (or maybe all) host OS desktops."

	"
	former _ self formerPosition ifNil:[^false].
	^(former dist: self position) > 10
	"
	^true! !


!DragAndDropAreaMorph methodsFor: 'drag and drop other morphs' stamp: 'jmv 1/24/2013 22:52'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. Redefined to answer true."

	^ true! !

!DragAndDropAreaMorph methodsFor: 'drag and drop other morphs' stamp: 'jmv 1/24/2013 22:52'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. Redefined to answer true."

	^ true! !

!DragAndDropAreaMorph methodsFor: 'drag and drop other morphs' stamp: 'jmv 1/24/2013 22:52'!
wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self.
	This method just answers super. It is included here to say it is relevant to D&D behavior."

	^super wantsDroppedMorph: aMorph event: evt! !


!DragAndDropAreaMorph reorganize!
('drag and drop other morphs' allowsMorphDrop allowsSubmorphDrag wantsDroppedMorph:event:)
('drag and drop us' aboutToBeGrabbedBy: justDroppedInto:event: justGrabbedFrom: shouldDropOnMouseUp wantsToBeDroppedInto:)
!

