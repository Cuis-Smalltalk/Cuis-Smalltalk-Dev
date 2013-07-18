'From Cuis 4.1 of 12 December 2012 [latest update: #1570] on 19 January 2013 at 4:25:13 pm'!
!classDefinition: #DragAndDropAreaMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #DragAndDropAreaMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!DragAndDropAreaMorph commentStamp: '<historical>' prior: 0!
A simple morph that allows dragging submorphs, and dropping other morphs on it.

| m |
m _ DragAndDropAreaMorph new.
m openInWorld.
m addMorph: EllipseMorph new.
m morphExtent: 320@240!


!DragAndDropAreaMorph methodsFor: 'drag and drop' stamp: 'jmv 1/19/2013 16:21'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^ true! !

!DragAndDropAreaMorph methodsFor: 'drag and drop' stamp: 'jmv 1/19/2013 16:21'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false."

	^ true! !

!DragAndDropAreaMorph methodsFor: 'drag and drop' stamp: 'jmv 1/19/2013 16:19'!
wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self."

	^true! !

