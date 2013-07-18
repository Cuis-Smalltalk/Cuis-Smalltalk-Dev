'From Cuis 4.1 of 12 December 2012 [latest update: #1574] on 22 January 2013 at 10:59:32 pm'!

!PasteUpMorph commentStamp: '<historical>' prior: 0!
A World, the entire Smalltalk screen, is a PasteUpMorph.  A World responds true to isWorld.  A PasteUpMorph that is a world, builds its menu in HandMorph buildWorldMenu.

worldState		If I am also a World, keeps the hands, damageRecorder, stepList etc.

Currently, creating additional instances is not working properly. Please use DragAndDropAreaMorph instead.!


!DragAndDropAreaMorph methodsFor: 'drag and drop' stamp: 'jmv 1/22/2013 22:57'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. Redefined to answer true."

	^ true! !

!DragAndDropAreaMorph methodsFor: 'drag and drop' stamp: 'jmv 1/22/2013 22:57'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. Redefined to answer true."

	^ true! !

!DragAndDropAreaMorph methodsFor: 'drag and drop' stamp: 'jmv 1/22/2013 22:58'!
wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self.
	This method just answers super. It is included here to say it is relevant to D&D behavior."

	^super wantsDroppedMorph: aMorph event: evt! !

