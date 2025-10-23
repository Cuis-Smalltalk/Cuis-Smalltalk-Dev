'From Cuis7.5 [latest update: #7664] on 21 October 2025 at 5:08:54 pm'!

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 10/21/2025 15:40:19'!
allowsMorphDrop
	"Answer whether we accept dropping morphs."

	^ true! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 10/21/2025 15:41:00'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo.
	Both 'aMorph allowsGrabWithHand' and 'aMorph owner allowsSubmorphDrag' must be true for aMorph to be grabbed by the hand. It is also required that 'aMorph handlesMouseDown:' be false."

	^ true! !

