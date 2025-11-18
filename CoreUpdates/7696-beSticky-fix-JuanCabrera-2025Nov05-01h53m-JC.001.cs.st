'From Cuis7.5 [latest update: #7689] on 5 November 2025 at 1:59:44 am'!

!PlacedMorph methodsFor: 'dropping/grabbing' stamp: 'JC 11/5/2025 01:58:42'!
aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."
	"Added isSticky check"
	self isSticky ifTrue: [^nil].
	^self "Grab me"! !

