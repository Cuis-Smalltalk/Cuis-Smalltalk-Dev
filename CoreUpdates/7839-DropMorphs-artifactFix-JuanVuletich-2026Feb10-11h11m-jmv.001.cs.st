'From Cuis7.7 [latest update: #7838] on 10 February 2026 at 11:34:29 am'!

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 10/Feb/2026 11:31:47'!
removedMorph: aMorph
	"Notify the receiver that aMorph was just removed from its children"

	self invalidateDisplayRect: self basicDisplayBounds for: nil.! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/Feb/2026 11:33:24'!
showHardwareCursor
	"Make the transition to using hardware cursor."

	Cursor defaultCursor activateCursor.! !

