'From Cuis7.7 [latest update: #7817] on 28 January 2026 at 9:29:04 am'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 27/Jan/2026 18:25:13'!
hideHardwareCursor

	(Cursor cursorAt: #blankCursor) activateCursor! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 27/Jan/2026 18:24:45'!
showHardwareCursor

	"Make the transition to using hardware cursor.
	Report one final damage rectangle to erase the image of the software cursor."
		"show hardware cursor"
	Cursor defaultCursor activateCursor.
	self invalidateDisplayRect: self basicDisplayBounds for: nil.! !

