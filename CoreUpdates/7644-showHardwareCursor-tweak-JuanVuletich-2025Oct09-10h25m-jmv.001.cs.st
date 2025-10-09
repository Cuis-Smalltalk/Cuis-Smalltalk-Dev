'From Cuis7.5 [latest update: #7643] on 9 October 2025 at 10:27:10 am'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/9/2025 09:30:40'!
showHardwareCursor

	"Make the transition to using hardware cursor.
	Report one final damage rectangle to erase the image of the software cursor."
	self invalidateDisplayRect: self basicDisplayBounds for: nil.
	Cursor currentCursor == (Cursor cursorAt: #blankCursor) ifTrue: [ 
		"show hardware cursor"
		Cursor defaultCursor activateCursor ].! !

