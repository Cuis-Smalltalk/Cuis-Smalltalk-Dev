'From Cuis 5.0 [latest update: #4923] on 14 October 2021 at 10:59:24 am'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/14/2021 10:56:28'!
showHardwareCursor

	"Make the transition to using hardware cursor.
	Report one final damage rectangle to erase the image of the software cursor."
	self invalidateDisplayRect: self displayFullBoundsForPatch for: nil.
	Cursor currentCursor == (Cursor cursorAt: #blankCursor) ifTrue: [ 
		"show hardware cursor"
		Cursor defaultCursor activateCursor ].! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 10/14/2021 10:51:49'!
dropMorphs: anEvent
	"Drop the morphs at the hands position"

	self showHardwareCursor.
	self submorphsReverseDo: [ :m |
		"Drop back to front to maintain z-order"
		self dropMorph: m event: anEvent ].! !

