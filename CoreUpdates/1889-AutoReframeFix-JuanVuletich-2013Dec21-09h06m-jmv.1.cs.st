'From Cuis 4.2 of 25 July 2013 [latest update: #1888] on 21 December 2013 at 9:08:44.780039 am'!

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 12/21/2013 09:08'!
mouseLeave: anEvent

	super mouseLeave: anEvent.
	hand ifNotNil: [
		hand morphInHandOrResizing: nil.
		hand _ nil.
		Cursor normal show ]! !

