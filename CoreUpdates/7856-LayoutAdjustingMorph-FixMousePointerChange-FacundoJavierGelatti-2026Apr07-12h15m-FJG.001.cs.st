'From Cuis7.7 [latest update: #7855] on 7 April 2026 at 12:15:58 pm'!

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'FJG 3/Apr/2026 11:17:42'!
mouseEnter: aMouseEnterEvent
	super mouseEnter: aMouseEnterEvent.
	aMouseEnterEvent mouseButton1Pressed ifFalse: [
		self cursor activateCursor ]! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'FJG 3/Apr/2026 11:17:05'!
mouseLeave: aMouseLeaveEvent
	super mouseLeave: aMouseLeaveEvent.
	hand ifNil: [
		Cursor defaultCursor activateCursor ].! !

