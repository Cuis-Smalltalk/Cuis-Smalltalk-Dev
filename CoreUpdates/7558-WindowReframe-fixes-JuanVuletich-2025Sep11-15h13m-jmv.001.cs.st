'From Cuis7.5 [latest update: #7557] on 11 September 2025 at 3:15:32 pm'!

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 9/11/2025 15:01:09'!
mouseEnter: aMouseEnterEvent
	super mouseEnter: aMouseEnterEvent.
	self cursor activateCursor.! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 9/11/2025 15:04:19'!
mouseLeave: aMouseLeaveEvent
	super mouseLeave: aMouseLeaveEvent.
	aMouseLeaveEvent mouseButton1Pressed ifFalse: [
		Cursor defaultCursor activateCursor ].! !


!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 9/11/2025 15:13:15'!
adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast

	self basicAdjustOwnerAt: aGlobalPoint.

	"If UI is becoming slow or is optimized for slow systems, resize without
	showing window contents, but only edges. But don't do it for rotated Windows!!"
	(owner isOrAnyOwnerIsRotated not and: [
		(Preferences at: #cheapWindowReframe) or: [millisecondSinceLast > 100]]) ifTrue: [
			owner displayBounds newRect2From: [ :f |
				self stopStepping.
				hand := nil.
				self basicAdjustOwnerAt: Sensor mousePoint.
				owner morphPosition extent: owner extentInWorld ].
			Cursor defaultCursor activateCursor ].! !

