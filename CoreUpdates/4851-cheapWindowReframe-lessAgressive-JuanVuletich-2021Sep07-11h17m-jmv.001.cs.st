'From Cuis 5.0 [latest update: #4849] on 7 September 2021 at 11:21:43 am'!

!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 9/7/2021 11:21:14'!
adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast

	self basicAdjustOwnerAt: aGlobalPoint.

	"If UI is becoming slow or is optimized for slow systems, resize without
	showing window contents, but only edges. But don't do it for rotated Windows!!"
	(owner isOrAnyOwnerIsRotated not and: [
		Preferences cheapWindowReframe or: [millisecondSinceLast > 200]]) ifTrue: [
			owner displayBounds newRectFrom: [ :f |
				self basicAdjustOwnerAt: Sensor mousePoint.
				owner morphPosition extent: owner morphExtentInWorld ]].! !

