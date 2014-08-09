'From Cuis 4.2 of 25 July 2013 [latest update: #2070] on 6 August 2014 at 3:13:37.254375 pm'!

!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 8/6/2014 15:13'!
stepAt: millisecondClockValue
	"got the #mouseLeave: message"

	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [
			Cursor normal show ].
		^self stopStepping ].

	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPosition.
	hand lastMouseEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p.
			Preferences cheapWindowReframe ifTrue: [
				owner morphBoundsInWorld newRectFrom: [ :f |
					self adjustOwnerAt: Sensor mousePoint.
					owner morphBoundsInWorld ]].
			]
		ifFalse: [
			self stopStepping.
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: p)) ifFalse: [
				hand _ nil.
				Cursor normal show ]]! !

