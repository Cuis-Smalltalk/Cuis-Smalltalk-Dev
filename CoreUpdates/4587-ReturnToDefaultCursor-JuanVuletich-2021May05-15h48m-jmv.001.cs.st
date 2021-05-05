'From Cuis 5.0 [latest update: #4586] on 5 May 2021 at 3:49:52 pm'!

!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 5/5/2021 15:46:30'!
stepAt: millisecondSinceLast
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [ Cursor defaultCursor activateCursor ].
		^ self stopStepping ].
	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPosition.
	hand lastMouseEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p.
			(Preferences cheapWindowReframe or: [ millisecondSinceLast > 200]) ifTrue: [
				owner displayBounds newRectFrom: [ :f |
					self adjustOwnerAt: Sensor mousePoint.
					owner morphPosition extent: owner morphExtent ]]]
		ifFalse: [
			self stopStepping.
			hand _ nil.
			Cursor defaultCursor activateCursor ].! !

