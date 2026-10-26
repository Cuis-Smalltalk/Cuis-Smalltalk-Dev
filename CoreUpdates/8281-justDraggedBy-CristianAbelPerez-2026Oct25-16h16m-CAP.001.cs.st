'From Cuis7.9 [latest update: #8279] on 25 October 2026 at 4:59:10 pm'!

!Morph methodsFor: 'dropping/grabbing' stamp: 'CAP 25/Oct/2026, 7:25:35 pm (UTC)'!
justDraggedBy: aHand
	"The receiver is being carried by a hand and receives this message on every hand movement.
	See also #aboutToBeGrabbedBy:, #justGrabbedFrom: and #justDroppedInto:event: and #rejectDropMorphEvent:"! !


!HandMorph methodsFor: 'events-processing' stamp: 'CAP 25/Oct/2026, 7:09:44 pm (UTC)'!
startMouseMoveDispatch: aMouseEvent

	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		mouseClickState handleEvent: aMouseEvent from: self ].

	self morphPosition: aMouseEvent eventPosition.
	self mouseFocus
		ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
		ifNil: [ aMouseEvent dispatchIn: owner ].

	"Morphs carried by the hand are being dragged by it"
	self submorphsDo: [ :m | m justDraggedBy: self ].

	self dispatchMouseOverEvent! !

