'From Cuis7.5 [latest update: #7517] on 4 September 2025 at 4:18:44 pm'!

!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 15:10:43'!
startDispatchFromHand: aHand
	"double dispatch the event dispatch"

	aHand startMouseButtonDispatch: self! !


!MouseMoveEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 15:11:18'!
startDispatchFromHand: aHand
	"double dispatch the event dispatch"

	aHand startMouseMoveDispatch: self! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 15:48:22'!
startMouseButtonDispatch: aMouseEvent

	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			self mouseOverHandler processMouseOver: lastMouseEvent.
			^self ]].

	"Issue a synthetic move event if we're not at the position of the event"
	aMouseEvent eventPosition = self morphPosition ifFalse: [
		"Issue a mouse move event to make the receiver appear at the given position"
		(MouseMoveEvent new
			setPosition: aMouseEvent eventPosition
			buttons: aMouseEvent buttons
			hand: self
			stamp: aMouseEvent timeStamp) startDispatchFromHand: self ].

	"Drop submorphs on button events"
	self hasSubmorphs
		ifTrue: [
			"Not if we are grabbing them"
			mouseClickState ifNil: [
				"Want to drop on mouseUp, NOT mouseDown"
				aMouseEvent isMouseUp ifTrue: [
					self dropMorphs: aMouseEvent ]]]
		ifFalse: [
			self mouseFocus
				ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
				ifNil: [ aMouseEvent dispatchIn: owner ]].

	self mouseOverHandler processMouseOver: lastMouseEvent.! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 15:43:48'!
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
		
	self mouseOverHandler processMouseOver: lastMouseEvent.! !


!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 15:41:31'!
startDispatchFromHand: aHand
	"double dispatch the event dispatch
	Generic MouseEvent behavior. See inheritance."

	aHand startMouseDispatch: self! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 15:41:07'!
startMouseDispatch: aMouseEvent
	"For MouseEvents with default dispatch behavior"

	self hasSubmorphs
		ifFalse: [
			self mouseFocus
				ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
				ifNil: [ aMouseEvent dispatchIn: owner ]].! !

