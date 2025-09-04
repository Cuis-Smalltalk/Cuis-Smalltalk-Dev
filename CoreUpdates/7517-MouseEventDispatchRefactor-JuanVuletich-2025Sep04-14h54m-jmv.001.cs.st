'From Cuis7.5 [latest update: #7516] on 4 September 2025 at 2:54:56 pm'!

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 14:54:40'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		self mouseFocus
			ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
			ifNil: [ aMouseEvent dispatchIn: owner ].
		^self].

	"any mouse event but mouseOver"
	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			self mouseOverHandler processMouseOver: lastMouseEvent.
			^self ]].

	aMouseEvent isMouseMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
				ifNil: [ aMouseEvent dispatchIn: owner ]
		] ifFalse: [
		aMouseEvent isMouseScroll ifTrue: [
			aMouseEvent dispatchIn: owner
		] ifFalse: [
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
							self dropMorphs: aMouseEvent ]
					]
				] ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
						ifNil: [ aMouseEvent dispatchIn: owner ]]]].
		self mouseOverHandler processMouseOver: lastMouseEvent.! !

