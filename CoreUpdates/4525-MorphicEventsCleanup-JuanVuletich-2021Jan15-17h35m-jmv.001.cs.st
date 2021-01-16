'From Cuis 5.0 [latest update: #4523] on 15 January 2021 at 5:36:41 pm'!

!MouseEvent methodsFor: 'converting' stamp: 'jmv 1/14/2021 21:22:43'!
asMouseMove

	^ MouseMoveEvent new
		setType: #mouseMove
		position: position
		buttons: buttons
		hand: source
		stamp: Time millisecondClockValue "VMs report events using #millisecondClockValue"! !


!HandMorph methodsFor: 'event handling' stamp: 'jmv 1/15/2021 17:35:56'!
processEventQueue
	"Process user input events from the local input devices.
	Answer true if any event was handled (but ignore MouseMove)"

	| evt evtBuf type hadAny mcs hadAnyMouseEvent |
	mcs _ mouseClickState.
	hadAny := false.
	hadAnyMouseEvent := false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		type := evtBuf first.
		evt := self createEventFrom: evtBuf ofType: type.
		evt ifNotNil: [
			"Finally, handle it"
			self startEventDispatch: evt.
			hadAny := true.
			evt isMouse ifTrue: [
				hadAnyMouseEvent := true ]]].
	hadAnyMouseEvent ifFalse: [
		mcs 
			ifNotNil: [ 
				"No mouse events during this cycle. Make sure click states time out accordingly"
				mcs
					handleEvent: lastMouseEvent asMouseMove
					from: self ]].
	^hadAny! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 1/15/2021 17:15:12'!
startKeyboardDispatch: aKeyboardEvent

	| focusedElement |
	
	focusedElement _ self keyboardFocus ifNil: [ self world ].
	focusedElement handleFocusEvent: aKeyboardEvent.
	
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 1/15/2021 17:15:17'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		^self mouseFocus
			ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
			ifNil: [ owner dispatchEvent: aMouseEvent ]].

	"any mouse event but mouseOver"
	lastMouseEvent _ aMouseEvent.	
	lastMouseEventTime _ Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			^self mouseOverHandler processMouseOver: lastMouseEvent  ]].

	aMouseEvent isMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
				ifNil: [ owner dispatchEvent: aMouseEvent ]
		] ifFalse: [
		aMouseEvent isMouseScroll ifTrue: [
			owner dispatchEvent: aMouseEvent
		] ifFalse: [
			"Issue a synthetic move event if we're not at the position of the event"
			aMouseEvent eventPosition = self morphPosition ifFalse: [
				"Issue a mouse move event to make the receiver appear at the given position"
				self startMouseDispatch: (MouseMoveEvent new
					setType: #mouseMove
					position: aMouseEvent eventPosition
					buttons: aMouseEvent buttons
					hand: self
					stamp: aMouseEvent timeStamp) ].
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
						ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent ]]]].
		self mouseOverHandler processMouseOver: lastMouseEvent! !

!methodRemoval: MouseEvent #asMouseMove: stamp: 'jmv 1/15/2021 17:35:33'!
MouseEvent removeSelector: #asMouseMove:!
