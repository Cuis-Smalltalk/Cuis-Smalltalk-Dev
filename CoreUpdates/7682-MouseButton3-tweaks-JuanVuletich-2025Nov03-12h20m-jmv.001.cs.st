'From Cuis7.5 [latest update: #7681] on 3 November 2025 at 12:21:38 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 11/3/2025 11:20:03'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"A #mouseDown MouseButtonEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	Notes:
		- MouseButtonEvent are sent to the current mouse focus, and if none,
			to the topmost morph under the hand that handles it."
	
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].
	
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(Preferences at: #focusFollowsMouse)
		ifTrue: [" MouseEnterToFocus. Clicks have no relation to keyboard focus. Just deliver the event."
			self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]
		ifFalse: [ "ClickToFocus. Still two different cases. Some morphs 'ignore' focus setting MouseDown events."
			self clickToFocusConsumesEvent
				ifTrue: [
					"#clickToFocusConsumesEvent means: Don't deliver a MouseDown if the
					soon-to-be MouseUp will set keyboard focus on us (see #processMouseUp:localPosition:).
					So, deliver it if we already have keyboard focus."
					self hasKeyboardFocus ifTrue: [
						self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]]
				ifFalse: [
					"(#clickToFocusConsumesEvent not) means: A keyboard focus setting MouseDown is also a
					regular MouseDown (after setting keyboard focus)"
					self hasKeyboardFocus ifFalse: [
						aMouseButtonEvent hand newKeyboardFocus: self ].
					self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]].

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue: [
		self startStepping: #processMouseStillDown
			in: self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ].! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 11/3/2025 11:21:10'!
startMouseButtonDispatch: aMouseButtonEvent

	| closedSomeMenu |
	lastMouseEvent := aMouseButtonEvent.	
	lastMouseEventTime := Time localMillisecondClock.
	
	closedSomeMenu := false.
	aMouseButtonEvent mouseButton1Pressed ifTrue: [
		owner submorphsDo: [ :m |
			((m is: #MenuMorph) and: [m activeSubMenu isNil]) ifTrue: [
				(m includesPixel: aMouseButtonEvent eventPosition) ifFalse: [
					closedSomeMenu := m deleteIfPopUp: aMouseButtonEvent ]]].
		closedSomeMenu ifTrue: [
			"Used this click to close some menu(s). Do not further process this event."
			aMouseButtonEvent wasHandled: true.
			^self ]].

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseButtonEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			aMouseButtonEvent wasHandled: true.
			^self ]].

	"Issue a synthetic move event if we're not at the position of the event"
	aMouseButtonEvent eventPosition = self morphPosition ifFalse: [
		"Issue a mouse move event to make the receiver appear at the given position"
		(MouseMoveEvent new
			setPosition: aMouseButtonEvent eventPosition
			buttons: aMouseButtonEvent buttons
			hand: self
			stamp: aMouseButtonEvent timeStamp) startDispatchFromHand: self ].

	"Drop submorphs on button events"
	self hasSubmorphs
		ifTrue: [
			"Not if we are grabbing them"
			mouseClickState ifNil: [
				"Want to drop on mouseUp, NOT mouseDown"
				aMouseButtonEvent isMouseUp ifTrue: [
					self dropMorphs: aMouseButtonEvent ]]]
		ifFalse: [
			(self mouseFocus notNil and: [ aMouseButtonEvent mouseButton3Changed not ])
				"Don't forward halo events to mouseFocus morph"
				ifTrue: [
					mouseFocus handleMouseFocusEvent: aMouseButtonEvent ]
				ifFalse: [
					aMouseButtonEvent dispatchIn: owner ]].

	self hasSubmorphs ifFalse: [
		self dispatchMouseOverEvent ].! !

