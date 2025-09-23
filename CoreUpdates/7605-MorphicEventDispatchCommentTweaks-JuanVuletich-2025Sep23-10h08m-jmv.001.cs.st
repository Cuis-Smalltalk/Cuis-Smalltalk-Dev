'From Cuis7.5 [latest update: #7604] on 23 September 2025 at 10:20:46 am'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/23/2025 10:08:46'!
processKeyDown: aKeyboardEvent
	"A KeyboardEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyDown: aKeyboardEvent.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/23/2025 10:08:54'!
processKeyUp: aKeyboardEvent
	"A KeyboardEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyUp: aKeyboardEvent.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/23/2025 10:08:59'!
processKeystroke: aKeyboardEvent
	"A KeyboardEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyStroke: aKeyboardEvent.
	self 
		valueOfProperty: #'keyStroke:'
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/23/2025 10:20:03'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"A #mouseDown MouseButtonEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	Notes:
		- MouseButtonEvent are sent to the current mouse focus, and if none,
			to the topmost morph under the hand that handles it."
	
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition.
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

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/23/2025 10:09:26'!
processMouseEnter: aMouseEnterEvent localPosition: localEventPosition
	"A MouseEnterEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self wantsBalloon ifTrue: [
		aMouseEnterEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEnterEvent) ifTrue: [
		aMouseEnterEvent wasHandled: true.
		self mouseEnter: aMouseEnterEvent ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/23/2025 10:09:34'!
processMouseLeave: aMouseLeaveEvent localPosition: localEventPosition
	"A MouseLeaveEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseLeaveEvent hand removePendingBalloonFor: self.
	(self handlesMouseOver: aMouseLeaveEvent) ifTrue: [
		aMouseLeaveEvent wasHandled: true.
		self mouseLeave: aMouseLeaveEvent ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/23/2025 10:20:21'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"A MouseMoveEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	
	Notes:
		- MouseMoveEvents are not sent if the hand is carrying morphs around.
		- MouseMoveEvents are sent to the current mouse focus, and if none, to the topmost
			morph under the hand that handles it."

	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.

	(self clickToFocusConsumesEvent and: [ (Preferences at: #focusFollowsMouse) not ])
		ifTrue: [ "ClickToFocus, and focus setting MouseDown was ignored. (see #processMouseDown:localPosition:)"
			self hasKeyboardFocus ifTrue: [
				"If we already have keyboardFocus, the MouseDown was delivered. Deliver also the MouseMove."
				self mouseMove: aMouseMoveEvent localPosition: localEventPosition  ]]
		ifFalse: [
			"See #processMouseDown:localPosition: Mouse down was a regular event. Deliver also the MouseMove."
			self mouseMove: aMouseMoveEvent localPosition: localEventPosition ].

	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/23/2025 10:09:53'!
processMouseOver: aMouseOverEvent localPosition: localEventPosition
	"A MouseOverEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseOverEvent hand noticeMouseOver: self event: aMouseOverEvent.
	
	"Open question: should any unhandled mouse move events be filtered out?
	(i.e. should mouseHover:localPosition: be called when a mouse button is pressed
	but the morph doesn't have mouse button handlers?
	Essentially, what are the limits of what is considered 'hovering'?"
	self handlesMouseHover ifTrue: [
		self
			mouseHover: aMouseOverEvent
			localPosition: localEventPosition ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/23/2025 10:11:39'!
processMouseScroll: aMouseScrollEvent localPosition: localEventPosition
	"A MouseScrollEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	(self handlesMouseScroll: aMouseScrollEvent) ifTrue: [
		self
			mouseScroll: aMouseScrollEvent
			localPosition: localEventPosition.
		aMouseScrollEvent wasHandled: true ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/23/2025 10:10:39'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"A #mouseUp MouseButtonEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	Notes:
		- MouseButtonEvent are sent to the current mouse focus, and if none,
			to the topmost morph under the hand that handles it."

	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	"If we click to focus, don't process this event as a regular click.
	See also #processMouseDown:localPosition:"
	"(self clickToFocusConsumesEvent and: [
		(Preferences at: #focusFollowsMouse) not and: [
			self hasKeyboardFocus not ]])
		ifTrue: [ aMouseButtonEvent hand newKeyboardFocus: self ]
		ifFalse: [ self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ]."
	(self clickToFocusConsumesEvent and: [ (Preferences at: #focusFollowsMouse) not ])
		ifTrue: [
			self hasKeyboardFocus
				ifFalse: [
					"Haven't set keyboard focus but must. Do it.
					MouseDown and any MouseMove were not delivered. Do not deliver #mouseButton1Up:localPosition:"
					aMouseButtonEvent hand newKeyboardFocus: self ]
				ifTrue: [
					"If we already have keyboardFocus, the MouseDown was delivered. Deliver also the MouseMove."
					self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ]]
		ifFalse: [
			"See #processMouseDown:localPosition: Mouse down was a regular event. Deliver also the MouseUp."
			self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ].

	self stopStepping: #processMouseStillDown.! !

