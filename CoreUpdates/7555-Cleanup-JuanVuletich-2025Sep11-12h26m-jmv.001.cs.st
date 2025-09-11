'From Cuis7.5 [latest update: #7554] on 11 September 2025 at 12:32:36 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/11/2025 12:31:21'!
processMouseEnter: aMouseEnterEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self wantsBalloon ifTrue: [
		aMouseEnterEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEnterEvent) ifTrue: [
		aMouseEnterEvent wasHandled: true.
		self mouseEnter: aMouseEnterEvent ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/11/2025 12:31:15'!
processMouseLeave: aMouseLeaveEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseLeaveEvent hand removePendingBalloonFor: self.
	(self handlesMouseOver: aMouseLeaveEvent) ifTrue: [
		aMouseLeaveEvent wasHandled: true.
		self mouseLeave: aMouseLeaveEvent ].! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/11/2025 12:30:19'!
startMouseButtonDispatch: aMouseEvent

	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			aMouseEvent wasHandled: true.
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

	self hasSubmorphs ifFalse: [
		self dispatchMouseOverEvent ].! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/11/2025 12:30:30'!
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

	self hasSubmorphs ifFalse: [
		self dispatchMouseOverEvent ].! !

!methodRemoval: MenuItemMorph #processMouseEnter:localPosition: stamp: 'jmv 9/11/2025 12:26:38'!
MenuItemMorph removeSelector: #processMouseEnter:localPosition:!
!methodRemoval: MenuItemMorph #processMouseLeave:localPosition: stamp: 'jmv 9/11/2025 12:26:41'!
MenuItemMorph removeSelector: #processMouseLeave:localPosition:!
!methodRemoval: MouseEvent #isDraggingEvent stamp: 'jmv 9/11/2025 12:31:26'!
MouseEvent removeSelector: #isDraggingEvent!
!methodRemoval: MorphicEvent #isDraggingEvent stamp: 'jmv 9/11/2025 12:31:32'!
MorphicEvent removeSelector: #isDraggingEvent!
