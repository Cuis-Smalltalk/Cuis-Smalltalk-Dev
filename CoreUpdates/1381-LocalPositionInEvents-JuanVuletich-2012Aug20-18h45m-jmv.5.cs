'From Cuis 4.0 of 21 April 2012 [latest update: #1380] on 20 August 2012 at 7:37:42 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:19'!
processDropMorph: aDropEvent localPosition: localEventPosition
	"Handle a dropping morph."
	| aMorph posInWorld |
	aMorph _ aDropEvent contents.
	"Do a symmetric check if both morphs like each other"
	((self wantsDroppedMorph: aMorph event: aDropEvent)	"I want her"
		and: [aMorph wantsToBeDroppedInto: self])		"she wants me"
			ifFalse: [
				^ self].
	aDropEvent wasHandled: true.
	posInWorld _ aMorph referencePosition.
	aMorph referencePosition: posInWorld.
	self acceptDroppingMorph: aMorph event: aDropEvent.
	aMorph justDroppedInto: self event: aDropEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:19'!
processKeyDown: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyDown: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:20'!
processKeyUp: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyUp: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:02'!
processKeystroke: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyStroke: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:11'!
processMouseDown: anEvent localPosition: localEventPosition
	"System level event handling."
	anEvent wasHandled ifTrue: [ ^self ]. "not interested"
	anEvent hand removePendingBalloonFor: self.
	anEvent wasHandled: true.
	self activateWindow.

	"Make me modal during mouse transitions"
	anEvent hand newMouseFocus: self event: anEvent.
	anEvent mouseButton3Changed ifTrue: [^self mouseButton3Down: anEvent].

	self mouseDown: anEvent.
	anEvent hand removeHaloFromClick: anEvent on: self.

	(self handlesMouseStillDown: anEvent) ifTrue:[
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue + self mouseStillDownThreshold
			arguments: {anEvent copy resetHandlerFields . localEventPosition}
			stepTime: self mouseStillDownStepRate ].
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:22'!
processMouseEnter: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	aMouseEvent isDraggingEvent ifTrue: [
		^self].
	self wantsBalloon ifTrue: [
		aMouseEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEvent) ifTrue: [
		aMouseEvent wasHandled: true.
		self mouseEnter: aMouseEvent ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:03'!
processMouseLeave: anEvent localPosition: localEventPosition
	"System level event handling."
	anEvent hand removePendingBalloonFor: self.
	anEvent isDraggingEvent ifTrue: [
		^self].
	(self handlesMouseOver: anEvent) ifTrue: [
		anEvent wasHandled: true.
		self mouseLeave: anEvent ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:25'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"System level event handling."

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self ]) ifFalse: [^self].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1].
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:21'!
processMouseOver: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	aMouseEvent hand mouseFocus == self ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		(self containsPoint: aMouseEvent eventPosition event: aMouseEvent) ifFalse: [
			^self ]].
	aMouseEvent hand noticeMouseOver: self event: aMouseEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:11'!
processMouseStillDown: anEvent localPosition: localEventPosition
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	(anEvent hand mouseFocus == self) 
		ifFalse: [
			^self stopSteppingSelector: #processMouseStillDown:localPosition: ].
	self mouseStillDown: anEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:21'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."

	aMouseButtonEvent wasHandled ifTrue: [^self]. "not interested"
	aMouseButtonEvent hand mouseFocus == self ifFalse: [^self]. "Not interested in other parties"
	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed
		ifTrue: [ self mouseButton3Up: aMouseButtonEvent ]
		ifFalse: [
			self mouseUp: aMouseButtonEvent.
			self stopSteppingSelector: #processMouseStillDown:localPosition: ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:20'!
processUnknownEvent: aMorphicEvent localPosition: localEventPosition
	"An event of an unknown type was sent to the receiver. What shall we do?!!"

	Beeper beep. 
	aMorphicEvent printString displayAt: 0@0.
	aMorphicEvent wasHandled: true! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:26'!
processWindowEvent: aWindowEvent localPosition: localEventPosition
	"Handle an event concerning our host window"

	aWindowEvent wasHandled ifTrue: [^self]. "not interested"
	(self wantsWindowEvent: aWindowEvent) ifFalse: [^self].
	aWindowEvent wasHandled: true.
	self windowEvent: aWindowEvent.
! !


!HaloHandleMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:20'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition

	super processMouseDown: aMouseButtonEvent localPosition: localEventPosition.
	self send: mouseDownSelector withEvent: aMouseButtonEvent! !

!HaloHandleMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:23'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition

	super processMouseMove: aMouseMoveEvent localPosition: localEventPosition.
	aMouseMoveEvent anyButtonPressed ifTrue: [
		self send: mouseMoveSelector withEvent: aMouseMoveEvent ]! !

!HaloHandleMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:21'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition

	super processMouseUp: aMouseButtonEvent localPosition: localEventPosition.
	self send: mouseUpSelector withEvent: aMouseButtonEvent! !


!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:02'!
processKeystroke: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue:[^self].
	self handlesKeyboard ifFalse:	[^ self].
	aKeyboardEvent wasHandled: true.
	self keyStroke: aKeyboardEvent! !

!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:25'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Re-implemented to allow for mouse-up move events"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent.
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1].
! !


!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:22'!
processMouseEnter: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	"Do #mouseEnter: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	self wantsBalloon ifTrue: [
		aMouseEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEvent) ifTrue:[
		aMouseEvent wasHandled: true.
		self mouseEnter: aMouseEvent ]! !

!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:22'!
processMouseLeave: aMouseEvent localPosition: localEventPosition
	"System level event handling."

	"Do #mouseLeave: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	aMouseEvent hand removePendingBalloonFor: self.
	(self handlesMouseOver: aMouseEvent) ifTrue: [
		aMouseEvent wasHandled: true.
		self mouseLeave: aMouseEvent ]! !

!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:21'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"The handling of control between menu item requires them to act on mouse up even if not the current focus. This is different from the default behavior which really only wants to handle mouse ups when they got mouse downs before"

	aMouseButtonEvent wasHandled ifTrue:[^self]. "not interested"
	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed
		ifTrue: [ self mouseButton3Up: aMouseButtonEvent ]
		ifFalse: [ self mouseUp: aMouseButtonEvent ]! !


!HierarchicalListMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:23'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1].
! !


!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:25'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition }
			stepTime: 1].
! !


!MessageNames methodsFor: 'selector list' stamp: 'jmv 8/20/2012 19:08'!
messageList
	"Answer the receiver's message list, computing it if necessary. The way 
	to force a recomputation is to set the messageList to nil"
	messageList
		ifNil: [
			messageList _ selectedSelector
				ifNil: [#()]
				ifNotNil: [
					Smalltalk allImplementorsOf: selectedSelector].
			self initializeMessageList: messageList.
			self messageListIndex: (messageList size > 0
				ifTrue: [1]
				ifFalse: [0])
			].
	^ messageList! !


!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:40'!
click: aMorphicEvent
	"Handle a single-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:41'!
doubleClick: aMorphicEvent
	"Handle a double-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:43'!
keyDown: aMorphicEvent
	"Handle a key down event. The default response is to do nothing."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:46'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: aKeyboardEvent)
		ifTrue: [ ^ self ]! !

!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:58'!
keyUp: anEvent
	"Handle a key up event. The default response is to do nothing."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:02'!
mouseDown: evt
	"Handle a mouse down event."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:03'!
mouseEnter: evt
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:03'!
mouseLeave: evt
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:03'!
mouseMove: evt
	"Handle a mouse move event."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:03'!
mouseStillDown: evt
	"Handle a mouse move event."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:03'!
mouseUp: evt
	"Handle a mouse up event."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:03'!
windowEvent: anEvent
	"Host window event"! !

!Morph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:49'!
handlesKeyboard
	"Return true if the receiver wishes to handle keyboard events"

	^ false
! !

!Morph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:49'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	"NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true." 

	^ false
! !

!Morph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:49'!
handlesMouseOver: aMorphicEvent
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?  The default response is false." 

	^ false! !

!Morph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:49'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"
	^ false! !


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:15'!
mouseUp: evt
	(self containsPoint: evt eventPosition)
		ifTrue: [
			self selected: 
				((evt eventPosition y - self morphPositionInWorld y // self class itemHeight) + 
					self firstVisible).
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !

!AutoCompleterMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:54'!
handlesMouseDown: aMouseButtonEvent

	^ true! !


!FillInTheBlankMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:02'!
mouseDown: evt
	(self containsPoint: evt eventPosition) ifFalse:[^ Beeper beep]. "sent in response to outside modal click"
	evt hand grabMorph: self. "allow repositioning"! !

!FillInTheBlankMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:54'!
handlesMouseDown: aMouseButtonEvent
	^true! !


!HaloHandleMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:50'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^(super handlesMouseDown: aMouseButtonEvent) | 
		mouseDownSelector notNil | mouseMoveSelector notNil | mouseUpSelector notNil! !

!HaloHandleMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:51'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"
	^(super handlesMouseStillDown: evt) | keyStrokeSelector notNil! !


!HaloMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:12'!
mouseMove: evt
	"Drag our target around"
	| thePoint |
	thePoint _ evt eventPosition - positionOffset.
	target morphPosition: thePoint! !

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 8/20/2012 18:13'!
popUpFor: aMorph event: aMorphicEvent
	"This message is sent by morphs that explicitly request the halo on a button click. Note: anEvent is in aMorphs coordinate frame."

	| hand anEvent |
	self flag: #workAround.	"We should really have some event/hand here..."
	anEvent _ aMorphicEvent
				ifNil: [
					hand _ aMorph world activeHand.
					hand ifNil: [ hand _ aMorph world firstHand ]. 
					hand lastEvent ]
				ifNotNil: [
					hand _ aMorphicEvent hand.
					aMorphicEvent ].
	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset _ anEvent eventPosition - aMorph morphPosition.
	self startStepping! !

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 8/20/2012 18:13'!
staysUpWhenMouseIsDownIn: aMorph
	^ ((aMorph == target) or: [aMorph hasOwner: self])! !

!HaloMorph methodsFor: 'event handling' stamp: 'jmv 8/20/2012 18:13'!
transferHalo: event
	"Transfer the halo to the next likely recipient"
	target ifNil: [ ^self delete ].
	target transferHalo: event from: target.! !


!HandleMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:45'!
keyStroke: aKeyboardEvent
	"Check for cursor keys"
	| keyValue |
	(owner is: #HandMorph) ifFalse: [ ^self ].
	keyValue _ aKeyboardEvent keyValue.
	keyValue = 28 ifTrue: [ ^self morphPosition: self morphPosition - (1@0) ].
	keyValue = 29 ifTrue: [ ^self morphPosition: self morphPosition + (1@0) ].
	keyValue = 30 ifTrue: [ ^self morphPosition: self morphPosition - (0@1) ].
	keyValue = 31 ifTrue: [ ^self morphPosition: self morphPosition + (0@1) ].
	"Special case for return"
	aKeyboardEvent isReturnKey ifTrue:[
		"Drop the receiver and be done"
	self flag: #arNote. "Probably unnecessary"
		owner releaseKeyboardFocus: self.
		self delete ]! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:00'!
clickAndHalf: evt
	self handleInteraction: [
		editor clickAndHalf: (evt ztranslatedBy: self morphPositionInWorld negated) ].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:57'!
keyStroke: aKeyboardEvent

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
		
	"Maybe disable? Precludes the use of up and down arrows with control,
	that are standard keystrokes in Windows to control the cursor.
	Problem: At least Mac and Win VM generate ctrl-up and ctrl-down for
	mouse wheel events.
	I guess most people would prefer the mouse wheel to work properly..."
	(owner scrollByKeyboard: aKeyboardEvent)
		ifTrue: [ ^self ].

	autoCompleter 
		ifNil: [ self processKeyStroke: aKeyboardEvent ]
		ifNotNil: [
			autoCompleter
				autoCompletionAround: [ self processKeyStroke: aKeyboardEvent ]
				keyStroke: aKeyboardEvent ]! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:59'!
mouseDown: event
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	event mouseButton2Pressed ifTrue: [^ self mouseButton2Activity].

	"If we don't focus, Get focus, and do nothing else (the user will need to click again to do further interaction)"
	self hasKeyboardFocus ifFalse: [
		^event hand newKeyboardFocus: self].

	super mouseDown: event.

	self handleInteraction: [editor mouseDown: (event ztranslatedBy: self morphPositionInWorld negated)].

	event hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: event
		clkSel: nil
		clkNHalf: #clickAndHalf:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:
		tripleClkSel: nil! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:00'!
mouseMove: evt
	evt mouseButton1Pressed ifFalse: [^ self enterClickableRegion: evt].
	self handleInteraction: [ editor mouseMove: (evt ztranslatedBy: self morphPositionInWorld negated)].
	(evt eventPosition y - owner morphPositionInWorld y between: 0 and: owner morphExtentInWorld y) ifFalse: [
		owner scrollSelectionIntoView ]! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:00'!
mouseUp: evt
	super mouseUp: evt.
	self pauseBlinking.
	self handleInteraction: [editor mouseUp: (evt ztranslatedBy: self morphPositionInWorld negated)].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:51'!
handlesKeyboard

	^self visible! !

!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:52'!
handlesMouseDown: aMouseButtonEvent
	^ true! !


!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:01'!
mouseDown: anEvent
	super mouseDown: anEvent.
	self cursor show.
	hand _ anEvent hand.
	self startStepping.
	Preferences fastDragWindowForMorphic ifTrue: [
		indicator _ RectangleIndicatorMorph new.
		indicator morphBoundsInWorld: self initialIndicatorBounds.
		indicator openInWorld ]! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:01'!
mouseEnter: anEvent
	super mouseEnter: anEvent.
	self cursor show.
	hand _ anEvent hand.
	self startStepping! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:01'!
mouseLeave: anEvent
	super mouseLeave: anEvent.
	hand _ nil! !

!LayoutAdjustingMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:55'!
handlesMouseDown: aMouseButtonEvent

	^ true! !

!LayoutAdjustingMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:55'!
handlesMouseOver: evt

	^ true! !


!MagnifierMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:55'!
handlesMouseDown: aMouseButtonEvent
	^aMouseButtonEvent mouseButton2Pressed
		or: [super handlesMouseDown: aMouseButtonEvent]! !


!MenuItemMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:55'!
handlesMouseDown: aMouseButtonEvent

	^ true! !

!MenuItemMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:55'!
handlesMouseOver: anEvent
	^true! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:48'!
keyStroke: aKeyboardEvent 
	| matchString char asc selectable help |
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self activeHand.
	char := aKeyboardEvent keyCharacter.
	asc := char asciiValue.
	aKeyboardEvent isReturnKey
		ifTrue: [
			selectedItem ifNotNil: 
					[selectedItem hasSubMenu 
						ifTrue: [
							aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
							^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
						ifFalse: 
							["self delete."

							^selectedItem invokeWithEvent: aKeyboardEvent]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: aKeyboardEvent].
			^self].
	asc = 27 
		ifTrue: 
			["escape key"

			self valueOfProperty: #matchString
				ifPresentDo: 
					[:str | 
					str isEmpty 
						ifFalse: 
							["If filtered, first ESC removes filter"

							self setProperty: #matchString toValue: String new.
							self selectItem: nil event: aKeyboardEvent.
							^self displayFiltered: aKeyboardEvent]].
			"If a stand-alone menu, just delete it"
			popUpOwner ifNil: [^self delete].
			"If a sub-menu, then deselect, and return focus to outer menu"
			self selectItem: nil event: aKeyboardEvent.
			aKeyboardEvent hand newMouseFocus: popUpOwner owner.
			^aKeyboardEvent hand newKeyboardFocus: popUpOwner owner].
	(asc = 28 or: [asc = 29]) 
		ifTrue: 
			["left or right arrow key"

			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: 
					[aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]].
	asc = 30 ifTrue: [^self moveSelectionDown: -1 event: aKeyboardEvent].	"up arrow key"
	asc = 31 ifTrue: [^self moveSelectionDown: 1 event: aKeyboardEvent].	"down arrow key"
	asc = 11 ifTrue: [^self moveSelectionDown: -5 event: aKeyboardEvent].	"page up key"
	asc = 12 ifTrue: [^self moveSelectionDown: 5 event: aKeyboardEvent].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsentPut: [String new].
	matchString := char = Character backspace 
				ifTrue: 
					[matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: aKeyboardEvent keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: aKeyboardEvent.
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self activeHand.
! !

!MenuMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:52'!
handlesKeyboard
	"Answer whether the receiver handle keyboard events"

	^self visible! !

!MenuMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:56'!
handlesMouseDown: aMouseButtonEvent
	^true! !

!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 17:50'!
handleFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."
	| eventPositionInOurCoordinates |
	eventPositionInOurCoordinates _ self internalizeFromWorld: aMorphicEvent eventPosition.

	self dispatchEvent: aMorphicEvent localPosition: eventPositionInOurCoordinates.

	"Need to handle keyboard input if we have the focus."
	aMorphicEvent isKeyboard ifTrue: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(aMorphicEvent isMouseOver or: [aMorphicEvent isMouse not]) ifTrue: [ ^self ].
	"What remains are mouse buttons and moves"
	aMorphicEvent isMove ifFalse: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates ]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: aMorphicEvent) ifTrue: [^self]].
	"Note: The following does not traverse upwards but it's the best I can do for now"
	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: aMorphicEvent) ifTrue: [^self]].! !

!MenuMorph methodsFor: 'misc' stamp: 'jmv 8/20/2012 17:49'!
activate: evt
	"Receiver should be activated; e.g., so that control passes correctly."
	evt hand newMouseFocus: self.! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 19:12'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	^ aMorph processUnknownEvent: self localPosition: positionInAMorph! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 19:01'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	^aMorph processDropMorph: self localPosition: positionInAMorph! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 19:04'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue: [
		^ aMorph
			processKeystroke: self
			localPosition: positionInAMorph ].
	type == #keyDown ifTrue: [
		^ aMorph
			processKeyDown: self
			localPosition: positionInAMorph ].
	type == #keyUp ifTrue: [ 
		^ aMorph
			processKeyUp: self
			localPosition: positionInAMorph ].
	^ super
		sentTo: aMorph
		localPosition: positionInAMorph.! !


!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 19:10'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	type == #mouseOver ifTrue: [
		^aMorph processMouseOver: self localPosition: positionInAMorph ].
	type == #mouseEnter ifTrue: [
		^ aMorph processMouseEnter: self localPosition: positionInAMorph ].
	type == #mouseLeave ifTrue: [
		^aMorph processMouseLeave: self localPosition: positionInAMorph ].
	^ super sentTo: aMorph localPosition: positionInAMorph! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 19:12'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"

	type == #mouseDown ifTrue: [
		^aMorph processMouseDown: self localPosition: positionInAMorph ].
	type == #mouseUp ifTrue: [
		^aMorph processMouseUp: self localPosition: positionInAMorph ].
	^super sentTo: aMorph localPosition: positionInAMorph! !


!MouseMoveEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 19:09'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"

	type == #mouseMove ifTrue: [
		^aMorph processMouseMove: self localPosition: positionInAMorph ].
	^ super sentTo: aMorph localPosition: positionInAMorph! !


!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:00'!
clickAndHalf: evt
	self handleInteraction: [ self editor clickAndHalf: evt ]! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:57'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	"Return - check for special action
	Note: Code below assumes that this was some
	input field reacting on Return. Break the keyboard
	focus so that the receiver can be safely deleted.
	jmv - Currently not implemented"
	"
	evt isReturnKey ifTrue: [
		action _ self crAction.
		action ifNotNil: [
			evt hand newKeyboardFocus: nil.
			^action value ] ].
	"
	self pauseBlinking.
	self handleInteraction: [ self editor processKeyStroke: aKeyboardEvent ].
	self updateFromContents.
	super keyStroke: aKeyboardEvent  "sends to keyStroke event handler, if any"! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:07'!
mouseDown: event
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	event hand newKeyboardFocus: self.

	self handleInteraction: [
		self editor mouseDown: event index: (self characterIndexAtPoint: event eventPosition) ].

	event hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: event
		clkSel: nil
		clkNHalf: #clickAndHalf:
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:07'!
mouseEnter: event
	super mouseEnter: event.
	Preferences focusFollowsMouse
		ifTrue: [ event hand newKeyboardFocus: self ]! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:07'!
mouseMove: evt
	evt mouseButton1Pressed ifFalse: [^ self].
	self handleInteraction: [
		self editor mouseMove: evt index: (self characterIndexAtPoint: evt eventPosition) ]! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:07'!
mouseUp: evt
	super mouseUp: evt.
	self pauseBlinking
! !

!OneLineEditorMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:52'!
handlesKeyboard

	^self visible! !

!OneLineEditorMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:52'!
handlesMouseDown: aMouseButtonEvent
	^ true! !

!OneLineEditorMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:52'!
handlesMouseOver: evt
	"implements #mouseEnter: and/or #mouseLeave:"
	^true! !


!PasteUpMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:40'!
click: aMorphicEvent
	^self mouseButton2Activity! !

!PasteUpMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:04'!
mouseDown: event
	"Handle a mouse down event."

	| grabbedMorph |
	grabbedMorph _ self morphToGrab: event.
	grabbedMorph ifNotNil:[
		grabbedMorph isSticky ifTrue:[^self].
		^event hand grabMorph: grabbedMorph].

	event mouseButton2Pressed ifTrue: [^self mouseButton2Activity].

	super mouseDown: event.

	event hand
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: event
		clkSel: #click:
		clkNHalf: nil
		dblClkSel: #doubleClick:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PasteUpMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:04'!
windowEvent: aMorphicEvent
	self windowEventHandler
		ifNotNil: [^self windowEventHandler windowEvent: aMorphicEvent].

	aMorphicEvent windowEventType == #windowClose
		ifTrue: [
			^TheWorldMenu basicNew quitSession]
! !

!PasteUpMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:56'!
handlesMouseDown: aMouseButtonEvent
	^true! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:05'!
mouseDown: event
	event mouseButton2Pressed ifTrue: [ ^ self mouseButton2Activity ].
	isPressed _ true.
	self redrawNeeded.
	(actWhen == #buttonDown or: [ actWhen == #buttonStillDown ])
		ifTrue: [
			self performAction ]
		ifFalse: [
			"Don't make multi-click slower if we act on button down, just do multiple actions"
			event hand
				waitForClicksOrDragOrSimulatedMouseButton2: self
				event: event
				clkSel: nil
				clkNHalf: nil
				dblClkSel: #doubleClick:
				dblClkNHalfSel: nil
				tripleClkSel: nil ]! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:05'!
mouseEnter: event
	"The mouse entered the receiver"
	mouseIsOver _ true.
	self redrawNeeded.
	^super mouseEnter: event! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:05'!
mouseLeave: event
	"The mouse has left the area of the receiver"
	mouseIsOver _ false.
	self redrawNeeded.
	^super mouseLeave: event! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:05'!
mouseStillDown: evt
	"Acting when down (instead of waiting until releasing the button)
	also means that the button actin is repeated if the button is kept pressed.
	See #handlesMouseStillDown:"
	self performAction! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:05'!
mouseUp: evt
	isPressed _ false.
	mouseIsOver _ false.
	(actWhen == #buttonUp and: [self containsPoint: evt eventPosition])
		ifTrue: [ self performAction ].
	self redrawNeeded! !

!PluggableButtonMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:57'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^true! !

!PluggableButtonMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:57'!
handlesMouseOver: evt
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?" 
	^true! !

!PluggableButtonMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:57'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"
	"Acting when down (instead of waiting until releasing the button)
	also means that the button action is repeated if the button is kept pressed"
	^actWhen == #buttonStillDown! !


!DraggeableButtonMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:58'!
mouseDown: aMouseEvent
	"Inform the model that this button has been released. "
	super mouseDown: aMouseEvent.
	grabSelector ifNotNil: [
		model perform: grabSelector ]! !

!DraggeableButtonMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:59'!
mouseMove: aMouseEvent
	dragSelector ifNotNil: [
		model perform: dragSelector with: aMouseEvent targetPoint ]! !

!DraggeableButtonMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:59'!
mouseUp: aMouseEvent
	isPressed _ false.
	mouseIsOver _ false.
	actWhen == #buttonUp
		ifTrue: [ self performAction ].
	self redrawNeeded! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 8/20/2012 17:53'!
doubleClickAndHalf: evt
	"Some subclasses might do something"! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 8/20/2012 17:53'!
keyStroke: aKeyboardEvent

	( self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	super keyStroke: aKeyboardEvent.
	(self scrollByKeyboard: aKeyboardEvent)
		ifTrue: [ ^self ].
	scroller keyStroke: aKeyboardEvent! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 8/20/2012 17:53'!
mouseDown: event

	event mouseButton2Pressed  "First check for option (menu) click"
		ifTrue: [^ self mouseButton2Activity].
	scroller mouseDown: event.
	event hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: event
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:
		dblClkNHalfSel: #doubleClickAndHalf:
		tripleClkSel: nil! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 8/20/2012 18:14'!
mouseMove: evt
	scroller  mouseMove: evt! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 8/20/2012 17:53'!
mouseUp: evt
	super mouseUp: evt.
	scroller mouseUp: evt! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 8/20/2012 17:54'!
scrollByKeyboard: event
	"If event is ctrl+up/down then scroll and answer true"
	event controlKeyPressed ifFalse: [ ^ false ].
	event keyValue = 30 ifTrue: [
		scrollBar scrollUp: 3.
		^ true ].
	event keyValue = 31 ifTrue: [
		scrollBar scrollDown: 3.
		^ true ].
	^ false.! !

!PluggableScrollPane methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:57'!
handlesMouseDown: aMouseButtonEvent
	^ true! !

!PluggableScrollPane methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:57'!
handlesMouseOver: evt
	"subclasses implement #mouseEnter: and/or #mouseLeave:"
	^true! !


!HierarchicalListMorph methodsFor: 'commands' stamp: 'jmv 8/20/2012 17:55'!
expandAll
	(selectedMorph isNil
		or: [selectedMorph isExpanded])
		ifTrue: [^self].
	self expandAll: selectedMorph.
	scroller adjustExtent.
	self setScrollDeltas! !

!HierarchicalListMorph methodsFor: 'commands' stamp: 'jmv 8/20/2012 17:55'!
expandAll: aMorph
	| allChildren |
	aMorph toggleExpandedState.
	allChildren _ OrderedCollection new: 10.
	aMorph recursiveAddTo: allChildren.
	allChildren do: [:each | 
		(each canExpand and: [each isExpanded not])
			ifTrue: [self expandAll: each]].
! !

!HierarchicalListMorph methodsFor: 'commands' stamp: 'jmv 8/20/2012 17:55'!
toggleExpandedState: aMorph event: event
	| oldState |
	"self setSelectedMorph: aMorph."
	event mouseButton2Pressed ifTrue: [
		oldState _ aMorph isExpanded.
		scroller submorphs copy do: [ :each |
			(each canExpand and: [each isExpanded = oldState]) ifTrue: [
				each toggleExpandedState.
			].
		].
	] ifFalse: [
		aMorph toggleExpandedState.
	].
	scroller adjustExtent.
	self setScrollDeltas! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:56'!
keyStroke: aKeyboardEvent 
	"Process potential command keys"

	| args aCharacter |
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self scrollByKeyboard: aKeyboardEvent)
		ifTrue: [ ^self ].
	aCharacter _ aKeyboardEvent keyCharacter.
	(self arrowKey: aCharacter)
		ifTrue: [ ^self ].
	keystrokeActionSelector ifNil: [^self].
	(args _ keystrokeActionSelector numArgs) = 1 
		ifTrue: [^mainView perform: keystrokeActionSelector with: aCharacter].
	args = 2 
		ifTrue: [
			^mainView 
				perform: keystrokeActionSelector
				with: aCharacter
				with: self].
	^self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:55'!
mouseDown: evt

	| aMorph |
	evt hand newKeyboardFocus: self.
	aMorph _ self itemFromPoint: evt eventPosition.
	(aMorph notNil and: [ aMorph inToggleArea: evt eventPosition ])
		ifTrue: [ ^self toggleExpandedState: aMorph event: evt ]. 
	evt mouseButton2Pressed  "First check for option (menu) click"
		ifTrue: [ ^ self mouseButton2Activity ].
	aMorph ifNil: [ ^super mouseDown: evt ].
	aMorph highlightForMouseDown.
	evt hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: evt 
		clkSel: #click:
		clkNHalf: nil
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:10'!
mouseEnter: event
	super mouseEnter: event.
	Preferences focusFollowsMouse
		ifTrue: [ event hand newKeyboardFocus: self ]! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:11'!
mouseUp: event 
	| aMorph |
	aMorph := self itemFromPoint: event eventPosition.
	aMorph ifNil: [^self].
	aMorph highlightedForMouseDown ifFalse: [^self].
	aMorph highlightForMouseDown: false.
	"No change if model is locked"
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	((autoDeselect isNil or: [autoDeselect]) and: [aMorph == selectedMorph]) 
		ifTrue: [self setSelectedMorph: nil]
		ifFalse: [self setSelectedMorph: aMorph].
	Cursor normal show! !

!HierarchicalListMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:51'!
handlesKeyboard

	^self visible! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:41'!
doubleClick: aMorphicEvent
	| index |
	doubleClickSelector ifNil: [^super doubleClick: aMorphicEvent].
	index _ self rowAtLocation: aMorphicEvent eventPosition.
	index = 0 ifTrue: [ ^super doubleClick: aMorphicEvent ].
	"selectedMorph ifNil: [self setSelectedMorph: aMorph]."
	^ self model perform: doubleClickSelector! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:56'!
keyStroke: aKeyboardEvent 
	"Process keys"
	
	| aCharacter |
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self scrollByKeyboard: aKeyboardEvent) 
		ifTrue: [ ^self ].
	aCharacter _ aKeyboardEvent keyCharacter.
	(self arrowKey: aCharacter)
		ifTrue: [ ^self ].
	aCharacter asciiValue = 27 ifTrue: [	" escape key"
		^ self mouseButton2Activity].
	aKeyboardEvent anyModifierKeyPressed
		ifTrue: [
			(self keystrokeAction: aCharacter)
				ifTrue: [ ^self ]].
	^ self keyboardSearch: aCharacter! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:11'!
mouseEnter: event
	super mouseEnter: event.
	Preferences focusFollowsMouse
		ifTrue: [ event hand newKeyboardFocus: self ]! !

!PluggableListMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:53'!
handlesKeyboard

	^self visible! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 8/20/2012 18:06'!
mouseDown: event

	| oldIndex oldVal row w |
	event mouseButton2Pressed ifTrue: [^ self mouseButton2Activity].

	self hasKeyboardFocus ifFalse: [
		event hand newKeyboardFocus: self ].

	row _ self rowAtLocation: event eventPosition.

	row = 0 ifTrue: [^super mouseDown: event].

	w _ self ownerThatIsA: SystemWindow.
	(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

		"Set meaning for subsequent dragging of selection"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row.
		oldIndex _ self getCurrentSelectionIndex.
		oldIndex ~= 0 ifTrue: [oldVal _ self listSelectionAt: oldIndex].

		"Set or clear new primary selection (listIndex)"
		dragOnOrOff
			ifTrue: [self changeModelSelection: row]
			ifFalse: [self changeModelSelection: 0].

		"Need to restore the old one, due to how model works, and set new one."
		oldIndex ~= 0 ifTrue: [self listSelectionAt: oldIndex put: oldVal].
		self listSelectionAt: row put: dragOnOrOff.
		"event hand releaseMouseFocus: aMorph."
		"aMorph changed"
	].
	event hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: event
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 8/20/2012 18:06'!
mouseMove: event 
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row b |
	b _ self morphBoundsInWorld.
	row _ (event eventPosition y < b top and: [ scrollBar value > 0.0 ])
		ifTrue: [
			scrollBar scrollUp: 1.
			"Leave at least one visible item unaffected, for better visual feedback to the user."
			(self rowAtLocation: b topLeft) + 2 ]
		ifFalse: [
			(event eventPosition y > b bottom and: [ scrollBar value < 1.0 ])
				ifTrue: [
					scrollBar scrollDown: 1.
					"Leave at least one visible item unaffected, for better visual feedback to the user."
					(self rowAtLocation: b bottomLeft) - 3 ]
				ifFalse: [ self rowAtLocation: event eventPosition ]].
	row = 0 ifTrue: [ ^ self ].

	"No change if model is locked"
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].

	dragOnOrOff ifNil: [
		"Was not set at mouse down, which means the mouse must have gone down in an area where there was no list item"
		dragOnOrOff _ (self listSelectionAt: row) not ].

	"Set meaning for subsequent dragging of selection"
	oldIndex _ self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [ oldVal _ self listSelectionAt: oldIndex ].

	"Set or clear new primary selection (listIndex)"
	dragOnOrOff 
		ifTrue: [ self changeModelSelection: row ]
		ifFalse: [ self changeModelSelection: 0 ].

	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [ self listSelectionAt: oldIndex put: oldVal ].
	
	"Select all in between if drag was too fast"
	"self listSelectionAt: row put: dragOnOrOff."
	(row min: dragStartRow) to: (row max: dragStartRow) do: [ :r |
		self listSelectionAt: r put: dragOnOrOff ]! !

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 8/20/2012 18:06'!
mouseUp: event

	dragOnOrOff _ nil.  "So improperly started drags will have not effect"
	dragStartRow _ nil! !


!ScrollBar methodsFor: 'events' stamp: 'jmv 8/20/2012 18:07'!
mouseDown: aMouseEvent
	"Update visual feedback"
	super mouseDown: aMouseEvent.
	self setNextDirectionFromEvent: aMouseEvent.
	self scrollByPage! !

!ScrollBar methodsFor: 'events' stamp: 'jmv 8/20/2012 18:07'!
mouseStillDown: evt
	self scrollByPage! !

!ScrollBar methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:57'!
handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^true! !

!ScrollBar methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:57'!
handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"
	^true! !


!TextModelMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:42'!
doubleClick: aMorphicEvent
	self textMorph doubleClick: aMorphicEvent! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:12'!
doubleClickAndHalf: event
	self textMorph doubleClickAndHalf: event! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 17:57'!
keyStroke: aKeyboardEvent
	"A keystroke was hit while the receiver had keyboard focus.  Pass the keywtroke on to my textMorph, and and also, if I have an event handler, pass it on to that handler"

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	self textMorph keyStroke: aKeyboardEvent! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 8/20/2012 18:11'!
mouseEnter: event
	super mouseEnter: event.
	Preferences focusFollowsMouse
		ifTrue: [ event hand newKeyboardFocus: self textMorph ]! !


!TranscriptMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:58'!
handlesMouseDown: aMouseButtonEvent
	^ true! !


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 19:12'!
sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"

	^ aMorph processWindowEvent: self localPosition: positionInAMorph! !


!TranscriptMorph reorganize!
('drawing' drawOn:)
('event handling testing' handlesMouseDown:)
('geometry' morphExtent:)
('menus' getMenu mouseDown:)
('menu commands' clearAll clearFile clearInternal dontLogToFile editContents logToFile)
!

!methodRemoval: TextModelMorph #handlesKeyboard!
TextModelMorph removeSelector: #handlesKeyboard!

!TextModelMorph reorganize!
('accessing' editor textMorph wrapFlag:)
('dependents access' canDiscardEdits)
('drawing' drawOn: wantsFrameAdornments)
('editor access' clearSelection scrollSelectionIntoView selectAll)
('events' doubleClick: doubleClickAndHalf: keyStroke: mouseEnter:)
('event handling' mouseButton2Activity)
('focus handling' focusText)
('geometry' innerHeight: scrollDeltaHeight)
('initialization' acceptOnCR: innerMorphClass model:)
('interactive error protocol' correctFrom:to:with: nextTokenFrom:direction: notify:at:in: selectFrom:to: selectInvisiblyFrom:to: selectionInterval)
('model access' setSelection: setTextColor: text)
('notifications' possiblyChanged)
('scrolling' mightNeedHorizontalScrollBar)
('shout' autoCompleter: styler:)
('unaccepted edits' askBeforeDiscardingEdits: hasUnacceptedEdits:)
('updating' update:)
!


!ScrollBar reorganize!
('access' color: interval: scrollDelta:pageDelta: value)
('accessing' adoptWidgetsColor: model:setValueSelector:)
('drawing' drawOn:)
('events' mouseDown: mouseStillDown:)
('event handling testing' handlesMouseDown: handlesMouseStillDown:)
('geometry' buttonExtent computeSlider expandSlider freeSliderRoom morphExtent: totalSliderRoom)
('initialization' defaultBounds defaultColor)
('initialize' buttonClass initialize initializeDownButton initializeSlider initializeUpButton recreateSubmorphs sliderClass)
('model access' setValue: value:)
('scrolling' scrollAbsolute: scrollByPage scrollDown scrollDown: scrollUp scrollUp: setNextDirectionFromEvent: sliderGrabbed sliderReleased)
('testing' isHorizontal)
!


!PluggableListMorphOfMany reorganize!
('drawing' listSelectionAt: listSelectionAt:put:)
('events' mouseDown: mouseMove: mouseUp:)
('initialization' model:listGetter:primarySelectionGetter:primarySelectionSetter:listSelectionGetter:listSelectionSetter:mainView:menuGetter:keystrokeAction:)
('updating' update:)
('model access' itemSelectedAmongMultiple:)
!

!methodRemoval: PluggableListMorph #handleMouseMove:localPosition:!
PluggableListMorph removeSelector: #handleMouseMove:localPosition:!

!PluggableListMorph reorganize!
('accessing' rowAtLocation:)
('as yet unclassified' listMorph)
('drawing' drawOn: highlightSelection unhighlightSelection)
('events' doubleClick: keyStroke: mouseDown: mouseEnter: mouseUp:)
('event handling testing' handlesKeyboard)
('event handling' keyboardFocusChange:)
('events-processing' processMouseMove:localPosition:)
('geometry' scrollDeltaHeight scrollDeltaWidth)
('initialization' autoDeselect: font font: initForKeystrokes initialize innerMorphClass listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction: textColor)
('keyboard navigation' arrowKey:)
('menu' getMenu)
('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard setListFont)
('model access' changeModelSelection: getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: keyboardSearch: keystrokeAction:)
('selection' getListSelector maximumSelection minimumSelection numSelectionsInView scrollSelectionIntoView selectedMorph: selection selection: selectionIndex selectionIndex:)
('updating' update: updateList verifyContents)
!

!methodRemoval: HierarchicalListMorph #handleMouseMove:localPosition:!
HierarchicalListMorph removeSelector: #handleMouseMove:localPosition:!

!HierarchicalListMorph reorganize!
('accessing' columns lineColor)
('drawing' drawLinesOn: drawOn: expandedForm highlightSelection notExpandedForm unhighlightSelection)
('commands' expandAll expandAll: toggleExpandedState:event:)
('events' keyStroke: mouseDown: mouseEnter: mouseUp:)
('event handling testing' handlesKeyboard)
('event handling' itemFromPoint: keyboardFocusChange:)
('events-processing' processMouseMove:localPosition:)
('geometry' scrollDeltaHeight scrollDeltaWidth)
('initialization' autoDeselect: currentlyExpanded indentingItemClass innerMorphClass list: listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction:)
('keyboard navigation' arrowKey: getSelectionIndex setSelectionIndex: toggleExpandedState:)
('model access' getList)
('selection' getCurrentSelectionItem maximumSelection minimumSelection numSelectionsInView scrollSelectionIntoView selectedMorph: selection: selectionIndex: setSelectedMorph:)
('updating' update:)
('private' addMorphsTo:from:allowSorting:withExpandedItems:atLevel: addSubmorphsAfter:fromCollection:allowSorting: insertNewMorphs: noteRemovalOfAll:)
('menu' getMenu)
!


!PluggableScrollPane reorganize!
('access' addToScroller: adoptWidgetsColor: drawKeyboardFocusIndicator: verticalScrollBar)
('access options' hideScrollBarsIndefinitely)
('events' doubleClickAndHalf: keyStroke: mouseDown: mouseMove: mouseUp: scrollByKeyboard:)
('event handling testing' handlesMouseDown: handlesMouseOver:)
('geometry' borderWidth: focusIndicatorRectangle hLeftoverScrollRange hScrollBarWidth hSetScrollDelta hTotalScrollRange mightNeedHorizontalScrollBar morphExtent: scrollDeltaHeight scrollDeltaWidth scrollerOffset scrollerOffset: setScrollDeltas someSubmorphPositionOrExtentChanged updateScrollBarsBounds vLeftoverScrollRange vScrollBarHeight vSetScrollDelta vTotalScrollRange viewableBounds viewableHeight viewableWidth xtraBorder)
('geometry testing' hIsScrollbarShowing vIsScrollbarShowing)
('initialization' defaultBorderColor initialize innerMorphClass scrollBarClass)
('scroll bar events' mouseButton2Activity)
('scrolling' hHideScrollBar hIsScrollbarNeeded hScrollBarValue: hShowScrollBar hideOrShowScrollBars scrollBy: scrollToShow: vHideScrollBar vIsScrollbarNeeded vScrollBarValue: vShowScrollBar)
('change reporting' invalidateBorderFeedback)
('drawing' clipsSubmorphs)
!


!DraggeableButtonMorph reorganize!
('initialization' initialize)
('accessing' dragSelector: grabSelector:)
('events' mouseDown: mouseMove: mouseUp:)
('testing' isRoundButton)
!


!PluggableButtonMorph reorganize!
('accessing' actWhen: action: actionSelector adoptWidgetsColor: icon: label: label:font: performAction roundButtonStyle:)
('drawing' clipsSubmorphs draw3DLookOn: drawEmbossedLabelOn: drawInconOn: drawOn: drawRegularLabelOn: drawRoundGradientLookOn: fontToUse iconColor)
('events' mouseDown: mouseEnter: mouseLeave: mouseStillDown: mouseUp:)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseStillDownStepRate)
('initialization' defaultBorderWidth defaultColor initialize model:)
('initialize-release' model:stateGetter:action:label:)
('updating' update:)
('private' getModelState magnifiedIcon mouseButton2Activity)
('testing' isPressed isRoundButton mouseIsOver)
('geometry' morphExtent:)
('geometry testing' containsPoint: isOrthoRectangularMorph)
('scrollbar button' updateDownButtonImage updateLeftButtonImage updateRightButtonImage updateUpButtonImage)
!


!PasteUpMorph reorganize!
('WiW support' shouldGetStepsFrom:)
('accessing' color:)
('alarms-scheduler' addAlarm:withArguments:for:at: removeAlarm:for:)
('caching' releaseCachedState)
('change reporting' invalidRect:from: redrawNeeded)
('classification' isPlayfieldLike isWorldMorph)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: dropEnabled morphToDropFrom: repelsMorph:event: wantsDroppedMorph:event:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('events' click: mouseDown: windowEvent:)
('event handling testing' handlesMouseDown:)
('event handling' morphToGrab: mouseButton2Activity wantsWindowEvent: windowEventHandler)
('events-processing' dispatchEvent:localPosition:)
('geometry' externalizeToWorld: internalizeFromWorld: morphExtent: morphPositionInWorld)
('initialization' becomeActiveDuring: clearWaitDelay defaultBorderColor defaultBorderWidth defaultColor initialize)
('interaction loop' doOneCycleNow)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand: addWorldToggleItemsToHaloMenu: deleteBalloonTarget:)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('printing' printOn:)
('project state' canvas firstHand hands handsDo: handsReverseDo: listOfSteppingMorphs stepListSize steppingMorphsNotInWorld viewBox viewBox:)
('stepping' cleanseStepList runLocalStepMethods runStepMethods startStepping: startStepping:at:selector:arguments:stepTime: stopStepping: stopStepping:selector:)
('stepping and presenter' step wantsSteps)
('structure' world)
('submorphs-accessing' allMorphsDo:)
('submorphs-add/remove' addAllMorphs: addMorphFront:)
('testing' isReallyVisible stepTime)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows expandAll findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu openRecentSubmissionsBrowser:)
('world state' addMorph:centeredNear: allNonFlapRelatedSubmorphs assuredNonDisplayCanvas deleteAllHalos displayWorld displayWorldSafely doOneCycle doOneSubCycle flashRects: fullRepaintNeeded haloMorphs install privateOuterDisplayWorld restoreMorphicDisplay startSteppingSubmorphsOf:)
('private')
!


!OneLineEditorMorph reorganize!
('accessing' baseFont contents contents: editor fitContents fontToUse keyboardFocusWatcher: measureContents)
('blink cursor' onBlinkCursor pauseBlinking startBlinking stopBlinking)
('drawing' characterIndexAtPoint: displayInsertionMarkAtX:top:bottom:emphasis:on: drawCaretOn: drawOn: drawSelectionOn:)
('editing' handleInteraction:)
('events' clickAndHalf: keyStroke: mouseDown: mouseEnter: mouseMove: mouseUp:)
('event handling testing' handlesKeyboard handlesMouseDown: handlesMouseOver:)
('events-processing' focusKeyboardFor: keyboardFocusChange:)
('initialization' defaultColor initWithContents:font:emphasis: initialize)
('testing' hasSelection hasVisibleCaret)
('typing/selecting keys' clearSelection selectAll)
('unaccepted edits' hasUnacceptedEdits:)
('private' installEditor mouseButton2Activity updateFromContents)
!


!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary: defaultTarget items lastItem popUpOwner stayUp stayUp:)
('construction' add:action: add:selector:argument: add:subMenu: add:target:action: add:target:selector: add:target:selector:argument: add:target:selector:argumentList: addLine addList: addServices:for:extraLines: addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:selector:argumentList: addWithLabel:enablement:action: addWithLabel:enablementSelector:target:selector:argumentList: balloonTextForLastItem: defaultTarget: labels:lines:selections: title:)
('control' activeSubmenu: deleteIfPopUp: popUpAdjacentTo:forHand:from: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem:event: wantsToBeDroppedInto:)
('dropping/grabbing' justDroppedInto:event:)
('events' keyStroke: mouseDown: mouseUp:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('events-processing' handleFocusEvent:)
('initialization' defaultBorderWidth defaultColor delete initialize intoWorld: setTitleParametersFor:)
('keyboard control' displayFiltered: keyboardFocusChange: moveSelectionDown:event:)
('menu' addCustomMenuItems:hand: addItem addTitle removeStayUpBox sightTarget: target:)
('modal control' invokeModal invokeModal: invokeModalAt:in:allowKeyboard: isModalInvokationDone isModalInvokationDone: modalSelection modalSelection:)
('private' adjustSubmorphsLayout morphicLayerNumber positionAt:relativeTo: selectedItem)
('drawing' drawOn:)
('geometry' moveRight:)
('misc' activate:)
!

!methodRemoval: MenuItemMorph #handleMouseEnter:localPosition:!
MenuItemMorph removeSelector: #handleMouseEnter:localPosition:!
!methodRemoval: MenuItemMorph #handleMouseLeave:localPosition:!
MenuItemMorph removeSelector: #handleMouseLeave:localPosition:!
!methodRemoval: MenuItemMorph #handleMouseUp:localPosition:!
MenuItemMorph removeSelector: #handleMouseUp:localPosition:!

!MenuItemMorph reorganize!
('accessing' arguments arguments: contentString contentString: contents: contents:withMarkers: contents:withMarkers:inverse: hasIcon hasIconOrMarker hasMarker hasSubMenu icon icon: isEnabled isEnabled: selector selector: subMenu subMenu: target target:)
('drawing' drawOn:)
('event handling testing' handlesMouseDown: handlesMouseOver:)
('events' activateOwnerMenu: activateSubmenu: deselectTimeOut: invokeWithEvent: mouseDown: mouseEnter: mouseLeave: mouseUp:)
('events-processing' processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseUp:localPosition:)
('grabbing' aboutToBeGrabbedBy: duplicateMorph:)
('initialization' defaultBounds deleteIfPopUp: initialize)
('layout' iconSeparation measureContents minItemWidth)
('selecting' deselect: isSelected: select:)
('private' offImage onImage)
!


!MagnifierMorph reorganize!
('initialization' defaultBorderWidth initialize)
('geometry' borderWidth: defaultExtent morphExtent:)
('drawing' drawOn: hasTranslucentColor)
('stepping' step stepTime wantsSteps)
('events' mouseDown:)
('event handling testing' handlesMouseDown:)
('menu' addCustomMenuItems:hand: chooseMagnification chooseMagnification: toggleTrackingPointer trackingPointerString)
('magnifying' magnification: magnifiedForm sourcePoint sourceRect sourceRectFrom:)
('round view' isRound toggleRoundString toggleRoundness)
!


!LayoutAdjustingMorph reorganize!
('accessing' adoptWidgetsColor: cursor handPoint initialIndicatorBounds)
('adjusting' adjustIndicatorAt: adjustOwnerAt:)
('drawing' drawOn:)
('events' mouseDown: mouseEnter: mouseLeave:)
('event handling testing' handlesMouseDown: handlesMouseOver:)
('stepping' step stepTime)
('testing' isOpaqueMorph)
!

!methodRemoval: InnerTextMorph #handleKeystroke:localPosition:!
InnerTextMorph removeSelector: #handleKeystroke:localPosition:!
!methodRemoval: InnerTextMorph #handleMouseMove:localPosition:!
InnerTextMorph removeSelector: #handleMouseMove:localPosition:!

!InnerTextMorph reorganize!
('accessing' askBeforeDiscardingEdits: autoCompleter: contents: contentsAsIs: crAction editor hasEditingConflicts hasEditingConflicts: isWrapped model: model:wrappedTo: textColor textColor: wrapFlag:)
('anchors' anchorMorph:at:)
('caching' releaseCachedState)
('classification')
('drawing' debugDrawLineRectsOn: drawOn:)
('editing' acceptContents acceptOnCR cancelEdits chooseEmphasisOrAlignment chooseFont enterClickableRegion: handleInteraction: hasUnacceptedEdits:)
('events' clickAndHalf: keyStroke: mouseDown: mouseMove: mouseUp:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('event handling' doubleClickAndHalf: keyboardFocusChange: processKeyStroke:)
('events-processing' processKeystroke:localPosition: processMouseMove:localPosition:)
('geometry' adjustExtent minimumExtent morphExtent:)
('initialization' defaultColor initialize)
('menu' addCustomMenuItems:hand: getMenu wrapOnOff wrapString)
('submorphs-add/remove' addMorphFrontFromWorldPosition: styler:)
('private' extentForComposing fit installEditorAndParagraph mouseButton2Activity paragraph releaseEditorAndParagraph removedMorph: resetParagraph selectionChanged updateFromParagraph)
('notifications' possiblyChanged)
('testing' canDiscardEdits hasUnacceptedEdits isOpaqueMorph)
('blinking cursor' onBlinkCursor pauseBlinking startBlinking stopBlinking)
('accept/cancel' acceptOnCR:)
('macpal' flash)
('miscellaneous' clearSelection disregardUnacceptedEdits selectAll)
('shout' formatAndStyleIfNeeded okToStyle stylerStyled)
('selection' scrollSelectionIntoView selectionRects)
!


!HandleMorph reorganize!
('dropping/grabbing' justDroppedInto:event:)
('events' keyStroke:)
('event handling')
('initialization' initialize)
('initialize' forEachPointDo:)
('stepping and presenter' step)
('testing' stepTime)
!


!HaloMorph reorganize!
('WiW support' morphicLayerNumber)
('accessing' haloBox: setTarget: target target:)
('drawing' drawOn:)
('dropping/grabbing' startDrag:with:)
('events' mouseMove:)
('event handling' popUpFor:event: staysUpWhenMouseIsDownIn: transferHalo:)
('events-processing' containsPoint:event: rejectsEvent:)
('geometry' morphFullBoundsInWorld)
('geometry testing' containsPoint: isOrthoRectangularMorph)
('handles' addCollapseHandle: addDebugHandle: addDismissHandle: addDragHandle: addDupHandle: addFontEmphHandle: addFontSizeHandle: addGrabHandle: addGrowHandle: addHelpHandle: addMenuHandle: addRecolorHandle: addRotateHandle: positionIn:horizontalPlacement:verticalPlacement:)
('initialization' defaultColor initialize)
('meta-actions' mouseButton3Down:)
('stepping' step)
('testing' stepTime)
('updating' redrawNeeded)
('private' addHandle: addHandles addHandlesForWorldHalos addName addNameBeneath:string: basicBox doDebug:with: doDrag:with: doDup:with: doGrab:with: doGrow:with: doMenu:with: doRecolor:with: doRot:with: endInteraction handleSize maybeCollapse:with: maybeDismiss:with: maybeDoDup:with: mouseDownInCollapseHandle:with: removeAllHandlesBut: setDismissColor:with: startGrow:with: startRot:with:)
('forward to target' chooseEmphasisOrAlignment chooseFont deleteBalloon mouseDownOnHelpHandle:)
!

!methodRemoval: HaloHandleMorph #handleMouseDown:localPosition:!
HaloHandleMorph removeSelector: #handleMouseDown:localPosition:!
!methodRemoval: HaloHandleMorph #handleMouseMove:localPosition:!
HaloHandleMorph removeSelector: #handleMouseMove:localPosition:!
!methodRemoval: HaloHandleMorph #handleMouseUp:localPosition:!
HaloHandleMorph removeSelector: #handleMouseUp:localPosition:!
!methodRemoval: HaloHandleMorph #handlesKeyboard!
HaloHandleMorph removeSelector: #handlesKeyboard!
!methodRemoval: HaloHandleMorph #handlesMouseOver:!
HaloHandleMorph removeSelector: #handlesMouseOver:!

!HaloHandleMorph reorganize!
('accessing' keyStrokeSelector: mouseDownSelector: mouseMoveSelector: mouseUpSelector:)
('act' send:withEvent:)
('drawing' drawOn:)
('event handling testing' handlesMouseDown: handlesMouseStillDown:)
('events-processing' processMouseDown:localPosition: processMouseMove:localPosition: processMouseUp:localPosition:)
!


!FillInTheBlankMorph reorganize!
('accessing' response response: selectionInterval)
('events' mouseDown:)
('event handling testing' handlesMouseDown:)
('initialization' autoCompleterClass createAcceptButton createCancelButton createQueryTextMorph: createTextPaneExtent:acceptBoolean: defaultColor delete initialize responseUponCancel: setQuery:initialAnswer:answerExtent:acceptOnCR: setQuery:initialAnswer:answerHeight:acceptOnCR:)
('invoking' getUserResponse morphicLayerNumber)
('menu' acceptClicked cancelClicked)
('drawing' drawOn:)
!


!AutoCompleterMorph reorganize!
('accessing' itemHeight selected selected:)
('actions' end help home moveDown moveUp pageDown pageUp resetMenu)
('as yet unclassified' lastActivity stillActive timeOfLastActivity timeout updateColor)
('drawing' drawOn:)
('events' mouseUp:)
('event handling testing' handlesMouseDown:)
('initialization' defaultBorderColor defaultBorderWidth defaultColor setCompleter:position:)
('paging' currentPage gotoPage: pageCount)
('stepping' step stepTime wantsSteps)
('private' firstVisible lastVisible visibleItemsCount)
!

!methodRemoval: Morph #handleDropMorph:localPosition:!
Morph removeSelector: #handleDropMorph:localPosition:!
!methodRemoval: Morph #handleKeyDown:localPosition:!
Morph removeSelector: #handleKeyDown:localPosition:!
!methodRemoval: Morph #handleKeyUp:localPosition:!
Morph removeSelector: #handleKeyUp:localPosition:!
!methodRemoval: Morph #handleKeystroke:localPosition:!
Morph removeSelector: #handleKeystroke:localPosition:!
!methodRemoval: Morph #handleMouseDown:localPosition:!
Morph removeSelector: #handleMouseDown:localPosition:!
!methodRemoval: Morph #handleMouseEnter:localPosition:!
Morph removeSelector: #handleMouseEnter:localPosition:!
!methodRemoval: Morph #handleMouseLeave:localPosition:!
Morph removeSelector: #handleMouseLeave:localPosition:!
!methodRemoval: Morph #handleMouseMove:localPosition:!
Morph removeSelector: #handleMouseMove:localPosition:!
!methodRemoval: Morph #handleMouseOver:localPosition:!
Morph removeSelector: #handleMouseOver:localPosition:!
!methodRemoval: Morph #handleMouseStillDown:localPosition:!
Morph removeSelector: #handleMouseStillDown:localPosition:!
!methodRemoval: Morph #handleMouseUp:localPosition:!
Morph removeSelector: #handleMouseUp:localPosition:!
!methodRemoval: Morph #handleUnknownEvent:localPosition:!
Morph removeSelector: #handleUnknownEvent:localPosition:!
!methodRemoval: Morph #handleWindowEvent:localPosition:!
Morph removeSelector: #handleWindowEvent:localPosition:!

!Morph reorganize!
('accessing' adoptWidgetsColor: balloonText beSticky color color: isLocked isSticky lock lock: resistsRemoval sticky: toggleResistsRemoval toggleStickiness unlock unlockContents)
('accessing - extension' assureExtension hasExtension initializeExtension privateExtension: resetExtension)
('accessing - properties' hasProperty: removeProperty: setProperty:toValue: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits rotationDegrees:)
('caching' fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidRect: invalidRect:from: privateInvalidateMorph:)
('classification' isPlayfieldLike isWorldMorph)
('copying' copy copyForClipboard duplicate)
('debug and other' addDebuggingItemsTo:hand: altSpecialCursor0 altSpecialCursor1 altSpecialCursor2 altSpecialCursor3 altSpecialCursor3: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' addPossiblyUncoveredAreasIn:to: changeClipSubmorphs clipSubmorphs: clippingBounds clipsSubmorphs drawDropHighlightOn: drawErrorOn: drawMouseDownHighlightOn: drawOn: drawSubmorphsOn: drawingFails drawingFailsNot fullDrawOn: hasClipSubmorphsString hide highlightForMouseDown highlightForMouseDown: highlightedForMouseDown imageForm imageForm: imageForm:forRectangle: isKnownFailing ownShadowForm refreshWorld shadowForm show visible visible:)
('dropping/grabbing' aboutToBeGrabbedBy: dragEnabled dragEnabled: dragNDropEnabled dropEnabled dropEnabled: dropHighlightColor enableDrag: enableDragNDrop enableDragNDrop: enableDrop: highlightForDrop highlightForDrop: highlightedForDrop justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: repelsMorph:event: resetHighlightForDrop separateDragAndDrop wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click: doubleClick: keyDown: keyStroke: keyUp: mouseDown: mouseEnter: mouseLeave: mouseMove: mouseStillDown: mouseUp: windowEvent:)
('event handling testing' handlesKeyboard handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:at: addAlarm:with:after: addAlarm:with:at: addAlarm:with:with:after: addAlarm:with:with:at: addAlarm:withArguments:after: addAlarm:withArguments:at: alarmScheduler removeAlarm:)
('events-processing' closeWindowFor: containsPoint:event: dispatchEvent:localPosition: focusKeyboardFor: handleFocusEvent: processDropMorph:localPosition: processKeyDown:localPosition: processKeyUp:localPosition: processKeystroke:localPosition: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseStillDown:localPosition: processMouseUp:localPosition: processUnknownEvent:localPosition: processWindowEvent:localPosition: rejectDropEvent: rejectsEvent:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus keyboardFocusChange:)
('geometry' basicExtent: externalize: externalizeDistanceToWorld: externalizeToWorld: innerBounds internalize: internalizeFromWorld: minimumExtent morphBoundsInWorld morphBoundsInWorld: morphExtent morphExtent: morphExtentInWorld morphFullBoundsInWorld morphHeight morphHeight: morphPosition morphPosition: morphPositionInOwner morphPositionInOwner: morphPositionInWorld morphPositionInWorld: morphWidth morphWidth: validateNotSent validateOwnerNotNil worldBoundsForHalo)
('geometry eToy' referencePosition referencePosition:)
('geometry testing' containsPoint: fullContainsPoint: isOrthoRectangularMorph)
('halos and balloon help' addHalo addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: addWorldHandlesTo:box: balloonHelpDelayTime comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloClass mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: setBalloonText:maxLineLength: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHaloHandleWithSelector:inHalo:)
('initialization' defaultBounds inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' acceptDroppingMorph:event: computeFullBounds layoutBounds layoutSubmorphs layoutSubmorphsAndComputeFullBounds someSubmorphPositionOrExtentChanged submorphBounds)
('layout-properties' layoutSpec layoutSpec:)
('macpal' flash)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor changeDragAndDrop collapse expand exportAsBMP exportAsJPEG exportAsPNG hasDragAndDropEnabledString lockUnlockMorph lockedString maybeAddCollapseItemTo: resistsRemovalString stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: changeColorTarget:selector:originalColor:hand: copyToClipboard: dismissMorph duplicateMorph: grabMorph: maybeDuplicateMorph mouseButton3Down: mouseButton3Up: potentialEmbeddingTargets resizeFromMenu resizeMorph)
('naming' nameForFindWindowFeature)
('object serialization' objectForDataStream:)
('player' okayToDuplicate)
('printing' printOn:)
('property extension' extension)
('rotate scale and flex' rotationDegrees)
('stepping' wantsSteps)
('stepping and presenter' arrangeToStartStepping arrangeToStartSteppingIn: shouldGetStepsFrom: startStepping startStepping:at:arguments:stepTime: startSteppingSelector: step stepAt: stopStepping stopSteppingSelector:)
('structure' activeHand allOwners allOwnersDo: firstOwnerSuchThat: hasOwner: isInWorld outermostWorldMorph owner ownerThatIsA: pasteUpMorph root veryLastLeave withAllOwnersDo: world)
('submorphs-accessing' allMorphs allMorphsDo: findA: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph morphsAt: morphsAt:behind:unlocked: morphsAt:unlocked: morphsAt:unlocked:do: noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorphBack: addMorphFront: addMorphFrontFromWorldPosition: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('submorphs-add/remove-layers' addMorphInFrontOfLayer: addMorphInLayer: morphicLayerNumber morphicLayerNumberWithin:)
('testing' hasModel isOpaqueMorph isReallyVisible shouldDropOnMouseUp stepTime)
('updating' redrawNeeded update:)
('user interface' activateWindow activateWindowAndSendTopToBack:)
('visual properties' defaultColor)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateOwner: privateRemove:)
!

