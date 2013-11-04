'From Cuis 4.2 of 25 July 2013 [latest update: #1858] on 3 November 2013 at 7:12:32.103586 pm'!

!HandMorph methodsFor: 'accessing' stamp: 'jmv 11/3/2013 17:59'!
lastMouseEvent
	^ lastMouseEvent! !

!HandMorph methodsFor: 'accessing' stamp: 'jmv 11/3/2013 19:09'!
lastMouseEventTime
	^ lastMouseEventTime! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 11/3/2013 17:59'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	self runningWorld addMorphFront: self.
	self positionAt: aPoint forHand: hand.
	"Acquire focus for valid pop up behavior"
	oldKeyboardFocus _ hand keyboardFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !


!HaloMorph methodsFor: 'event handling' stamp: 'jmv 11/3/2013 17:59'!
popUpFor: aMorph event: aMorphicEvent
	"This message is sent by morphs that explicitly request the halo on a button click. Note: anEvent is in aMorphs coordinate frame."

	| hand anEvent |
	self flag: #workAround.	"We should really have some event/hand here..."
	anEvent _ aMorphicEvent
				ifNil: [
					hand _ aMorph world activeHand.
					hand ifNil: [ hand _ aMorph world firstHand ]. 
					hand lastMouseEvent ]
				ifNotNil: [
					hand _ aMorphicEvent hand.
					aMorphicEvent ].
	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset _ anEvent eventPosition - aMorph morphPositionInWorld.
	self startStepping! !


!HandMorph methodsFor: 'event handling' stamp: 'jmv 11/3/2013 19:09'!
processEventQueue
	"Process user input events from the local input devices."

	| evt evtBuf type hadAny mcs |
	mcs _ mouseClickState.
	hadAny := false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		evt := nil.	"for unknown event types"
		type := evtBuf first.
		type = EventSensor eventTypeMouse
			ifTrue: [ evt := self generateMouseEvent: evtBuf ].
		type = EventSensor eventTypeKeyboard 
			ifTrue: [ evt := self generateKeyboardEvent: evtBuf ].
		type = EventSensor eventTypeWindow
			ifTrue: [ evt _ self generateWindowEvent: evtBuf ].
		"All other events are ignored"
		evt
			ifNil: [
				^hadAny]
			ifNotNil: [
				"Finally, handle it"
				self startEventDispatch: evt.
				hadAny := true.
				"For better user feedback, return immediately after a mouse event has been processed."
				evt isMouse ifTrue: [ ^hadAny ]]].
	"note: if we come here we didn't have any mouse events"
	mcs 
		ifNotNil: [ 
			"No mouse events during this cycle. Make sure click states time out accordingly"
			mcs
				handleEvent: (self lastMouseEvent asMouseMove: (Time localMillisecondClock - self lastMouseEventTime max: 0))
				from: self ].
	^hadAny! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 11/3/2013 19:04'!
startKeyboardDispatch: aKeyboardEvent

	self keyboardFocus ifNotNil: [
		keyboardFocus handleFocusEvent: aKeyboardEvent ].
	self mouseOverHandler processMouseOver: self lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 11/3/2013 17:59'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		^self mouseFocus
			ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
			ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]].

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
				ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]
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
					mouseClickState ifNil: [self dropMorphs: aMouseEvent ]]
				ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]]].
		self mouseOverHandler processMouseOver: self lastMouseEvent! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 11/3/2013 19:02'!
handleInteraction: interactionBlock
	"Perform the changes in interactionBlock, noting any change in selection
	and possibly a change in the size of the composition"
	| oldSize m |
	oldSize _ model textSize.
	m _ model.
	self selectionChanged.  "Note old selection"

	interactionBlock value.

	self selectionChanged.  "Note new selection"
	oldSize = m textSize
		ifFalse: [ self updateFromTextComposition ]! !


!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 11/3/2013 17:59'!
step
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
			self adjustOwnerAt: p ]
		ifFalse: [
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: p)) ifFalse: [
				hand _ nil.
				Cursor normal show.
				self stopStepping ]]! !


!PopUpMenu class methodsFor: 'dialogs' stamp: 'jmv 11/3/2013 18:59'!
inform: aString
	"PopUpMenu inform: 'I like Cuis'"

		WorldState addDeferredUIMessage: [ (PopUpMenu labels: ' OK ') startUpWithCaption: aString ]! !

!methodRemoval: HandMorph #lastEvent!
HandMorph removeSelector: #lastEvent!
