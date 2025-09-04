'From Cuis7.5 [latest update: #7516] on 4 September 2025 at 4:02:21 pm'!

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 15:56:13'!
startDropEventDispatch: aDropEvent

	aDropEvent dispatchIn: owner.! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 15:56:03'!
startDropFilesEventDispatch: aDropFilesEvent

	aDropFilesEvent dispatchIn: owner.! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 15:57:40'!
startKeyboardDispatch: aKeyboardEvent

	| focusedElement |
	
	focusedElement := self keyboardFocus ifNil: [ self world ].
	aKeyboardEvent wasHandled not ifTrue: [
		aKeyboardEvent sendEventTo: focusedElement ].! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 15:58:18'!
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
		
	self mouseOverHandler processMouseOver: lastMouseEvent.! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 15:55:43'!
startWindowEventDispatch: aWindowEvent

	aWindowEvent dispatchIn: owner.! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 9/4/2025 15:57:30'!
dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| morphData dropEvent |
	morphData := self grabMorphDataFor: aMorph.
	dropEvent := DropEvent new 
			setPosition: self morphPosition 
			contents: aMorph 
			hand: self
			formerOwner: (morphData at: 1)
			formerPosition: (morphData at: 2).
	dropEvent dispatchIn: owner.
	dropEvent wasHandled ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self forgetGrabMorphDataFor: aMorph.! !

