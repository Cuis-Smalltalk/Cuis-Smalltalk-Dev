'From Cuis 4.0 of 21 April 2012 [latest update: #1380] on 20 August 2012 at 8:28:59 pm'!

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 20:07'!
startDropEventDispatch: aDropEvent

	owner dispatchEvent: aDropEvent localPosition: aDropEvent eventPosition.
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 20:24'!
startKeyboardDispatch: aKeyboardEvent

	self keyboardFocus ifNotNil: [
		keyboardFocus handleFocusEvent: aKeyboardEvent ].
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 20:28'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		^self mouseFocus
			ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
			ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]].

	"any mouse event but mouseOver"
	lastMouseEvent _ aMouseEvent.	
	lastMouseEventTime _ Time millisecondClockValue.

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
				ifTrue: [ self dropMorphs: aMouseEvent ]
				ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]]].
		self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 20:08'!
startWindowEventDispatch: aWindowEvent

	owner dispatchEvent: aWindowEvent localPosition: aWindowEvent eventPosition.
	self mouseOverHandler processMouseOver: lastMouseEvent! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 20:10'!
startDispatchFrom: aHand
	"double dispatch the event dispatch"
	"An event of an unknown type was sent. What shall we do?!!"

	Beeper beep. 
	self printString displayAt: 0@0.
	self wasHandled: true! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 20:10'!
startDispatchFrom: aHand
	"double dispatch the event dispatch"

	aHand startDropEventDispatch: self! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 20:09'!
startDispatchFrom: aHand
	"double dispatch the event dispatch"

	aHand startKeyboardDispatch: self! !


!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 20:28'!
startDispatchFrom: aHand
	"double dispatch the event dispatch"

	aHand startMouseDispatch: self! !


!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 8/20/2012 20:09'!
startDispatchFrom: aHand
	"double dispatch the event dispatch"

	aHand startWindowEventDispatch: self! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 20:10'!
startEventDispatch: aMorphicEvent

	owner ifNil: [ ^ self ].
	aMorphicEvent startDispatchFrom: self! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/20/2012 20:23'!
keyboardFocus

	keyboardFocus ifNotNil: [
		keyboardFocus world
			ifNil: [ keyboardFocus _ nil ]].
	^ keyboardFocus! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/20/2012 20:24'!
mouseFocus

	mouseFocus ifNotNil: [
		mouseFocus world
			ifNil: [ mouseFocus _ nil ]].
	^mouseFocus! !

!methodRemoval: PasteUpMorph #becomeActiveDuring:!
PasteUpMorph removeSelector: #becomeActiveDuring:!
!methodRemoval: HandMorph #sendFocusEvent:to:in:!
HandMorph removeSelector: #sendFocusEvent:to:in:!
!methodRemoval: HandMorph #sendKeyboardEvent:!
HandMorph removeSelector: #sendKeyboardEvent:!
!methodRemoval: HandMorph #sendMouseEvent:!
HandMorph removeSelector: #sendMouseEvent:!
!methodRemoval: HandMorph #startMouseOverDispatch:!
HandMorph removeSelector: #startMouseOverDispatch:!
