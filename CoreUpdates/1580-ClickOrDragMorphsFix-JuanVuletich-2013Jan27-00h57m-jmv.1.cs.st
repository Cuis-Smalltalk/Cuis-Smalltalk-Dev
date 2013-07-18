'From Cuis 4.1 of 12 December 2012 [latest update: #1579] on 27 January 2013 at 12:59:37 am'!
!classDefinition: #MouseClickState category: #'Morphic-Kernel'!
Object subclass: #MouseClickState
	instanceVariableNames: 'clickClient clickSelector clickAndHalfSelector dblClickSelector tripleClickSelector sendMouseButton2Activity buttonDownCount buttonUpCount clickDone clickAndHalfDone doubleClickDone lastClickDown dblClickAndHalfSelector doubleClickAndHalfDone dragSelector dragDone '
	classVariableNames: 'DoubleClickTimeout '
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Morph methodsFor: 'as yet unclassified' stamp: 'jmv 1/27/2013 00:29'!
dragEvent: aMouseEvent localPosition: aPoint

	aMouseEvent hand grabMorph: self! !


!HandMorph methodsFor: 'double click support' stamp: 'jmv 1/27/2013 00:24'!
waitForClicksOrDrag: aMorph event: evt dragSel: dragSel clkSel: clkSel
"juntar los 3? documentar mejor? uno solo completo y shortcuts?"
	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: dragSel
			click: clkSel
			clickAndHalf: nil 
			dblClick: nil
			dblClickAndHalf: nil
			tripleClick: nil
			event: evt
			sendMouseButton2Activity: false! !


!MouseClickState methodsFor: 'initialize' stamp: 'jmv 1/27/2013 00:12'!
client: aMorph drag: aDragSelector click: aClickSelector clickAndHalf: aClickAndHalfSelector dblClick: aDblClickSelector dblClickAndHalf: aDblClickAndHalfSelector tripleClick: aTripleClickSelector event: firstClickEvent sendMouseButton2Activity: aBoolean

	clickClient _ aMorph.
	dragSelector _ aDragSelector.
	clickSelector _ aClickSelector.
	clickAndHalfSelector _ aClickAndHalfSelector.
	dblClickSelector _ aDblClickSelector.
	dblClickAndHalfSelector _ aDblClickAndHalfSelector.
	tripleClickSelector _ aTripleClickSelector.
	sendMouseButton2Activity _ aBoolean.
	buttonDownCount _ 1.
	buttonUpCount _ 0.
	dragDone _ false.
	clickDone _ false.
	clickAndHalfDone _ false.
	doubleClickDone _ false.
	doubleClickAndHalfDone _ false.
	lastClickDown _ firstClickEvent! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 1/27/2013 00:29'!
didDrag
	dragDone ifFalse: [
		dragSelector ifNotNil: [
			clickClient perform: dragSelector with: lastClickDown with: self lastClickLocalPosition ].
		dragDone _ true ]! !


!HaloMorph methodsFor: 'event handling' stamp: 'jmv 1/27/2013 00:19'!
transferHalo: event localPosition: localEventPosition
	"Transfer the halo to the next likely recipient"
	target ifNil: [ ^self delete ].
	target transferHalo: event from: target.! !


!HandMorph methodsFor: 'double click support' stamp: 'jmv 1/27/2013 00:08'!
waitForClicksOrDrag: aMorph event: evt clkSel: clkSel dblClkSel: dblClkSel

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: nil 
			dblClick: dblClkSel
			dblClickAndHalf: nil
			tripleClick: nil
			event: evt
			sendMouseButton2Activity: false! !

!HandMorph methodsFor: 'double click support' stamp: 'jmv 1/27/2013 00:08'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: Preferences tapAndHoldEmulatesButton2! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 1/27/2013 00:28'!
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
				ifTrue: [
					"Not if we are grabbing them"
					mouseClickState ifNil: [self dropMorphs: aMouseEvent ]]
				ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]]].
		self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 1/27/2013 00:58'!
dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| dropEvent |
	dropEvent _ DropEvent new setPosition: self morphPosition contents: aMorph hand: self.
	owner dispatchEvent: dropEvent localPosition: dropEvent eventPosition.
	dropEvent wasHandled ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self mouseOverHandler processMouseOver: aMouseEvent! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 1/27/2013 00:29'!
dropMorphs: anEvent
	"Drop the morphs at the hands position"
	self submorphsReverseDo:[:m|
		"Drop back to front to maintain z-order"
		self dropMorph: m event: anEvent ]! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 1/27/2013 00:20'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| aMorphHandlesIt grabAMorph handledByInner eventPositionInChild |
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchWith: aMorph localPosition: positionInAMorph ].

	"Try to get out quickly"
	(aMorph morphFullBoundsInWorld containsPoint: self eventPosition) ifFalse: [
		^#rejected ].

	"Install the prospective handler for the receiver"
	aMorphHandlesIt _ false.
	grabAMorph _ false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isWorldMorph or: [
					self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self) ifTrue: [
				eventHandler _ aMorph.
				aMorphHandlesIt _ true ].
			"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
			self mouseButton1Pressed ifTrue: [
				aMorph owner ifNotNil: [ :o |
					(o allowsSubmorphDrag and: [ aMorph isSticky not ]) ifTrue: [
						grabAMorph _ true ]]]].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ aMorph containsPoint: positionInAMorph event: self ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sentTo: aMorph localPosition: positionInAMorph ]
			ifFalse: [
				(grabAMorph and: [ handledByInner not ]) ifTrue: [
					self hand waitForClicksOrDrag: aMorph event: self dragSel: #dragEvent:localPosition: clkSel: #dragEvent:localPosition:.
					false ifTrue: [ self hand grabMorph: aMorph ].
					self wasHandled: true.
					^self ]]].

	handledByInner ifTrue: [ ^self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected! !


!MouseClickState methodsFor: 'actions' stamp: 'jmv 1/27/2013 00:29'!
didClick
	clickDone ifFalse: [
		clickSelector ifNotNil: [
			clickClient perform: clickSelector with: lastClickDown with: self lastClickLocalPosition ].
		clickDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 1/27/2013 00:32'!
handleEvent: aMouseEvent from: aHand
	"Process the given mouse event to detect a click, double-click, or drag.
	Return true if the event should be processed by the sender, false if it shouldn't.
	NOTE: This method heavily relies on getting *all* mouse button events."

	| timedOut distance |
	timedOut _ (aMouseEvent timeStamp - lastClickDown timeStamp) > DoubleClickTimeout.
	distance _ (aMouseEvent eventPosition - lastClickDown eventPosition) r.
	"Real action dispatch might be done after the triggering event, for example, because of waiting for timeout.
	So, count the button downs and ups(clicks), to be processed, maybe later, maybe in a mouseMove..."
	aMouseEvent isMouseDown ifTrue: [
		lastClickDown _ aMouseEvent.
		buttonDownCount _ buttonDownCount + 1 ].
	aMouseEvent isMouseUp ifTrue: [
		buttonUpCount _ buttonUpCount + 1 ].

	"Simulate button 2 if timeout during first click (i.e. tap & hold). Useful for opening menus on pen computers."
	(buttonDownCount = 1 and: [ buttonUpCount = 0]) ifTrue: [
		(timedOut and: [ sendMouseButton2Activity and: [ distance = 0]]) ifTrue: [
			aHand dontWaitForMoreClicks.
			clickClient mouseButton2Activity.
			^ false ].
		"If we have already moved, then it won't be a double or triple click... why wait?"
		(timedOut or: [distance > 0]) ifTrue: [
			aHand dontWaitForMoreClicks.
			dragSelector
				ifNotNil: [ self didDrag ]
				ifNil: [ self didClick ].
			^ false ]].

	"If we're over triple click, or timed out, or mouse moved, don't allow more clicks."
	(buttonDownCount = 4 or: [ timedOut or: [ distance > 0 ]]) ifTrue: [
		aHand dontWaitForMoreClicks.
		^ false ].

	"Simple click."
	(buttonDownCount = 1 and: [ buttonUpCount = 1 ]) ifTrue: [
		self didClick ].

	"Click & hold"
	(buttonDownCount = 2 and: [ buttonUpCount = 1]) ifTrue: [
		self didClickAndHalf ].

	"Double click."
	(buttonDownCount = 2 and: [ buttonUpCount = 2]) ifTrue: [
		self didDoubleClick ].

	"Double click & hold."
	(buttonDownCount = 3 and: [ buttonUpCount = 2]) ifTrue: [
		self didDoubleClickAndHalf ].

	"Triple click"
	(buttonDownCount = 3 and: [ buttonUpCount = 3]) ifTrue: [
		self didTripleClick ].

	"This means: if a mouseDown, then don't further process this event (so we can turn it into a double or triple click on next buttonUp)"
	^ aMouseEvent isMouseDown! !

!methodRemoval: SystemWindow #shouldDropOnMouseUp!
SystemWindow removeSelector: #shouldDropOnMouseUp!
!methodRemoval: MouseClickState #client:click:clickAndHalfSelector:dblClick:dblClickAndHalf:tripleClick:event:sendMouseButton2Activity:!
MouseClickState removeSelector: #client:click:clickAndHalfSelector:dblClick:dblClickAndHalf:tripleClick:event:sendMouseButton2Activity:!
!classDefinition: #MouseClickState category: #'Morphic-Kernel'!
Object subclass: #MouseClickState
	instanceVariableNames: 'clickClient dragSelector clickSelector clickAndHalfSelector dblClickSelector tripleClickSelector sendMouseButton2Activity buttonDownCount buttonUpCount dragDone clickDone clickAndHalfDone doubleClickDone lastClickDown dblClickAndHalfSelector doubleClickAndHalfDone'
	classVariableNames: 'DoubleClickTimeout'
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
!methodRemoval: DragAndDropAreaMorph #shouldDropOnMouseUp!
DragAndDropAreaMorph removeSelector: #shouldDropOnMouseUp!
!methodRemoval: Morph #shouldDropOnMouseUp!
Morph removeSelector: #shouldDropOnMouseUp!
