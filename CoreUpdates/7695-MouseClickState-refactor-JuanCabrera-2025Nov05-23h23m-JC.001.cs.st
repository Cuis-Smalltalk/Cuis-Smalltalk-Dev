'From Cuis7.5 [latest update: #7689] on 5 November 2025 at 11:45:01 pm'!

!MouseClickState methodsFor: 'initialization' stamp: 'JC 11/5/2025 23:27:48'!
client: aMorph click: aClickSelector clickAndHalf: aClickAndHalfSelector dblClick: aDblClickSelector dblClickAndHalf: aDblClickAndHalfSelector tripleClick: aTripleClickSelector event: firstClickEvent sendMouseButton2Activity: aBoolean

	clickClient := aMorph.
	clickSelector := aClickSelector.
	clickAndHalfSelector := aClickAndHalfSelector.
	dblClickSelector := aDblClickSelector.
	dblClickAndHalfSelector := aDblClickAndHalfSelector.
	tripleClickSelector := aTripleClickSelector.
	sendMouseButton2Activity := aBoolean.
	buttonDownCount := 1.
	buttonUpCount := 0.
	dragDone := false.
	clickDone := false.
	clickAndHalfDone := false.
	doubleClickDone := false.
	doubleClickAndHalfDone := false.
	lastClickDown := firstClickEvent! !

!MouseClickState methodsFor: 'initialization' stamp: 'JC 11/5/2025 23:29:59'!
client: aMorph click: aClickSelector dblClick: aDblClickSelector event: firstClickEvent 

	clickClient := aMorph.
	clickSelector := aClickSelector.
	dblClickSelector := aDblClickSelector.
	sendMouseButton2Activity := false.
	buttonDownCount := 1.
	buttonUpCount := 0.
	dragDone := false.
	clickDone := false.
	clickAndHalfDone := false.
	doubleClickDone := false.
	doubleClickAndHalfDone := false.
	lastClickDown := firstClickEvent! !

!MouseClickState methodsFor: 'initialization' stamp: 'JC 11/5/2025 23:27:43'!
client: aMorph click: aClickSelector event: firstClickEvent sendMouseButton2Activity: aBoolean

	clickClient := aMorph.
	clickSelector := aClickSelector.
	sendMouseButton2Activity := aBoolean.
	buttonDownCount := 1.
	buttonUpCount := 0.
	dragDone := false.
	clickDone := false.
	clickAndHalfDone := false.
	doubleClickDone := false.
	doubleClickAndHalfDone := false.
	lastClickDown := firstClickEvent! !

!MouseClickState methodsFor: 'initialization' stamp: 'JC 11/5/2025 23:29:19'!
client: aMorph drag: aDragSelector click: aClickSelector event: firstClickEvent 

	clickClient := aMorph.
	dragSelector := aDragSelector.
	clickSelector := aClickSelector.
	sendMouseButton2Activity := false.
	buttonDownCount := 1.
	buttonUpCount := 0.
	dragDone := false.
	clickDone := false.
	clickAndHalfDone := false.
	doubleClickDone := false.
	doubleClickAndHalfDone := false.
	lastClickDown := firstClickEvent! !


!HandMorph methodsFor: 'double click support' stamp: 'JC 11/5/2025 23:33:35'!
waitForClicksOrDrag: aMorph event: evt clkSel: clkSel dblClkSel: dblClkSel

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState :=
		MouseClickState new
			client: aMorph
			click: clkSel
			dblClick: dblClkSel
			event: evt! !

!HandMorph methodsFor: 'double click support' stamp: 'JC 11/5/2025 23:33:21'!
waitForClicksOrDrag: aMorph event: evt dragSel: dragSel clkSel: clkSel

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState :=
		MouseClickState new
			client: aMorph
			drag: dragSel
			click: clkSel
			event: evt! !

!HandMorph methodsFor: 'double click support' stamp: 'JC 11/5/2025 23:33:14'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel

	"Wait until the difference between click, or drag gesture is known, then inform the given morph what transpired." 

	mouseClickState :=
		MouseClickState new
			client: aMorph
			click: clkSel
			event: evt
			sendMouseButton2Activity: (Preferences at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'double click support' stamp: 'JC 11/5/2025 23:32:01'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState :=
		MouseClickState new
			client: aMorph
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: (Preferences at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !
