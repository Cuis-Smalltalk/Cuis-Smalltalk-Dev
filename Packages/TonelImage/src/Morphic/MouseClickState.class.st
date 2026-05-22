"
MouseClickState is a simple class managing the distinction between clicks, double clicks, and drag operations. It has been factored out of HandMorph due to the many instVars.

Instance variables:
	clickClient 			<Morph>			The client wishing to receive #click:, #dblClick:, or #drag messages
	clickState 			<Symbol>			The internal state of handling the last event (#firstClickDown, #firstClickUp, #firstClickTimedOut)
	firstClickDown 		<MorphicEvent>	The #mouseDown event after which the client wished to receive #click: or similar messages
	clickSelector 		<Symbol>			The selector to use for sending #click: messages
	dblClickSelector 	<Symbol>			The selector to use for sending #doubleClick: messages
	tripleClickSelector 	<Symbol>			The selector to use for sending #tripleClick: messages
"
Class {
	#name : 'MouseClickState',
	#superclass : 'Object',
	#instVars : [
		'clickClient',
		'dragSelector',
		'clickSelector',
		'clickAndHalfSelector',
		'dblClickSelector',
		'tripleClickSelector',
		'sendMouseButton2Activity',
		'buttonDownCount',
		'buttonUpCount',
		'dragDone',
		'clickDone',
		'clickAndHalfDone',
		'doubleClickDone',
		'lastClickDown',
		'dblClickAndHalfSelector',
		'doubleClickAndHalfDone'
	],
	#classVars : [
		'DoubleClickTimeout'
	],
	#category : 'Morphic-Kernel',
	#package : 'Morphic',
	#tag : 'Kernel'
}

{ #category : 'cached state access' }
MouseClickState class >> doubleClickTimeout [
	DoubleClickTimeout ifNil: [
		DoubleClickTimeout := 500 ].
	^DoubleClickTimeout
]

{ #category : 'cached state access' }
MouseClickState class >> releaseClassCachedState [
	DoubleClickTimeout := nil
]

{ #category : 'initialization' }
MouseClickState >> client: aMorph click: aClickSelector clickAndHalf: aClickAndHalfSelector dblClick: aDblClickSelector dblClickAndHalf: aDblClickAndHalfSelector tripleClick: aTripleClickSelector event: firstClickEvent sendMouseButton2Activity: aBoolean [

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
	lastClickDown := firstClickEvent
]

{ #category : 'initialization' }
MouseClickState >> client: aMorph click: aClickSelector dblClick: aDblClickSelector event: firstClickEvent  [

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
	lastClickDown := firstClickEvent
]

{ #category : 'initialization' }
MouseClickState >> client: aMorph click: aClickSelector event: firstClickEvent sendMouseButton2Activity: aBoolean [

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
	lastClickDown := firstClickEvent
]

{ #category : 'initialization' }
MouseClickState >> client: aMorph drag: aDragSelector click: aClickSelector clickAndHalf: aClickAndHalfSelector dblClick: aDblClickSelector dblClickAndHalf: aDblClickAndHalfSelector tripleClick: aTripleClickSelector event: firstClickEvent sendMouseButton2Activity: aBoolean [

	clickClient := aMorph.
	dragSelector := aDragSelector.
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
	lastClickDown := firstClickEvent
]

{ #category : 'initialization' }
MouseClickState >> client: aMorph drag: aDragSelector click: aClickSelector event: firstClickEvent  [

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
	lastClickDown := firstClickEvent
]

{ #category : 'actions' }
MouseClickState >> didClick [
	clickDone ifFalse: [
		clickSelector ifNotNil: [
			clickClient perform: clickSelector with: lastClickDown with: self lastClickLocalPosition ].
		clickDone := true ]
]

{ #category : 'actions' }
MouseClickState >> didClickAndHalf [

	clickAndHalfDone ifFalse: [
		clickAndHalfSelector ifNotNil: [
			"Focus was lost at buttonUp. Set it again."
			lastClickDown hand newMouseFocus: clickClient.
			clickClient perform: clickAndHalfSelector with: lastClickDown with: self lastClickLocalPosition ].
		clickAndHalfDone := true ]
]

{ #category : 'actions' }
MouseClickState >> didDoubleClick [

	doubleClickDone ifFalse: [
		dblClickSelector ifNotNil: [
			clickClient perform: dblClickSelector with: lastClickDown with: self lastClickLocalPosition ].
		doubleClickDone := true ]
]

{ #category : 'actions' }
MouseClickState >> didDoubleClickAndHalf [

	doubleClickAndHalfDone ifFalse: [
		dblClickAndHalfSelector ifNotNil: [
			"Focus was lost at buttonUp. Set it again."
			lastClickDown hand newMouseFocus: clickClient.
			clickClient perform: dblClickAndHalfSelector with: lastClickDown with: self lastClickLocalPosition ].
		doubleClickAndHalfDone := true ]
]

{ #category : 'actions' }
MouseClickState >> didDrag [
	dragDone ifFalse: [
		dragSelector ifNotNil: [
			clickClient perform: dragSelector with: lastClickDown with: self lastClickLocalPosition ].
		dragDone := true ]
]

{ #category : 'actions' }
MouseClickState >> didTripleClick [

	tripleClickSelector ifNotNil: [
		clickClient perform: tripleClickSelector with: lastClickDown with: self lastClickLocalPosition]
]

{ #category : 'actions' }
MouseClickState >> handleEvent: aMouseEvent from: aHand [
	"Process the given mouse event to detect a click, double-click, or drag
	(for any of the three mouse buttons, be either real or emulated).
	Return true if the event was consumed (don't further process it).
	Return false if the event should be processed by the sender.
	NOTE: This method heavily relies on getting *all* mouse button events."

	| timedOut mouseWasMoved |
	timedOut := (aMouseEvent timeStamp - lastClickDown timeStamp) > self class doubleClickTimeout.
	mouseWasMoved := (aMouseEvent eventPosition - lastClickDown eventPosition) r > 2.

	aMouseEvent isMouseDown ifTrue: [
		"If it is a different button, it is not multiple click."
		aMouseEvent buttons = lastClickDown buttons ifFalse: [
			aHand dontWaitForMoreClicks.
			^false ].
		lastClickDown := aMouseEvent ].

	"Real action dispatch might be done after the triggering event, for example, because of waiting for timeout.
	So, count the button downs and ups(clicks), to be processed, maybe later, maybe in a mouseMove..."
	aMouseEvent isMouseDown ifTrue: [
		buttonDownCount := buttonDownCount + 1 ].
	aMouseEvent isMouseUp ifTrue: [
		buttonUpCount := buttonUpCount + 1 ].

	"Drag, or tap & hold"
	(buttonDownCount = 1 and: [ buttonUpCount = 0]) ifTrue: [
		mouseWasMoved ifTrue: [
			aHand dontWaitForMoreClicks.
			dragSelector
				ifNotNil: [ self didDrag ]
				"If we have already moved, then it won't be a double or triple click... why wait?"
				ifNil: [ self didClick ].
			^ false ].
		timedOut ifTrue: [
			aHand dontWaitForMoreClicks.
			"Simulate button 2 via tap & hold. Useful for opening menus on pen computers."
			sendMouseButton2Activity ifTrue: [
				clickClient mouseButton2Activity ].
			^ false ]].

	"If we're over triple click, or timed out, or mouse moved, don't allow more clicks."
	buttonDownCount = 4 | timedOut | mouseWasMoved ifTrue: [
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

	timedOut ifTrue: [
		aHand dontWaitForMoreClicks ].

	"This means: if a mouseDown, then don't further process this event (so we can turn it into a double or triple click on next buttonUp)"
	^ aMouseEvent isMouseDown
]

{ #category : 'private' }
MouseClickState >> lastClickLocalPosition [

	^clickClient internalizeFromWorld: lastClickDown eventPosition
]

{ #category : 'private' }
MouseClickState >> notWaitingForMultipleClicks [

	^ clickAndHalfSelector isNil and: [
		dblClickSelector isNil and: [
			dblClickAndHalfSelector isNil and: [
				tripleClickSelector isNil ]]]
]
