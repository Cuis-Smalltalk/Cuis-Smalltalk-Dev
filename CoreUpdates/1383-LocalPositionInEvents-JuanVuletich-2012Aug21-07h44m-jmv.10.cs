'From Cuis 4.0 of 21 April 2012 [latest update: #1382] on 21 August 2012 at 8:40:11 am'!

!Morph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:10'!
click: aMouseButtonEvent localPosition: localEventPosition
	"Handle a single-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:07'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	"Handle a double-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:25'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition
	"Special gestures (cmd-mouse on the Macintosh; Alt-mouse on Windows and Unix) allow a mouse-sensitive morph to be moved or bring up a halo for the morph."
	| h doNotDrag |
	h _ aMouseButtonEvent hand halo.
	"Prevent wrap around halo transfers originating from throwing the event back in"
	doNotDrag _ false.
	h ifNotNil:[
		(h target == self) ifTrue: [ doNotDrag _ true].
		(h target hasOwner: self) ifTrue: [ doNotDrag _ true].
		(self hasOwner: h target) ifTrue: [ doNotDrag _ true]].

	"cmd-drag on flexed morphs works better this way"
	h _ self addHalo: aMouseButtonEvent.
	doNotDrag ifTrue: [ ^self ].
	"Initiate drag transition if requested"
	aMouseButtonEvent hand 
		waitForClicksOrDrag: h
		event: aMouseButtonEvent
		clkSel: nil
		dblClkSel: nil.
	"Pass focus explicitly here"
	aMouseButtonEvent hand newMouseFocus: h.! !

!Morph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:25'!
mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition
	"Ignored. Theoretically we should never get here since control is transferred to the halo on #mouseButton3Down: but subclasses may implement this differently."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/21/2012 07:56'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:14'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Handle a mouse move event."! !

!Morph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:19'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event."! !


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:17'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	(self containsPoint: aMouseButtonEvent eventPosition)
		ifTrue: [
			self selected: 
				((aMouseButtonEvent eventPosition y - self morphPositionInWorld y // self class itemHeight) + 
					self firstVisible).
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !


!FillInTheBlankMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 07:55'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	(self containsPoint: aMouseButtonEvent eventPosition) ifFalse: [
		^ Beeper beep]. "sent in response to outside modal click"
	aMouseButtonEvent hand grabMorph: self. "allow repositioning"! !


!HaloMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:25'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition
	"Transfer the halo to the next likely recipient"
	target ifNil:[^self delete].
	aMouseButtonEvent hand obtainHalo: self.
	positionOffset _ aMouseButtonEvent eventPosition - target morphPosition.
	"wait for drags or transfer"
	aMouseButtonEvent hand 
		waitForClicksOrDrag: self 
		event: aMouseButtonEvent
		clkSel: #transferHalo:
		dblClkSel: nil! !

!HaloMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:13'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Drag our target around"
	| thePoint |
	thePoint _ aMouseMoveEvent eventPosition - positionOffset.
	target morphPosition: thePoint! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:01'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition
	self handleInteraction: [
		editor clickAndHalf: (aMouseButtonEvent ztranslatedBy: self morphPositionInWorld negated) localPosition: localEventPosition ].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:05'!
doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self handleInteraction: [
		editor doubleClickAndHalf: (aMouseButtonEvent ztranslatedBy: self morphPositionInWorld negated) localPosition: localEventPosition ].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:06'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	aMouseButtonEvent mouseButton2Pressed ifTrue: [^ self mouseButton2Activity].

	"If we don't focus, Get focus, and do nothing else (the user will need to click again to do further interaction)"
	self hasKeyboardFocus ifFalse: [
		^aMouseButtonEvent hand newKeyboardFocus: self].

	super mouseDown: aMouseButtonEvent localPosition: localEventPosition.

	self handleInteraction: [ editor mouseDown: (aMouseButtonEvent ztranslatedBy: self morphPositionInWorld negated) localPosition: localEventPosition ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:16'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent mouseButton1Pressed ifFalse: [ ^ self enterClickableRegion: aMouseMoveEvent ].
	self handleInteraction: [ editor mouseMove: (aMouseMoveEvent ztranslatedBy: self morphPositionInWorld negated) localPosition: localEventPosition].
	(aMouseMoveEvent eventPosition y - owner morphPositionInWorld y between: 0 and: owner morphExtentInWorld y) ifFalse: [
		owner scrollSelectionIntoView ]! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:18'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition

	super mouseUp: aMouseButtonEvent localPosition: localEventPosition.
	self pauseBlinking.
	self handleInteraction: [editor mouseUp: (aMouseButtonEvent ztranslatedBy: self morphPositionInWorld negated) localPosition: localEventPosition].
	owner scrollSelectionIntoView! !


!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 07:55'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	super mouseDown: aMouseButtonEvent localPosition: localEventPosition.
	self cursor show.
	hand _ aMouseButtonEvent hand.
	self startStepping.
	Preferences fastDragWindowForMorphic ifTrue: [
		indicator _ RectangleIndicatorMorph new.
		indicator morphBoundsInWorld: self initialIndicatorBounds.
		indicator openInWorld ]! !


!MagnifierMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 07:55'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	aMouseButtonEvent mouseButton2Pressed
		ifTrue: [ self chooseMagnification: aMouseButtonEvent ]
		ifFalse: [ super mouseDown: aMouseButtonEvent localPosition: localEventPosition ]! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 07:56'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	aMouseButtonEvent shiftPressed ifTrue: [ ^ super mouseDown: aMouseButtonEvent localPosition: localEventPosition ].  "enable label editing" 
	aMouseButtonEvent hand newMouseFocus: owner. "Redirect to menu for valid transitions"
	owner selectItem: self event: aMouseButtonEvent! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:18'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	aMouseButtonEvent hand mouseFocus == owner ifFalse: [ ^self ].
	"This will happen if the menu has toggles in it. (for instance, the 'show...' button)
	Update the look, refresh the world and wait a bit,
	to give the user some visual feedback"
	self contentString ifNotNil: [
		self contents: self contentString withMarkers: true inverse: true.
		self refreshWorld.
		(Delay forMilliseconds: 200) wait].
	self deselect: aMouseButtonEvent.
	self invokeWithEvent: aMouseButtonEvent! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 07:56'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self fullContainsPoint: aMouseButtonEvent eventPosition ]) 
		ifFalse: [ ^self deleteIfPopUp: aMouseButtonEvent ]. "click outside"
	self isSticky ifTrue: [ ^self ].
	"Grab the menu and drag it to some other place"
	aMouseButtonEvent hand grabMorph: self! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:19'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self fullContainsPoint: aMouseButtonEvent eventPosition) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		aMouseButtonEvent hand releaseMouseFocus: self.
		^ self deleteIfPopUp: aMouseButtonEvent ].
	stayUp ifFalse:[
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ]! !


!MouseClickState methodsFor: 'private' stamp: 'jmv 8/21/2012 08:35'!
lastClickLocalPosition

	^clickClient internalizeFromWorld: lastClickDown eventPosition! !


!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:02'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition
	self handleInteraction: [ self editor clickAndHalf: aMouseButtonEvent localPosition: localEventPosition ]! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:03'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	aMouseButtonEvent hand newKeyboardFocus: self.

	self handleInteraction: [
		self editor mouseDown: aMouseButtonEvent index: (self characterIndexAtPoint: aMouseButtonEvent eventPosition) ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:15'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent mouseButton1Pressed ifFalse: [ ^ self ].
	self handleInteraction: [
		self editor mouseMove: aMouseMoveEvent index: (self characterIndexAtPoint: aMouseMoveEvent eventPosition) ]! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:19'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition

	super mouseUp: aMouseButtonEvent localPosition: localEventPosition.
	self pauseBlinking
! !


!PasteUpMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:10'!
click: aMouseButtonEvent localPosition: localEventPosition

	^self mouseButton2Activity! !

!PasteUpMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:10'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."

	| grabbedMorph |
	grabbedMorph _ self morphToGrab: aMouseButtonEvent.
	grabbedMorph ifNotNil: [
		grabbedMorph isSticky ifTrue: [ ^self ].
		^aMouseButtonEvent hand grabMorph: grabbedMorph].

	aMouseButtonEvent mouseButton2Pressed ifTrue: [ ^self mouseButton2Activity ].

	super mouseDown: aMouseButtonEvent localPosition: localEventPosition.

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:08'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	aMouseButtonEvent mouseButton2Pressed ifTrue: [ ^ self mouseButton2Activity ].
	isPressed _ true.
	self redrawNeeded.
	(actWhen == #buttonDown or: [ actWhen == #buttonStillDown ])
		ifTrue: [
			self performAction ]
		ifFalse: [
			"Don't make multi-click slower if we act on button down, just do multiple actions"
			aMouseButtonEvent hand
				waitForClicksOrDragOrSimulatedMouseButton2: self
				event: aMouseButtonEvent
				clkSel: nil
				clkNHalf: nil
				dblClkSel: #doubleClick:localPosition:
				dblClkNHalfSel: nil
				tripleClkSel: nil ]! !

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:19'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition

	isPressed _ false.
	mouseIsOver _ false.
	(actWhen == #buttonUp and: [self containsPoint: aMouseButtonEvent eventPosition])
		ifTrue: [ self performAction ].
	self redrawNeeded! !


!DraggeableButtonMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 07:55'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Inform the model that this button has been released. "
	super mouseDown: aMouseButtonEvent localPosition: localEventPosition.
	grabSelector ifNotNil: [
		model perform: grabSelector ]! !

!DraggeableButtonMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:13'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	dragSelector ifNotNil: [
		model perform: dragSelector with: aMouseMoveEvent targetPoint ]! !

!DraggeableButtonMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:17'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	isPressed _ false.
	mouseIsOver _ false.
	actWhen == #buttonUp
		ifTrue: [ self performAction ].
	self redrawNeeded! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 8/21/2012 08:05'!
doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition
	"Some subclasses might do something"! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 8/21/2012 08:09'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	aMouseButtonEvent mouseButton2Pressed  "First check for option (menu) click"
		ifTrue: [^ self mouseButton2Activity].
	scroller mouseDown: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 8/21/2012 08:16'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	scroller  mouseMove: aMouseMoveEvent localPosition: localEventPosition! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 8/21/2012 08:21'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition

	super mouseUp: aMouseButtonEvent localPosition: localEventPosition.
	scroller mouseUp: aMouseButtonEvent localPosition: localEventPosition! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:10'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	| aMorph |
	aMouseButtonEvent hand newKeyboardFocus: self.
	aMorph _ self itemFromPoint: aMouseButtonEvent eventPosition.
	(aMorph notNil and: [ aMorph inToggleArea: aMouseButtonEvent eventPosition ])
		ifTrue: [ ^self toggleExpandedState: aMorph event: aMouseButtonEvent ]. 
	aMouseButtonEvent mouseButton2Pressed  "First check for option (menu) click"
		ifTrue: [ ^ self mouseButton2Activity ].
	aMorph ifNil: [ ^super mouseDown: aMouseButtonEvent localPosition: localEventPosition ].
	aMorph highlightForMouseDown.
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:17'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	| aMorph |
	aMorph _ self itemFromPoint: aMouseButtonEvent eventPosition.
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


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:08'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	| index |
	doubleClickSelector ifNil: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index _ self rowAtLocation: aMouseButtonEvent eventPosition.
	index = 0 ifTrue: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	"selectedMorph ifNil: [self setSelectedMorph: aMorph]."
	^ self model perform: doubleClickSelector! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:10'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	| row |
	aMouseButtonEvent mouseButton2Pressed  "First check for option (menu) click"
		ifTrue: [ ^ self mouseButton2Activity ].
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		self listMorph noSelection].
	row _ self rowAtLocation: aMouseButtonEvent eventPosition.
	row = 0  ifTrue: [ ^super mouseDown: aMouseButtonEvent localPosition: localEventPosition ].
	"self dragEnabled ifTrue: [aMorph highlightForMouseDown]."
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:20'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	"The mouse came up within the list; take appropriate action"

	| row |
	row _ self rowAtLocation: aMouseButtonEvent eventPosition.
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	(autoDeselect == false and: [row = 0]) ifTrue: [^ self].  "work-around the no-mans-land bug"
	"No change if model is locked"
	((autoDeselect == nil or: [autoDeselect]) and: [row == self selectionIndex])
		ifTrue: [self changeModelSelection: 0]
		ifFalse: [self changeModelSelection: row].
	Cursor normal show.
! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 8/21/2012 08:08'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	| oldIndex oldVal row w |
	aMouseButtonEvent mouseButton2Pressed ifTrue: [ ^ self mouseButton2Activity ].

	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].

	row _ self rowAtLocation: aMouseButtonEvent eventPosition.

	row = 0 ifTrue: [ ^super mouseDown: aMouseButtonEvent localPosition: localEventPosition ].

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
	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 8/21/2012 08:15'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row b |
	b _ self morphBoundsInWorld.
	row _ (aMouseMoveEvent eventPosition y < b top and: [ scrollBar value > 0.0 ])
		ifTrue: [
			scrollBar scrollUp: 1.
			"Leave at least one visible item unaffected, for better visual feedback to the user."
			(self rowAtLocation: b topLeft) + 2 ]
		ifFalse: [
			(aMouseMoveEvent eventPosition y > b bottom and: [ scrollBar value < 1.0 ])
				ifTrue: [
					scrollBar scrollDown: 1.
					"Leave at least one visible item unaffected, for better visual feedback to the user."
					(self rowAtLocation: b bottomLeft) - 3 ]
				ifFalse: [ self rowAtLocation: aMouseMoveEvent eventPosition ]].
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

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 8/21/2012 08:20'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition

	dragOnOrOff _ nil.  "So improperly started drags will have not effect"
	dragStartRow _ nil! !


!ScrollBar methodsFor: 'events' stamp: 'jmv 8/21/2012 07:59'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Update visual feedback"

	super mouseDown: aMouseButtonEvent localPosition: localEventPosition.
	self setNextDirectionFromEvent: aMouseButtonEvent.
	self scrollByPage! !


!SimpleEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:01'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	| here |
	here _ self pointIndex.
	(here between: 2 and: string size)
		ifTrue: [ self selectWord ]
		ifFalse: [
			"if at beginning or end, select entire string"
			self selectAll ].! !


!TextEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:01'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self selectWord.

	doWordSelection _ true.
	doParagraphSelection _ false.
	initialSelectionStart _ self startBlock.
	initialSelectionStop _ self stopBlock! !

!TextEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:05'!
doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	| here b interval |
	b _ paragraph characterBlockAtPoint: aMouseButtonEvent eventPosition.
	here _ b stringIndex.
	interval _ self privateCurrentString encompassParagraph: (here to: here).
	self selectFrom: interval first to: interval last.

	doWordSelection _ false.
	doParagraphSelection _ true.
	initialSelectionStart _ self startBlock.
	initialSelectionStop _ self stopBlock! !

!TextEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:00'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	| clickPoint b |

	initialSelectionStart _ nil.
	initialSelectionStop _ nil.
	doWordSelection _ false.
	doParagraphSelection _ false.

	"Multiple selection of text.
	Windows uses Control, Mac uses Command (i.e. commandAlt)
	On the Mac, command-button1 is translated to command-button3 by the VM. do:
		Preferences disable: #commandClickOpensHalo
	to disable this behavior and make command-button1 work for multiple selection. "
	(aMouseButtonEvent controlKeyPressed or: [ aMouseButtonEvent commandAltKeyPressed ]) ifTrue: [
		self selectionInterval size > 0 ifTrue: [
			selectionStartBlocks _ selectionStartBlocks copyWith: self startBlock.
			selectionStopBlocks _ selectionStopBlocks copyWith: self stopBlock ]]
	ifFalse: [
		selectionStartBlocks _ #().
		selectionStopBlocks _ #() ].

	clickPoint _ aMouseButtonEvent eventPosition.
	b _ paragraph characterBlockAtPoint: clickPoint.

	(paragraph clickAt: clickPoint) ifTrue: [
		markBlock _ b.
		pointBlock _ b.
		aMouseButtonEvent hand releaseKeyboardFocus: self.
		^ self ].
	
	aMouseButtonEvent shiftPressed
		ifFalse: [
			(self markIndex = b stringIndex and: [ self pointIndex = b stringIndex ])
				ifTrue: [
					markBlock _ b.
					pointBlock _ b ]
				ifFalse: [
					markBlock _ b.
					pointBlock _ b.	
					self setEmphasisHereFromText ]]! !

!TextEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:15'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Change the selection in response to mouse-down drag"

	| b interval i1 i2 |

	doWordSelection ifTrue: [
		pointBlock _ (paragraph characterBlockAtPoint: (aMouseMoveEvent eventPosition)).
		self selectWordLeftDelimiters: '' rightDelimiters: ''.
		markBlock _ self startBlock min: initialSelectionStart.
		pointBlock _ self stopBlock max: initialSelectionStop.
		self storeSelectionInParagraph.
		^self ].

	doParagraphSelection ifTrue: [
		b _ paragraph characterBlockAtPoint: aMouseMoveEvent eventPosition.
		i1 _ b stringIndex min: initialSelectionStart stringIndex.
		i2 _ b stringIndex max: initialSelectionStop stringIndex-1.
		interval _ self privateCurrentString encompassParagraph: (i1 to: i2).
		self selectFrom: interval first to: interval last.
		markBlock _ self startBlock min: initialSelectionStart.
		pointBlock _ self stopBlock max: initialSelectionStop.
		self storeSelectionInParagraph.
		^self ].

	pointBlock _ (paragraph characterBlockAtPoint: (aMouseMoveEvent eventPosition)).
	self storeSelectionInParagraph! !

!TextEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:20'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	| cursorBlock cursorIndex startBlock startIndex stopBlock stopIndex |

	aMouseButtonEvent shiftPressed
		ifTrue: [
			"Squeak classic behavior for click, move, shift-click sequence "
			"pointBlock _(paragraph characterBlockAtPoint: (evt eventPosition))."

			"Mac behavior"
			cursorBlock _ paragraph characterBlockAtPoint: aMouseButtonEvent eventPosition.
			cursorIndex _ cursorBlock stringIndex.
			startBlock _ self startBlock min: cursorBlock.
			startIndex _ startBlock stringIndex.
			stopBlock _ self stopBlock max: cursorBlock.
			stopIndex _ stopBlock stringIndex.
			(stopIndex - cursorIndex) < (cursorIndex - startIndex)
				ifTrue: [
					markBlock _ startBlock.
					pointBlock _ cursorBlock ]
				ifFalse: [
					markBlock _ stopBlock.
					pointBlock _ cursorBlock ]].
	self storeSelectionInParagraph! !


!SmalltalkEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:03'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	| here |
	here _ self pointIndex.
	(here between: 2 and: model textSize)
		ifTrue: [
			super clickAndHalf: aMouseButtonEvent localPosition: localEventPosition ]
		ifFalse: [
			"if at beginning or end, select entire string"
			self selectAll ]! !


!TextModelMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:09'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition

	self textMorph doubleClick: aMouseButtonEvent localPosition: localEventPosition! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:06'!
doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self textMorph doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition! !


!TranscriptMorph methodsFor: 'menus' stamp: 'jmv 8/21/2012 08:00'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	"Invoke the menu"
	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self getMenu ifNotNil: [ :menu |
			menu invokeModal ].
		^self ].

	super mouseDown: aMouseButtonEvent localPosition: localEventPosition! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 8/21/2012 08:25'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."
	aMouseButtonEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.

	"Make me modal during mouse transitions"
	aMouseButtonEvent hand newMouseFocus: self event: aMouseButtonEvent.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ ^self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition ].

	self mouseDown: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue:[
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue + self mouseStillDownThreshold
			arguments: {aMouseButtonEvent copy resetHandlerFields . localEventPosition}
			stepTime: self mouseStillDownStepRate ].
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/21/2012 08:16'!
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
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1].
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/21/2012 08:26'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."

	aMouseButtonEvent wasHandled ifTrue: [^self]. "not interested"
	aMouseButtonEvent hand mouseFocus == self ifFalse: [^self]. "Not interested in other parties"
	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed
		ifTrue: [ self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition ]
		ifFalse: [
			self mouseUp: aMouseButtonEvent localPosition: localEventPosition.
			self stopSteppingSelector: #processMouseStillDown:localPosition: ]! !


!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 8/21/2012 08:16'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Re-implemented to allow for mouse-up move events"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1].
! !


!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 8/21/2012 08:26'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"The handling of control between menu item requires them to act on mouse up even if not the current focus. This is different from the default behavior which really only wants to handle mouse ups when they got mouse downs before"

	aMouseButtonEvent wasHandled ifTrue:[^self]. "not interested"
	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed
		ifTrue: [ self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition ]
		ifFalse: [ self mouseUp: aMouseButtonEvent localPosition: localEventPosition ]! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 8/21/2012 08:36'!
selectItem: aMenuItem event: anEvent
	selectedItem ifNotNil: [ selectedItem deselect: anEvent ].
	selectedItem _ aMenuItem.
	selectedItem ifNotNil: [ selectedItem select: anEvent ]! !


!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:35'!
click

	clickDone ifFalse: [
		clickSelector ifNotNil: [
			clickClient perform: clickSelector with: lastClickDown with: self lastClickLocalPosition ].
		clickDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:35'!
clickAndAHalf

	clickAndHalfDone ifFalse: [
		clickAndHalfSelector ifNotNil: [
			"Focus was lost at buttonUp. Set it again."
			lastClickDown hand newMouseFocus: clickClient event: lastClickDown.
			clickClient perform: clickAndHalfSelector with: lastClickDown with: self lastClickLocalPosition ].
		clickAndHalfDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:35'!
doubleClick

	doubleClickDone ifFalse: [
		dblClickSelector ifNotNil: [
			clickClient perform: dblClickSelector with: lastClickDown with: self lastClickLocalPosition ].
		doubleClickDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:35'!
doubleClickAndHalf

	doubleClickAndHalfDone ifFalse: [
		dblClickAndHalfSelector ifNotNil: [
			"Focus was lost at buttonUp. Set it again."
			lastClickDown hand newMouseFocus: clickClient event: lastClickDown.
			clickClient perform: dblClickAndHalfSelector with: lastClickDown with: self lastClickLocalPosition ].
		doubleClickAndHalfDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:29'!
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
		distance > 0 ifTrue: [
			aHand dontWaitForMoreClicks.
			self click.
			^ false ]].

	"If we're over triple click, or timed out, or mouse moved, don't allow more clicks."
	(buttonDownCount = 4 or: [ timedOut or: [ distance > 0 ]]) ifTrue: [
		aHand dontWaitForMoreClicks.
		^ false ].

	"Simple click."
	(buttonDownCount = 1 and: [ buttonUpCount = 1 ]) ifTrue: [
		self click ].

	"Click & hold"
	(buttonDownCount = 2 and: [ buttonUpCount = 1]) ifTrue: [
		self clickAndAHalf ].

	"Double click."
	(buttonDownCount = 2 and: [ buttonUpCount = 2]) ifTrue: [
		self doubleClick ].

	"Double click & hold."
	(buttonDownCount = 3 and: [ buttonUpCount = 2]) ifTrue: [
		self doubleClickAndHalf ].

	"Triple click"
	(buttonDownCount = 3 and: [ buttonUpCount = 3]) ifTrue: [
		self tripleClick ].

	"This means: if a mouseDown, then don't further process this event (so we can turn it into a double or triple click on next buttonUp)"
	^ aMouseEvent isMouseDown! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:35'!
tripleClick

	tripleClickSelector ifNotNil: [
		clickClient perform: tripleClickSelector with: lastClickDown with: self lastClickLocalPosition]! !


!HierarchicalListMorph methodsFor: 'events-processing' stamp: 'jmv 8/21/2012 08:16'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1].
! !


!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 8/21/2012 08:16'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition }
			stepTime: 1].
! !

!methodRemoval: TranscriptMorph #mouseDown:!
TranscriptMorph removeSelector: #mouseDown:!
!methodRemoval: TextModelMorph #doubleClick:!
TextModelMorph removeSelector: #doubleClick:!
!methodRemoval: TextModelMorph #doubleClickAndHalf:!
TextModelMorph removeSelector: #doubleClickAndHalf:!
!methodRemoval: SmalltalkEditor #clickAndHalf:!
SmalltalkEditor removeSelector: #clickAndHalf:!
!methodRemoval: TextEditor #clickAndHalf:!
TextEditor removeSelector: #clickAndHalf:!
!methodRemoval: TextEditor #doubleClickAndHalf:!
TextEditor removeSelector: #doubleClickAndHalf:!
!methodRemoval: TextEditor #mouseDown:!
TextEditor removeSelector: #mouseDown:!
!methodRemoval: TextEditor #mouseMove:!
TextEditor removeSelector: #mouseMove:!
!methodRemoval: TextEditor #mouseUp:!
TextEditor removeSelector: #mouseUp:!
!methodRemoval: SimpleEditor #clickAndHalf:!
SimpleEditor removeSelector: #clickAndHalf:!
!methodRemoval: ScrollBar #mouseDown:!
ScrollBar removeSelector: #mouseDown:!
!methodRemoval: PluggableListMorphOfMany #mouseDown:!
PluggableListMorphOfMany removeSelector: #mouseDown:!
!methodRemoval: PluggableListMorphOfMany #mouseMove:!
PluggableListMorphOfMany removeSelector: #mouseMove:!
!methodRemoval: PluggableListMorphOfMany #mouseUp:!
PluggableListMorphOfMany removeSelector: #mouseUp:!
!methodRemoval: PluggableListMorph #doubleClick:!
PluggableListMorph removeSelector: #doubleClick:!
!methodRemoval: PluggableListMorph #mouseDown:!
PluggableListMorph removeSelector: #mouseDown:!
!methodRemoval: PluggableListMorph #mouseUp:!
PluggableListMorph removeSelector: #mouseUp:!
!methodRemoval: HierarchicalListMorph #mouseDown:!
HierarchicalListMorph removeSelector: #mouseDown:!
!methodRemoval: HierarchicalListMorph #mouseUp:!
HierarchicalListMorph removeSelector: #mouseUp:!
!methodRemoval: PluggableScrollPane #doubleClickAndHalf:!
PluggableScrollPane removeSelector: #doubleClickAndHalf:!
!methodRemoval: PluggableScrollPane #mouseDown:!
PluggableScrollPane removeSelector: #mouseDown:!
!methodRemoval: PluggableScrollPane #mouseMove:!
PluggableScrollPane removeSelector: #mouseMove:!
!methodRemoval: PluggableScrollPane #mouseUp:!
PluggableScrollPane removeSelector: #mouseUp:!
!methodRemoval: DraggeableButtonMorph #mouseDown:!
DraggeableButtonMorph removeSelector: #mouseDown:!
!methodRemoval: DraggeableButtonMorph #mouseMove:!
DraggeableButtonMorph removeSelector: #mouseMove:!
!methodRemoval: DraggeableButtonMorph #mouseUp:!
DraggeableButtonMorph removeSelector: #mouseUp:!
!methodRemoval: PluggableButtonMorph #mouseDown:!
PluggableButtonMorph removeSelector: #mouseDown:!
!methodRemoval: PluggableButtonMorph #mouseUp:!
PluggableButtonMorph removeSelector: #mouseUp:!
!methodRemoval: PasteUpMorph #click:!
PasteUpMorph removeSelector: #click:!
!methodRemoval: PasteUpMorph #mouseDown:!
PasteUpMorph removeSelector: #mouseDown:!
!methodRemoval: OneLineEditorMorph #clickAndHalf:!
OneLineEditorMorph removeSelector: #clickAndHalf:!
!methodRemoval: OneLineEditorMorph #mouseDown:!
OneLineEditorMorph removeSelector: #mouseDown:!
!methodRemoval: OneLineEditorMorph #mouseMove:!
OneLineEditorMorph removeSelector: #mouseMove:!
!methodRemoval: OneLineEditorMorph #mouseUp:!
OneLineEditorMorph removeSelector: #mouseUp:!

!MouseClickState reorganize!
('initialize' client:click:clickAndHalfSelector:dblClick:dblClickAndHalf:tripleClick:event:sendMouseButton2Activity:)
('actions' click clickAndAHalf doubleClick doubleClickAndHalf handleEvent:from: tripleClick)
('private' lastClickLocalPosition)
!

!methodRemoval: MenuMorph #mouseDown:!
MenuMorph removeSelector: #mouseDown:!
!methodRemoval: MenuMorph #mouseUp:!
MenuMorph removeSelector: #mouseUp:!
!methodRemoval: MenuItemMorph #mouseDown:!
MenuItemMorph removeSelector: #mouseDown:!
!methodRemoval: MenuItemMorph #mouseUp:!
MenuItemMorph removeSelector: #mouseUp:!
!methodRemoval: MagnifierMorph #mouseDown:!
MagnifierMorph removeSelector: #mouseDown:!
!methodRemoval: LayoutAdjustingMorph #mouseDown:!
LayoutAdjustingMorph removeSelector: #mouseDown:!
!methodRemoval: InnerTextMorph #clickAndHalf:!
InnerTextMorph removeSelector: #clickAndHalf:!
!methodRemoval: InnerTextMorph #doubleClickAndHalf:!
InnerTextMorph removeSelector: #doubleClickAndHalf:!
!methodRemoval: InnerTextMorph #mouseDown:!
InnerTextMorph removeSelector: #mouseDown:!
!methodRemoval: InnerTextMorph #mouseMove:!
InnerTextMorph removeSelector: #mouseMove:!
!methodRemoval: InnerTextMorph #mouseUp:!
InnerTextMorph removeSelector: #mouseUp:!

!InnerTextMorph reorganize!
('accessing' askBeforeDiscardingEdits: autoCompleter: contents: contentsAsIs: crAction editor hasEditingConflicts hasEditingConflicts: isWrapped model: model:wrappedTo: textColor textColor: wrapFlag:)
('anchors' anchorMorph:at:)
('caching' releaseCachedState)
('classification')
('drawing' debugDrawLineRectsOn: drawOn:)
('editing' acceptContents acceptOnCR cancelEdits chooseEmphasisOrAlignment chooseFont enterClickableRegion: handleInteraction: hasUnacceptedEdits:)
('events' clickAndHalf:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseDown:localPosition: mouseMove:localPosition: mouseUp:localPosition:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('event handling' keyboardFocusChange: processKeyStroke:)
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

!methodRemoval: HaloMorph #mouseButton3Down:!
HaloMorph removeSelector: #mouseButton3Down:!
!methodRemoval: HaloMorph #mouseMove:!
HaloMorph removeSelector: #mouseMove:!

!HaloMorph reorganize!
('WiW support' morphicLayerNumber)
('accessing' haloBox: setTarget: target target:)
('drawing' drawOn:)
('dropping/grabbing' startDrag:with:)
('events' mouseButton3Down:localPosition: mouseMove:localPosition:)
('event handling' popUpFor:event: staysUpWhenMouseIsDownIn: transferHalo:)
('events-processing' containsPoint:event: rejectsEvent:)
('geometry' morphFullBoundsInWorld)
('geometry testing' containsPoint: isOrthoRectangularMorph)
('handles' addCollapseHandle: addDebugHandle: addDismissHandle: addDragHandle: addDupHandle: addFontEmphHandle: addFontSizeHandle: addGrabHandle: addGrowHandle: addHelpHandle: addMenuHandle: addRecolorHandle: addRotateHandle: positionIn:horizontalPlacement:verticalPlacement:)
('initialization' defaultColor initialize)
('stepping' step)
('testing' stepTime)
('updating' redrawNeeded)
('private' addHandle: addHandles addHandlesForWorldHalos addName addNameBeneath:string: basicBox doDebug:with: doDrag:with: doDup:with: doGrab:with: doGrow:with: doMenu:with: doRecolor:with: doRot:with: endInteraction handleSize maybeCollapse:with: maybeDismiss:with: maybeDoDup:with: mouseDownInCollapseHandle:with: removeAllHandlesBut: setDismissColor:with: startGrow:with: startRot:with:)
('forward to target' chooseEmphasisOrAlignment chooseFont deleteBalloon mouseDownOnHelpHandle:)
!

!methodRemoval: FillInTheBlankMorph #mouseDown:!
FillInTheBlankMorph removeSelector: #mouseDown:!
!methodRemoval: AutoCompleterMorph #mouseUp:!
AutoCompleterMorph removeSelector: #mouseUp:!
!methodRemoval: Morph #click:!
Morph removeSelector: #click:!
!methodRemoval: Morph #doubleClick:!
Morph removeSelector: #doubleClick:!
!methodRemoval: Morph #mouseButton3Down:!
Morph removeSelector: #mouseButton3Down:!
!methodRemoval: Morph #mouseButton3Up:!
Morph removeSelector: #mouseButton3Up:!
!methodRemoval: Morph #mouseDown:!
Morph removeSelector: #mouseDown:!
!methodRemoval: Morph #mouseMove:!
Morph removeSelector: #mouseMove:!
!methodRemoval: Morph #mouseUp:!
Morph removeSelector: #mouseUp:!

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
('events' click:localPosition: doubleClick:localPosition: keyDown: keyStroke: keyUp: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseDown:localPosition: mouseEnter: mouseLeave: mouseMove:localPosition: mouseStillDown: mouseUp:localPosition: windowEvent:)
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
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: changeColorTarget:selector:originalColor:hand: copyToClipboard: dismissMorph duplicateMorph: grabMorph: maybeDuplicateMorph potentialEmbeddingTargets resizeFromMenu resizeMorph)
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

