'From Cuis 4.1 of 12 December 2012 [latest update: #1556] on 15 January 2013 at 5:38:42 pm'!

!Morph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:27'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."! !

!Morph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:21'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 1 up event."! !

!Morph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:51'!
mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 2 down event."
	self mouseButton2Activity! !

!Morph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:21'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 2 up event."! !

!Morph methodsFor: 'event handling' stamp: 'jmv 1/14/2013 22:51'!
mouseButton2Activity
	"This method may be redefined, for example, to open a pop-up menu"! !


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:12'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	(self morphContainsPoint: localEventPosition)
		ifTrue: [
			self selected: (localEventPosition y // self class itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !


!FillInTheBlankMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:26'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	(self morphContainsPoint: localEventPosition) ifFalse: [
		^ Beeper beep]. "sent in response to outside modal click"
	aMouseButtonEvent hand grabMorph: self. "allow repositioning"! !


!HaloHandleMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:26'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	self send: mouseDownSelector withEvent: aMouseButtonEvent! !

!HaloHandleMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:12'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event."
	self send: mouseUpSelector withEvent: aMouseButtonEvent! !

!HaloHandleMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:17'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Handle a mouse move event."
	aMouseMoveEvent anyButtonPressed ifTrue: [
		self send: mouseMoveSelector withEvent: aMouseMoveEvent ]! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:42'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	"If we don't focus, Get focus, and do nothing else (the user will need to click again to do further interaction)"
	self hasKeyboardFocus ifFalse: [
		^aMouseButtonEvent hand newKeyboardFocus: self].

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	self handleInteraction: [ editor mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:15'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self pauseBlinking.
	self handleInteraction: [ editor mouseButton1Up: aMouseButtonEvent  localPosition: localEventPosition ].
	owner scrollSelectionIntoView! !


!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:30'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	self cursor show.
	hand _ aMouseButtonEvent hand.
	self startStepping! !


!MagnifierMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:45'!
mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition

	self chooseMagnification: aMouseButtonEvent! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:45'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	aMouseButtonEvent shiftPressed ifTrue: [
		^ super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].  "enable label editing" 
	aMouseButtonEvent hand newMouseFocus: owner. "Redirect to menu for valid transitions"
	owner selectItem: self event: aMouseButtonEvent! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:13'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
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


!MenuMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:27'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self fullContainsPoint:localEventPosition ]) 
		ifFalse: [ ^self deleteIfPopUp: aMouseButtonEvent ]. "click outside"
	self isSticky ifTrue: [ ^self ].
	"Grab the menu and drag it to some other place"
	aMouseButtonEvent hand grabMorph: self! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:13'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self fullContainsPoint: localEventPosition) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		aMouseButtonEvent hand ifNotNil: [ :h | h releaseMouseFocus: self ].
		^ self deleteIfPopUp: aMouseButtonEvent ].
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ]! !


!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:27'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	aMouseButtonEvent hand newKeyboardFocus: self.

	self handleInteraction: [
		self editor mouseDown: aMouseButtonEvent index: (self characterIndexAtPoint: localEventPosition) ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:16'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self pauseBlinking
! !


!PasteUpMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:47'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."

	| grabbedMorph |
	grabbedMorph _ self morphToGrab: aMouseButtonEvent.
	grabbedMorph ifNotNil: [
		grabbedMorph isSticky ifTrue: [ ^self ].
		^aMouseButtonEvent hand grabMorph: grabbedMorph].

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:47'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

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

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:13'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	isPressed _ false.
	mouseIsOver _ false.
	(actWhen == #buttonUp and: [ self morphContainsPoint: localEventPosition ])
		ifTrue: [ self performAction ].
	self redrawNeeded! !


!DraggeableButtonMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:30'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Inform the model that this button has been released. "
	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	grabSelector ifNotNil: [
		model perform: grabSelector with: localEventPosition ]! !

!DraggeableButtonMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:12'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	isPressed _ false.
	mouseIsOver _ false.
	actWhen == #buttonUp
		ifTrue: [ self performAction ].
	self redrawNeeded! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 1/14/2013 22:41'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	scroller mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 1/14/2013 23:16'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	scroller mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:39'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |
	aMouseButtonEvent hand newKeyboardFocus: self.
	itemMorph _ self itemFromPoint: localEventPosition.
	(itemMorph notNil and: [
		itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)) ])
		ifTrue: [ ^self toggleExpandedState: itemMorph event: aMouseButtonEvent ]. 
	itemMorph ifNil: [ ^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].
	itemMorph highlightForMouseDown.
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:12'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| itemMorph |
	itemMorph _ self itemFromPoint: localEventPosition.
	itemMorph ifNil: [^self].
	itemMorph highlightedForMouseDown ifFalse: [^self].
	itemMorph highlightForMouseDown: false.
	"No change if model is locked"
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	((autoDeselect isNil or: [autoDeselect]) and: [itemMorph == selectedMorph]) 
		ifTrue: [self setSelectedMorph: nil]
		ifFalse: [self setSelectedMorph: itemMorph].
	Cursor normal show! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:48'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| row |
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		self listMorph noSelection].
	row _ self rowAtLocation: localEventPosition.
	row = 0  ifTrue: [
		^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].
	"self dragEnabled ifTrue: [aMorph highlightForMouseDown]."
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:13'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"The mouse came up within the list; take appropriate action"

	| row |
	row _ self rowAtLocation: localEventPosition.
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	(autoDeselect == false and: [row = 0]) ifTrue: [^ self].  "work-around the no-mans-land bug"
	"No change if model is locked"
	((autoDeselect == nil or: [autoDeselect]) and: [row == self selectionIndex])
		ifTrue: [
			aMouseButtonEvent mouseButton1Changed ifTrue: [
				self changeModelSelection: 0]]
		ifFalse: [self changeModelSelection: row].
	Cursor normal show.
! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 1/14/2013 22:43'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| oldIndex oldVal row w |
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].

	row _ self rowAtLocation: localEventPosition.

	row = 0 ifTrue: [
		^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

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

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 1/14/2013 23:13'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	dragOnOrOff _ nil.  "So improperly started drags will have not effect"
	dragStartRow _ nil! !


!ScrollBar methodsFor: 'events' stamp: 'jmv 1/14/2013 22:30'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Update visual feedback"

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	self setNextDirectionFromEvent: aMouseButtonEvent.
	self scrollByPage! !


!TextEditor methodsFor: 'events' stamp: 'jmv 1/14/2013 22:28'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	| b |

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

	b _ paragraph characterBlockAtPoint: localEventPosition.

	(paragraph clickAt: localEventPosition) ifTrue: [
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

!TextEditor methodsFor: 'events' stamp: 'jmv 1/14/2013 23:13'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| cursorBlock cursorIndex startBlock startIndex stopBlock stopIndex |

	aMouseButtonEvent shiftPressed
		ifTrue: [
			"Squeak classic behavior for click, move, shift-click sequence "
			"pointBlock _(paragraph characterBlockAtPoint: (evt eventPosition))."

			"Mac behavior"
			cursorBlock _ paragraph characterBlockAtPoint: localEventPosition.
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


!TranscriptMorph methodsFor: 'menus' stamp: 'jmv 1/14/2013 22:55'!
mouseButton2Activity

	"Invoke the menu"
	self getMenu ifNotNil: [ :menu |
		menu invokeModal ].! !


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 1/14/2013 22:12'!
chooseInstVarThenDo: aBlock
	"Put up a menu of all the instance variables in the receiver, and when
the user chooses one, evaluate aBlock with the chosen variable as its
parameter.  If the list is 6 or larger, then offer an alphabetical
formulation as an alternative. triggered by a 'show alphabetically' item
at the top of the list."

	| lines labelStream allVars index count offerAlpha |
	(count _ self allInstVarNames size) = 0 ifTrue: 
		[ ^ self inform: 'There are no\instance variables.' withNewLines ].

	allVars _ OrderedCollection new.
	lines _ OrderedCollection new.
	labelStream _ WriteStream on: (String new: 200).

	(offerAlpha _ count > 5)
		ifTrue: [
			lines add: 1.
			allVars add: 'show alphabetically'.
			labelStream nextPutAll: allVars first; newLine].
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class instVarNames.
		vars do: [ :var |
			labelStream nextPutAll: var; newLine.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	labelStream skip: -1 "cut last CR".
	(lines size > 0 and: [ lines last = allVars size ]) ifTrue: [
		lines removeLast ].  "dispense with inelegant line beneath last item"
	index _ (PopUpMenu labels: labelStream contents lines: lines)
startUpWithCaption: 'Instance variables in
', self name.
	index = 0 ifTrue: [^ self].
	(index = 1 and: [offerAlpha]) ifTrue: [
		^ self chooseInstVarAlphabeticallyThenDo: aBlock].
	aBlock value: (allVars at: index)! !


!Morph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:04'!
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
	"good gesture. implement it"
	aMouseButtonEvent hand 
		waitForClicksOrDrag: h
		event: aMouseButtonEvent
		clkSel: nil
		dblClkSel: nil.
	"Pass focus explicitly here"
	aMouseButtonEvent hand newMouseFocus: h.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/15/2013 17:12'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."
	aMouseButtonEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		^self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition ].

	self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue:[
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue + self mouseStillDownThreshold
			arguments: {aMouseButtonEvent copy resetHandlerFields . localEventPosition}
			stepTime: self mouseStillDownStepRate ].
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 1/14/2013 23:17'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."

	aMouseButtonEvent wasHandled ifTrue: [^self]. "not interested"
	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		^self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		^self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition ].
	
	self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self stopSteppingSelector: #processMouseStillDown:localPosition:! !


!HaloMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:03'!
mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition
	"Transfer the halo to the next likely recipient"
	target ifNil:[^self delete].
	aMouseButtonEvent hand obtainHalo: self.
	positionOffset _ aMouseButtonEvent eventPosition - target morphPositionInWorld.
	"wait for click to transfer halo"
	aMouseButtonEvent hand 
		waitForClicksOrDrag: self 
		event: aMouseButtonEvent
		clkSel: #transferHalo:localPosition:
		dblClkSel: nil! !


!HierarchicalListMorph methodsFor: 'commands' stamp: 'jmv 1/14/2013 22:58'!
toggleExpandedState: aMorph event: event

	"self setSelectedMorph: aMorph."
	aMorph toggleExpandedState.
	scroller adjustExtent.
	self setScrollDeltas! !


!TextModelMorph methodsFor: 'accessing' stamp: 'jmv 1/14/2013 22:54'!
textMorph
self flag: #jmvVer.
"Podemos reemplazar los senders locales por accesos directos (el doble encapsulamiento es tonto) una vez que quede definido el shape de la clase!!
Y la variable deberia ser innerMorph o algo asi...
Y el getter para callers externos tambien deberia ser #innerMorph"
	^ scroller! !

!methodRemoval: TranscriptMorph #mouseDown:localPosition:!
TranscriptMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: TextEditor #mouseDown:localPosition:!
TextEditor removeSelector: #mouseDown:localPosition:!
!methodRemoval: TextEditor #mouseUp:localPosition:!
TextEditor removeSelector: #mouseUp:localPosition:!
!methodRemoval: ScrollBar #mouseDown:localPosition:!
ScrollBar removeSelector: #mouseDown:localPosition:!
!methodRemoval: PluggableListMorphOfMany #mouseDown:localPosition:!
PluggableListMorphOfMany removeSelector: #mouseDown:localPosition:!
!methodRemoval: PluggableListMorphOfMany #mouseUp:localPosition:!
PluggableListMorphOfMany removeSelector: #mouseUp:localPosition:!
!methodRemoval: PluggableListMorph #mouseDown:localPosition:!
PluggableListMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: PluggableListMorph #mouseUp:localPosition:!
PluggableListMorph removeSelector: #mouseUp:localPosition:!
!methodRemoval: HierarchicalListMorph #mouseDown:localPosition:!
HierarchicalListMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: HierarchicalListMorph #mouseUp:localPosition:!
HierarchicalListMorph removeSelector: #mouseUp:localPosition:!
!methodRemoval: PluggableScrollPane #mouseDown:localPosition:!
PluggableScrollPane removeSelector: #mouseDown:localPosition:!
!methodRemoval: PluggableScrollPane #mouseUp:localPosition:!
PluggableScrollPane removeSelector: #mouseUp:localPosition:!
!methodRemoval: DraggeableButtonMorph #mouseDown:localPosition:!
DraggeableButtonMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: DraggeableButtonMorph #mouseUp:localPosition:!
DraggeableButtonMorph removeSelector: #mouseUp:localPosition:!
!methodRemoval: PluggableButtonMorph #mouseButton2Activity!
PluggableButtonMorph removeSelector: #mouseButton2Activity!
!methodRemoval: PluggableButtonMorph #mouseDown:localPosition:!
PluggableButtonMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: PluggableButtonMorph #mouseUp:localPosition:!
PluggableButtonMorph removeSelector: #mouseUp:localPosition:!
!methodRemoval: PasteUpMorph #mouseDown:localPosition:!
PasteUpMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: OneLineEditorMorph #mouseButton2Activity!
OneLineEditorMorph removeSelector: #mouseButton2Activity!
!methodRemoval: OneLineEditorMorph #mouseDown:localPosition:!
OneLineEditorMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: OneLineEditorMorph #mouseUp:localPosition:!
OneLineEditorMorph removeSelector: #mouseUp:localPosition:!
!methodRemoval: MenuMorph #mouseDown:localPosition:!
MenuMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: MenuMorph #mouseUp:localPosition:!
MenuMorph removeSelector: #mouseUp:localPosition:!
!methodRemoval: MenuItemMorph #mouseDown:localPosition:!
MenuItemMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: MenuItemMorph #mouseUp:localPosition:!
MenuItemMorph removeSelector: #mouseUp:localPosition:!
!methodRemoval: MenuItemMorph #processMouseUp:localPosition:!
MenuItemMorph removeSelector: #processMouseUp:localPosition:!
!methodRemoval: MagnifierMorph #mouseDown:localPosition:!
MagnifierMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: LayoutAdjustingMorph #mouseDown:localPosition:!
LayoutAdjustingMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: InnerTextMorph #mouseDown:localPosition:!
InnerTextMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: InnerTextMorph #mouseUp:localPosition:!
InnerTextMorph removeSelector: #mouseUp:localPosition:!
!methodRemoval: HaloHandleMorph #processMouseDown:localPosition:!
HaloHandleMorph removeSelector: #processMouseDown:localPosition:!
!methodRemoval: HaloHandleMorph #processMouseMove:localPosition:!
HaloHandleMorph removeSelector: #processMouseMove:localPosition:!
!methodRemoval: HaloHandleMorph #processMouseUp:localPosition:!
HaloHandleMorph removeSelector: #processMouseUp:localPosition:!

!HaloHandleMorph reorganize!
('accessing' keyStrokeSelector: mouseDownSelector: mouseMoveSelector: mouseUpSelector:)
('act' send:withEvent:)
('drawing' drawOn:)
('event handling testing' handlesMouseDown: handlesMouseStillDown:)
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
!

!methodRemoval: FillInTheBlankMorph #mouseDown:localPosition:!
FillInTheBlankMorph removeSelector: #mouseDown:localPosition:!
!methodRemoval: AutoCompleterMorph #mouseUp:localPosition:!
AutoCompleterMorph removeSelector: #mouseUp:localPosition:!
!methodRemoval: Morph #mouseDown:localPosition:!
Morph removeSelector: #mouseDown:localPosition:!
!methodRemoval: Morph #mouseUp:localPosition:!
Morph removeSelector: #mouseUp:localPosition:!
