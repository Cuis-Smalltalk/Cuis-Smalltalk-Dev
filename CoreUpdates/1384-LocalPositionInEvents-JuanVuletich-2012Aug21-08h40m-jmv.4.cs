'From Cuis 4.0 of 21 April 2012 [latest update: #1382] on 21 August 2012 at 9:17:21 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/21/2012 09:01'!
morphExtentInOwner
"assume it is always in owner's coordinates!!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^ extent! !


!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:47'!
didClick

	clickDone ifFalse: [
		clickSelector ifNotNil: [
			clickClient perform: clickSelector with: lastClickDown with: self lastClickLocalPosition ].
		clickDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:48'!
didClickAndHalf

	clickAndHalfDone ifFalse: [
		clickAndHalfSelector ifNotNil: [
			"Focus was lost at buttonUp. Set it again."
			lastClickDown hand newMouseFocus: clickClient event: lastClickDown.
			clickClient perform: clickAndHalfSelector with: lastClickDown with: self lastClickLocalPosition ].
		clickAndHalfDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:48'!
didCoubleClick

	doubleClickDone ifFalse: [
		dblClickSelector ifNotNil: [
			clickClient perform: dblClickSelector with: lastClickDown with: self lastClickLocalPosition ].
		doubleClickDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:48'!
didCoubleClickAndHalf

	doubleClickAndHalfDone ifFalse: [
		dblClickAndHalfSelector ifNotNil: [
			"Focus was lost at buttonUp. Set it again."
			lastClickDown hand newMouseFocus: clickClient event: lastClickDown.
			clickClient perform: dblClickAndHalfSelector with: lastClickDown with: self lastClickLocalPosition ].
		doubleClickAndHalfDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:48'!
didCripleClick

	tripleClickSelector ifNotNil: [
		clickClient perform: tripleClickSelector with: lastClickDown with: self lastClickLocalPosition]! !


!SimpleEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:42'!
clickAndHalf

	| here |
	here _ self pointIndex.
	(here between: 2 and: string size)
		ifTrue: [ self selectWord ]
		ifFalse: [
			"if at beginning or end, select entire string"
			self selectAll ].! !


!TextEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:42'!
clickAndHalf

	self selectWord.

	doWordSelection _ true.
	doParagraphSelection _ false.
	initialSelectionStart _ self startBlock.
	initialSelectionStop _ self stopBlock! !

!TextEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:46'!
doubleClickAndHalf

	| here interval |
	here _ self pointIndex.
	interval _ self privateCurrentString encompassParagraph: (here to: here).
	self selectFrom: interval first to: interval last.

	doWordSelection _ false.
	doParagraphSelection _ true.
	initialSelectionStart _ self startBlock.
	initialSelectionStop _ self stopBlock! !


!SmalltalkEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 08:43'!
clickAndHalf

	| here |
	here _ self pointIndex.
	(here between: 2 and: model textSize)
		ifTrue: [
			super clickAndHalf ]
		ifFalse: [
			"if at beginning or end, select entire string"
			self selectAll ]! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 8/21/2012 09:16'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. The dispatch is similar to the default dispatch with one difference: Morphs are given the chance to reject an entire drop operation. If the operation is rejected, no drop will be executed."
	| inside eventPositionInChild |

	"Try to get out quickly"
	(aMorph morphFullBoundsInWorld containsPoint: self eventPosition)
		ifFalse: [ ^#rejected ].

	"Give aMorph a chance to repel the dropping morph"
	aMorph rejectDropEvent: self.
	self wasHandled ifTrue: [^self ].

	"Go looking if any of our submorphs wants it"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				inside _ true
			]]].

	inside ifFalse: [ inside _ aMorph containsPoint: self eventPosition event: self ].
	inside ifTrue: [ ^ self sentTo: aMorph localPosition: positionInAMorph].
	^#rejected! !


!HierarchicalListMorph methodsFor: 'events-processing' stamp: 'jmv 8/21/2012 09:05'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ aMouseMoveEvent hand mouseFocus == self]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown:localPosition:
			at: Time millisecondClockValue
			arguments: {aMouseMoveEvent copy resetHandlerFields . localEventPosition}
			stepTime: 1].
! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 09:14'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self handleInteraction: [
		editor clickAndHalf ].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:47'!
doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self handleInteraction: [
		editor doubleClickAndHalf ]
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 09:13'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	aMouseButtonEvent mouseButton2Pressed ifTrue: [ ^ self mouseButton2Activity ].

	"If we don't focus, Get focus, and do nothing else (the user will need to click again to do further interaction)"
	self hasKeyboardFocus ifFalse: [
		^aMouseButtonEvent hand newKeyboardFocus: self].

	super mouseDown: aMouseButtonEvent localPosition: localEventPosition.

	self handleInteraction: [ editor mouseDown: aMouseButtonEvent localPosition: localEventPosition ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 09:16'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent mouseButton1Pressed ifFalse: [ ^ self enterClickableRegion: aMouseMoveEvent ].
	self handleInteraction: [ editor mouseMove: aMouseMoveEvent localPosition: localEventPosition].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 09:13'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition

	super mouseUp: aMouseButtonEvent localPosition: localEventPosition.
	self pauseBlinking.
	self handleInteraction: [ editor mouseUp: aMouseButtonEvent  localPosition: localEventPosition ].
	owner scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 8/21/2012 09:01'!
extentForComposing
	self flag: #jmvVer2.	"like #extent ..."
	^wrapFlag
		ifTrue: [ self morphExtentInOwner x @ 9999999 ]
		ifFalse: [ 9999999@9999999 ]! !


!MouseClickState methodsFor: 'actions' stamp: 'jmv 8/21/2012 08:49'!
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
			self didClick.
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
		self didCoubleClick ].

	"Double click & hold."
	(buttonDownCount = 3 and: [ buttonUpCount = 2]) ifTrue: [
		self didCoubleClickAndHalf ].

	"Triple click"
	(buttonDownCount = 3 and: [ buttonUpCount = 3]) ifTrue: [
		self didCripleClick ].

	"This means: if a mouseDown, then don't further process this event (so we can turn it into a double or triple click on next buttonUp)"
	^ aMouseEvent isMouseDown! !


!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 8/21/2012 08:43'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition
	self handleInteraction: [ self editor clickAndHalf ]! !


!Paragraph methodsFor: 'editing' stamp: 'jmv 8/21/2012 09:02'!
clickAt: clickPoint
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	| startBlock action target range boxes box t |
	action _ false.
	startBlock _ self characterBlockAtPoint: clickPoint.
	t _ model actualContents.
	(t attributesAt: startBlock stringIndex) do: [ :att | 
		att mayActOnClick ifTrue:
				[(target _ model) ifNil: [ target _ editor morph].
				range _ t rangeOf: att startingAt: startBlock stringIndex.
				boxes _ self selectionRectsFrom: (self characterBlockForIndex: range first) 
							to: (self characterBlockForIndex: range last+1).
				box _ boxes detect: [ :each | each containsPoint: clickPoint] ifNone: nil.
				box ifNotNil: [
					box _ editor morph morphBoundsInWorld.
					editor morph allOwnersDo: [ :m | box _ box intersect: (m morphBoundsInWorld) ].
					Utilities
						awaitMouseUpIn: box
						repeating: nil
						ifSucceed: [(att actOnClickFor: target in: self at: clickPoint editor: editor) ifTrue: [action _ true]].
					Cursor currentCursor == Cursor webLink ifTrue:[Cursor normal show].
				]]].
	^ action! !

!Paragraph methodsFor: 'private' stamp: 'jmv 8/21/2012 08:59'!
lineIndexForPoint: aPoint
	"Answer the index of the line in which to select the character nearest to aPoint."
	| i py |
	py _ aPoint y truncated.

	"Find the first line at this y-value"
	i _ (self fastFindFirstLineSuchThat: [ :line | line bottom > py]) min: lines size.

	"Now find the first line at this x-value"
	[ i < lines size and: [ (lines at: i+1) top = (lines at: i) top
				and: [ aPoint x >= (lines at: i+1) left ]]]
		whileTrue: [ i _ i + 1 ].
	^ i! !


!TextEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 09:07'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
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

!TextEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 09:10'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Change the selection in response to mouse-down drag"

	| b interval i1 i2 |
	doWordSelection ifTrue: [
		pointBlock _ (paragraph characterBlockAtPoint: localEventPosition).
		self selectWordLeftDelimiters: '' rightDelimiters: ''.
		markBlock _ self startBlock min: initialSelectionStart.
		pointBlock _ self stopBlock max: initialSelectionStop.
		self storeSelectionInParagraph.
		^self ].

	doParagraphSelection ifTrue: [
		b _ paragraph characterBlockAtPoint: localEventPosition.
		i1 _ b stringIndex min: initialSelectionStart stringIndex.
		i2 _ b stringIndex max: initialSelectionStop stringIndex-1.
		interval _ self privateCurrentString encompassParagraph: (i1 to: i2).
		self selectFrom: interval first to: interval last.
		markBlock _ self startBlock min: initialSelectionStart.
		pointBlock _ self stopBlock max: initialSelectionStop.
		self storeSelectionInParagraph.
		^self ].

	pointBlock _ (paragraph characterBlockAtPoint: localEventPosition).
	self storeSelectionInParagraph! !

!TextEditor methodsFor: 'events' stamp: 'jmv 8/21/2012 09:13'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
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

!methodRemoval: UserInputEvent #ztranslateBy:!
UserInputEvent removeSelector: #ztranslateBy:!
!methodRemoval: UserInputEvent #ztranslatedBy:!
UserInputEvent removeSelector: #ztranslatedBy:!
!methodRemoval: SmalltalkEditor #clickAndHalf:localPosition:!
SmalltalkEditor removeSelector: #clickAndHalf:localPosition:!
!methodRemoval: TextEditor #clickAndHalf:localPosition:!
TextEditor removeSelector: #clickAndHalf:localPosition:!
!methodRemoval: TextEditor #doubleClickAndHalf:localPosition:!
TextEditor removeSelector: #doubleClickAndHalf:localPosition:!
!methodRemoval: SimpleEditor #clickAndHalf:localPosition:!
SimpleEditor removeSelector: #clickAndHalf:localPosition:!
!methodRemoval: MouseClickState #click!
MouseClickState removeSelector: #click!
!methodRemoval: MouseClickState #clickAndAHalf!
MouseClickState removeSelector: #clickAndAHalf!
!methodRemoval: MouseClickState #doubleClick!
MouseClickState removeSelector: #doubleClick!
!methodRemoval: MouseClickState #doubleClickAndHalf!
MouseClickState removeSelector: #doubleClickAndHalf!
!methodRemoval: MouseClickState #tripleClick!
MouseClickState removeSelector: #tripleClick!
