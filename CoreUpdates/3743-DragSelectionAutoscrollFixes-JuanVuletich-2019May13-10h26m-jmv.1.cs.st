'From Cuis 5.0 of 7 November 2016 [latest update: #3741] on 13 May 2019 at 10:30:38 am'!

!TextEditor methodsFor: 'events' stamp: 'jmv 5/13/2019 10:03:18'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Change the selection in response to mouse-down drag"

	| newPointBlock goingBackwards newStartBlock newStopBlock interval i1 i2 |
	newPointBlock _ textComposition characterBlockAtPoint: localEventPosition.
	goingBackwards _ newPointBlock stringIndex < markBlock stringIndex.

	doWordSelection ifTrue: [
		pointBlock _ newPointBlock.
		self selectWordLeftDelimiters: '' rightDelimiters: ''.
		newStartBlock _ self startBlock min: initialSelectionStart.
		newStopBlock _ self stopBlock max: initialSelectionStop.
		markBlock _ goingBackwards ifTrue: [newStopBlock] ifFalse: [newStartBlock].
		pointBlock _ goingBackwards ifTrue: [newStartBlock] ifFalse: [newStopBlock].
		self storeSelectionInComposition.
		^self ].

	doParagraphSelection ifTrue: [
		i1 _ newPointBlock stringIndex min: initialSelectionStart stringIndex.
		i2 _ newPointBlock stringIndex max: initialSelectionStop stringIndex-1.
		interval _ self privateCurrentString encompassParagraph: (i1 to: i2).
		self selectFrom: interval first to: interval last.
		newStartBlock _ self startBlock min: initialSelectionStart.
		newStopBlock _ self stopBlock max: initialSelectionStop.
		markBlock _ goingBackwards ifTrue: [newStopBlock] ifFalse: [newStartBlock].
		pointBlock _ goingBackwards ifTrue: [newStartBlock] ifFalse: [newStopBlock].
		self storeSelectionInComposition.
		^self ].

	pointBlock _ newPointBlock.
	self storeSelectionInComposition! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 5/13/2019 10:22:02'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
	scroller mouseButton1Down: aMouseButtonEvent localPosition: eventPositionLocalToScroller.
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 5/13/2019 10:22:51'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	scroller mouseButton1Up: aMouseButtonEvent localPosition: eventPositionLocalToScroller! !

!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 5/13/2019 10:22:12'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
	scroller  mouseMove: aMouseMoveEvent localPosition: eventPositionLocalToScroller! !


!TextModelMorph methodsFor: 'editor access' stamp: 'jmv 5/13/2019 10:30:16'!
scrollSelectionIntoView
	"Scroll my text into view if necessary and return true, else return false"

	| delta |
	delta _ self textMorph morphPosition.
	self editor pointIndex > 1
		ifTrue: [
			self scrollToShow: (self editor pointBlock translatedBy: delta) ]
		ifFalse: [
			self scrollToShow: (self editor selectionRectangle translatedBy: delta) ]! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 5/13/2019 10:21:37'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
	scroller mouseButton1Down: aMouseButtonEvent localPosition: eventPositionLocalToScroller.
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 5/13/2019 09:23:53'!
processKeyStroke: evt
	| action |

	(acceptOnCR and: [evt isReturnKey]) ifTrue: [^ self acceptContents].

	self pauseBlinking.
	
	"Return - check for special action"
	evt isReturnKey ifTrue: [	
		action _ self crAction.
		action ifNotNil: [ ^action value]].
	
	"Esc - check for special action"
	evt isEsc ifTrue: [			
		action _ self escAction.
		action ifNotNil: [ ^action value]].
	
	self handleInteraction: [ editor processKeyStroke: evt ].
	self scrollSelectionIntoView! !

