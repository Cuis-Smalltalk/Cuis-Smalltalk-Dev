'From Cuis 4.1 of 12 December 2012 [latest update: #1554] on 14 January 2013 at 5:23:48 pm'!

!MouseClickState methodsFor: 'actions' stamp: 'jmv 1/14/2013 17:21'!
didDoubleClick

	doubleClickDone ifFalse: [
		dblClickSelector ifNotNil: [
			clickClient perform: dblClickSelector with: lastClickDown with: self lastClickLocalPosition ].
		doubleClickDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 1/14/2013 17:21'!
didDoubleClickAndHalf

	doubleClickAndHalfDone ifFalse: [
		dblClickAndHalfSelector ifNotNil: [
			"Focus was lost at buttonUp. Set it again."
			lastClickDown hand newMouseFocus: clickClient.
			clickClient perform: dblClickAndHalfSelector with: lastClickDown with: self lastClickLocalPosition ].
		doubleClickAndHalfDone _ true ]! !

!MouseClickState methodsFor: 'actions' stamp: 'jmv 1/14/2013 17:21'!
didTripleClick

	tripleClickSelector ifNotNil: [
		clickClient perform: tripleClickSelector with: lastClickDown with: self lastClickLocalPosition]! !


!MouseClickState methodsFor: 'actions' stamp: 'jmv 1/14/2013 17:22'!
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
		self didDoubleClick ].

	"Double click & hold."
	(buttonDownCount = 3 and: [ buttonUpCount = 2]) ifTrue: [
		self didDoubleClickAndHalf ].

	"Triple click"
	(buttonDownCount = 3 and: [ buttonUpCount = 3]) ifTrue: [
		self didTripleClick ].

	"This means: if a mouseDown, then don't further process this event (so we can turn it into a double or triple click on next buttonUp)"
	^ aMouseEvent isMouseDown! !

!methodRemoval: MouseClickState #didCoubleClick!
MouseClickState removeSelector: #didCoubleClick!
!methodRemoval: MouseClickState #didCoubleClickAndHalf!
MouseClickState removeSelector: #didCoubleClickAndHalf!
!methodRemoval: MouseClickState #didCripleClick!
MouseClickState removeSelector: #didCripleClick!
