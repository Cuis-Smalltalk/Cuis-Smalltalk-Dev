'From Cuis 5.0 [latest update: #4844] on 6 September 2021 at 3:38:16 pm'!

!HandMorph methodsFor: 'double click support' stamp: 'jmv 9/6/2021 15:30:21'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel

	"Wait until the difference between click, or drag gesture is known, then inform the given morph what transpired." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: nil
			dblClick: nil
			dblClickAndHalf: nil
			tripleClick: nil
			event: evt
			sendMouseButton2Activity: Preferences tapAndHoldEmulatesButton2.

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !


!MouseClickState methodsFor: 'private' stamp: 'jmv 9/6/2021 15:33:13'!
notWaitingForMultipleClicks

	^ clickAndHalfSelector isNil and: [
		dblClickSelector isNil and: [
			dblClickAndHalfSelector isNil and: [
				tripleClickSelector isNil ]]]! !


!PasteUpMorph methodsFor: 'events' stamp: 'jmv 9/6/2021 15:30:26'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent
		clkSel: #click:localPosition:.! !


!MouseClickState methodsFor: 'actions' stamp: 'jmv 9/6/2021 15:33:53'!
handleEvent: aMouseEvent from: aHand
	"Process the given mouse event to detect a click, double-click, or drag.
	Return true if the event should be processed by the sender, false if it shouldn't.
	NOTE: This method heavily relies on getting *all* mouse button events."

	| timedOut distance |
	timedOut _ (aMouseEvent timeStamp - lastClickDown timeStamp) > self class doubleClickTimeout.
	timedOut ifTrue: [ aHand dontWaitForMoreClicks ].
	distance _ (aMouseEvent eventPosition - lastClickDown eventPosition) r.
	"Real action dispatch might be done after the triggering event, for example, because of waiting for timeout.
	So, count the button downs and ups(clicks), to be processed, maybe later, maybe in a mouseMove..."
	aMouseEvent isMouseDown ifTrue: [
		lastClickDown _ aMouseEvent.
		buttonDownCount _ buttonDownCount + 1 ].
	aMouseEvent isMouseUp ifTrue: [
		buttonUpCount _ buttonUpCount + 1 ].

	"Drag, or tap & hold"
	(buttonDownCount = 1 and: [ buttonUpCount = 0]) ifTrue: [
		(self notWaitingForMultipleClicks or: [ distance > 0 ]) ifTrue: [
			"If we have already moved, then it won't be a double or triple click... why wait?"
			aHand dontWaitForMoreClicks.
			dragSelector
				ifNotNil: [ self didDrag ]
				ifNil: [ self didClick ].
			^ false ].
		timedOut ifTrue: [
			aHand dontWaitForMoreClicks.
			"Simulate button 2 via tap & hold. Useful for opening menus on pen computers."
			sendMouseButton2Activity ifTrue: [
				clickClient mouseButton2Activity ].
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

