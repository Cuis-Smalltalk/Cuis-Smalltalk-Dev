'From Cuis6.3 [latest update: #6325] on 23 April 2024 at 10:28:25 am'!

!MouseEvent methodsFor: 'testing' stamp: 'jmv 4/23/2024 10:24:16'!
isMouseButton
	^ false! !


!MouseButtonEvent methodsFor: 'testing' stamp: 'jmv 4/23/2024 10:24:28'!
isMouseButton
	^ true! !


!MouseClickState methodsFor: 'actions' stamp: 'jmv 4/23/2024 10:27:44'!
handleEvent: aMouseEvent from: aHand
	"Process the given mouse event to detect a click, double-click, or drag.
	Return true if the event should be processed by the sender, false if it shouldn't.
	NOTE: This method heavily relies on getting *all* mouse button events."

	| timedOut distance |
	(aMouseEvent isMouseButton and: [aMouseEvent mouseButton1Changed not]) ifTrue: [
		aHand dontWaitForMoreClicks.
		^ false ].
	timedOut := (aMouseEvent timeStamp - lastClickDown timeStamp) > self class doubleClickTimeout.
	timedOut ifTrue: [ aHand dontWaitForMoreClicks ].
	distance := (aMouseEvent eventPosition - lastClickDown eventPosition) r.
	"Real action dispatch might be done after the triggering event, for example, because of waiting for timeout.
	So, count the button downs and ups(clicks), to be processed, maybe later, maybe in a mouseMove..."
	aMouseEvent isMouseDown ifTrue: [
		lastClickDown := aMouseEvent.
		buttonDownCount := buttonDownCount + 1 ].
	aMouseEvent isMouseUp ifTrue: [
		buttonUpCount := buttonUpCount + 1 ].

	"Drag, or tap & hold"
	(buttonDownCount = 1 and: [ buttonUpCount = 0]) ifTrue: [
		distance > 0 ifTrue: [
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

	"If we're over triple click, or timed out, or mouse moved, don't allow more clicks (distance must be significant, the movement needs to be intentional)."
	(buttonDownCount = 4 or: [ timedOut or: [ distance > 2 ]]) ifTrue: [
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

