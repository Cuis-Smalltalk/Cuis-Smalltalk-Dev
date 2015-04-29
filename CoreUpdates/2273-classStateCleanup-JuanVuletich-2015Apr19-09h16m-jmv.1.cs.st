'From Cuis 4.2 of 25 July 2013 [latest update: #2257] on 19 April 2015 at 9:39:31.204912 am'!
!classDefinition: #TextAction category: #'System-TextAttributes'!
TextAttribute subclass: #TextAction
	instanceVariableNames: ''
	classVariableNames: 'Purple '
	poolDictionaries: ''
	category: 'System-TextAttributes'!

!MouseClickState class methodsFor: 'cached state access' stamp: 'jmv 4/19/2015 09:18'!
doubleClickTimeout
	DoubleClickTimeout ifNil: [
		DoubleClickTimeout _ 350 ].
	^DoubleClickTimeout! !

!MouseClickState class methodsFor: 'cached state access' stamp: 'jmv 4/19/2015 09:19'!
releaseClassCachedState
	DoubleClickTimeout _ nil! !


!MouseClickState methodsFor: 'actions' stamp: 'jmv 4/19/2015 09:18'!
handleEvent: aMouseEvent from: aHand
	"Process the given mouse event to detect a click, double-click, or drag.
	Return true if the event should be processed by the sender, false if it shouldn't.
	NOTE: This method heavily relies on getting *all* mouse button events."

	| timedOut distance |
	timedOut _ (aMouseEvent timeStamp - lastClickDown timeStamp) > self class doubleClickTimeout.
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


!TextAction class methodsFor: 'as yet unclassified' stamp: 'jmv 4/19/2015 09:38'!
textActionColor
	^Color r: 0.4 g: 0 b: 1.0! !

!methodRemoval: TextAction class #initialize!
TextAction class removeSelector: #initialize!
TextAction initialize!
!classDefinition: #TextAction category: #'System-TextAttributes'!
TextAttribute subclass: #TextAction
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-TextAttributes'!
!methodRemoval: MouseClickState class #initialize!
MouseClickState class removeSelector: #initialize!
MouseClickState initialize!

!MouseClickState class reorganize!
('cached state access' doubleClickTimeout releaseClassCachedState)
!

