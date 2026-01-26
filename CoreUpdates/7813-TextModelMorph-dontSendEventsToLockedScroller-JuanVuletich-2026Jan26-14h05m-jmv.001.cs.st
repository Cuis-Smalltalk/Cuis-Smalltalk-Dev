'From Cuis7.7 [latest update: #7810] on 26 January 2026 at 2:12:54 pm'!

!TextModelMorph methodsFor: 'dependents access' stamp: 'jmv 26/Jan/2026 14:05:50'!
canDiscardEdits
	"Return true if this view either has no text changes or does not care."

	^ scroller isLocked or: [scroller canDiscardEdits]! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 14:06:09'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	scroller isLocked ifFalse: [
		scroller clickAndHalf: aMouseButtonEvent localPosition: localEventPosition ].! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 14:06:15'!
doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	scroller isLocked ifFalse: [
		scroller doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition ].! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 14:06:30'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	scroller isLocked ifFalse: [
		eventPositionLocalToScroller := localEventPosition - scroller morphPosition.
		scroller mouseButton1Down: aMouseButtonEvent localPosition: eventPositionLocalToScroller ].

	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

!TextModelMorph methodsFor: 'event handling' stamp: 'jmv 26/Jan/2026 14:05:11'!
mouseButton2Activity
	"Button 2 presses inside us, but not inside our InnerTextMorph (i.e. scroller) are caught here.
	Forward them to the InnerTextMorph, so they behave the same as if the button press actually occured there."

	scroller isLocked ifFalse: [
		scroller mouseButton2Activity ].! !

!TextModelMorph methodsFor: 'focus handling' stamp: 'jmv 26/Jan/2026 14:10:28'!
focusText

	scroller isLocked ifFalse: [
		self world activeHand newKeyboardFocus: scroller ].! !

