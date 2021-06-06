'From Cuis 5.0 [latest update: #4631] on 6 June 2021 at 7:50:16 pm'!

!TextModelMorph methodsFor: 'events' stamp: 'jmnv 6/6/2021 19:23:59'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
	scroller mouseButton1Down: aMouseButtonEvent localPosition: eventPositionLocalToScroller.

	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

!methodRemoval: TextModelMorph #doubleClick:localPosition: stamp: 'jmv 6/6/2021 19:50:00'!
TextModelMorph removeSelector: #doubleClick:localPosition:!
