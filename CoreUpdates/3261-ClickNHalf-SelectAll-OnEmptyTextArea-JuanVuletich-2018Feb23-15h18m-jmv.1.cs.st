'From Cuis 5.0 of 7 November 2016 [latest update: #3260] on 23 February 2018 at 3:19:20 pm'!

!TextModelMorph methodsFor: 'events' stamp: 'jmv 2/23/2018 14:55:30'!
clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self textMorph clickAndHalf: aMouseButtonEvent localPosition: localEventPosition! !

!TextModelMorph methodsFor: 'events' stamp: 'jmv 2/23/2018 14:56:20'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	scroller mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

