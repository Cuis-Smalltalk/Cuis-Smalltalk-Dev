'From Cuis 6.0 [latest update: #5046] on 25 January 2022 at 10:15:45 am'!

!HierarchicalListMorph methodsFor: 'events' stamp: 'len 1/25/2022 10:15:18'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	doubleClickSelector ifNotNil: [^ self model perform: doubleClickSelector].
	selectedMorph ifNotNil: [self toggleExpandedState: selectedMorph]! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'len 1/25/2022 10:14:08'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |
	aMouseButtonEvent hand newKeyboardFocus: self.
	itemMorph _ self itemFromPoint: localEventPosition.
	itemMorph ifNil: [ ^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].
	self highlightedMorph: itemMorph.
	(itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)))
		ifTrue: [ ^self toggleExpandedState: itemMorph event: aMouseButtonEvent ]. 
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

