'From Cuis 4.1 of 12 December 2012 [latest update: #1563] on 19 January 2013 at 3:10:21 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 1/19/2013 15:08'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"

	"Draw receiver itself"
	(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [
		aCanvas clipBy: self morphBoundsInWorld during: [ :c | self drawOn: c ]].	"Needs bounds 'in owner' if inside a scroller"
	self drawSubmorphsOn: aCanvas! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 1/19/2013 15:07'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |
	aMouseButtonEvent hand newKeyboardFocus: self.
	itemMorph _ self itemFromPoint: localEventPosition.
	(itemMorph notNil and: [
		itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)) ])
		ifTrue: [ ^self toggleExpandedState: itemMorph event: aMouseButtonEvent ]. 
	itemMorph ifNil: [ ^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 1/19/2013 15:07'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| itemMorph |
	itemMorph _ self itemFromPoint: localEventPosition.
	itemMorph ifNil: [^self].
	"No change if model is locked"
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	((autoDeselect isNil or: [autoDeselect]) and: [itemMorph == selectedMorph]) 
		ifTrue: [self setSelectedMorph: nil]
		ifFalse: [self setSelectedMorph: itemMorph].
	Cursor normal show! !

!methodRemoval: Morph #drawMouseDownHighlightOn:!
Morph removeSelector: #drawMouseDownHighlightOn:!
!methodRemoval: Morph #highlightForMouseDown!
Morph removeSelector: #highlightForMouseDown!
!methodRemoval: Morph #highlightForMouseDown:!
Morph removeSelector: #highlightForMouseDown:!
!methodRemoval: Morph #highlightedForMouseDown!
Morph removeSelector: #highlightedForMouseDown!
