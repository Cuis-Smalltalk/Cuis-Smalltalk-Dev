'From Cuis 5.0 [latest update: #4938] on 22 October 2021 at 3:05:28 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 10/22/2021 14:49:20'!
externalizeBoundingRectOf: aRectangle

	^aRectangle! !


!MovableMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 10/22/2021 14:49:26'!
externalizeBoundingRectOf: aRectangle

	^location externalBoundingRectOf: aRectangle.! !


!InnerListMorph methodsFor: 'list management' stamp: 'jmv 10/22/2021 14:42:22'!
drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!!"

	^ 0 @ (self drawYForRow: row) extent: extent x @ font lineSpacing! !


!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 10/22/2021 15:05:07'!
scrollSelectionIntoView

	| r |
	selectedMorph ifNotNil: [
		r _ scroller externalizeBoundingRectOf: 
			(selectedMorph morphPosition extent: selectedMorph morphExtentInOwner).
		self scrollToShow: r ]! !


!PluggableListMorph methodsFor: 'scrolling' stamp: 'jmv 10/22/2021 14:59:44'!
scrollSelectionIntoView
	"make sure that the current selection is visible"
	| row r |
	row _ self getCurrentSelectionIndex.
	row = 0 
		ifTrue: [
			"Value is 0, but we need to propagate it to model"
			scrollBar internalScrollValue: scrollBar scrollValue ]
		ifFalse: [
			r _ scroller externalizeBoundingRectOf: (scroller drawBoundsForRow: row).
			self scrollToShow: r ].
	self scrollMySiblings
! !

!methodRemoval: MovableMorph #externalBoundingRectOf: stamp: 'jmv 10/22/2021 14:49:30'!
MovableMorph removeSelector: #externalBoundingRectOf:!
!methodRemoval: Morph #externalBoundingRectOf: stamp: 'jmv 10/22/2021 14:49:28'!
Morph removeSelector: #externalBoundingRectOf:!
