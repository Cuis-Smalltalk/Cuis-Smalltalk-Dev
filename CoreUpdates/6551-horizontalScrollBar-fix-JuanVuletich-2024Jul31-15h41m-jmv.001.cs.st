'From Cuis7.1 [latest update: #6557] on 31 July 2024 at 3:42:39 pm'!

!PluggableListMorph methodsFor: 'scrolling' stamp: 'jmv 7/31/2024 15:40:07'!
scrollSelectionIntoView
	"make sure that the current selection is visible"
	| row r |
	row := self getCurrentSelectionIndex.
	row = 0 
		ifTrue: [
			"Value is 0, but we need to propagate it to model"
			scrollBar internalScrollValue: scrollBar scrollValue ]
		ifFalse: [
			r := scroller externalizeBoundingRectOf: (scroller drawBoundsForRow: row).
			r := r left: 0.
			self scrollToShow: r ].
	self updateSiblingsScroller! !

