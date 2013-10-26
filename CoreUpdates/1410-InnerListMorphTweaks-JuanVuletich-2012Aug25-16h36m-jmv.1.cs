'From Cuis 4.0 of 21 April 2012 [latest update: #1409] on 25 August 2012 at 5:09:04 pm'!

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 8/25/2012 17:09'!
drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!!"

	self flag: #jmvVer2.
	"revisar senders"
	^ 0 @ (row - 1 * font height) extent: self morphExtentInOwner x @ font height! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2012 17:07'!
draw: item atRow: row on: canvas
	"display the given item at row row"
	| drawBounds f |
	drawBounds _ self drawBoundsForRow: row.
	drawBounds _ drawBounds intersect: (0@0 extent: extent).
	drawBounds _ (self externalizeToWorld: drawBounds origin) extent: drawBounds extent.
	f _ (item is: #Text) ifTrue: [ font emphasized: (item emphasisAt: 1) ] ifFalse: [ font ].
	canvas drawString: item in: drawBounds font: f color: (self colorForRow: row)! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2012 17:07'!
drawBackgroundForMulti: row on: aCanvas
	| selectionDrawBounds c |
	"shade the background darker, if this row is selected"
	selectionDrawBounds _ self drawBoundsForRow: row.
	selectionDrawBounds _ selectionDrawBounds intersect: (0@0 extent: extent).
	selectionDrawBounds _ (self externalizeToWorld: selectionDrawBounds origin) extent: selectionDrawBounds extent.
	c _ (selectedRow notNil and: [ row = selectedRow])
		ifTrue: [ Theme current listHighlightFocused: owner hasKeyboardFocus ]
		ifFalse: [ Theme current listMultiHighlightFocused: owner hasKeyboardFocus ].
	aCanvas fillRectangle: selectionDrawBounds colorOrInfiniteForm: c! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 8/25/2012 17:08'!
drawSelectionOn: aCanvas
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: selectedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: (0@0 extent: extent).
	selectionDrawBounds _ (self externalizeToWorld: selectionDrawBounds origin) extent: selectionDrawBounds extent.
	aCanvas
		fillRectangle: selectionDrawBounds
		colorOrInfiniteForm: (Theme current listHighlightFocused: owner hasKeyboardFocus).! !


!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 8/25/2012 16:56'!
scrollSelectionIntoView
	"make sure that the current selection is visible"
	| row r |
	row _ self getCurrentSelectionIndex.
	row = 0 ifTrue: [
		^ scrollBar setValue: scrollBar value ].
	self flag: #jmvVer2.
	r _ self listMorph drawBoundsForRow: row.
	r _ ((self listMorph externalize: r origin) extent: r extent).
	self scrollToShow: r! !

!methodRemoval: InnerListMorph #ydrawBoundsForRow:!
InnerListMorph removeSelector: #ydrawBoundsForRow:!
!methodRemoval: InnerListMorph #zdrawBoundsForRow:!
InnerListMorph removeSelector: #zdrawBoundsForRow:!
