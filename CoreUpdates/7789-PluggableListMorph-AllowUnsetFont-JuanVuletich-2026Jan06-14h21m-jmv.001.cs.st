'From Cuis7.7 [latest update: #7784] on 6 January 2026 at 2:31:10 pm'!

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 6/Jan/2026 14:29:10'!
fontToUse
	"return the font used for drawing.  The response is never nil"
	^font ifNil: [Preferences at: #standardListFont]! !


!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 6/Jan/2026 14:28:17'!
scrollDeltaHeight
	"Return the increment in pixels which this pane should be scrolled."
	^ scroller listItemHeight! !


!InnerListMorph methodsFor: 'initialization' stamp: 'jmv 6/Jan/2026 14:29:18'!
initialize
	super initialize.
	self color: `Color black`.
	font := nil.
	listItems := #().
	selectedRow := nil.
	highlightedRow := nil! !

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 6/Jan/2026 14:26:14'!
drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!!"

	^ 0 @ (self drawYForRow: row) extent: extent x @ self listItemHeight! !

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 6/Jan/2026 14:26:19'!
drawYForRow: row
	"calculate the vertical position that row should be drawn at.  This might be outside our bounds!!"
	^ row - 1 * self listItemHeight! !

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 6/Jan/2026 14:27:49'!
rowAtLocation: aPoint
	"return the number of the row at aPoint"

	listItems isEmpty ifTrue: [ ^0 ].
	^aPoint y // self listItemHeight + 1 min: listItems size max: 1! !

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 6/Jan/2026 14:27:56'!
rowAtLocation: aPoint ifNone: aNoneBlock

	| potentialRowNumber |
	
	potentialRowNumber := aPoint y // self listItemHeight + 1.
	
	^(listItems isInBounds: potentialRowNumber) 
		ifTrue: [ potentialRowNumber ]
		ifFalse: [ aNoneBlock value ]! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 6/Jan/2026 14:25:04'!
draw: item atRow: row on: canvas
	"display the given item at row row"
	| f c |
	(item is: #Text)
		ifTrue: [
			f := self fontToUse emphasized: (item emphasisAt: 1).
			c := (item colorAt: 1) ifNil: [Theme current text]]
		ifFalse: [ 
			f := self fontToUse.
			c :=  Theme current text].
	canvas
		drawString: item string
		at: 0 @ (self drawYForRow: row)
		font: f
		color: c! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 6/Jan/2026 14:24:16'!
font

	^ self fontToUse! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 6/Jan/2026 14:28:42'!
font: newFontOrNil
	font := newFontOrNil.
	self adjustExtent! !

!InnerListMorph methodsFor: 'geometry' stamp: 'jmv 6/Jan/2026 14:25:25'!
listItemHeight
	^ self fontToUse lineSpacing
! !

!methodRemoval: InnerListMorph #fontPreferenceChanged stamp: 'jmv 6/Jan/2026 14:30:39'!
InnerListMorph removeSelector: #fontPreferenceChanged!
!methodRemoval: PluggableListMorph #fontPreferenceChanged stamp: 'jmv 6/Jan/2026 14:29:50'!
PluggableListMorph removeSelector: #fontPreferenceChanged!
