"
The morph that displays the list in a PluggableListMorph.
"
Class {
	#name : 'InnerListMorph',
	#superclass : 'InnerPluggableMorph',
	#instVars : [
		'listItems',
		'font',
		'selectedRow',
		'highlightedRow'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'geometry' }
InnerListMorph >> adjustExtent [
	"Adjust our height to match the underlying list,
	but make it wider if neccesary to fill the available width in our PluggableListMorph
	(this is needed to make the selection indicator no narrower than the list)"

	self morphExtent: self desiredExtent.
]

{ #category : 'geometry' }
InnerListMorph >> desiredExtent [
	^self desiredWidth @ self desiredHeight
]

{ #category : 'geometry' }
InnerListMorph >> desiredHeight [
	^(listItems size max: 1) * self listItemHeight
]

{ #category : 'geometry' }
InnerListMorph >> desiredWidth [
"Ok, this is a bit messed up. We need to return the width of the widest item in the list. If we grab every item in the list, it defeats the purpose of LazyListMorph. If we don't, then we don't know the size. This is a compromise -- if the list is less then 30 items, we grab them all. If not, we grab currently visible ones, until we've checked itemsToCheck of them, then take the max width out of that 'sampling', then enlarge it a bit. If you know a better way, please chime in."

	| maxW count itemsToCheck |

	itemsToCheck := 200.
	maxW := owner viewableWidth.
	count := 0.
	listItems do: [ :each |
		each ifNotNil: [
			maxW := maxW max: (self widthToDisplayItem: each contents)]].
	(count < itemsToCheck) ifTrue: [
		1 to: listItems size do: [:i | 
			(listItems at: i) ifNil: [ | item | 
				item := self item: i.
				maxW := maxW max: (self widthToDisplayItem: item contents) ].
			((count := count + 1) > itemsToCheck) ifTrue: [
				^maxW * 13//10  ]]].
	^maxW
]

{ #category : 'drawing' }
InnerListMorph >> draw: item atRow: row on: canvas [
	"display the given item at row row"
	| f c |
	(item is: #Text)
		ifTrue: [
			f := font emphasized: (item emphasisAt: 1).
			c := (item colorAt: 1) ifNil: [Theme current text]]
		ifFalse: [ 
			f := font.
			c :=  Theme current text].
	canvas
		drawString: item string
		at: 0 @ (self drawYForRow: row)
		font: f
		color: c
]

{ #category : 'drawing' }
InnerListMorph >> drawBackgroundForMulti: row on: aCanvas [
	| selectionDrawBounds c |
	"shade the background darker, if this row is selected"
	selectionDrawBounds := self drawBoundsForRow: row.
	selectionDrawBounds := selectionDrawBounds intersect: self localBounds.
	c := (selectedRow notNil and: [ row = selectedRow])
		ifTrue: [ Theme current listHighlightFocused: owner hasKeyboardFocus ]
		ifFalse: [ Theme current listMultiHighlightFocused: owner hasKeyboardFocus ].
	aCanvas fillRectangle: selectionDrawBounds color: c
]

{ #category : 'list management' }
InnerListMorph >> drawBoundsForRow: row [
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!"

	^ 0 @ (self drawYForRow: row) extent: extent x @ font lineSpacing
]

{ #category : 'drawing' }
InnerListMorph >> drawHighlightOn: aCanvas [
"Ademas, poner el mouse pointer de relojito si tarda... Detectarlo automaticamente, etc. Aunque no estoy seguro de como hacerlo... quizas colgar un cachito de codigo en un proceso de mayor prioridad, onda 'si pasa 1 segundo, y todavia no te resetee este flag, entonces pone el relojito'"
	| selectionDrawBounds |
	highlightedRow  ifNil: [ ^self ].
	highlightedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds := self drawBoundsForRow: highlightedRow.
	selectionDrawBounds := selectionDrawBounds intersect: self localBounds.
	aCanvas
		fillRectangle: selectionDrawBounds
		color: ((Theme current listHighlightFocused: owner hasKeyboardFocus) alpha: 0.3)
]

{ #category : 'drawing' }
InnerListMorph >> drawOn: aCanvas [

	listItems isEmpty ifTrue: [ ^self ].
 
	self drawHighlightOn: aCanvas.
	self drawSelectionOn: aCanvas.

	(self visibleRowIntervalForCanvas: aCanvas)
		do: [ :row |
			(owner itemSelectedAmongMultiple:  row) ifTrue: [
				self drawBackgroundForMulti: row on: aCanvas. ].
			self draw: (self item: row) displayStringOrText atRow: row on: aCanvas ]
]

{ #category : 'drawing' }
InnerListMorph >> drawSelectionOn: aCanvas [
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds := self drawBoundsForRow: selectedRow.
	selectionDrawBounds := selectionDrawBounds intersect: self localBounds.
	aCanvas
		fillRectangle: selectionDrawBounds
		color: (Theme current listHighlightFocused: owner hasKeyboardFocus)
]

{ #category : 'list management' }
InnerListMorph >> drawYForRow: row [
	"calculate the vertical position that row should be drawn at.  This might be outside our bounds!"
	^ row - 1 * font lineSpacing
]

{ #category : 'drawing' }
InnerListMorph >> font [
	"return the font used for drawing.  The response is never nil"
	^font
]

{ #category : 'drawing' }
InnerListMorph >> font: newFont [
	font := newFont ifNil: [ FontFamily defaultFamilyAndPointSize ].
	self adjustExtent
]

{ #category : 'geometry' }
InnerListMorph >> fontPreferenceChanged [

	super fontPreferenceChanged.
	self font: (Preferences at: #standardListFont).
]

{ #category : 'list access' }
InnerListMorph >> getListItem: index [
	"grab a list item directly from the model"
	^owner getListItem: index
]

{ #category : 'list access' }
InnerListMorph >> getListSize [
	"return the number of items in the list"
	^owner getListSize
]

{ #category : 'event handling testing' }
InnerListMorph >> handlesKeyboardFocus [
	"Return true if the receiver wishes to handle keyboard focus.
	Answer false because it is handled by our owner, a subinstance of PluggableListMorph"

	^false
]

{ #category : 'list management' }
InnerListMorph >> highlightedRow: n [
	highlightedRow = n ifFalse: [
		highlightedRow ifNotNil: [
			self invalidateLocalRect: (self drawBoundsForRow: highlightedRow) ].
		highlightedRow := n.
		highlightedRow ifNotNil: [
			self invalidateLocalRect: (self drawBoundsForRow: highlightedRow) ].
		].
]

{ #category : 'initialization' }
InnerListMorph >> initialize [
	super initialize.
	self color: `Color black`.
	font := Preferences at: #standardListFont.
	listItems := #().
	selectedRow := nil.
	highlightedRow := nil
]

{ #category : 'events-processing' }
InnerListMorph >> invalidDrop: aDropEvent [
	"A drag and drop operation was requested, but it was not accepted.
	Usually the user just meant to click, and the small drag was unintended."

	owner invalidDrop: aDropEvent.
]

{ #category : 'list access' }
InnerListMorph >> item: index [
	"return the index-th item, using the 'listItems' cache"
	(index between: 1 and: listItems size)
		ifFalse: [ "there should have been an update, but there wasn't!"  ^self getListItem: index].
	(listItems at: index) ifNil: [ 
		listItems at: index put: (self getListItem: index). ].
	^listItems at: index
]

{ #category : 'list management' }
InnerListMorph >> listChanged [
	"set newList to be the list of strings to display"
	listItems := Array new: self getListSize withAll: nil.
	selectedRow := nil.
	self adjustExtent
]

{ #category : 'geometry' }
InnerListMorph >> listItemHeight [
	^ font lineSpacing

]

{ #category : 'private' }
InnerListMorph >> noSelection [
	selectedRow := nil.
	highlightedRow := nil
]

{ #category : 'list management' }
InnerListMorph >> rowAtLocation: aPoint [
	"return the number of the row at aPoint"

	listItems isEmpty ifTrue: [ ^0 ].
	^aPoint y // font lineSpacing + 1 min: listItems size max: 1
]

{ #category : 'list management' }
InnerListMorph >> rowAtLocation: aPoint ifNone: aNoneBlock [

	| potentialRowNumber |
	
	potentialRowNumber := aPoint y // font lineSpacing + 1.
	
	^(listItems isInBounds: potentialRowNumber) 
		ifTrue: [ potentialRowNumber ]
		ifFalse: [ aNoneBlock value ]
]

{ #category : 'list management' }
InnerListMorph >> selectedRow [
	"return the currently selected row, or nil if none is selected"
	^selectedRow
]

{ #category : 'list management' }
InnerListMorph >> selectedRow: index [
	"select the index-th row.  if nil, remove the current selection"
	selectedRow = index ifFalse: [
		selectedRow ifNotNil: [
			self invalidateLocalRect: (self drawBoundsForRow: selectedRow) ].
		highlightedRow ifNotNil: [
			self invalidateLocalRect: (self drawBoundsForRow: highlightedRow) ].
		selectedRow := index.
		highlightedRow := nil.
		selectedRow ifNotNil: [
			self invalidateLocalRect: (self drawBoundsForRow: selectedRow) ].
		].
]

{ #category : 'drawing' }
InnerListMorph >> visibleRowIntervalForCanvas: aCanvas [
	"return the interval visible rows in aCanvas's clip rectangle"
	| tx clipRect rows |
	tx := aCanvas currentTransformation.
	clipRect := aCanvas clipRect.
	rows := clipRect corners collect: [ :corner |
		self rowAtLocation: (tx internalizePosition: corner) ].
	^rows min to: rows max.
]

{ #category : 'geometry' }
InnerListMorph >> widthToDisplayItem: item [
	^self font widthOfStringOrText: item
	
]
