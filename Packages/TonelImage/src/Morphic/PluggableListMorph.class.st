"
When a PluggableListMorph is in focus, type in a letter (or several letters quickly) to go to the next item that begins with that letter. Special keys (up, down, home, etc.) are also supported.

It is possible to set sibling lists for multiscrolling by calling >>#scrollSiblings: with a collection of siblings. The sibling lists are scrolled vertically together with the receiver. For an example of usage see CodePackageListWindow >>#buildMorphicWindow.

"
Class {
	#name : 'PluggableListMorph',
	#superclass : 'PluggableScrollPane',
	#instVars : [
		'list',
		'getListSelector',
		'getIndexSelector',
		'setIndexSelector',
		'autoDeselect',
		'lastKeystrokeTime',
		'lastKeystrokes',
		'doubleClickSelector',
		'limitLayoutHeight'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'instance creation' }
PluggableListMorph class >> withModel: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel
]

{ #category : 'dropping/grabbing' }
PluggableListMorph >> acceptDroppingMorph: aMorph event: dropEvent [
	| localPosition row dropSelectorArgument dropActionSelector args |
	localPosition := self internalizeFromWorld: dropEvent eventPosition.
	row := self rowAtLocation: localPosition ifNone: nil.
	dropSelectorArgument := aMorph
		valueOfProperty: #dropSelectorArgument
		ifAbsent: [self error: 'aMorph is missing dropSelectorArgument property'].
	dropActionSelector := self valueOfProperty: #dropActionSelector.
	args := dropActionSelector numArgs.
	args = 1 ifTrue: [ ^model perform: dropActionSelector with: dropSelectorArgument ].
	args = 2 ifTrue: [ ^model perform: dropActionSelector with: row with: dropSelectorArgument ].
	self error: 'dropActionSelector must be a 1- or 2-keyword symbol'
]

{ #category : 'dropping/grabbing' }
PluggableListMorph >> acceptDropsFrom: aMorph performing: dropActionSelector [
	(dropActionSelector numArgs between: 1 and: 2) ifFalse: [ self error: 'dropActionSelector must be a 1- or 2-keyword symbol' ].
	self setProperty: #allowsMorphDrop toValue: true.
	self setProperty: #acceptedDragSource toValue: aMorph.
	self setProperty: #dropActionSelector toValue: dropActionSelector
]

{ #category : 'menus' }
PluggableListMorph >> addCustomMenuItems:  aMenu hand: aHandMorph [
	"Add halo menu items to be handled by the invoking hand. The halo menu is invoked by clicking on the menu-handle of the receiver's halo."

	super addCustomMenuItems: aMenu hand: aHandMorph.
	aMenu addLine.
	aMenu add: 'copy list to clipboard' target: self action: #copyListToClipboard.
	aMenu add: 'copy selection to clipboard' target: self action: #copySelectionToClipboard
]

{ #category : 'dropping/grabbing' }
PluggableListMorph >> allowItemDrag: aBoolean [
	self setProperty: #allowsItemDrag toValue: aBoolean
]

{ #category : 'testing' }
PluggableListMorph >> allowsMultipleSelection [
	^false
]

{ #category : 'keyboard navigation' }
PluggableListMorph >> arrowKey: aKeyboardEvent [
	"Handle a keyboard navigation event. Answer nil if not handled."
	aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
	aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
	aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
	aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
	aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
	aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ].
	^ nil
]

{ #category : 'initialization' }
PluggableListMorph >> autoDeselect: trueOrFalse [
	"Enable/disable autoDeselect (see class comment)"
	autoDeselect := trueOrFalse.
]

{ #category : 'private' }
PluggableListMorph >> changeSelectionTo: nextSelection [

	nextSelection = self getCurrentSelectionIndex ifFalse: [
		| window |
		window := self owningWindow.
		(window isNil or: [ window okToChangeDueTo: self ]) ifTrue: [
			"No change if model is locked"
			"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
			scroller highlightedRow: nextSelection.
			"Update the model in next world cycle, so user gets the immediate feedback."
			UISupervisor whenUIinSafeState: [ self setSelectionIndex: nextSelection ].
		]
	]
]

{ #category : 'event handling testing' }
PluggableListMorph >> clickToFocusConsumesEvent [
	"Only relevant if #focusFollowsMouse == false.
	According to the preference #clickToFocusClickToSelect:
	- #always: Require one click to focus, then another to select.
	- #never: A single click sets focus and the sets or clears selection.
	- #ifSelection: We don't react to a focus setting click if we have a selection,
					so we don't lose selection just for focusing."

	| prefSymbol | 
	prefSymbol := Preferences at: #clickToFocusClickToSelect.
	prefSymbol == #never ifTrue: [ ^false ].
	prefSymbol == #always ifTrue: [ ^true ].
	"Assume #ifSelection"
	^self getCurrentSelectionIndex > 0
]

{ #category : 'events-processing' }
PluggableListMorph >> clickedAt: localEventPosition maybeDeselect: allowDeselecting [
	"The mouse came up within the list; take appropriate action"
	"No change if model is locked"

	| row |
	row := self rowAtLocation: localEventPosition.
	self owningWindow ifNotNil: [ :w |
		(w okToChangeDueTo: self) ifFalse: [ ^ self ]].
	(autoDeselect == false and:  [row = 0 ]) ifTrue: [ ^ self ].
	(autoDeselect and: [ row == self visualSelectionIndex ])
		ifTrue: [
			allowDeselecting ifTrue: [
				self setSelectionIndex: 0 ]]
		ifFalse: [ self setSelectionIndex: row ]
]

{ #category : 'menus' }
PluggableListMorph >> copyListToClipboard [
	"Copy my items to the clipboard as a multi-line string"

	Clipboard storeObject: (String streamContents: [ :strm |
		list
			do: [:ea | strm nextPutAll: ea asString]
			separatedBy: [ strm newLine ]]).
]

{ #category : 'menus' }
PluggableListMorph >> copySelectionToClipboard [
	"Copy my selected item to the clipboard as a string"

	self visualSelection 
		ifNotNil: [ :sel |
			Clipboard storeObject: sel asString ]
		ifNil: [
			self flash ]
]

{ #category : 'initialization' }
PluggableListMorph >> defaultColor [
	^Theme current defaultListColor
]

{ #category : 'keyboard navigation' }
PluggableListMorph >> deleteAction [
	"Handles deleting action, which happens when the user presses backspace or delete key within me"
	| deleteActionBlock |
	deleteActionBlock := self valueOfProperty: #deleteAction ifAbsent: nil.
	deleteActionBlock isNil
		ifTrue: [ self flash ]
		ifFalse: [ deleteActionBlock value ].
	^self
]

{ #category : 'geometry' }
PluggableListMorph >> desiredExtent [
	^ scroller desiredExtent + (padding + borderWidth + 1 * 2)
]

{ #category : 'geometry' }
PluggableListMorph >> desiredHeight [
	^ scroller desiredHeight + (padding y + borderWidth + 1 * 2)
]

{ #category : 'events' }
PluggableListMorph >> doubleClick: aMouseButtonEvent localPosition: localEventPosition [
	| index |
	doubleClickSelector ifNil: [
		^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index := self rowAtLocation: localEventPosition.
	index = 0 ifTrue: [
		^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index == self visualSelectionIndex
		ifFalse: [ self setSelectionIndex: index ].
	^ (self window ifNil: [model]) perform: doubleClickSelector
]

{ #category : 'initialization' }
PluggableListMorph >> doubleClickSelector: aSymbol [
	doubleClickSelector := aSymbol.
	self autoDeselect: false. 
]

{ #category : 'events' }
PluggableListMorph >> dragEvent: aMouseEvent localPosition: localEventPosition [
	| row listItem label draggingGuide |
	scroller highlightedRow: nil.
	row := self rowAtLocation: localEventPosition ifNone: [ ^self ].
	listItem := self getListItem: row.
	
	label := LabelMorph contents: listItem.
	draggingGuide := DraggingGuideMorph new.
	draggingGuide addMorph: label.
	draggingGuide morphExtent: label morphExtent.
	draggingGuide setProperty: #dragSource toValue: self.
	draggingGuide setProperty: #dropSelectorArgument toValue: listItem.
	draggingGuide setProperty: #dragLocalPos toValue: localEventPosition.
	
	aMouseEvent hand attachMorphBeside: draggingGuide
]

{ #category : 'drawing' }
PluggableListMorph >> drawOn: aCanvas [
	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: (Preferences at: #focusIndicatorWidth)
			color: Theme current focusIndicator ].
]

{ #category : 'events' }
PluggableListMorph >> focusKeyboardFor: aKeyboardEvent [
	(super focusKeyboardFor: aKeyboardEvent)
		ifTrue: [^ true].
	aKeyboardEvent isArrowLeft
		ifTrue: [ ^ aKeyboardEvent hand keyboardFocusPreviousWithinWindow ].
	aKeyboardEvent isArrowRight
		ifTrue: [ ^ aKeyboardEvent hand keyboardFocusNextWithinWindow ].
	^ false
]

{ #category : 'initialization' }
PluggableListMorph >> font [

	^ scroller font

]

{ #category : 'initialization' }
PluggableListMorph >> font: aFontOrNil [
	scroller font: aFontOrNil.

]

{ #category : 'geometry' }
PluggableListMorph >> fontPreferenceChanged [

	super fontPreferenceChanged.
	self font: (Preferences at: #standardListFont).
]

{ #category : 'model access' }
PluggableListMorph >> getCurrentSelectionIndex [
	"Answer the index of the current selection."

	getIndexSelector ifNil: [^0].
	^model perform: getIndexSelector
]

{ #category : 'model access' }
PluggableListMorph >> getList [
	"Answer the list to be displayed.  Caches the returned list in the 'list' ivar"
	getListSelector
		ifNil: [^ #()].
	list := model perform: getListSelector.
	list
		ifNil: [^ #()].
	list := list collect: [ :item | item displayStringOrText ].
	^ list
]

{ #category : 'model access' }
PluggableListMorph >> getListItem: index [
	"get the index-th item in the displayed list"
	^list at: index
]

{ #category : 'model access' }
PluggableListMorph >> getListSize [
	"return the current number of items in the displayed list"
	^list ifNotNil: [ list size ] ifNil: [ 0 ]
]

{ #category : 'initialization' }
PluggableListMorph >> initForKeystrokes [
	lastKeystrokeTime := 0.
	lastKeystrokes := ''.
]

{ #category : 'initialization' }
PluggableListMorph >> initialize [
	super initialize.
	scroller morphWidth: extent x.
	limitLayoutHeight := false.
]

{ #category : 'initialization' }
PluggableListMorph >> innerMorphClass [
	^InnerListMorph
]

{ #category : 'events-processing' }
PluggableListMorph >> invalidDrop: aDropEvent [
	"A drag and drop operation was requested, but it was not accepted.
	Usually the user just meant to click, and the small drag was unintended."

	| droppedMorph localEventPosition |
	droppedMorph := aDropEvent contents.
	((droppedMorph is: #DraggingGuideMorph) and: [
		(droppedMorph valueOfProperty: #dragSource) == self]) ifTrue: [
			localEventPosition := droppedMorph valueOfProperty: #dragLocalPos.
			self clickedAt: localEventPosition maybeDeselect: true.
			aDropEvent wasHandled: true ].
]

{ #category : 'testing' }
PluggableListMorph >> is: aSymbol [
	^ aSymbol == #PluggableListMorph or: [ aSymbol == #ListMorph or: [ super is: aSymbol ] ]
]

{ #category : 'model access' }
PluggableListMorph >> itemSelectedAmongMultiple: index [
	"return whether the index-th row is selected.  Always false in PluggableListMorph, but sometimes true in PluggableListMorphOfMany"
	^false
]

{ #category : 'events' }
PluggableListMorph >> keyStroke: aKeyboardEvent  [
	"Process keys"

	| aCharacter |
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].
	
	(self arrowKey: aKeyboardEvent) ifNotNil: [
		aKeyboardEvent wasHandled: true.
		^self ].

	"All the cases below consume the event."
	aKeyboardEvent wasHandled: true.
	aKeyboardEvent isReturnKey ifTrue: [
		doubleClickSelector notNil ifTrue: [(self window ifNil: [model]) perform: doubleClickSelector].
		^self].
	aKeyboardEvent isEsc ifTrue: [
		self mouseButton2Activity.
		^self ].
	aKeyboardEvent isDelete ifTrue: [
		self deleteAction.
		^self ].
	aKeyboardEvent isBackspace ifTrue: [
		self deleteAction.
		^self ].
	
	aCharacter := aKeyboardEvent keyCharacter.
	self keyboardSearch: aCharacter
]

{ #category : 'event handling' }
PluggableListMorph >> keyboardFocusChange: aBoolean [
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw focus feedback"
	drawKeyboardFocusIndicator
		ifTrue: [self redrawNeeded]
		ifFalse: [
			scroller selectedRow
				ifNotNil: [ :i |
					scroller selectedRow: nil; selectedRow: i ]]
]

{ #category : 'event handling' }
PluggableListMorph >> keyboardFocused [
	"Just given focus via keyboard.
	Autoselect first row."

	(scroller selectedRow = 0 and: [
		self allowsMultipleSelection not and: [ self getListSize > 0 ]])
			ifTrue: [ self navigateToTop ].
]

{ #category : 'model access' }
PluggableListMorph >> keyboardSearch: aChar [
	| oldSelection nextSelection max milliSeconds nextSelectionList nextSelectionText |
	nextSelection := oldSelection := self getCurrentSelectionIndex.
	max := self maximumSelection.
	milliSeconds := Time localMillisecondClock.
	milliSeconds - lastKeystrokeTime > 300 ifTrue: ["just use the one current character for selecting"
		lastKeystrokes := ''].
	lastKeystrokes := lastKeystrokes copyWith: aChar asLowercase.
	lastKeystrokeTime := milliSeconds.
	nextSelectionList := OrderedCollection newFrom: (list copyFrom: oldSelection + 1 to: max).
	nextSelectionList addAll: (list copyFrom: 1 to: oldSelection).
	"Get rid of blanks and style used in some lists"
	nextSelectionText := nextSelectionList detect: [:a | a asString withBlanksTrimmed asLowercase beginsWith: lastKeystrokes]
				ifNone: [^ self flash"match not found"].
	self owningWindow ifNotNil: [ :w |
		(w okToChangeDueTo: self) ifFalse: [^ self]].
	nextSelection := list findFirst: [:a | a == nextSelectionText].
	"No change if model is locked"
	oldSelection == nextSelection ifTrue: [^ self flash].
	self setSelectionIndex: nextSelection
]

{ #category : 'layout' }
PluggableListMorph >> limitLayoutHeight [
	limitLayoutHeight := true
]

{ #category : 'layout' }
PluggableListMorph >> maximumLayoutHeight [
	| answer |
	limitLayoutHeight ifTrue: [
		answer := self desiredHeight.
		self hIsScrollbarShowing ifTrue: [
			answer := answer + hScrollBar thickness].
		^answer ].
	^nil
]

{ #category : 'selection' }
PluggableListMorph >> maximumSelection [
	^ self getListSize
]

{ #category : 'selection' }
PluggableListMorph >> minimumSelection [
	^ 1
]

{ #category : 'initialization' }
PluggableListMorph >> model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel [

	self model: anObject.
	getListSelector := getListSel.
	getIndexSelector := getSelectionSel.
	setIndexSelector := setSelectionSel.
	autoDeselect := true.
	self
		updateList;
		initForKeystrokes

]

{ #category : 'events' }
PluggableListMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [

	| row |
	row := self 
		rowAtLocation: localEventPosition
		ifNone:  [
			super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
			^self ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update
	the view. Model update will be done on mouse button up, so this feedback will be visible before that."
	scroller highlightedRow: row.

	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil
		dragSel: ((self valueOfProperty: #allowsItemDrag) = true ifTrue: [ #dragEvent:localPosition: ] ifFalse: [ nil ])
]

{ #category : 'events' }
PluggableListMorph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [
	"The mouse came up within the list; take appropriate action"

	self clickedAt: localEventPosition maybeDeselect: aMouseButtonEvent mouseButton1Changed
]

{ #category : 'event handling' }
PluggableListMorph >> mouseButton2Activity [
	scroller highlightedRow: nil.
	super mouseButton2Activity
]

{ #category : 'events' }
PluggableListMorph >> mouseLeave: aMouseLeaveEvent [
	super mouseLeave: aMouseLeaveEvent.
	scroller highlightedRow: nil
]

{ #category : 'events' }
PluggableListMorph >> mouseMove: aMouseMoveEvent localPosition: localEventPosition [

	| eventPositionLocalToScroller |

	scroller highlightedRow: (
		(self viewableArea containsPoint: localEventPosition) ifTrue: [
			self rowAtLocation: localEventPosition ifNone: nil]).

	scroller ifNotNil: [
		eventPositionLocalToScroller := localEventPosition - scroller morphPosition.
		scroller  mouseMove: aMouseMoveEvent localPosition: eventPositionLocalToScroller ].
]

{ #category : 'keyboard navigation' }
PluggableListMorph >> navigateDown [
	"move down, wrapping to top if needed"
	| nextSelection |
	nextSelection := self getCurrentSelectionIndex + 1.
	nextSelection > self maximumSelection ifTrue: [ nextSelection := self minimumSelection ].
	self changeSelectionTo: nextSelection
]

{ #category : 'keyboard navigation' }
PluggableListMorph >> navigateOnePageDown [

	self changeSelectionTo: (self getCurrentSelectionIndex + self numSelectionsInView min: self maximumSelection)
]

{ #category : 'keyboard navigation' }
PluggableListMorph >> navigateOnePageUp [

	self changeSelectionTo: (self minimumSelection max: self getCurrentSelectionIndex - self numSelectionsInView)
]

{ #category : 'keyboard navigation' }
PluggableListMorph >> navigateToBottom [
	
	self changeSelectionTo: self maximumSelection
]

{ #category : 'keyboard navigation' }
PluggableListMorph >> navigateToTop [
	
	self changeSelectionTo: self minimumSelection
]

{ #category : 'keyboard navigation' }
PluggableListMorph >> navigateUp [
	"move up, wrapping to bottom if needed"
	| nextSelection |
	nextSelection := self getCurrentSelectionIndex - 1.
	nextSelection < self minimumSelection ifTrue: [ nextSelection := self maximumSelection ].
	self changeSelectionTo: nextSelection
]

{ #category : 'selection' }
PluggableListMorph >> numSelectionsInView [
	^ self viewableHeight // scroller listItemHeight
]

{ #category : 'selection' }
PluggableListMorph >> privateVisualSelection: item [
	"Called internally to set a new selection.
	Does not update model"

	self privateVisualSelectionIndex: (list indexOf: item)
]

{ #category : 'selection' }
PluggableListMorph >> privateVisualSelectionIndex: index [
	"Called internally to select the index-th item.
	Does not update model"
	| row |
	row := index ifNil: [ 0 ].
	row := row min: self getListSize.  "make sure we don't select past the end"
	scroller selectedRow: row.
	self scrollSelectionIntoView
]

{ #category : 'accessing - properties' }
PluggableListMorph >> rejectDrops [

	self removeProperty: #allowsMorphDrop.
	self removeProperty: #acceptedDragSource.
	self removeProperty: #dropActionSelector.
]

{ #category : 'accessing' }
PluggableListMorph >> rowAtLocation: aPoint [
	"Return the row at the given point or 0 if outside"

	^scroller rowAtLocation: (scroller internalize: aPoint)
]

{ #category : 'accessing' }
PluggableListMorph >> rowAtLocation: aPoint ifNone: aNoneBlock  [

	^scroller rowAtLocation: (scroller internalize: aPoint) ifNone: aNoneBlock
]

{ #category : 'scrolling' }
PluggableListMorph >> scrollBy: delta [
	"Scroll self and any siblings"
	super scrollBy: delta.
	self updateSiblingsScroller
]

{ #category : 'geometry' }
PluggableListMorph >> scrollDeltaHeight [
	"Return the increment in pixels which this pane should be scrolled."
	^ self font lineSpacing
]

{ #category : 'geometry' }
PluggableListMorph >> scrollDeltaWidth [
"A guess -- assume that the width of a char is approx 1/2 the height of the font"
	^ self scrollDeltaHeight // 2


]

{ #category : 'scrolling' }
PluggableListMorph >> scrollSelectionIntoView [
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
	self updateSiblingsScroller
]

{ #category : 'scrolling' }
PluggableListMorph >> scrollSiblings: aCollection [
	"Set a collection of scroll panes to be scroller together with the receiver."
	self setProperty: #scrollSiblings toValue: aCollection
]

{ #category : 'scrolling' }
PluggableListMorph >> scrollToShow: aRectangle [
	super scrollToShow: aRectangle.
	self updateSiblingsScroller
]

{ #category : 'selection' }
PluggableListMorph >> selection: item [
	"Public. Call to set selection.
	Usually, view is updated from model updates.
	If model updating fails (no model index setter defined) then just update visuals."

	self selectionIndex: (list indexOf: item)
]

{ #category : 'selection' }
PluggableListMorph >> selectionIndex: anInteger [
	"Public. Call to set selection.
	Usually, view is updated from model updates.
	If model updating fails (no model index setter defined) then just update visuals."

	(self setSelectionIndex: anInteger) ifFalse: [
		self privateVisualSelectionIndex: anInteger ]
]

{ #category : 'model access' }
PluggableListMorph >> setSelectionIndex: anInteger [
	"Change the model's selected item index to be anInteger."

	setIndexSelector ifNotNil: [
		model perform: setIndexSelector with: anInteger.
		self privateVisualSelectionIndex: self getCurrentSelectionIndex.
		^ true ].
	^ false
]

{ #category : 'geometry' }
PluggableListMorph >> someSubmorphPositionOrExtentChanged [
	"Some submorph changed position or extent.
	Scrollbars are likely affected."

	self updateScrollBarsBounds
]

{ #category : 'updating' }
PluggableListMorph >> update: aSymbol  [
	"Refer to the comment in View|update:."

	super update: aSymbol.
	aSymbol == getListSelector ifTrue: [
		self updateList.
		^ self].
	aSymbol == getIndexSelector ifTrue: [
		self privateVisualSelectionIndex: self getCurrentSelectionIndex ]
]

{ #category : 'updating' }
PluggableListMorph >> updateList [
	| index |
	"the list has changed -- update from the model"
	self getList.
	scroller listChanged.
	self setScrollDeltas.
	index := self getCurrentSelectionIndex.
	self privateVisualSelectionIndex: index.
	self redrawNeeded.
]

{ #category : 'scrolling' }
PluggableListMorph >> updateSiblingsScroller [
	"Update the siblings scroller to match the receiver."
	(self valueOfProperty: #scrollSiblings) ifNotNil: [:aCollection|
		| yOffset |
		yOffset := self scrollerOffset y.
		aCollection do: [:each| each scrollerOffset: each scrollerOffset x @ yOffset]]
]

{ #category : 'scrolling' }
PluggableListMorph >> vScrollBarValue: scrollValue [
	super vScrollBarValue: scrollValue.
	self updateSiblingsScroller
]

{ #category : 'updating' }
PluggableListMorph >> verifyContents [
	"Verify the contents of the receiver, reconstituting if necessary.  Called whenever window is reactivated, to react to possible structural changes."

	| newList oldList |
	oldList := list ifNil: [ #() ].
	newList := self getList.
	oldList = newList ifFalse: [
		self updateList ]
]

{ #category : 'selection' }
PluggableListMorph >> visualSelection  [
	self visualSelectionIndex = 0 ifTrue: [ ^nil ].
	list ifNotNil: [ ^list at: self visualSelectionIndex ].
	^ self getListItem: self visualSelectionIndex
]

{ #category : 'selection' }
PluggableListMorph >> visualSelectionIndex [
	"return the index we have currently selected, or 0 if none"
	^scroller selectedRow ifNil: [ 0 ]
]

{ #category : 'dropping/grabbing' }
PluggableListMorph >> wantsDroppedMorph: aMorph event: evt [

	^(aMorph is: #DraggingGuideMorph)
		and: [ (aMorph valueOfProperty: #dragSource) = (self valueOfProperty: #acceptedDragSource) ]
]
