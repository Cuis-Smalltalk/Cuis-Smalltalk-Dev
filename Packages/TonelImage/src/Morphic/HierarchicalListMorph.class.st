"
Display a hierarchical list of items.  Each item should be wrapped with a ListItemWrapper.

For a simple example, look at submorphsExample.  For beefier examples, look at ObjectExplorer or FileList2.
"
Class {
	#name : 'HierarchicalListMorph',
	#superclass : 'PluggableScrollPane',
	#instVars : [
		'selectedMorph',
		'getListSelector',
		'autoDeselect',
		'autoExpand',
		'sortingSelector',
		'getSelectionSelector',
		'setSelectionSelector',
		'highlightedMorph',
		'doubleClickSelector'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'instance creation' }
HierarchicalListMorph class >> withModel: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel
]

{ #category : 'dropping/grabbing' }
HierarchicalListMorph >> acceptDroppingMorph: aMorph event: dropEvent [
	"This message is sent when a morph is dropped onto a morph that has agreed to accept the dropped morph by responding 'true' to the wantsDroppedMorph:event: message. This default implementation just adds the given morph to the receiver."
	| localPosition targetItem dropActionSelector args |
	localPosition := self internalizeFromWorld: dropEvent eventPosition.
	targetItem := ((self itemFromPoint: localPosition) ifNil: [ ^self ]) withoutListWrapper.
	dropActionSelector := self valueOfProperty: #dropActionSelector.
	args := dropActionSelector numArgs.
	args = 1 ifTrue: [ ^model perform: dropActionSelector with: targetItem].
	args = 2 ifTrue: [ | dropSelectorArgument |
		dropSelectorArgument := aMorph
			valueOfProperty: #dropSelectorArgument
			ifAbsent: [self error: 'aMorph is missing dropSelectorArgument property'].
		^model perform: dropActionSelector with: targetItem with: dropSelectorArgument ].
	self error: 'dropActionSelector must be a 1- or 2-keyword symbol'
]

{ #category : 'dropping/grabbing' }
HierarchicalListMorph >> acceptDropsFrom: aMorph performing: dropActionSelector [
	(dropActionSelector numArgs between: 1 and: 2) ifFalse: [ self error: 'dropActionSelector must be a 1- or 2-keyword symbol' ].
	self setProperty: #allowsMorphDrop toValue: true.
	self setProperty: #acceptedDragSource toValue: aMorph.
	self setProperty: #dropActionSelector toValue: dropActionSelector
]

{ #category : 'private' }
HierarchicalListMorph >> addMorphsTo: morphList from: aCollection allowSorting: sortBoolean withExpandedItems: expandedItems atLevel: newIndent [

	| priorMorph newCollection firstAddition |
	priorMorph := nil.
	newCollection := (sortBoolean and: [sortingSelector notNil]) ifTrue: [
		aCollection asOrderedCollection sort: [ :a :b | 
			(a perform: sortingSelector) <= (b perform: sortingSelector)]
	] ifFalse: [
		aCollection
	].
	firstAddition := nil.
	newCollection do: [:item | 
		priorMorph := self indentingItemClass basicNew 
			initWithContents: item 
			prior: priorMorph 
			forList: self
			indentLevel: newIndent.
		firstAddition ifNil: [firstAddition := priorMorph].
		morphList add: priorMorph.
		((item hasEquivalentIn: expandedItems) or: [priorMorph isExpanded]) ifTrue: [
			priorMorph beExpanded.
			priorMorph 
				addChildrenForList: self 
				addingTo: morphList
				withExpandedItems: expandedItems.
		].
	].
	^firstAddition
	

]

{ #category : 'private' }
HierarchicalListMorph >> addSubmorphsAfter: parentMorph fromCollection: aCollection allowSorting: sortBoolean [

	| priorMorph morphList newCollection |
	priorMorph := nil.
	newCollection := (sortBoolean and: [sortingSelector notNil]) ifTrue: [
		aCollection asOrderedCollection sort: [ :a :b | 
			(a perform: sortingSelector) <= (b perform: sortingSelector)]
	] ifFalse: [
		aCollection
	].
	morphList := OrderedCollection new.
	newCollection do: [:item | 
		priorMorph := self indentingItemClass basicNew 
			initWithContents: item 
			prior: priorMorph 
			forList: self
			indentLevel: parentMorph indentLevel + 1.
		morphList add: priorMorph.
	].
	scroller addAllMorphs: morphList after: parentMorph.
	^morphList
	

]

{ #category : 'dropping/grabbing' }
HierarchicalListMorph >> allowItemDrag: aBoolean [
	self setProperty: #allowsItemDrag toValue: aBoolean
]

{ #category : 'keyboard navigation' }
HierarchicalListMorph >> arrowKey: aKeyboardEvent [

	"Handle a keyboard navigation event. Answer nil if not handled."
	aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
	aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
	aKeyboardEvent isArrowLeft ifTrue: [ ^ self navigateLeft ].
	aKeyboardEvent isArrowRight ifTrue: [ ^ self navigateRight ].
	aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
	aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
	aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
	aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ].
	^ nil
]

{ #category : 'initialization' }
HierarchicalListMorph >> autoDeselect: trueOrFalse [

	autoDeselect := trueOrFalse
]

{ #category : 'accessing' }
HierarchicalListMorph >> autoExpand [

	^autoExpand ifNil: [false]
]

{ #category : 'initialization' }
HierarchicalListMorph >> autoExpand: trueOrFalse [

	autoExpand := trueOrFalse
]

{ #category : 'private' }
HierarchicalListMorph >> changeSelectionTo: nextSelection [

	nextSelection = self visualSelectionIndex ifFalse: [
		"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
		self highlightedMorph: (self listMorphAt: nextSelection).
		"Update the model in next world cycle, so user gets the immediate feedback."
		UISupervisor whenUIinSafeState: [ self setSelectionIndex: nextSelection ]].
]

{ #category : 'event handling testing' }
HierarchicalListMorph >> clickToFocusConsumesEvent [
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
	^selectedMorph notNil
]

{ #category : 'initialization' }
HierarchicalListMorph >> currentlyExpanded [

	^(scroller submorphs select: [ :each | each isExpanded]) collect: [ :each |
		each complexContents
	].
	
]

{ #category : 'initialization' }
HierarchicalListMorph >> defaultColor [
	^Theme current defaultListColor
]

{ #category : 'events' }
HierarchicalListMorph >> doubleClick: aMouseButtonEvent localPosition: localEventPosition [
	doubleClickSelector ifNotNil: [
		^ (self window ifNil: [model]) perform: doubleClickSelector].
	selectedMorph ifNotNil: [
		self toggleExpandedState: selectedMorph.
		self scrollSelectionIntoView ]
]

{ #category : 'initialization' }
HierarchicalListMorph >> doubleClickSelector: aSymbol [
	doubleClickSelector := aSymbol.
	self autoDeselect: false. 
]

{ #category : 'events' }
HierarchicalListMorph >> dragEvent: aMouseEvent localPosition: localEventPosition [
	| draggedItem draggingGuide label |
	draggedItem := ((self itemFromPoint: localEventPosition) ifNil: [ ^self ]) withoutListWrapper.
	label := LabelMorph contents: draggedItem asString.
	draggingGuide := DraggingGuideMorph new.
	draggingGuide addMorph: label.
	draggingGuide morphExtent: label morphExtent.
	draggingGuide setProperty: #dragSource toValue: self.
	draggingGuide setProperty: #dropSelectorArgument toValue: draggedItem.	
	aMouseEvent hand attachMorphBeside: draggingGuide
]

{ #category : 'drawing' }
HierarchicalListMorph >> drawOn: aCanvas [

	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle 
			borderWidth: (Preferences at: #focusIndicatorWidth)
			color: Theme current focusIndicator ]
]

{ #category : 'commands' }
HierarchicalListMorph >> expandAll [

	^self expandAllAsPer: [:each | true]
]

{ #category : 'commands' }
HierarchicalListMorph >> expandAllAsPer: aBlock [

	scroller submorphs isEmpty ifTrue: [^self].
	self roots do: [:each| each beFullyExpandedAsPer: aBlock].
	scroller adjustExtent.
	self setScrollDeltas
]

{ #category : 'events' }
HierarchicalListMorph >> focusKeyboardFor: aKeyboardEvent [
	(super focusKeyboardFor: aKeyboardEvent)
		ifTrue: [^ true].
	aKeyboardEvent shiftPressed ifTrue: [
		aKeyboardEvent isArrowLeft
			ifTrue: [ ^ aKeyboardEvent hand keyboardFocusPreviousWithinWindow ].
		aKeyboardEvent isArrowRight
			ifTrue: [ ^ aKeyboardEvent hand keyboardFocusNextWithinWindow ]].
	^ false
]

{ #category : 'selection' }
HierarchicalListMorph >> getCurrentSelectionItem [

	^model perform: (getSelectionSelector ifNil: [^nil])
	
]

{ #category : 'model access' }
HierarchicalListMorph >> getList  [
	"Answer the list to be displayed."

	^(model perform: (getListSelector ifNil: [^#()])) ifNil: [#()]


]

{ #category : 'selection' }
HierarchicalListMorph >> highlightedMorph: aMorph [

	highlightedMorph ifNotNil: [
		highlightedMorph isHighlighted: false ].
	highlightedMorph := aMorph.
	highlightedMorph ifNotNil: [
		highlightedMorph isHighlighted: true ]
]

{ #category : 'initialization' }
HierarchicalListMorph >> indentingItemClass [
	
	^IndentingListItemMorph
]

{ #category : 'private' }
HierarchicalListMorph >> indexForItem: item [
	| i |
	item ifNil: [
		^ 0 ].
	i := scroller submorphs findFirst: [ :m | m complexContents == item ].
	i > 0 ifTrue: [
		^ i ].
	i := scroller submorphs findFirst: [ :m | m withoutListWrapper = item withoutListWrapper ].
	^ i
]

{ #category : 'initialization' }
HierarchicalListMorph >> innerMorphClass [
	^InnerHierarchicalListMorph
]

{ #category : 'private' }
HierarchicalListMorph >> insertNewMorphs: morphList [

	scroller addAllMorphs: morphList.
	scroller adjustExtent.
	self setScrollDeltas.
	self privateVisualSelection: self getCurrentSelectionItem
]

{ #category : 'testing' }
HierarchicalListMorph >> is: aSymbol [
	^ aSymbol == #ListMorph or: [ super is: aSymbol ]
]

{ #category : 'event handling' }
HierarchicalListMorph >> itemFromPoint: aPoint [
	"Return the list element (morph) at the given point or nil if outside"
	^scroller itemFromPoint: (scroller internalize: aPoint)
]

{ #category : 'events' }
HierarchicalListMorph >> keyStroke: aKeyboardEvent  [
	"Process potential command keys"

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	(self arrowKey: aKeyboardEvent) ifNotNil: [
		aKeyboardEvent wasHandled: true.
		^self ].

	aKeyboardEvent isEsc ifTrue: [
		" escape key"
		self mouseButton2Activity.
		aKeyboardEvent wasHandled: true ].
]

{ #category : 'event handling' }
HierarchicalListMorph >> keyboardFocusChange: aBoolean [
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw focus feedback"


	drawKeyboardFocusIndicator
		ifTrue: [self redrawNeeded]
		ifFalse: [
			selectedMorph ifNotNil: [ selectedMorph redrawNeeded ]]
]

{ #category : 'initialization' }
HierarchicalListMorph >> list: aCollection [

	| wereExpanded morphList |
	wereExpanded := self currentlyExpanded.
	scroller removeAllMorphs.
	(aCollection isNil or: [aCollection isEmpty]) ifTrue: [^ self selectedMorph: nil].
	morphList := OrderedCollection new.
	self 
		addMorphsTo: morphList
		from: aCollection 
		allowSorting: false
		withExpandedItems: wereExpanded
		atLevel: 0.
	self insertNewMorphs: morphList.
]

{ #category : 'initialization' }
HierarchicalListMorph >> listItemHeight [
	"This should be cleaned up.  The list should get spaced by this parameter."
	^ 12
]

{ #category : 'selection' }
HierarchicalListMorph >> listMorphAt:  idx [
	"Called internally to select the index-th item."
	| theMorph index |
	idx ifNil: [^ self].
	index := idx min: scroller submorphs size max: 0.
	theMorph := index = 0 ifTrue: [ nil ] ifFalse: [ scroller submorphs at: index ].
	^theMorph
]

{ #category : 'selection' }
HierarchicalListMorph >> maximumSelection [

	^ scroller submorphs size

]

{ #category : 'selection' }
HierarchicalListMorph >> minimumSelection [
	^ 1
]

{ #category : 'initialization' }
HierarchicalListMorph >> model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel [

	self model: anObject.
	getListSelector := getListSel.
	getSelectionSelector := getSelectionSel.
	setSelectionSelector := setSelectionSel.
	autoDeselect := true.
	self list: self getList.
]

{ #category : 'events' }
HierarchicalListMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [

	| itemMorph |
	itemMorph := self itemFromPoint: localEventPosition.
	itemMorph ifNil: [
		super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].
	self highlightedMorph: itemMorph.
	(itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)))
		ifTrue: [
			self toggleExpandedState: itemMorph event: aMouseButtonEvent.
			^self ]. 
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil
		dragSel: ((self valueOfProperty: #allowsItemDrag) = true ifTrue: [ #dragEvent:localPosition: ])
]

{ #category : 'events' }
HierarchicalListMorph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [

	highlightedMorph ifNil: [ ^self ].
	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	(autoDeselect and: [ highlightedMorph == selectedMorph ])
		ifTrue: [ self setSelectedMorph: nil ]
		ifFalse: [ self setSelectedMorph: highlightedMorph ].
	self highlightedMorph: nil
]

{ #category : 'event handling' }
HierarchicalListMorph >> mouseButton2Activity [
	self highlightedMorph: nil.
	super mouseButton2Activity
]

{ #category : 'keyboard navigation' }
HierarchicalListMorph >> navigateDown [
	"move down, wrapping to top if needed"
	| nextSelection |
	nextSelection := self visualSelectionIndex + 1.
	nextSelection > self maximumSelection ifTrue: [ nextSelection := self minimumSelection ].
	self changeSelectionTo: nextSelection
]

{ #category : 'keyboard navigation' }
HierarchicalListMorph >> navigateLeft [
	| oldSelection nextSelection |
	oldSelection := self visualSelectionIndex.
	nextSelection := oldSelection.
	selectedMorph ifNotNil: [
		selectedMorph isExpanded
			ifTrue: [
				self toggleExpandedState: selectedMorph.
				self scrollSelectionIntoView ]
			ifFalse: [
				oldSelection > self minimumSelection ifTrue: [
					nextSelection := (oldSelection-1 to: 1 by: -1) 
						detect: [ :i | ( scroller submorphs at: i) indentLevel < selectedMorph indentLevel ] 
						ifNone: [ oldSelection ]].
				]].
	self changeSelectionTo: nextSelection
]

{ #category : 'keyboard navigation' }
HierarchicalListMorph >> navigateOnePageDown [

	self changeSelectionTo: (self visualSelectionIndex + self numSelectionsInView min: self maximumSelection)
]

{ #category : 'keyboard navigation' }
HierarchicalListMorph >> navigateOnePageUp [

	self changeSelectionTo: (self minimumSelection max: self visualSelectionIndex - self numSelectionsInView)
]

{ #category : 'keyboard navigation' }
HierarchicalListMorph >> navigateRight [
	| oldSelection nextSelection |
	oldSelection := self visualSelectionIndex.
	nextSelection := oldSelection.
	selectedMorph ifNotNil: [
			(selectedMorph canExpand and: [ selectedMorph isExpanded not ])
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					self scrollSelectionIntoView ]
				ifFalse: [ nextSelection := oldSelection + 1 ]].
	self changeSelectionTo: nextSelection
]

{ #category : 'keyboard navigation' }
HierarchicalListMorph >> navigateToBottom [
	
	self changeSelectionTo: self maximumSelection
]

{ #category : 'keyboard navigation' }
HierarchicalListMorph >> navigateToTop [
	
	self changeSelectionTo: self minimumSelection
]

{ #category : 'keyboard navigation' }
HierarchicalListMorph >> navigateUp [
	"move up, wrapping to bottom if needed"
	| nextSelection |
	nextSelection := self visualSelectionIndex - 1.
	nextSelection < self minimumSelection ifTrue: [ nextSelection := self maximumSelection ].
	self changeSelectionTo: nextSelection
]

{ #category : 'private' }
HierarchicalListMorph >> noteRemovalOfAll: aCollection [

	scroller removeAllMorphsIn: aCollection.
	(aCollection includes: selectedMorph) ifTrue: [ self setSelectedMorph: nil ]
]

{ #category : 'selection' }
HierarchicalListMorph >> numSelectionsInView [
	^ self viewableHeight // self listItemHeight
]

{ #category : 'selection' }
HierarchicalListMorph >> privateVisualSelection: item [
	"Called internally to set a new selection.
	Does not update model"
	| index |
	item isNil ifTrue: [^ self privateVisualSelectionIndex: 0].
	index := self indexForItem: item.
	index > 0 ifFalse:
		[self expandAllAsPer: [:each| each complexContents isAncestorOf: item withoutListWrapper].
		index := self indexForItem: item].
	self privateVisualSelectionIndex: index
]

{ #category : 'selection' }
HierarchicalListMorph >> privateVisualSelectionIndex: idx [
	"Called internally to select the index-th item.
	Does not update model"
	self selectedMorph: (self listMorphAt: idx).
	self scrollSelectionIntoView
]

{ #category : 'accessing' }
HierarchicalListMorph >> rejectDrops [

	self removeProperty: #allowsMorphDrop.
	self removeProperty: #acceptedDragSource.
	self removeProperty: #dropActionSelector.
]

{ #category : 'accessing' }
HierarchicalListMorph >> roots [
	"Answer the root items. These are instances of IndentingListItemMorph and submorphs of the scroller."
	| list |
	list := self getList collect: [:each| each withoutListWrapper].
	^ scroller submorphs select: [:each| list includes: each withoutListWrapper]
]

{ #category : 'geometry' }
HierarchicalListMorph >> scrollDeltaHeight [
	scroller hasSubmorphs ifFalse: [ ^1].
	^ scroller firstSubmorph morphHeight
]

{ #category : 'geometry' }
HierarchicalListMorph >> scrollDeltaWidth [
"A guess -- assume that the width of a char is approx 1/2 the height of the font"
	^ self scrollDeltaHeight // 2



]

{ #category : 'selection' }
HierarchicalListMorph >> scrollSelectionIntoView [

	| r |
	selectedMorph ifNotNil: [
		r := scroller externalizeBoundingRectOf: selectedMorph fullBoundsInOwner.
		self scrollToShow: r ]
]

{ #category : 'accessing' }
HierarchicalListMorph >> selectedMorph [
	^selectedMorph
]

{ #category : 'selection' }
HierarchicalListMorph >> selectedMorph: aMorph [

	selectedMorph ifNotNil: [
		selectedMorph isSelected: false ].
	selectedMorph := aMorph.
	selectedMorph ifNotNil: [
		selectedMorph isSelected: true ]
]

{ #category : 'selection' }
HierarchicalListMorph >> selection: item [
	"Called to set a new selection.
	Updates both model and view."
	"Assumes scroller submorphs is exactly our list.
	Note: MAY NOT work right if list includes repeated items"

	self selectionIndex: (self indexForItem: item)
]

{ #category : 'selection' }
HierarchicalListMorph >> selectionIndex: anInteger [
	"Public. Call to set selection.
	Usually, view is updated from model updates.
	If model updating fails (no model index setter defined) then just update visuals."

	(self setSelectionIndex: anInteger) ifFalse: [
		self privateVisualSelectionIndex: anInteger ]
]

{ #category : 'selection' }
HierarchicalListMorph >> setSelectedMorph: aMorph [
	setSelectionSelector ifNil: [ ^ false ].
	model 
		perform: setSelectionSelector
		with: aMorph complexContents	."leave last wrapper in place"
	^ true

 
]

{ #category : 'keyboard navigation' }
HierarchicalListMorph >> setSelectionIndex: idx [
	"Change the model's selected item index to be anInteger."

	^self setSelectedMorph: (self listMorphAt: idx)
]

{ #category : 'geometry' }
HierarchicalListMorph >> someSubmorphPositionOrExtentChanged [
	"Some submorph changed position or extent.
	Scrollbars are likely affected."

	self updateScrollBarsBounds
]

{ #category : 'keyboard navigation' }
HierarchicalListMorph >> toggleExpandedState: aMorph [
	aMorph toggleExpandedState.
	scroller adjustExtent.
	self setScrollDeltas
]

{ #category : 'commands' }
HierarchicalListMorph >> toggleExpandedState: aMorph event: event [

	"self setSelectedMorph: aMorph."
	((self autoExpand or: [event shiftPressed]) and: [aMorph isExpanded not])
		ifTrue: [aMorph beFullyExpanded]
		ifFalse: [aMorph toggleExpandedState].
	scroller adjustExtent.
	self setScrollDeltas
]

{ #category : 'updating' }
HierarchicalListMorph >> update: aSymbol [
	super update: aSymbol.
	aSymbol == getSelectionSelector 
		ifTrue: [
			self privateVisualSelection: self getCurrentSelectionItem.
			^self ].
	aSymbol == getListSelector 
		ifTrue: [
			self list: self getList.
			^self ].

	"Indeed not pretty"
	( aSymbol notEmpty and: [aSymbol first == #openPath]) 
		ifTrue: [
			^(scroller submorphs at: 1 ifAbsent: [^self]) 
				openPath: aSymbol allButFirst adaptor: #asString compare: #=]
]

{ #category : 'selection' }
HierarchicalListMorph >> visualSelectionIndex [
	^scroller submorphs indexOf: selectedMorph
]

{ #category : 'dropping/grabbing' }
HierarchicalListMorph >> wantsDroppedMorph: aMorph event: evt [
	"Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self."

	^(aMorph is: #DraggingGuideMorph)
		and: [ (aMorph valueOfProperty: #dragSource) = (self valueOfProperty: #acceptedDragSource) ]
]
