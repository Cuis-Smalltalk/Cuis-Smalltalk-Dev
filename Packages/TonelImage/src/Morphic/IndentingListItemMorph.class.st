"
An IndentingListItemMorph is a LabelMorph that draws itself with an optional toggle at its left, as part of the display of the SimpleHierarchicalListMorph.

It will also display lines around the toggle if the #showLinesInHierarchyViews Preference is set.

Instance variables:

indentLevel <SmallInteger> 	the indent level, from 0 at the root and increasing by 1 at each level of the hierarchy.

isExpanded <Boolean>		true if this item is expanded (showing its children)

complexContents <ListItemWrapper>	an adapter wrapping my represented item that can answer its children, etc.
	
firstChild <IndentingListItemMorph|nil>	my first child, or nil if none
	
container <SimpleHierarchicalListMorph>	my container
	
nextSibling <IndentingListItemMorph|nil>	the next item in the linked list of siblings, or nil if none.

icon 										a 16 x 16 form or nil

Contributed by Bob Arning as part of the ObjectExplorer package.
Don't blame him if it's not perfect.  We wanted to get it out for people to play with.
"
Class {
	#name : 'IndentingListItemMorph',
	#superclass : 'LabelMorph',
	#instVars : [
		'indentLevel',
		'isExpanded',
		'complexContents',
		'firstChild',
		'container',
		'nextSibling',
		'isSelected',
		'icon',
		'isHighlighted'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
IndentingListItemMorph class >> includeInNewMorphMenu [
	"Not to be instantiated from the menu"
	^ false
]

{ #category : 'private-container protocol' }
IndentingListItemMorph >> addChildrenForList: hostList addingTo: morphList withExpandedItems: expandedItems [

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode delete].
	].
	firstChild := nil.
	complexContents hasContents ifFalse: [^self].
	firstChild := hostList 
		addMorphsTo: morphList
		from: complexContents contents 
		allowSorting: true
		withExpandedItems: expandedItems
		atLevel: indentLevel + 1.
	
]

{ #category : 'accessing' }
IndentingListItemMorph >> balloonText [

	^complexContents balloonText ifNil: [super balloonText]
]

{ #category : 'accessing' }
IndentingListItemMorph >> beExpanded [

	isExpanded := true
]

{ #category : 'accessing' }
IndentingListItemMorph >> beFullyExpanded [

	self beFullyExpandedAsPer: [:each | true]
]

{ #category : 'accessing' }
IndentingListItemMorph >> beFullyExpandedAsPer: aBlock [

	| allChildren |
	allChildren := OrderedCollection new.
	self recursiveAddTo: allChildren.
	allChildren do:
		[:each |
			| shouldExpandEach |
			shouldExpandEach := each canExpand and: [aBlock value: each].
			shouldExpandEach ~= each isExpanded ifTrue:
				[
					each toggleExpandedState.
					each beFullyExpandedAsPer: aBlock
				]
		]
]

{ #category : 'accessing' }
IndentingListItemMorph >> canExpand [

	^complexContents hasContents
]

{ #category : 'private-container protocol' }
IndentingListItemMorph >> complexContents [

	^complexContents
]

{ #category : 'geometry' }
IndentingListItemMorph >> desiredWidth [

	^(indentLevel + 2) * self indentWidth + (self font widthOfStringOrText: self contents)
]

{ #category : 'drawing' }
IndentingListItemMorph >> drawOn: aCanvas [

	| x colorToUse centeringOffset |
	isHighlighted ifTrue: [
		aCanvas
			fillRectangle: self localBounds
			color: ((Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) alpha: 0.3) ].

	isSelected ifTrue: [
		aCanvas
			fillRectangle: self localBounds
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].

	x := indentLevel * self indentWidth.

	complexContents hasContents ifTrue: [
		isExpanded
			ifTrue: [ aCanvas drawExpandedAt: x@(extent y//2) height: self indentWidth]
			ifFalse: [ aCanvas drawNotExpandedAt: x@(extent y//2) height: self indentWidth ]].
	x := self indentWidth * 3 // 2 + x.

	icon isNil ifFalse: [
		centeringOffset := ((extent y - icon height) / 2.0) roundedHAFZ.
		 aCanvas 
			image: icon
			at:  (x @ centeringOffset).
		x := self indentWidth // 2 + icon width + x. ].

	colorToUse := complexContents preferredColor ifNil: [ color ].
	aCanvas
		drawString: contents asString
		at: x@0
		font: self fontToUse
		color: colorToUse
]

{ #category : 'accessing' }
IndentingListItemMorph >> firstChild [

	^firstChild
]

{ #category : 'geometry' }
IndentingListItemMorph >> fontPreferenceChanged [

	super fontPreferenceChanged.
	self font: (Preferences at: #standardListFont).
]

{ #category : 'event handling testing' }
IndentingListItemMorph >> handlesKeyboardFocus [
	"Return true if the receiver wishes to handle keyboard focus.
	Answer false because it is handled by our owner, a subinstance of HierarchicalListMorph"

	^false
]

{ #category : 'event handling testing' }
IndentingListItemMorph >> handlesMouseOver: event [
	"implements #mouseEnter: and/or #mouseLeave:"
	^ true
]

{ #category : 'private' }
IndentingListItemMorph >> hasToggle [
	^ complexContents hasContents
]

{ #category : 'mouse events' }
IndentingListItemMorph >> inToggleArea: aPoint [

	^self toggleRectangle containsPoint: aPoint
]

{ #category : 'accessing' }
IndentingListItemMorph >> indentLevel [

	^indentLevel
]

{ #category : 'geometry' }
IndentingListItemMorph >> indentWidth [
	^FontFamily defaultPointSize
]

{ #category : 'initialization' }
IndentingListItemMorph >> initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel [

	| o |
	container := hostList.
	complexContents := anObject.
	self initWithContents: anObject asString font: (Preferences at: #standardListFont) emphasis: nil.
	indentLevel := 0.
	isExpanded := false.
 	nextSibling := firstChild := nil.
	priorMorph ifNotNil: [
		priorMorph nextSibling: self.
	].
	o := anObject withoutListWrapper.
	icon := o ifNotNil: [ (o respondsTo: #icon) ifTrue: [ o icon ] ].
	icon isSymbol ifTrue: [ icon := Theme current perform: icon ].
	indentLevel := newLevel.

]

{ #category : 'initialization' }
IndentingListItemMorph >> initialize [

	super initialize.
	indentLevel := 0.
	isExpanded := false.
	isSelected := false.
	isHighlighted := false.
	icon := nil
]

{ #category : 'accessing' }
IndentingListItemMorph >> isExpanded [

	^isExpanded
]

{ #category : 'accessing' }
IndentingListItemMorph >> isFirstItem [
	^owner submorphs first == self
]

{ #category : 'accessing' }
IndentingListItemMorph >> isHighlighted: aBoolean [

	isHighlighted := aBoolean.
	self redrawNeeded
]

{ #category : 'accessing' }
IndentingListItemMorph >> isSelected: aBoolean [

	isSelected := aBoolean.
	isHighlighted := false.
	self redrawNeeded
]

{ #category : 'accessing' }
IndentingListItemMorph >> isSoleItem [
	^self isFirstItem and: [ owner submorphs size = 1 ]
]

{ #category : 'events' }
IndentingListItemMorph >> mouseEnter: aMouseEnterEvent [
	isHighlighted := true.
	self redrawNeeded.
	super mouseEnter: aMouseEnterEvent
]

{ #category : 'events' }
IndentingListItemMorph >> mouseLeave: aMouseLeaveEvent [
	isHighlighted := false.
	self redrawNeeded.
	super mouseLeave: aMouseLeaveEvent.
]

{ #category : 'accessing' }
IndentingListItemMorph >> nextSibling [

	^nextSibling
]

{ #category : 'accessing' }
IndentingListItemMorph >> nextSibling: anotherMorph [

	nextSibling := anotherMorph
]

{ #category : 'private-container protocol' }
IndentingListItemMorph >> openPath: anArray adaptor: aSymbol compare: comparison [

	anArray isEmpty ifTrue: [ ^container setSelectedMorph: nil ].
	self withSiblingsDo: [ :each | 
		(anArray first isNil or: [
			(each complexContents perform: aSymbol)
				perform: comparison
					with: (anArray first perform: aSymbol) ]) ifTrue: [
			each isExpanded ifFalse: [
				each toggleExpandedState.
				owner adjustExtent.
				container setScrollDeltas ].
			each redrawNeeded.
			anArray size = 1 ifTrue: [
				^container setSelectedMorph: each ].
			each firstChild ifNil: [^container setSelectedMorph: nil ].
			^each firstChild openPath: anArray allButFirst adaptor: aSymbol compare: comparison ]].
	^container setSelectedMorph: nil
]

{ #category : 'private-container protocol' }
IndentingListItemMorph >> recursiveAddTo: aCollection [

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode recursiveAddTo: aCollection].
	].
	aCollection add: self
	
]

{ #category : 'private-container protocol' }
IndentingListItemMorph >> toggleExpandedState [
	| newChildren toDelete c |
	isExpanded := isExpanded not.
	toDelete := OrderedCollection new.
	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode |
			aNode recursiveAddTo: toDelete ]].
	container noteRemovalOfAll: toDelete.
	(isExpanded and: [ complexContents hasContents ]) ifFalse: [
		firstChild := nil.
"	 	nextSibling := firstChild := nil."
		^ self redrawNeeded ].
	(c := complexContents contents) isEmpty ifTrue: [ ^ self redrawNeeded ].
	newChildren := container
		addSubmorphsAfter: self
		fromCollection: c
		allowSorting: true.
	firstChild := newChildren first
]

{ #category : 'private' }
IndentingListItemMorph >> toggleRectangle [

	^(indentLevel * self indentWidth @ 0) extent: self indentWidth@extent y
]

{ #category : 'private' }
IndentingListItemMorph >> withSiblingsDo: aBlock [

	| node |
	node := self.
	[node isNil] whileFalse: [
		aBlock value: node.
		node := node nextSibling
	].
]

{ #category : 'converting' }
IndentingListItemMorph >> withoutListWrapper [

	^complexContents withoutListWrapper
]
