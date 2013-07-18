'From Cuis 4.1 of 12 December 2012 [latest update: #1576] on 23 January 2013 at 7:39:52 pm'!
!classDefinition: #IndentingListItemMorph category: #'Morphic-Views for Models'!
StringMorph subclass: #IndentingListItemMorph
	instanceVariableNames: 'indentLevel isExpanded complexContents firstChild container nextSibling isSelected icon '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!IndentingListItemMorph commentStamp: '<historical>' prior: 0!
An IndentingListItemMorph is a StringMorph that draws itself with an optional toggle at its left, as part of the display of the SimpleHierarchicalListMorph.

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
Don't blame him if it's not perfect.  We wanted to get it out for people to play with.!


!Color methodsFor: 'access' stamp: 'KenD 1/23/2013 18:19'!
icon
	"Answer a swatch to display in a menu or browser"
	^self swatch! !

!Color methodsFor: 'access' stamp: 'KenD 1/23/2013 18:17'!
iconOrThumbnailOfSize: aNumberOrPoint 
	"Answer an appropiate form to represent the receiver"
	| form |
	form := Form extent: aNumberOrPoint asPoint asPoint depth: 32.
	form fillColor: self.
	^ form! !

!Color methodsFor: 'access' stamp: 'KenD 1/23/2013 18:18'!
swatch
	"Answer a swatch to display in a menu or browser"
	^self iconOrThumbnailOfSize: 16! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'KenD 1/23/2013 19:09'!
drawOn: aCanvas

	| tRect sRect colorToUse sLeft aForm centeringOffset |
	isSelected ifTrue: [
		aCanvas
			fillRectangle: (0@0 extent: extent)
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].
	
	complexContents hasContents ifTrue: [
		tRect _ self toggleRectangle.
		aForm _ isExpanded 
			ifTrue: [ container expandedForm ]
			ifFalse: [ container notExpandedForm ].
		centeringOffset _ ((tRect height - aForm extent y) / 2.0) rounded.
		aCanvas 
			image: aForm 
			at: (tRect topLeft translatedBy: 0 @ centeringOffset) ].

	icon isNil 
	ifFalse: [
		 aCanvas 
			image: icon
			at:  (tRect topLeft translatedBy:  icon width  @ centeringOffset).
		sLeft _ indentLevel * 12 + 16 + (icon width + 2).
	]
	ifTrue: [
		sLeft _ indentLevel * 12 + 16.
	].
	sRect _ sLeft@0 extent: extent - (sLeft@0).
	colorToUse _ complexContents preferredColor ifNil: [ color ].

	aCanvas
		drawString: contents asString
		in: sRect
		font: self fontToUse
		color: colorToUse! !

!IndentingListItemMorph methodsFor: 'initialization' stamp: 'KenD 1/23/2013 19:21'!
initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel

	container _ hostList.
	complexContents _ anObject.
	self initWithContents: anObject asString font: Preferences standardListFont emphasis: nil.
	indentLevel _ 0.
	isExpanded _ false.
 	nextSibling _ firstChild _ nil.
	priorMorph ifNotNil: [
		priorMorph nextSibling: self.
	].
	((anObject isKindOf: ObjectExplorerWrapper) 
		and: [anObject withoutListWrapper class includesSelector: #icon])		
	ifTrue: [icon := anObject withoutListWrapper icon].
	indentLevel _ newLevel.
! !

!IndentingListItemMorph methodsFor: 'initialization' stamp: 'KenD 1/23/2013 18:27'!
initialize

	super initialize.
	indentLevel _ 0.
	isExpanded _ false.
	isSelected _ false.
	icon _ nil! !

!classDefinition: #IndentingListItemMorph category: #'Morphic-Views for Models'!
StringMorph subclass: #IndentingListItemMorph
	instanceVariableNames: 'indentLevel isExpanded complexContents firstChild container nextSibling isSelected icon'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
