'From Cuis 4.0 of 21 April 2012 [latest update: #1423] on 1 September 2012 at 11:44:04 pm'!
!classDefinition: #HierarchicalListMorph category: #'Morphic-Views for Models'!
PluggableScrollPane subclass: #HierarchicalListMorph
	instanceVariableNames: 'selectedMorph getListSelector keystrokeActionSelector autoDeselect columns sortingSelector getSelectionSelector setSelectionSelector lineColor menuGetter mainView '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
!classDefinition: #IndentingListItemMorph category: #'Morphic-Views for Models'!
StringMorph subclass: #IndentingListItemMorph
	instanceVariableNames: 'indentLevel isExpanded complexContents firstChild container nextSibling isSelected '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/1/2012 23:07'!
zzimage: aForm at: aPoint
	"Draw a translucent image using the best available way of representing translucency."

	| p |
	p _ currentTransformation transform: aPoint.
	self image: aForm
		at: p rounded
		sourceRect: aForm boundingBox! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/1/2012 23:34'!
zzdrawString: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: c
	| font portRect bounds |
	bounds _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	port colorMap: nil.
	portRect _ port clipRect.
	port clipByX1: bounds left + origin x 
		y1: bounds top + origin y 
		x2: bounds right + origin x 
		y2: bounds bottom + origin y.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [port clipRect: portRect. ^self].
	port clipWidth = 0 ifTrue: [port clipRect: portRect. ^self].
	font _ fontOrNil ifNil: [StrikeFont default].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: (bounds topLeft + origin)
		strikeFont: font
		kern: font baseKern negated.
	port clipRect: portRect! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/1/2012 23:33'!
zzdrawString: s in: boundsRect font: fontOrNil color: c
	^self zzdrawString: s from: 1 to: s size in: boundsRect font: fontOrNil color: c! !


!IndentingListItemMorph methodsFor: 'accessing' stamp: 'jmv 9/1/2012 23:41'!
isSelected: aBoolean

	isSelected _ aBoolean.
	self redrawNeeded! !


!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/1/2012 22:57'!
drawLineToggleToTextFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
	"If I am not the only item in my container, draw the line between:
		- my toggle (if any) or my left edge (if no toggle)
		- and my text left edge"

	| myBounds myCenter hLineY hLineLeft |
	anIndentingListItemMorph isSoleItem ifTrue: [ ^ self ].
	myBounds _ anIndentingListItemMorph toggleRectangle.
	myBounds _ anIndentingListItemMorph location displayBoundsOfTransformOf: myBounds.
	myCenter _ myBounds center.
	hLineY _ myCenter y.
	hasToggle
		ifTrue: [ hLineLeft _ myBounds right - 3 ]
		ifFalse: [ hLineLeft _ myCenter x - 1 ].
	"Draw line from toggle to text"
	aCanvas
		zzline: hLineLeft @ hLineY
		to: myBounds right + 0 @ hLineY
		width: 1
		color: lineColor! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/1/2012 22:33'!
drawLinesFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor 
	| hasToggle |
	hasToggle _ anIndentingListItemMorph hasToggle.
	"Draw line from toggle to text"
	self
		drawLineToggleToTextFor: anIndentingListItemMorph
		on: aCanvas
		lineColor: lineColor
		hasToggle: hasToggle.

	"Draw the line from toggle to the nextSibling's toggle"
	anIndentingListItemMorph nextSibling ifNotNil: [
		self
			drawLinesToNextSiblingFor: anIndentingListItemMorph
			on: aCanvas
			lineColor: lineColor
			hasToggle: hasToggle ].

	"If it have children and am expanded, draw a line to its first child"
	(anIndentingListItemMorph firstChild notNil and: [
			anIndentingListItemMorph isExpanded ]) ifTrue: [
		self
			drawLinesToFirstChildFor: anIndentingListItemMorph
			on: aCanvas
			lineColor: lineColor]! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/1/2012 22:57'!
drawLinesOn: aCanvas 
	| lColor |
	lColor _ Theme current line.

	self submorphs do: [ :submorph | 
		(submorph isExpanded or: [
			(aCanvas isVisible: submorph morphBoundsInWorld) or: [
				submorph nextSibling notNil and: [
					aCanvas isVisible:
						submorph nextSibling morphBoundsInWorld ] ] ])
		ifTrue: [
			self
				drawLinesFor: submorph
				on: aCanvas
				lineColor: lColor ]]! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/1/2012 22:57'!
drawLinesToFirstChildFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor
	"Draw line from me to next sibling"

	| vLineX vLineTop vLineBottom childCenter |
	childCenter _ anIndentingListItemMorph firstChild location externalizePosition:
			anIndentingListItemMorph firstChild toggleRectangle center.
	vLineX _ childCenter x - 1.
	vLineTop _ (anIndentingListItemMorph location
		externalizePosition: anIndentingListItemMorph morphExtent) y.
	anIndentingListItemMorph firstChild hasToggle
		ifTrue: [ vLineBottom _ childCenter y - 7 ]
		ifFalse: [ vLineBottom _ childCenter y ].
	aCanvas
		zzline: vLineX @ vLineTop
		to: vLineX @vLineBottom
		width: 1
		color: lineColor! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/1/2012 22:58'!
drawLinesToNextSiblingFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
	| vLineX myCenter vLineTop vLineBottom nextSibCenter |

	self flag: #jmvVer2. "complicated... not intuitive... who should draw this?"
	"the center of the toggle in our own coordinates (and not in those of child, that is not our child, but our sibling in the morphic hierarchy!!)"
	nextSibCenter _ anIndentingListItemMorph nextSibling location externalizePosition:
		anIndentingListItemMorph nextSibling toggleRectangle center.

	myCenter _ anIndentingListItemMorph location externalizePosition:
		 anIndentingListItemMorph toggleRectangle center.
	vLineX _ myCenter x - 1.
	hasToggle
		ifTrue: [ vLineTop _ myCenter y + 5 ]
		ifFalse: [ vLineTop _ myCenter y ].
	anIndentingListItemMorph nextSibling hasToggle
		ifTrue: [ vLineBottom _ nextSibCenter y - 7 ]
		ifFalse: [ vLineBottom _ nextSibCenter y ].
	"Draw line from me to next sibling"
	aCanvas
		zzline: vLineX @ vLineTop
		to: vLineX @ vLineBottom
		width: 1
		color: lineColor! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/1/2012 20:04'!
drawOn: aCanvas

	super drawOn: aCanvas.

	Preferences showLinesInHierarchyViews ifTrue:[
		self drawLinesOn: aCanvas ]! !


!HierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/1/2012 20:19'!
drawOn: aCanvas

	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			zzframeRectangle: self focusIndicatorRectangle 
			borderWidth: Preferences focusIndicatorWidth
			color: Theme current focusIndicator ]! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 9/1/2012 23:41'!
selectedMorph: aMorph

	selectedMorph ifNotNil: [
		selectedMorph isSelected: false ].
	selectedMorph _ aMorph.
	selectedMorph ifNotNil: [
		selectedMorph isSelected: true ]! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 9/1/2012 23:40'!
selectionIndex: idx
	"Called internally to select the index-th item."
	| theMorph index |
	idx ifNil: [^ self].
	index _ idx min: scroller submorphs size max: 0.
	theMorph _ index = 0 ifTrue: [ nil ] ifFalse: [ scroller submorphs at: index ].
	self selectedMorph: theMorph.
	self scrollSelectionIntoView! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 9/1/2012 23:38'!
drawOn: aCanvas

	| tRect sRect colorToUse sLeft aForm centeringOffset |
	isSelected ifTrue: [
		aCanvas
			zzfillRectangle: (0@0 extent: self morphExtent)
			colorOrInfiniteForm: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].

	complexContents hasContents ifTrue: [
		tRect _ self toggleRectangle.
		aForm _ isExpanded 
			ifTrue: [ container expandedForm ]
			ifFalse: [ container notExpandedForm ].
		centeringOffset _ ((tRect height - aForm extent y) / 2.0) rounded.
		aCanvas 
			zzimage: aForm 
			at: (tRect topLeft translatedBy: 0 @ centeringOffset) ].

	sLeft _ indentLevel * 12 + 16.
	sRect _ sLeft@0 extent: self morphExtent - (sLeft@0).
	colorToUse _ complexContents preferredColor ifNil: [ color ].
	aCanvas
		zzdrawString: contents asString
		in: sRect
		font: self fontToUse
		color: colorToUse! !

!IndentingListItemMorph methodsFor: 'initialization' stamp: 'jmv 9/1/2012 20:15'!
initialize

	super initialize.
	indentLevel _ 0.
	isExpanded _ false.
	isSelected _ false! !


!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 9/1/2012 20:28'!
selectionIndex: index
	"Called internally to select the index-th item."
	| row |
	row _ index ifNil: [ 0 ].
	row _ row min: self getListSize.  "make sure we don't select past the end"
	self listMorph selectedRow: row.
	self scrollSelectionIntoView! !

!methodRemoval: PluggableListMorph #highlightSelection!
PluggableListMorph removeSelector: #highlightSelection!
!methodRemoval: PluggableListMorph #unhighlightSelection!
PluggableListMorph removeSelector: #unhighlightSelection!
!methodRemoval: ListItemWrapper #highlightingColor!
ListItemWrapper removeSelector: #highlightingColor!
!methodRemoval: InnerHierarchicalListMorph #drawLinesFor:On:lineColor:!
InnerHierarchicalListMorph removeSelector: #drawLinesFor:On:lineColor:!

!InnerHierarchicalListMorph reorganize!
('geometry' adjustExtent desiredWidth)
('drawing' drawLineToggleToTextFor:on:lineColor:hasToggle: drawLinesFor:on:lineColor: drawLinesOn: drawLinesToFirstChildFor:on:lineColor: drawLinesToNextSiblingFor:on:lineColor:hasToggle: drawOn:)
!

!methodRemoval: IndentingListItemMorph #drawLineToggleToTextOn:lineColor:hasToggle:!
IndentingListItemMorph removeSelector: #drawLineToggleToTextOn:lineColor:hasToggle:!
!methodRemoval: IndentingListItemMorph #drawLinesOn:lineColor:!
IndentingListItemMorph removeSelector: #drawLinesOn:lineColor:!
!methodRemoval: IndentingListItemMorph #drawLinesToFirstChildOn:lineColor:!
IndentingListItemMorph removeSelector: #drawLinesToFirstChildOn:lineColor:!
!methodRemoval: IndentingListItemMorph #drawLinesToNextSiblingOn:lineColor:hasToggle:!
IndentingListItemMorph removeSelector: #drawLinesToNextSiblingOn:lineColor:hasToggle:!
!methodRemoval: IndentingListItemMorph #drawToggleOn:in:!
IndentingListItemMorph removeSelector: #drawToggleOn:in:!
!methodRemoval: IndentingListItemMorph #highlight!
IndentingListItemMorph removeSelector: #highlight!
!methodRemoval: IndentingListItemMorph #unhighlight!
IndentingListItemMorph removeSelector: #unhighlight!
!classDefinition: #IndentingListItemMorph category: #'Morphic-Views for Models'!
StringMorph subclass: #IndentingListItemMorph
	instanceVariableNames: 'indentLevel isExpanded complexContents firstChild container nextSibling isSelected'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
!methodRemoval: HierarchicalListMorph #columns!
HierarchicalListMorph removeSelector: #columns!
!methodRemoval: HierarchicalListMorph #drawLinesOn:!
HierarchicalListMorph removeSelector: #drawLinesOn:!
!methodRemoval: HierarchicalListMorph #highlightSelection!
HierarchicalListMorph removeSelector: #highlightSelection!
!methodRemoval: HierarchicalListMorph #lineColor!
HierarchicalListMorph removeSelector: #lineColor!
!methodRemoval: HierarchicalListMorph #unhighlightSelection!
HierarchicalListMorph removeSelector: #unhighlightSelection!
!classDefinition: #HierarchicalListMorph category: #'Morphic-Views for Models'!
PluggableScrollPane subclass: #HierarchicalListMorph
	instanceVariableNames: 'selectedMorph getListSelector keystrokeActionSelector autoDeselect sortingSelector getSelectionSelector setSelectionSelector menuGetter mainView'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!HierarchicalListMorph reorganize!
('drawing' drawOn: expandedForm notExpandedForm)
('commands' expandAll expandAll: toggleExpandedState:event:)
('events' keyStroke: mouseDown:localPosition: mouseEnter: mouseUp:localPosition:)
('event handling testing' handlesKeyboard)
('event handling' itemFromPoint: keyboardFocusChange:)
('events-processing' processMouseMove:localPosition:)
('geometry' scrollDeltaHeight scrollDeltaWidth)
('initialization' autoDeselect: currentlyExpanded indentingItemClass innerMorphClass list: listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction:)
('keyboard navigation' arrowKey: getSelectionIndex setSelectionIndex: toggleExpandedState:)
('model access' getList)
('selection' getCurrentSelectionItem maximumSelection minimumSelection numSelectionsInView scrollSelectionIntoView selectedMorph: selection: selectionIndex: setSelectedMorph:)
('updating' update:)
('private' addMorphsTo:from:allowSorting:withExpandedItems:atLevel: addSubmorphsAfter:fromCollection:allowSorting: insertNewMorphs: noteRemovalOfAll:)
('menu' getMenu)
!

