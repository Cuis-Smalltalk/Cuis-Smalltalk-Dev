'From Cuis 6.0 [latest update: #5777] on 24 April 2023 at 4:39:41 pm'!

!InnerListMorph methodsFor: 'geometry' stamp: 'jmv 4/24/2023 16:13:19'!
desiredExtent
	^self desiredWidth @ self desiredHeight! !

!InnerListMorph methodsFor: 'geometry' stamp: 'jmv 4/24/2023 16:12:42'!
desiredHeight
	^(listItems size max: 1) * self listItemHeight! !

!InnerListMorph methodsFor: 'geometry' stamp: 'jmv 4/24/2023 15:26:40'!
listItemHeight
	^ font lineSpacing
! !


!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 4/24/2023 16:17:48'!
desiredExtent
	^ scroller desiredExtent + (self xtraBorder + borderWidth + 1 * 2)! !


!InnerListMorph methodsFor: 'geometry' stamp: 'jmv 4/24/2023 16:35:42'!
adjustExtent
	"Adjust our height to match the underlying list,
	but make it wider if neccesary to fill the available width in our PluggableListMorph
	(this is needed to make the selection indicator no narrower than the list)"

	self morphExtent: self desiredExtent.! !


!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 4/24/2023 15:29:09'!
numSelectionsInView
	^ self viewableHeight // scroller listItemHeight! !


!MessageSetWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2023 16:25:07'!
buildMorphicWindow
	"Answer a morphic window with the given label that can display the receiver"

	| listPane desiredExtent desiredHeight wine availableHeight listPropHeight |
	listPane := self buildMorphicMessageList.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + ScrollBar scrollbarThickness ].
	listPropHeight := desiredHeight / availableHeight min: 0.4.
	self layoutMorph
		addMorph: listPane proportionalHeight: listPropHeight;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 - listPropHeight.
	model changed: #editSelection! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2023 16:23:45'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight listPropHeight |
	listPane := PluggableListMorphOfMany
		model: model
		listGetter: #list
		primarySelectionGetter: #listIndex
		primarySelectionSetter: #toggleListIndex:
		listSelectionGetter: #listSelectionAt:
		listSelectionSetter: #listSelectionAt:put:
		mainView: self
		menuGetter: #listMenu
		keystrokeAction: #changeListKey:from:.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + ScrollBar scrollbarThickness ].
	listPropHeight := desiredHeight / availableHeight min: 0.4.
	self layoutMorph
		addMorph: listPane proportionalHeight: listPropHeight;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 - listPropHeight.! !


!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2023 16:25:55'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight listPropHeight |
	listPane := PluggableListMorph
		model: model
		listGetter: #list
		indexGetter: #listIndex
		indexSetter: #toggleListIndex:
		mainView: self
		menuGetter: #listMenu
		keystrokeAction: #changeListKey:from:.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + ScrollBar scrollbarThickness ].
	listPropHeight := desiredHeight / availableHeight min: 0.4.
	self layoutMorph
		addMorph: listPane proportionalHeight: listPropHeight;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 - listPropHeight.! !

!methodRemoval: PluggableListMorph #listItemHeight stamp: 'jmv 4/24/2023 16:34:44'!
PluggableListMorph removeSelector: #listItemHeight!

!InnerListMorph reorganize!
('initialization' initialize)
('list management' drawBoundsForRow: drawYForRow: highlightedRow: listChanged rowAtLocation: rowAtLocation:ifNone: selectedRow selectedRow:)
('drawing' bottomVisibleRowForCanvas: draw:atRow:on: drawBackgroundForMulti:on: drawHighlightOn: drawOn: drawSelectionOn: font font: topVisibleRowForCanvas:)
('list access' getListItem: getListSize item:)
('private' noSelection)
('geometry' adjustExtent desiredExtent desiredHeight desiredWidth fontPreferenceChanged listItemHeight widthToDisplayItem:)
('events-processing' invalidDrop:)
!

