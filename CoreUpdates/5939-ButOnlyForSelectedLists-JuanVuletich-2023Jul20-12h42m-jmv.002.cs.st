'From Cuis 6.0 [latest update: #5938] on 20 July 2023 at 12:59:20 pm'!
!classDefinition: #PluggableListMorph category: 'Morphic-Widgets'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getIndexSelector setIndexSelector keystrokeActionSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector menuGetter mainView limitLayoutHeight '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PluggableListMorph methodsFor: 'layout' stamp: 'jmv 7/20/2023 12:53:41'!
limitLayoutHeight
	limitLayoutHeight := true! !


!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 7/20/2023 12:57:35'!
initialize
	super initialize.
	scroller morphWidth: extent x.
	limitLayoutHeight := false.! !

!PluggableListMorph methodsFor: 'layout' stamp: 'jmv 7/20/2023 12:58:57'!
maximumLayoutHeight
	limitLayoutHeight ifTrue: [
		^self desiredHeight ].
	^nil! !


!MessageSetWindow methodsFor: 'GUI building' stamp: 'jmv 7/20/2023 12:55:47'!
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
	listPropHeight := desiredHeight / availableHeight.
	listPropHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			listPropHeight := 0.4.
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].
	model changed: #editSelection! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 7/20/2023 12:56:12'!
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
	listPropHeight := desiredHeight / availableHeight.
	listPropHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			listPropHeight := 0.4.
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].! !


!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 7/20/2023 12:56:18'!
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
	listPropHeight := desiredHeight / availableHeight.
	listPropHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			listPropHeight := 0.4.
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].! !

!classDefinition: #PluggableListMorph category: 'Morphic-Widgets'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getIndexSelector setIndexSelector keystrokeActionSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector menuGetter mainView limitLayoutHeight'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PluggableListMorph reorganize!
('accessing' rowAtLocation: rowAtLocation:ifNone:)
('accessing - properties' rejectDrops)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: acceptDropsFrom:performing: allowItemDrag: wantsDroppedMorph:event:)
('event handling' keyboardFocusChange: keyboardFocused mouseButton2Activity)
('event handling testing' handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: focusKeyboardFor: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter: mouseLeave:)
('events-processing' clickedAt:maybeDeselect: invalidDrop: processMouseMove:localPosition:)
('geometry' desiredExtent desiredHeight fontPreferenceChanged scrollDeltaHeight scrollDeltaWidth someSubmorphPositionOrExtentChanged)
('initialization' autoDeselect: doubleClickSelector: font font: initForKeystrokes initialize innerMorphClass model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction:)
('keyboard navigation' arrowKey: deleteAction navigateDown navigateOnePageDown navigateOnePageUp navigateToBottom navigateToTop navigateUp)
('menu' getMenu)
('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard)
('model access' getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: keyboardSearch: keystrokeAction: setSelectionIndex:)
('selection' maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: selection: selectionIndex: visualSelection visualSelectionIndex)
('testing' allowsMultipleSelection is:)
('updating' update: updateList verifyContents)
('private' changeSelectionTo:)
('scrolling' scrollBy: scrollSelectionIntoView scrollSiblings: scrollToShow: updateSiblingsScroller vScrollBarValue:)
('layout' limitLayoutHeight maximumLayoutHeight)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
PluggableListMorph allSubInstancesDo: [ :m | m instVarNamed: 'limitLayoutHeight' put: false ].!

