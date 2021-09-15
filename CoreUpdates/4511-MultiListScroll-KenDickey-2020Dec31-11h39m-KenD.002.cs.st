'From Cuis 5.0 [latest update: #4494] on 1 January 2021 at 3:56:49 pm'!
!classDefinition: #PluggableListMorph category: 'Morphic-Widgets'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getIndexSelector setIndexSelector keystrokeActionSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector menuGetter mainView leftSibling rightSibling scrollSiblings '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PluggableListMorph commentStamp: '<historical>' prior: 0!
...

When a PluggableListMorph is in focus, type in a letter (or several
letters quickly) to go to the next item that begins with that letter.
Special keys (up, down, home, etc.) are also supported.

leftSibling and rightSibling have two uses.
 [A] One can use left and right arrow keys to shift focus to a sibling
 [B] When scrollSiblings is true, one can do "multiscrolling" -- vertical scroll siblings with self

For [B] Sample usage see: CodePackageListWindow >>buildMorphicWindow!


!PluggableScrollPane methodsFor: 'access options' stamp: 'KenD 12/31/2020 13:05:54'!
alwaysHideVerticalScrollbar

	hideScrollBars _ #alwaysHideVertical.
	self vHideScrollBar.! !


!PluggableListMorph methodsFor: 'siblings' stamp: 'KenD 1/1/2021 13:10:42'!
scrollSiblings
	"Do I scroll my siblings with myself?"
	^ scrollSiblings! !

!PluggableListMorph methodsFor: 'siblings' stamp: 'KenD 1/1/2021 13:11:00'!
scrollSiblings: aBoolean
	"Do I scroll my siblings with myself?"
	scrollSiblings := aBoolean! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'KenD 1/1/2021 13:14:57'!
scrollBy: delta
	"Scroll self and any siblings"
	super scrollBy: delta.
	self scrollMySiblings! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'KenD 1/1/2021 15:34:25'!
scrollMySiblings
	"vertical scroll my siblings along with my self"
	| yOffset |
	yOffset := self scrollerOffset y.
	scrollSiblings ifTrue: [
		self vScrollLeftSibling:  yOffset; 
			vScrollRightSibling: yOffset
	]! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'KenD 1/1/2021 13:14:49'!
scrollSelectionIntoView
	"make sure that the current selection is visible"
	| row r |
	row _ self getCurrentSelectionIndex.
	row = 0 
		ifTrue: [
			"Value is 0, but we need to propagate it to model"
			scrollBar internalScrollValue: scrollBar scrollValue ]
		ifFalse: [
			self flag: #jmvVer2.
			r _ self listMorph drawBoundsForRow: row.
			r _ ((self listMorph externalize: r origin) extent: r extent).
			self scrollToShow: r ].
	self scrollMySiblings
! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'KenD 1/1/2021 13:14:29'!
scrollToShow: aRectangle

	super scrollToShow: aRectangle.
	self scrollMySiblings ! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'KenD 1/1/2021 15:41:31'!
vPrivateScrollTo: scrollValue

	self scrollerOffset: (self scrollerOffset x @ scrollValue)! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'KenD 1/1/2021 15:43:22'!
vScrollBarValue: scrollValue

	super vScrollBarValue: scrollValue.
	self scrollMySiblings! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'KenD 1/1/2021 15:32:18'!
vScrollLeftSibling: yOffset
	"vertical scroll my LEFT siblings along with my self"
	self vPrivateScrollTo: yOffset.
	scrollSiblings ifTrue: [
		leftSibling  ifNotNil: [ :left    | 
			 left  vScrollLeftSibling: yOffset ]
	]! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'KenD 1/1/2021 15:32:25'!
vScrollRightSibling: yOffset
	"vertical scroll my RIGHT siblings along with my self"
	self vPrivateScrollTo: yOffset.
	scrollSiblings ifTrue: [
		rightSibling  ifNotNil: [ :left    | 
			 left  vScrollRightSibling: yOffset ]
	]! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'KenD 12/31/2020 13:09:16'!
updateScrollBarsBounds
	
	| t |
	hideScrollBars = #hide ifTrue: [^self].
	t _ self scrollBarClass scrollbarThickness.
	(hideScrollBars = #hideVertical)
	ifFalse: [
		scrollBar
			morphPosition: extent x - t - borderWidth @ borderWidth
			extent: t @ self vScrollBarHeight.
	].
	hScrollBar
		morphPosition: borderWidth @ (extent y - t - borderWidth)
		extent: self hScrollBarWidth@t! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'KenD 12/31/2020 13:18:18'!
vIsScrollbarNeeded
	"Return whether the vertical scrollbar is needed"
	
	"Don't show it if we were told not to."
	hideScrollBars = #hide ifTrue: [ ^false ].
	
	hideScrollBars = #alwaysHideVertical  ifTrue: [ ^false ].

	hideScrollBars = #alwaysShowVertical ifTrue: [ ^true ].

	^self vLeftoverScrollRange > 0! !


!PluggableListMorph methodsFor: 'initialization' stamp: 'KenD 1/1/2021 13:11:40'!
initialize
	super initialize.
	scroller morphWidth: extent x.
	scrollSiblings := false. "user must override"! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 1/1/2021 15:49:40'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames dirtyFlagsPane namesPane fileNamesPane 
	upperRow  description summary backColor labelBackground |
	backColor := self textBackgroundColor.	
	labelBackground := Theme current background.
	
	dirtyFlags := PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: backColor;
			   hideScrollBarsIndefinitely.
	dirtyFlagsPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Unsaved?');
		addMorphUseAll: dirtyFlags.

	names := PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: backColor.
	namesPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Package Name');
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: backColor;
			   alwaysHideVerticalScrollbar.
	fileNamesPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' File Name');
		addMorphUseAll: fileNames.

	upperRow := LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlagsPane proportionalWidth: 0.13;
		addAdjusterAndMorph: namesPane proportionalWidth: 0.27;
		addAdjusterAndMorph: fileNamesPane proportionalWidth: 0.6.
		
	description := (TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:) emptyTextDisplayMessage: 'Please enter a description for this package'.

	summary := (TextModelMorph
		textProvider: model
		textGetter: #summary) emptyTextDisplayMessage: 'Package summary (No package selected?)'.
		
	names leftSibling: dirtyFlags rightSibling: fileNames; scrollSiblings: true.
	dirtyFlags rightSibling: names; scrollSiblings: true.
	fileNames leftSibling: names; scrollSiblings: true.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: self buildButtonPane fixedHeight: Theme current buttonPaneHeight;
		addAdjusterAndMorph: summary fixedHeight: 60;
		addAdjusterAndMorph: description proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.15.
	self setLabel: 'Installed Packages'! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'KenD 12/31/2020 11:40:22'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout updateReqButton |
	requirements := PluggableListMorph
		model: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	requirements color: Theme current textPane.
		
	deleteReqButton := PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'delete'::
							setBalloonText: 'Remove selected Feature requirement'.
	deleteReqButton color: self widgetsColor.
	updateReqButton _ PluggableButtonMorph 
							model: requirements model
							action: #updateSelectedRequirement 
							label: 'update'::
							setBalloonText: 'Update requirement to current Feature revision'.
	updateReqButton color: self widgetsColor.
							
	buttonLayout := LayoutMorph newRow.
	buttonLayout 
		addMorph: deleteReqButton 
		layoutSpec: (LayoutSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop);
		color: self widgetsColor quiteWhiter;
		addMorph: updateReqButton 
		layoutSpec: (LayoutSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop);
		color: self widgetsColor quiteWhiter.
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LayoutMorph newRow.
	^ reqLayout 
		doAdoptWidgetsColor;
		addMorph: requirements 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.8
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #leftOrTop);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.2 
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #rightOrBottom);
		color: `Color transparent`;
		yourself
		! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'KenD 1/1/2021 15:54:53'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| dirtyFlags changeSetList classList dirtyFlagsPane changeSetListPane classListPane
	  messageList upperPanes backColor labelBackground |
	backColor _ self textBackgroundColor.
	labelBackground _ Theme current background.
	model myChangeSet ifNil: [
		self flag: #ojo. "Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	dirtyFlags _ PluggableListMorph
		model: model
		listGetter: #changeSetDirtyFlags
		indexGetter: nil
		indexSetter: nil.
	dirtyFlags color: backColor.
	dirtyFlagsPane _ LayoutMorph newColumn
		color: Theme current background;
		addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Unsaved?');
		addMorphUseAll: dirtyFlags.

	changeSetList _ (PluggableListMorphByItem
				model: model
				listGetter: #changeSetList
				indexGetter: #currentCngSet
				indexSetter: #showChangeSetNamed:
				mainView: self
				menuGetter: #changeSetMenu
				keystrokeAction: #changeSetListKey:from:)
			autoDeselect: false.
	changeSetList color: backColor.
	changeSetListPane _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Change Set name');
		addMorphUseAll: changeSetList.

	classList _ PluggableListMorphByItem
				model: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:
				mainView: self
				menuGetter: #classListMenu
				keystrokeAction: #classListKey:from:.
	classList color: backColor.
	classListPane _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: dirtyFlagsPane proportionalWidth: 0.13;
		addAdjusterAndMorph: changeSetListPane proportionalWidth: 0.47;
		addAdjusterAndMorph: classListPane proportionalWidth: 0.4.
			
	"Scroll Sibling Panes together."
	changeSetList leftSibling: dirtyFlags; scrollSiblings: true.
	dirtyFlags rightSibling: changeSetList; scrollSiblings: true.

	messageList _ PluggableListMorphByItem
				model: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:
				mainView: self
				menuGetter: #messageMenu
				keystrokeAction: #messageListKey:from:.
	messageList color: backColor.
	messageList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Methods');
		addMorphUseAll: messageList.
		

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString! !

!classDefinition: #PluggableListMorph category: 'Morphic-Widgets'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getIndexSelector setIndexSelector keystrokeActionSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector menuGetter mainView leftSibling rightSibling scrollSiblings'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PluggableListMorph reorganize!
('accessing' rowAtLocation: rowAtLocation:ifNone:)
('accessing - properties' acceptDropsFrom:performing:whenOutsideList: makeItemsDraggable makeItemsUndraggable rejectDrops)
('drawing' drawOn: flashRow:)
('dropping/grabbing' acceptDroppingMorph:atRow:withEvent: acceptDroppingMorph:event: acceptDroppingMorph:outsideListWithEvent: wantsDroppedMorph:event:)
('event handling' keyboardFocusChange: mouseButton2Activity)
('event handling testing' allowsMorphDrop handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: gainFocusFrom: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter: mouseLeave:)
('events-processing' processMouseMove:localPosition:)
('geometry' fontPreferenceChanged scrollDeltaHeight scrollDeltaWidth)
('initialization' autoDeselect: doubleClickSelector: font font: initForKeystrokes initialize innerMorphClass listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction:)
('keyboard navigation' arrowKey: deleteAction navigateDown navigateLeft navigateOnePageDown navigateOnePageUp navigateRight navigateToBottom navigateToTop navigateUp)
('menu' getMenu)
('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard)
('model access' getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: keyboardSearch: keystrokeAction: setSelectionIndex:)
('selection' maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: selection: selectionIndex: visualSelection visualSelectionIndex)
('testing' is:)
('updating' update: updateList verifyContents)
('private' changeSelectionTo: itemsAreDraggable listMorph)
('siblings' leftSibling: leftSibling:rightSibling: rightSibling: scrollSiblings scrollSiblings:)
('scrolling' scrollBy: scrollMySiblings scrollSelectionIntoView scrollToShow: vPrivateScrollTo: vScrollBarValue: vScrollLeftSibling: vScrollRightSibling:)
!

