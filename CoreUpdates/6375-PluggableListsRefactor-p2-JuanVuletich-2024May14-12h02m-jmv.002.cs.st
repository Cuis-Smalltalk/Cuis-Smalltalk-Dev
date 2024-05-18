'From Cuis7.1 [latest update: #6368] on 14 May 2024 at 12:18:03 pm'!

!HierarchicalListMorph class methodsFor: 'instance creation' stamp: 'jmv 5/14/2024 12:03:23'!
withModel: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel! !


!PluggableListMorph class methodsFor: 'instance creation' stamp: 'jmv 5/14/2024 12:02:58'!
withModel: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel! !


!PluggableListMorphOfMany class methodsFor: 'instance creation' stamp: 'jmv 5/14/2024 12:03:59'!
withModel: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel
	^self new
		model: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel! !


!HierarchicalListMorph class methodsFor: 'instance creation' stamp: 'jmv 5/14/2024 12:05:27'!
model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel
	"Deprecated. Please inline in senders."

	^ (self withModel: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel)
		mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel! !


!PluggableListMorph class methodsFor: 'instance creation' stamp: 'jmv 5/14/2024 12:05:33'!
model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel
	"Deprecated. Please inline in senders."

	^ (self withModel: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel)
		mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel! !


!PluggableListMorphOfMany class methodsFor: 'instance creation' stamp: 'jmv 5/14/2024 12:07:09'!
model: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel
	"Deprecated. Please inline in senders."

	^(self withModel: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel)
		mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:15:53'!
buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| names fileNames namesPane fileNamesPane 
	upperRow  description summary labelBackground |
	labelBackground := Theme current background.
	
	names := PluggableListMorph
		withModel: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	namesPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' Package Name');
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		withModel: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames alwaysHideVerticalScrollbar.
	fileNamesPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: ' File Name');
		addMorphUseAll: fileNames.

	upperRow := LayoutMorph newRow.
	upperRow
		addMorph: namesPane proportionalWidth: 0.4;
		addAdjusterAndMorph: fileNamesPane proportionalWidth: 0.6.
		
	description := (TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:) emptyTextDisplayMessage: 'Please enter a description for this package'.

	summary := (TextModelMorph
		textProvider: model
		textGetter: #summary) emptyTextDisplayMessage: 'Package summary (No package selected?)'.
	
	names scrollSiblings: {fileNames}.
	fileNames scrollSiblings: {names}.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: self buildButtonPane fixedHeight: Theme current buttonPaneHeight;
		addAdjusterAndMorph: summary fixedHeight: 60;
		addAdjusterAndMorph: description proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.15.
	self setLabel: 'Installed Packages'! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:16:01'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout updateReqButton |
	requirements := PluggableListMorph
		withModel: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
		
	deleteReqButton := (PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'delete')
							setBalloonText: 'Remove selected Feature requirement'.
	deleteReqButton color: self widgetsColor.
	updateReqButton := (PluggableButtonMorph 
							model: requirements model
							action: #updateSelectedRequirement 
							label: 'update')
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


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:07:38'!
buildMorphicClassList

	^(PluggableListMorph
		withModel: model
		listGetter: #classList
		indexGetter: #classListIndex
		indexSetter: #classListIndex:)
			mainView: self
			menuGetter: #classListMenu
			keystrokeAction: #classListKey:from:! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:08:02'!
buildMorphicMessageCatList

	^(PluggableListMorph
		withModel: model
		listGetter: #messageCategoryList
		indexGetter: #messageCategoryListIndex
		indexSetter: #messageCategoryListIndex:)
			mainView: self
			menuGetter: #messageCategoryMenu
			keystrokeAction: #messageCatListKey:from:! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:08:43'!
buildMorphicMessageList
	"Build a morphic message list, with #messageList as its list-getter"

	^(PluggableListMorph
		withModel: model
		listGetter: #messageList
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:)
			mainView: self
			menuGetter: #messageListMenu
			keystrokeAction: #messageListKey:from:! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:09:33'!
buildMorphicSystemCategoryList
	^(HierarchicalListMorph
		withModel: model
		listGetter: #systemCategoryRoots
		indexGetter: #selectedSystemCategory
		indexSetter: #setSelectedSystemCategoryTreeItem:)
			mainView: self menuGetter: #systemCategoryMenu keystrokeAction: #systemCatListKey:from:;
			autoDeselect: true;
			autoExpand: true;
			yourself! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:09:47'!
buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes messageCatList messageList classColumn classList |
	mySingletonList := (PluggableListMorph
				withModel: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:)
					mainView: self
					menuGetter: #systemCatSingletonMenu
					keystrokeAction: #systemCatSingletonKey:from:.
	mySingletonList hideScrollBarsIndefinitely.

	classList := self buildMorphicClassList.
	classColumn := self buildMorphicClassColumnWith: classList.
	messageCatList := self buildMorphicMessageCatList.
	messageList := self buildMorphicMessageList.
	
	upperPanes := LayoutMorph newRow.
	upperPanes
		addMorph: classColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	messageList allowItemDrag: true.
	messageCatList
		acceptDropsFrom: messageList
		performing: #categorizeUnderCategoryAt:selector:.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: (Preferences at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:10:07'!
buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| sysCatList msgCatList upperPanes clsLayout clsList msgList |
	model setSelectedSystemCategory: model systemCategoryList first.
	sysCatList := (PluggableListMorph
			withModel: model 
			listGetter: #systemCategorySingleton
			indexGetter: #indexIsOne 
			indexSetter: #indexIsOne:)
				mainView: self
				menuGetter: #codeFileListMenu
				keystrokeAction: #codeFileListKey:from:.
	sysCatList hideScrollBarsIndefinitely.
	
	msgCatList := (PluggableListMorph
			withModel: model 
			listGetter: #messageCategoryList
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:)
				mainView: self
				menuGetter: #messageCategoryMenu
				keystrokeAction: nil.

	clsList := self buildMorphicClassList.
	clsLayout := self buildMorphicClassColumnWith: clsList.
	msgList := self buildMorphicMessageList.

	upperPanes := LayoutMorph newRow.
	upperPanes
		addMorph: clsLayout proportionalWidth: 0.3;
		addAdjusterAndMorph: msgCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: msgList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: sysCatList fixedHeight: (Preferences at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:06:15'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := (PluggableListMorphOfMany
		withModel: model
		listGetter: #list
		primarySelectionGetter: #listIndex
		primarySelectionSetter: #toggleListIndex:
		listSelectionGetter: #listSelectionAt:
		listSelectionSetter: #listSelectionAt:put:)
			mainView: self
			menuGetter: #listMenu
			keystrokeAction: #changeListKey:from:.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + ScrollBar scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].! !


!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:10:27'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := (PluggableListMorph
		withModel: model
		listGetter: #list
		indexGetter: #listIndex
		indexSetter: #toggleListIndex:)
			mainView: self
			menuGetter: #listMenu
			keystrokeAction: #changeListKey:from:.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + ScrollBar scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:14:30'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| changeSetList classList changeSetListPane classListPane
	  messageList upperPanes labelBackground |
	labelBackground := Theme current background.
	model myChangeSet ifNil: [
		"Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	changeSetList := (PluggableListMorphByItem
				withModel: model
				listGetter: #changeSetList
				indexGetter: #currentChangeSetName
				indexSetter: #showChangeSetNamed:)
					mainView: self
					menuGetter: #changeSetMenu
					keystrokeAction: #changeSetListKey:from:;
						autoDeselect: false.
	changeSetListPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Change Set name');
		addMorphUseAll: changeSetList.

	classList := (PluggableListMorphByItem
				withModel: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:)
					mainView: self
					menuGetter: #classListMenu
					keystrokeAction: #classListKey:from:.
	classListPane := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes := LayoutMorph newRow.
	upperPanes
		addMorph: changeSetListPane proportionalWidth: 0.6;
		addAdjusterAndMorph: classListPane proportionalWidth: 0.4.
	
	messageList := (PluggableListMorphByItem
				withModel: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:)
					mainView: self
					menuGetter: #messageMenu
					keystrokeAction: #messageListKey:from:.
	messageList := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Methods');
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:11:14'!
contextVariableInspector

	^ contextVariableInspector ifNil: [
		contextVariableInspector := (PluggableListMorph
			withModel: model contextVariablesInspector 
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:)
				mainView: self
				menuGetter: #contextFieldListMenu
				keystrokeAction: #inspectorKey:from: ]! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:11:30'!
receiverInspector

	^ receiverInspector ifNil: [
		receiverInspector := (PluggableListMorph
			withModel: model receiverInspector
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:)
				mainView: self
				menuGetter: #receiverFieldListMenu
				keystrokeAction: #inspectorKey:from: ]! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:11:42'!
stackList

	^ stackList ifNil: [
		stackList := (PluggableListMorph
			withModel: model 
			listGetter: #contextStackList
			indexGetter: #contextStackIndex
			indexSetter: #toggleContextStackIndex:)
				mainView: self
				menuGetter: #contextStackMenu
				keystrokeAction: #contextStackKey:from: ]! !


!MethodSetWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:11:53'!
buildMorphicMessageList
	"Build a morphic message list, with #messageList as its list-getter"

	^(PluggableListMorph
		withModel: model
		listGetter: #messageList
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:)
			mainView: self
			menuGetter: #messageListMenu
			keystrokeAction: #messageListKey:from:! !


!MessageNamesWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:12:11'!
buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"
	"MessageNames open"
	| selectorListView firstRow searchButton secondRow |
	textMorph := (TextModelMorph
		textProvider: model
		textGetter: #searchString
		textSetter: #searchString:
		selectionGetter: #contentsSelection)
		setBalloonText: 'See MessageNames class comment for search string options';
		emptyTextDisplayMessage: 'Type here, then hit Search'.
	textMorph askBeforeDiscardingEdits: false.
	textMorph acceptOnCR: true.
	textMorph hideScrollBarsIndefinitely.
	searchButton := PluggableButtonMorph new
		model: textMorph innerTextMorph;
		label: 'Search';
		action: #acceptContents.
	searchButton setBalloonText: 'Type some letters into the pane at right, and then press this Search button (or hit RETURN) and all method selectors that match what you typed will appear in the list pane below.  Click on any one of them, and all the implementors of that selector will be shown in the right-hand pane, and you can view and edit their code without leaving this tool.'.
	firstRow := LayoutMorph newRow.
	firstRow color: self windowColor.
	firstRow
		doAdoptWidgetsColor;
		
			addMorph: searchButton
			proportionalWidth: 0.25;
		
			addMorph: textMorph
			proportionalWidth: 0.75.
	selectorListView := (PluggableListMorph
		withModel: model
		listGetter: #selectorList
		indexGetter: #selectorListIndex
		indexSetter: #selectorListIndex:)
			mainView: self
			menuGetter: #selectorListMenu
			keystrokeAction: #selectorListKey:from:.
	secondRow := LayoutMorph newRow.
	secondRow
		
			addMorph: selectorListView
			proportionalWidth: 0.5;
		
			addAdjusterAndMorph: self buildMorphicMessageList
			proportionalWidth: 0.5.
	self layoutMorph
		
			addMorph: firstRow
			fixedHeight: self defaultButtonPaneHeight + 4;
		
			addAdjusterAndMorph: secondRow
			proportionalHeight: 0.5;
		
			addAdjusterAndMorph: self buildLowerPanes
			proportionalHeight: 0.5.
	model changed: #editSelection! !


!ProtocolBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:12:26'!
buildSelectiveClassList
	"Define the class hierarchy list pane"

	^(PluggableListMorph
		withModel: model
		listGetter: #selectiveClassList
		indexGetter: #selectiveClassListIndex
		indexSetter: #selectiveClassListIndex:)
			mainView: self 
			menuGetter: #protocolClassListMenu 
			keystrokeAction: #protocolClassListKey:from:! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:12:40'!
buildMorphicWindowMessage: messageString
	| notifyPane |
	model expandStack.
	messageString
		ifNil: [
			notifyPane := PluggableListMorph
				withModel: model
				listGetter: #contextStackList
				indexGetter: #contextStackIndex
				indexSetter: #openFullAt: ]
		ifNotNil: [
			notifyPane := TextModelMorph withText: messageString.
			notifyPane askBeforeDiscardingEdits: false ].
	self layoutMorph
		addMorph: (self buttonRowForPreDebugWindow: model) proportionalHeight: 0.2;
		addAdjusterAndMorph: notifyPane proportionalHeight: 0.8! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:12:57'!
morphicDirectoryTreePane

	^(HierarchicalListMorph
		withModel: model
		listGetter: #topDirectoryWrappers
		indexGetter: #currentDirectorySelected
		indexSetter: #setSelectedDirectoryTo:)
			mainView: self
			menuGetter: #directoryTreeMenu
			keystrokeAction: nil;
				autoDeselect: false;
				yourself! !

!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:14:41'!
morphicFileListPane

	^(PluggableListMorph
		withModel: model 
		listGetter: #fileList 
		indexGetter: #fileListIndex
		indexSetter: #fileListIndex: )
			mainView: self
			menuGetter: #fileListMenu
			keystrokeAction: #fileListKey:from:! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:13:09'!
buildList

	| list |
	
	list := (PluggableListMorph
			withModel: model 
			listGetter: #fieldList
			indexGetter: #selectionIndex
			indexSetter: #toggleIndex:)
				mainView: self
				menuGetter: #fieldListMenu
				keystrokeAction: #inspectorKey:from:.
	list doubleClickSelector: #inspectSelection.
	
	^list! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:16:18'!
buildMorphicWindow

	| textMorph |
	listMorph := (HierarchicalListMorph
			withModel: model
			listGetter: #getList
			indexGetter: #getCurrentSelection
			indexSetter: #noteNewSelection:)
				mainView: self
				menuGetter: #genericMenu
				keystrokeAction: #explorerKey:from:.
	listMorph autoDeselect: false.
	listMorph doubleClickSelector: #inspectSelection.
	textMorph := ((TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false;
			emptyTextDisplayMessage: 'Smalltalk expression (self is selected item)').
	self layoutMorph
		addMorph: listMorph proportionalHeight: 0.8;
		addAdjusterAndMorph: textMorph proportionalHeight: 0.2.
	self setLabel: model rootObject shortPrintString.! !


!ProcessBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:13:40'!
buildMorphicWindow
	"Create a pluggable version of me, answer a window"
	| aTextMorph list1 list2 upperRow |
	list1 := (PluggableListMorph
				withModel: model
				listGetter: #processNameList
				indexGetter: #processListIndex
				indexSetter: #processListIndex:)
					mainView: self
					menuGetter: #processListMenu
					keystrokeAction: #processListKey:from:.
	list2 := (PluggableListMorph
				withModel: model
				listGetter: #stackNameList
				indexGetter: #stackListIndex
				indexSetter: #stackListIndex:)
					mainView: self
					menuGetter: #stackListMenu
					keystrokeAction: #stackListKey:from:.
	upperRow := LayoutMorph newRow.
	upperRow
		addMorph: list1 proportionalWidth: 0.5;
		addAdjusterAndMorph: list2 proportionalWidth: 0.5.
	aTextMorph := (TextModelMorph
				textProvider: model
				textGetter: #selectedMethod) emptyTextDisplayMessage: 'Method source (not selected?)'.
	aTextMorph askBeforeDiscardingEdits: false.
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: aTextMorph proportionalHeight: 0.5.
	self setLabel: 'Process Browser'! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:13:52'!
buildErrorsList
	^ PluggableListMorph
		withModel: model
		listGetter: #errorsList
		indexGetter: #selectedErrorTest
		indexSetter: #debugErrorTest:! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:14:05'!
buildFailuresList
	^ PluggableListMorph
		withModel: model
		listGetter: #failuresList
		indexGetter: #selectedFailureTest
		indexSetter: #debugFailureTest:! !

!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 5/14/2024 12:06:30'!
buildUpperControls
	| refreshButton filterButton stopButton runOneButton runButton runProfiledButton buttonsRow theTestsList column |

	runOneButton := self buildRunOneButton.
	runButton := self buildRunButton.
	runProfiledButton := self buildRunProfiledButton.	

	refreshButton := self buildRefreshButton.
	filterButton := self buildFilterButton.
	stopButton := self buildStopButton.

	theTestsList := (PluggableListMorphOfMany
				withModel: model
				listGetter: #tests
				primarySelectionGetter: #selectedSuite
				primarySelectionSetter: #selectedSuite:
				listSelectionGetter: #listSelectionAt:
				listSelectionSetter: #listSelectionAt:put:)
					mainView: self
					menuGetter: #listMenu
					keystrokeAction: #listKey:from:.
	theTestsList autoDeselect: false.
	theTestsList color: self textBackgroundColor.

	buttonsRow := LayoutMorph newRow.
	buttonsRow doAdoptWidgetsColor.
	buttonsRow color: self buttonColor.
	buttonsRow
		addMorphs: {runButton. runOneButton. runProfiledButton};
		addMorphs: {refreshButton. filterButton. stopButton}.

	column := LayoutMorph newColumn.
	column
		addMorph: theTestsList proportionalHeight: 1;
		addMorph: buttonsRow fixedHeight: self defaultButtonPaneHeight.

	^ column! !

