'From Cuis 5.0 of 7 November 2016 [latest update: #3544] on 27 December 2018 at 10:16:55 am'!

!Browser methodsFor: 'message category functions' stamp: 'HAW 12/27/2018 10:07:52'!
categorizeUnderNewCategoryMessageAt: aSelectorToCategorize

	self selectedClassOrMetaClass ifNotNil: [ :class | | newCategory |
		newCategory _ self newCategoryNameIfNone: [ ^self ].
		class organization 
			addCategory: newCategory;
			classify: aSelectorToCategorize under: newCategory suppressIfDefault: false.
			
		self changed: #messageCategoryList.
		self changed: #messageList]! !

!Browser methodsFor: 'message category functions' stamp: 'HAW 12/27/2018 10:04:30'!
newCategoryNameIfNone: aNoneBlock

	| labels lines menuIndex newName reject |
	
	labels _ OrderedCollection with: 'new...'.
	reject _ Set new.
	reject
		addAll: self selectedClassOrMetaClass organization categories;
		add: ClassOrganizer nullCategory;
		add: ClassOrganizer default.
	lines _ OrderedCollection new.
	self selectedClassOrMetaClass allSuperclasses do: [:cls | | cats |
		cls = Object ifFalse: [
			cats _ cls organization categories reject:
				 [:cat | reject includes: cat].
			cats isEmpty ifFalse: [
				lines add: labels size.
				labels addAll: cats asArray sort.
				reject addAll: cats]]].
	newName _ (labels size = 1 or: [
		menuIndex _ (PopUpMenu labelArray: labels lines: lines)
		startUpWithCaption: 'Add Category'.
		menuIndex = 0 ifTrue: [^ aNoneBlock value].
		menuIndex = 1])
			ifTrue: [
				self request: 'Please type new category name'
					initialAnswer: 'category name']
			ifFalse: [
				labels at: menuIndex].
	
	^ newName isEmpty
		ifTrue: aNoneBlock
		ifFalse: [newName asSymbol].
! !


!PluggableListMorph methodsFor: 'accessing - properties' stamp: 'HAW 12/27/2018 09:32:34'!
acceptDropsFrom: aMorph performing: aDropActionSelector whenOutsideList: aDropOutsideListActionSelector 

	(aDropActionSelector numArgs between: 1 and: 2) ifFalse: [ self error: 'dropActionSelector must be a 1- or 2-keyword symbol' ].
	aDropOutsideListActionSelector numArgs = 1 ifFalse: [ self error: 'dropOutsideListActionSelector must be a 1-keyword symbol' ].

	self setProperty: #allowsMorphDrop toValue: true.
	self setProperty: #acceptedDragSource toValue: aMorph.
	self setProperty: #dropActionSelector toValue: aDropActionSelector.
	self setProperty: #dropOutsideListActionSelector toValue: aDropOutsideListActionSelector ! !

!PluggableListMorph methodsFor: 'dropping/grabbing' stamp: 'HAW 12/27/2018 10:14:02'!
acceptDroppingMorph: aMorph atRow: row withEvent: dropEvent

	| args dropActionSelector |

	self flashRow: row.

	dropActionSelector _ self valueOfProperty: #dropActionSelector.
	args _ dropActionSelector numArgs.
	args = 1 ifTrue: [ ^model perform: dropActionSelector with: row].
	args = 2 ifTrue: [ | dropSelectorArgument |
		dropSelectorArgument _ aMorph
			valueOfProperty: #dropSelectorArgument
			ifAbsent: [self error: 'aMorph is missing dropSelectorArgument property'].
		^model perform: dropActionSelector with: row with: dropSelectorArgument ].
	
	self error: 'dropActionSelector must be a 1- or 2-keyword symbol'
	
	! !

!PluggableListMorph methodsFor: 'dropping/grabbing' stamp: 'HAW 12/27/2018 09:28:18'!
acceptDroppingMorph: aMorph outsideListWithEvent: dropEvent

	| dropActionSelector dropSelectorArgument |
	
	dropActionSelector _ self valueOfProperty: #dropOutsideListActionSelector.
	dropSelectorArgument _ aMorph
		valueOfProperty: #dropSelectorArgument
		ifAbsent: [self error: 'aMorph is missing dropSelectorArgument property'].
	model perform: dropActionSelector with: dropSelectorArgument.
	! !


!Browser methodsFor: 'message category functions' stamp: 'HAW 12/27/2018 10:02:04'!
addCategory
	"Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection"

	| oldIndex newName |

	selectedClassName ifNil: [ ^self ].
	
	oldIndex _ self messageCategoryListIndex.
	newName _ self newCategoryNameIfNone: [ ^self ].
	
	self classOrMetaClassOrganizer
		addCategory: newName
		before: selectedMessageCategory.
	self changed: #messageCategoryList.
	self messageCategoryListIndex:
		(oldIndex = 0
			ifTrue: [self classOrMetaClassOrganizer categories size + 1]
			ifFalse: [oldIndex]).
	self changed: #messageCategoryList.
	
! !

!Browser methodsFor: 'message category functions' stamp: 'HAW 12/27/2018 09:54:08'!
categorizeUnderCategoryAt: aMessageCategoryListIndex messageAt: aSelectorToCategorize

	self selectedClassOrMetaClass ifNotNil: [ :class | | categorySelector |
		categorySelector _ self messageCategoryList at: aMessageCategoryListIndex ifAbsent: [^self].
		categorySelector ~= Categorizer allCategory
			ifTrue: [
				class organization classify: aSelectorToCategorize under: categorySelector suppressIfDefault: false.
				self changed: #messageList]]! !


!PluggableListMorph methodsFor: 'accessing - properties' stamp: 'HAW 12/27/2018 09:19:35'!
rejectDrops

	self removeProperty: #allowsMorphDrop.
	self removeProperty: #acceptedDragSource.
	self removeProperty: #dropActionSelector.
	self removeProperty: #dropOutsideListActionSelector! !

!PluggableListMorph methodsFor: 'dropping/grabbing' stamp: 'HAW 12/27/2018 10:12:59'!
acceptDroppingMorph: aMorph event: dropEvent

	| localPosition row |
	
	localPosition _ self internalizeFromWorld: dropEvent eventPosition.
	row _ self rowAtLocation: localPosition ifNone: [ ^self acceptDroppingMorph: aMorph outsideListWithEvent: dropEvent ].
	
	self acceptDroppingMorph: aMorph atRow: row withEvent: dropEvent.
	
	! !

!PluggableListMorph methodsFor: 'events' stamp: 'HAW 12/27/2018 10:10:27'!
dragEvent: aMouseEvent localPosition: localEventPosition

	| row dragged listItem |
	
	row _ self rowAtLocation: localEventPosition ifNone: [ ^self ].
	listItem _ self getListItem: row.

	dragged _ DraggingGuideMorph new.
	dragged addMorph: (StringMorph contents: listItem).
	dragged setProperty: #dragSource toValue: self.
	dragged setProperty: #dropSelectorArgument toValue: listItem.
	
	aMouseEvent hand attachMorphBeside: dragged.! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'HAW 12/27/2018 10:09:58'!
buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes messageCatList messageList |

	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	messageList makeItemsDraggable.
	messageCatList 
		acceptDropsFrom: messageList 
		performing: #categorizeUnderCategoryAt:messageAt: 
		whenOutsideList: #categorizeUnderNewCategoryMessageAt:.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: self buildMorphicSystemCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: self buildMorphicClassColumn proportionalWidth: 0.2;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

!methodRemoval: PluggableListMorph #acceptDropsFrom:performing:!
PluggableListMorph removeSelector: #acceptDropsFrom:performing:!

!Browser reorganize!
('accessing' acceptedStringOrText contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit)
('class list' classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically listClassesHierarchically: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categorizeUnderCategoryAt:messageAt: categorizeUnderNewCategoryMessageAt: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories newCategoryNameIfNone: removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories changeSystemCategories: editSystemCategories fileOutSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('*Refactoring' insertSuperclass isEditingMethod removeClass renameClass safelyRemoveClass)
('*DenotativeObject' compile:receiver:in: debug debug:receiver:in:withResultDo: debugAndInspect debugAndWithResultDo: debugDenotativeObjectMethodTest isTest: runDenotativeObjectMessageCategoryTests runDenotativeObjectMethodTest runDenotativeObjectSystemCategoryTests runDenotativeObjectTests send sendAndInspect sendAndWithResultDo: stringToEvalSending:to:onCancel:)
('*LiveTypeInformation' classDefinitionText classOfInstVarNamed: classOfTempVarNamed: contents:notifying: selectedMessage)
!


!PluggableListMorph reorganize!
('accessing' rowAtLocation: rowAtLocation:ifNone:)
('accessing - properties' acceptDropsFrom:performing:whenOutsideList: makeItemsDraggable makeItemsUndraggable rejectDrops)
('drawing' blinkRow: drawOn: flashRow:)
('dropping/grabbing' acceptDroppingMorph:atRow:withEvent: acceptDroppingMorph:event: acceptDroppingMorph:outsideListWithEvent: wantsDroppedMorph:event:)
('event handling' keyboardFocusChange: mouseButton2Activity)
('event handling testing' allowsMorphDrop handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter:)
('events-processing' processMouseMove:localPosition:)
('geometry' fontPreferenceChanged scrollDeltaHeight scrollDeltaWidth)
('initialization' autoDeselect: doubleClickSelector: font font: initForKeystrokes initialize innerMorphClass listItemHeight model:listGetter:indexGetter:indexSetter:mainView:menuGetter:keystrokeAction: textColor)
('keyboard navigation' arrowKey:)
('layout')
('menu' getMenu)
('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard)
('model access' getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: keyboardSearch: keystrokeAction: setSelectionIndex:)
('selection' maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: scrollSelectionIntoView selection: selectionIndex: visualSelection visualSelectionIndex)
('testing' is:)
('updating' update: updateList verifyContents)
('private' itemsAreDraggable listMorph)
!


!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumn buildMorphicClassList buildMorphicCodePane buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCatList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCommentButton createInstanceButton windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu classListMenu2 messageCategoryMenu messageListMenu messageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses openSystemCategoryBrowser)
('keyboard shortcuts' systemCatListKey:from: systemCatSingletonKey:from:)
('updating' disableCodePaneEditing editSelectionChanged enableCodePaneEditing isEditSelectionNone update:)
('*Refactoring' addInstVar addParameter classRefactoringMenu messageRefactoringMenu openClassRefactoringMenu openMessageRefactoringMenu pushUpSelector removeAllUnreferencedInstVar removeInstVar removeParameter renameInstVar renameSelector)
('*DenotativeObject-Tools' classNameRequester findClass findClassDialogTitle)
!

