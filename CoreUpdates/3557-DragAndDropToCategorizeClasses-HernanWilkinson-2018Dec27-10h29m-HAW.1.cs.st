'From Cuis 5.0 of 7 November 2016 [latest update: #3544] on 27 December 2018 at 1:42:37 pm'!

!Browser methodsFor: 'message category functions' stamp: 'HAW 12/27/2018 13:11:10'!
categorizeUnderCategoryAt: aMessageCategoryListIndex selector: aSelectorToCategorize

	self selectedClassOrMetaClass ifNotNil: [ :class | | categorySelector |
		categorySelector _ self messageCategoryList at: aMessageCategoryListIndex ifAbsent: [^self].
		categorySelector ~= Categorizer allCategory
			ifTrue: [
				class organization classify: aSelectorToCategorize under: categorySelector suppressIfDefault: false.
				self changed: #messageList]]! !

!Browser methodsFor: 'message category functions' stamp: 'HAW 12/27/2018 13:10:15'!
categorizeUnderNewCategorySelector: aSelectorToCategorize

	self selectedClassOrMetaClass ifNotNil: [ :class | | newCategory |
		newCategory _ self newCategoryNameIfNone: [ ^self ].
		class organization 
			addCategory: newCategory;
			classify: aSelectorToCategorize under: newCategory suppressIfDefault: false.
			
		self changed: #messageCategoryList.
		self changed: #messageList]! !

!Browser methodsFor: 'system category functions' stamp: 'HAW 12/27/2018 13:30:46'!
categorizeUnderCategoryAt: aSystemCategoryIndex class: aClassName 

	systemOrganizer classify: aClassName under: (self systemCategoryList at: aSystemCategoryIndex).
	self changed: #classList! !

!Browser methodsFor: 'system category functions' stamp: 'HAW 12/27/2018 13:39:38'!
categorizeUnderNewCategoryClass: aClassName

	| newCategory |
	
	newCategory _ self newCategoryNameIfNone: [ ^self ].
	
	systemOrganizer
		addCategory: newCategory;
		classify: aClassName under: newCategory.
		
	self changed: #systemCategoryList.
	self changed: #classList.! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'HAW 12/27/2018 13:14:38'!
buildMorphicClassColumnWith: classList

	| column |
	
	column _ LayoutMorph newColumn.
	column
		addMorphUseAll: classList;
		addAdjusterAndMorph: self buildMorphicSwitches fixedHeight: (Theme current minimalWindows ifTrue: [AbstractFont default height + 4] ifFalse: [AbstractFont default height *2-4]).
		
	^column! !


!Browser methodsFor: 'system category functions' stamp: 'HAW 12/27/2018 13:37:37'!
addSystemCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"
	
	| oldIndex newName |
	
	oldIndex _ self systemCategoryListIndex.
	newName _ self newCategoryNameIfNone: [ ^self ].
	
	systemOrganizer
		addCategory: newName
		before: selectedSystemCategory.
	self systemCategoryListIndex:
		(oldIndex = 0
			ifTrue: [self systemCategoryList size]
			ifFalse: [oldIndex]).
	self changed: #systemCategoryList.! !

!Browser methodsFor: 'system category functions' stamp: 'HAW 12/27/2018 13:38:14'!
newCategoryNameIfNone: aNoneBlock

	| newName |
	
	newName _ self
		request: 'Please type new category name'
		initialAnswer: 'Category-Name'.
	
	^newName isEmpty
		ifTrue: aNoneBlock 
		ifFalse: [newName asSymbol].! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'HAW 12/27/2018 13:15:08'!
buildMorphicClassColumn
	
	^self buildMorphicClassColumnWith: self buildMorphicClassList! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'HAW 12/27/2018 13:16:43'!
buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes messageCatList messageList systemCatList classList |

	systemCatList _ self buildMorphicSystemCatList.
	classList _ self buildMorphicClassList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	classList makeItemsDraggable.
	systemCatList 
		acceptDropsFrom: classList 
		performing: #categorizeUnderCategoryAt:class:
		whenOutsideList: #categorizeUnderNewCategoryClass:.
		
	messageList makeItemsDraggable.
	messageCatList 
		acceptDropsFrom: messageList 
		performing: #categorizeUnderCategoryAt:selector: 
		whenOutsideList: #categorizeUnderNewCategorySelector:.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: systemCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: (self buildMorphicClassColumnWith: classList) proportionalWidth: 0.2;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !


!Browser reorganize!
('accessing' acceptedStringOrText contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit)
('class list' classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically listClassesHierarchically: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categorizeUnderCategoryAt:selector: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategoryAt:class: categorizeUnderNewCategoryClass: changeSystemCategories: editSystemCategories fileOutSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newCategoryNameIfNone: potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('*Refactoring' insertSuperclass isEditingMethod removeClass renameClass safelyRemoveClass)
('*DenotativeObject' compile:receiver:in: debug debug:receiver:in:withResultDo: debugAndInspect debugAndWithResultDo: debugDenotativeObjectMethodTest isTest: runDenotativeObjectMessageCategoryTests runDenotativeObjectMethodTest runDenotativeObjectSystemCategoryTests runDenotativeObjectTests send sendAndInspect sendAndWithResultDo: stringToEvalSending:to:onCancel:)
('*LiveTypeInformation' classDefinitionText classOfInstVarNamed: classOfTempVarNamed: contents:notifying: selectedMessage)
!


!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumn buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCodePane buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCatList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCommentButton createInstanceButton windowColor)
('menu building' addExtraMenu2ItemsTo: classListMenu classListMenu2 messageCategoryMenu messageListMenu messageListMenu2 systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses openSystemCategoryBrowser)
('keyboard shortcuts' systemCatListKey:from: systemCatSingletonKey:from:)
('updating' disableCodePaneEditing editSelectionChanged enableCodePaneEditing isEditSelectionNone update:)
('*Refactoring' addInstVar addParameter classRefactoringMenu messageRefactoringMenu openClassRefactoringMenu openMessageRefactoringMenu pushUpSelector removeAllUnreferencedInstVar removeInstVar removeParameter renameInstVar renameSelector)
('*DenotativeObject-Tools' classNameRequester findClass findClassDialogTitle)
!

