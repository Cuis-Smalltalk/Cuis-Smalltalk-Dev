'From Cuis7.1 [latest update: #6479] on 18 June 2024 at 2:29:30 pm'!

!Browser methodsFor: 'class list' stamp: 'jmv 6/18/2024 11:10:08'!
selectedClass
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	^selectedClassName ifNotNil: [
		Smalltalk at: selectedClassName ifAbsent: nil ]! !

!Browser methodsFor: 'running tests' stamp: 'jmv 6/18/2024 11:11:52'!
runClassTests

	self selectedClass ifNotNil: [ :selectedClass |
		(ProgressiveTestRunner for: (TestSuite forClass: selectedClass)) value ].
! !


!CodeFileBrowser methodsFor: 'class list' stamp: 'jmv 6/18/2024 14:19:47'!
selectedClass
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	^selectedClassName ifNotNil: [
		caseCodeSource classNamed: selectedClassName ]! !

!methodRemoval: HierarchyBrowser #selectedClassName stamp: 'jmv 6/18/2024 14:28:38'!
HierarchyBrowser removeSelector: #selectedClassName!
!methodRemoval: Browser #selectedClassName stamp: 'jmv 6/18/2024 14:28:45'!
Browser removeSelector: #selectedClassName!

!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass makeNewSubclass plusButtonHit)
('class list' classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically: recent selectClass: selectedClass)
('code pane' compileMessage:notifying:)
('message category functions' addCategory addExtensionCategory alphabetizeMessageCategories categorizeUnderCategoryAt:selector: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories newMethodCategoryNameIfNone: removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser removeMessageInHierarchy)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName: selector:refactoredTo:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategory:class: changeSystemCategories: editSystemCategories fileOutSystemCategory moveAllToOtherSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newSystemCategoryNameIfNone: newSystemCategoryNameInitialAnswer potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectedSystemCategory setSelectedSystemCategory: setSelectedSystemCategoryTreeItem: systemCategoryList systemCategoryRoots systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('running tests' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('refactorings' addAsSubclassResponsabilitySelector addInstVar addParameter changeKeywordOrder inlineMethod insertSuperclass moveToInstanceOrClassMethod pushDownInstanceVariable pushDownSelectorToOneSubclass pushDownSelectorToSubclasses pushUpInstanceVariable pushUpSelector removeAllUnreferencedInstVar removeClass removeInstVar removeParameter renameClass renameGlobal renameInstVar renameSelector safelyRemoveClass)
('testing' is: isEditingClass isEditingExistingClass isEditingMethod isEditingNewClass isEditingNone)
('self-updating' classRenamed:from:to:inCategory: updateIfNeeded)
!

