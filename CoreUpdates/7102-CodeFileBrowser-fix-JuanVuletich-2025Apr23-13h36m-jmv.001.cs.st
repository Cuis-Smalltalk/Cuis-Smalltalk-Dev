'From Cuis7.3 [latest update: #7101] on 23 April 2025 at 1:54:34 pm'!

!CodeFileBrowser methodsFor: 'testing' stamp: 'jmv 4/23/2025 13:39:03'!
isEditingExistingClass

	^false! !

!CodeFileBrowser methodsFor: 'testing' stamp: 'jmv 4/23/2025 13:41:19'!
isEditingExistingMethod

	^false! !

!CodeFileBrowser methodsFor: 'testing' stamp: 'jmv 4/23/2025 13:39:37'!
isEditingMethod

	^false! !

!CodeFileBrowser methodsFor: 'testing' stamp: 'jmv 4/23/2025 13:42:58'!
isEditingNewClass

	^false! !


!SmalltalkCompleter methodsFor: 'entries - private' stamp: 'jmv 4/23/2025 13:42:29'!
parse: source in: contextClass and: specificModel
	
	| isMethod |
	parser := SHParserST80 new.
	parser
		workspace: ((specificModel is: #providesBindings) ifTrue: [specificModel]);
		classOrMetaClass: contextClass;
		source: source.

	isMethod := (specificModel is: #Browser)
		ifTrue: [ specificModel isEditingMethod ]
		ifFalse: [ specificModel is: #CodeProvider ].
	parser parse: isMethod.

	^ parser rangesWithoutExcessCode.! !

!methodRemoval: CodeFileBrowser #isEditingNone stamp: 'jmv 4/23/2025 13:44:20'!
CodeFileBrowser removeSelector: #isEditingNone!

!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass fileOutClassAlphabetically makeNewSubclass plusButtonHit)
('class list' basicClassList classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: createHierarchyTreeOf:comparingWith: enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically: recent selectClass: selectedClass)
('code pane' compileMessage:notifying:)
('message category functions' addCategory addExtensionCategory alphabetizeMessageCategories categorizeUnderCategoryAt:selector: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories newMethodCategoryNameIfNone: removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser removeMessageInHierarchy)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName: selector:refactoredTo:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategory:class: changeSystemCategories: editSystemCategories fileOutSystemCategory moveAllToOtherSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newSystemCategoryNameIfNone: newSystemCategoryNameInitialAnswer potentialClassNames removeSystemCategory renameSystemCategory renameSystemCategoryNamed: systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectedSystemCategory setSelectedSystemCategory: setSelectedSystemCategoryTreeItem: systemCategoryList systemCategoryRoots systemCategorySingleton)
('shout styling' aboutToStyleWith: allowsStylingFor:isTextDirty:)
('initialization' defaultBrowserTitle initialize labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('running tests' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('refactorings' addAsSubclassResponsibilitySelector addInstVar addParameter changeKeywordOrder inlineMethod insertSuperclass moveToInstanceOrClassMethod pushDownInstanceVariable pushDownSelectorToOneSubclass pushDownSelectorToSubclasses pushUpInstanceVariable pushUpSelector removeAllUnreferencedInstVar removeClass removeInstVar removeParameter renameClass renameGlobal renameInstVar renameSelector safelyRemoveClass)
('testing' is: isEditingClass isEditingExistingClass isEditingExistingMethod isEditingMethod isEditingNewClass isEditingNone)
('self-updating' classRenamed:from:to:inCategory: updateIfNeeded)
('refactorings - category' addCategoryAsSubclassResponsibilitySelectors pushDownCategorySelectorsToOneSubclass pushDownCategorySelectorsToSubclasses pushUpCategorySelectors)
!


!CodeFileBrowser reorganize!
('accessing' acceptedStringOrText baseCodeSource baseCodeSource: caseCodeSource caseCodeSource: contents:notifying: contentsSymbol)
('class list' classList createHierarchyTreeOf: hierarchicalClassList renameClass selectedClass)
('diffs' methodDiffFor:selector: modifiedClassDefinition)
('edit pane' selectedBytecodes selectedMessage)
('fileIn/fileOut' fileIn fileInClass fileInMessage fileInMessageCategories fileOut fileOutClass fileOutMessage fileOutMessageCategories)
('infoView' annotation extraInfo infoViewContents updateInfoView)
('message list' messageList)
('metaclass' selectedClassOrMetaClass setClassOrganizer)
('other' didCodeChangeElsewhere labelString)
('removing' removeClass removeMessage removeMessageCategory removeUnmodifiedCategories removeUnmodifiedClasses removeUnmodifiedMethods)
('shout styling' allowsStylingFor:isTextDirty:)
('private' pvtBaseClassOrMetaclass pvtBaseSelectedMessageSourceCodeFor: pvtCaseSelectedMessageSourceCodeFor:)
('controls' whatToShowSubmenuSpec)
('change list' browseCodeFileChanges)
('testing' isEditingExistingClass isEditingExistingMethod isEditingMethod isEditingNewClass)
!

