'From Cuis 5.0 [latest update: #4689] on 22 July 2021 at 6:36:33 pm'!
!classDefinition: #MethodCategoriesPrompter category: #'System-Text'!
Object subclass: #MethodCategoriesPrompter
	instanceVariableNames: 'startClass rejectingFirst prompt categories lines reject'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!MethodCategoriesPrompter methodsFor: 'initialization' stamp: 'HAW 7/22/2021 17:12:06'!
initializeStaringFrom: aClass rejectingFirst: aRejectingFirst prompting: aPrompt 

	startClass := aClass.
	rejectingFirst := aRejectingFirst.
	prompt := aPrompt ! !

!MethodCategoriesPrompter methodsFor: 'value' stamp: 'HAW 7/22/2021 18:31:08'!
valueIfNone: aNoneBlock

	| selectedCategoryIndex categoryName |
	
	self initializeCategories.
	
	selectedCategoryIndex := self promptCategory.
	selectedCategoryIndex = 0 ifTrue: [^ aNoneBlock value].
	
	categoryName := selectedCategoryIndex = 1 ifTrue: [ self requestNewCategory ] ifFalse: [ categories at: selectedCategoryIndex ].
	categoryName isEmpty ifTrue: [ ^aNoneBlock value ].
	
	^categoryName
	! !

!MethodCategoriesPrompter methodsFor: 'categories initialization - private' stamp: 'HAW 7/22/2021 18:16:53'!
addCategories

	startClass allSuperclasses do: [ :superclass | self addCategoriesOf: superclass ]! !

!MethodCategoriesPrompter methodsFor: 'categories initialization - private' stamp: 'HAW 7/22/2021 18:20:44'!
addCategoriesOf: aSuperclass

	| superclassCategories |
	
	superclassCategories := aSuperclass methodCategoriesAsSortedCollection reject: [ :category | reject includes: category].
	
	superclassCategories isEmpty ifFalse: [
		lines add: categories size.
		categories addAll: superclassCategories.
		reject addAll: superclassCategories]! !

!MethodCategoriesPrompter methodsFor: 'categories initialization - private' stamp: 'HAW 7/22/2021 18:30:11'!
createCategories

	| classCategories |
	
	categories := OrderedCollection with: 'new ...'.
	lines := OrderedCollection with: 1. 
	classCategories := startClass methodCategoriesAsSortedCollection.
	
	reject := classCategories asSet.
	reject
		add: ClassOrganizer nullCategory;
		add: ClassOrganizer default.
	
	startClass isMeta ifTrue: [ self initializeCategoriesWhenMeta: classCategories].
	rejectingFirst ifFalse: [ categories addAll: classCategories ]! !

!MethodCategoriesPrompter methodsFor: 'categories initialization - private' stamp: 'HAW 7/22/2021 18:30:11'!
initializeCategories
	
	self 
		createCategories;
		addCategories! !

!MethodCategoriesPrompter methodsFor: 'categories initialization - private' stamp: 'HAW 7/22/2021 18:21:19'!
initializeCategoriesWhenMeta: classCategories

	categories add: Categorizer instanceCreation.
	classCategories remove: Categorizer instanceCreation ifAbsent: [].
	reject add: Categorizer instanceCreation! !

!MethodCategoriesPrompter methodsFor: 'prompting - private' stamp: 'HAW 7/22/2021 18:21:47'!
promptCategory
	
	| selectedLabelIndex |
	
	selectedLabelIndex := categories size = 1 
		ifTrue: [ 1 ]
	 	ifFalse: [ (PopUpMenu labelArray: categories lines: lines) startUpWithCaption: prompt ].
	
	^selectedLabelIndex! !

!MethodCategoriesPrompter methodsFor: 'prompting - private' stamp: 'HAW 7/22/2021 17:18:46'!
requestNewCategory
	
	^self request: 'New category name?' initialAnswer: 'category-name'! !


!MethodCategoriesPrompter class methodsFor: 'instance creation' stamp: 'HAW 7/22/2021 17:10:52'!
staringFrom: aClass rejectingFirst: rejectingFirst prompting: aPrompt 
	
	^self new initializeStaringFrom: aClass rejectingFirst: rejectingFirst prompting: aPrompt 
! !


!CodeProvider methodsFor: 'categories' stamp: 'HAW 7/22/2021 17:09:38'!
categoryFromUserWithPrompt: aPrompt for: aClass
	"self new categoryFromUserWithPrompt: 'testing' for: SystemDictionary"

	^(MethodCategoriesPrompter 
		staringFrom: aClass 
		rejectingFirst: false
		prompting: aPrompt) valueIfNone: [ nil ]! !


!Browser methodsFor: 'message category functions' stamp: 'HAW 7/22/2021 17:09:55'!
newMethodCategoryNameIfNone: aNoneBlock
	
	^(MethodCategoriesPrompter 
		staringFrom: self selectedClassOrMetaClass 
		rejectingFirst: true
		prompting: 'Add Category') valueIfNone: aNoneBlock! !


!Debugger methodsFor: 'context stack menu' stamp: 'HAW 7/22/2021 18:24:24'!
askForCategoryIn: aClass default: aDefaultCategory

	^(MethodCategoriesPrompter 
		staringFrom: aClass 
		rejectingFirst: false
		prompting: 'Select category for the new method' ) valueIfNone: [ aDefaultCategory ]! !

!methodRemoval: MethodCategoriesPrompter #initializeCategoriesAndLines stamp: 'HAW 7/22/2021 18:30:03'!
MethodCategoriesPrompter removeSelector: #initializeCategoriesAndLines!
!methodRemoval: MethodCategoriesPrompter #createCategoriesAndLines stamp: 'HAW 7/22/2021 18:30:11'!
MethodCategoriesPrompter removeSelector: #createCategoriesAndLines!
!methodRemoval: MethodCategoriesPrompter #calculateCategoriesAndLines stamp: 'HAW 7/22/2021 18:29:37'!
MethodCategoriesPrompter removeSelector: #calculateCategoriesAndLines!
!methodRemoval: MethodCategoriesPrompter #addMethodCategories stamp: 'HAW 7/22/2021 18:16:44'!
MethodCategoriesPrompter removeSelector: #addMethodCategories!
!methodRemoval: MethodCategoriesPrompter #selectedIndexFrom:separatedWith:propting: stamp: 'HAW 7/22/2021 18:20:14'!
MethodCategoriesPrompter removeSelector: #selectedIndexFrom:separatedWith:propting:!
!methodRemoval: MethodCategoriesPrompter #selectCategory stamp: 'HAW 7/22/2021 18:21:47'!
MethodCategoriesPrompter removeSelector: #selectCategory!
!methodRemoval: MethodCategoriesPrompter #addMethodCategoriesOf: stamp: 'HAW 7/22/2021 18:16:53'!
MethodCategoriesPrompter removeSelector: #addMethodCategoriesOf:!
!methodRemoval: MethodCategoriesPrompter #initializeLabelsAndLines stamp: 'HAW 7/22/2021 18:20:55'!
MethodCategoriesPrompter removeSelector: #initializeLabelsAndLines!
!methodRemoval: MethodCategoriesPrompter #initializeLabelsWhenClassIsMeta: stamp: 'HAW 7/22/2021 18:21:19'!
MethodCategoriesPrompter removeSelector: #initializeLabelsWhenClassIsMeta:!
!methodRemoval: Debugger #addCategoriesStartingFrom:to:separatingWith: stamp: 'HAW 7/22/2021 15:44:09'!
Debugger removeSelector: #addCategoriesStartingFrom:to:separatingWith:!
!methodRemoval: Debugger #addCategoriesStartingFrom:to:separatingWith:rejectingStartingCategories: stamp: 'HAW 7/22/2021 15:44:17'!
Debugger removeSelector: #addCategoriesStartingFrom:to:separatingWith:rejectingStartingCategories:!
!methodRemoval: Debugger #addCategoriesOf:to:separatingWith: stamp: 'HAW 7/22/2021 15:42:23'!
Debugger removeSelector: #addCategoriesOf:to:separatingWith:!
!methodRemoval: CodeProvider #requestNewCategory stamp: 'HAW 7/22/2021 18:27:33'!
CodeProvider removeSelector: #requestNewCategory!
!methodRemoval: CodeProvider #addMethodCategoriesStartingFrom:to:separatingWith:rejecting: stamp: 'HAW 7/22/2021 18:26:36'!
CodeProvider removeSelector: #addMethodCategoriesStartingFrom:to:separatingWith:rejecting:!
!methodRemoval: CodeProvider #addCategoriesStartingFrom:rejectingStartingCategories: stamp: 'HAW 7/22/2021 16:50:21'!
CodeProvider removeSelector: #addCategoriesStartingFrom:rejectingStartingCategories:!
!methodRemoval: CodeProvider #selectCategoryFrom:propting:ifNone: stamp: 'HAW 7/22/2021 18:27:43'!
CodeProvider removeSelector: #selectCategoryFrom:propting:ifNone:!
!methodRemoval: CodeProvider #selectedIndexFrom:separatedWith:propting: stamp: 'HAW 7/22/2021 18:27:22'!
CodeProvider removeSelector: #selectedIndexFrom:separatedWith:propting:!
!methodRemoval: CodeProvider #methodCategoriesStartingFrom:rejectingFirst: stamp: 'HAW 7/22/2021 18:26:47'!
CodeProvider removeSelector: #methodCategoriesStartingFrom:rejectingFirst:!
!methodRemoval: CodeProvider #addCategoriesOf:to:separatingWith: stamp: 'HAW 7/22/2021 15:42:23'!
CodeProvider removeSelector: #addCategoriesOf:to:separatingWith:!
!methodRemoval: CodeProvider #addCategoriesStartingFrom:to:separatingWith: stamp: 'HAW 7/22/2021 15:44:09'!
CodeProvider removeSelector: #addCategoriesStartingFrom:to:separatingWith:!
!methodRemoval: CodeProvider #addCategoriesStartingFrom:to:separatingWith:rejectingStartingCategories: stamp: 'HAW 7/22/2021 16:46:01'!
CodeProvider removeSelector: #addCategoriesStartingFrom:to:separatingWith:rejectingStartingCategories:!
!methodRemoval: CodeProvider #addCategoriesStartingFrom:separatingWith:rejectingStartingCategories: stamp: 'HAW 7/22/2021 16:46:13'!
CodeProvider removeSelector: #addCategoriesStartingFrom:separatingWith:rejectingStartingCategories:!
!methodRemoval: CodeProvider #selectCategoryFrom:separatedWith:propting:ifNone: stamp: 'HAW 7/22/2021 16:49:39'!
CodeProvider removeSelector: #selectCategoryFrom:separatedWith:propting:ifNone:!

!CodeProvider reorganize!
('annotation' addPriorVersionsCountForSelector:ofClass:to: annotation annotationForClassCommentFor: annotationForClassDefinitionFor: annotationForHierarchyFor: annotationForSelector:ofClass: annotationRequests annotationSeparator)
('categories' categoryFromUserWithPrompt:for: categoryOfCurrentMethod changeCategory letUserReclassify:in: methodCategoryChanged selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText commentContents contentsSymbol contentsSymbol: currentMethodRefactored methodNodeOf:ifErrorsParsing:)
('commands' hierarchyBrowser removeClass)
('controls' contentsSymbolQuints sourceAndDiffsQuintsOnly)
('diffs' defaultDiffsSymbol diffFromPriorSourceFor: shouldDiffWords showDiffs: showLineDiffs: showPrettyLineDiffs: showPrettyWordDiffs: showWordDiffs: showingAnyKindOfDiffs showingAnyKindOfPrettyDiffs showingLineDiffs showingLineDiffsString showingPrettyLineDiffs showingPrettyLineDiffsString showingPrettyWordDiffs showingPrettyWordDiffsString showingWordDiffs showingWordDiffsString toggleDiffing toggleLineDiffing togglePlainSource togglePrettyLineDiffing togglePrettyPrint togglePrettyWordDiffing toggleWordDiffing)
('misc' isThereAnOverride isThisAnOverride okayToAccept priorSourceOrNil releaseCachedState)
('self-updating' didCodeChangeElsewhere updateIfNeeded)
('what to show' prettyPrintString showByteCodes: showComment showDecompile: showDocumentation: showingByteCodes showingByteCodesString showingDecompile showingDecompileString showingDocumentation showingDocumentationString showingPlainSource showingPlainSourceString showingPrettyPrint showingSource toggleDecompile toggleShowDocumentation toggleShowingByteCodes)
('message list' decompiledSource selectedBytecodes selectedMessage sourceStringPrettifiedAndDiffed)
('message list menu' copySelector fileOutMessage inspectCompiledMethod revertToPreviousVersion timeStamp unusedMethods)
('message category functions' canShowMultipleMessageCategories)
('testing' is: isEditingMethod)
('accessing' currentCompiledMethod selectedClass selectedClassOrMetaClass selectedMessageName)
('evaluation' doItContext)
('shout styling' isModeStyleable shouldStyle:with:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
!


!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit)
('class list' classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically listClassesHierarchically: originalSelectedClassName recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categorizeUnderCategoryAt:selector: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories newMethodCategoryNameIfNone: removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategoryAt:class: categorizeUnderNewCategoryClass: changeSystemCategories: editSystemCategories fileOutSystemCategory moveAllToOtherSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newSystemCategoryNameIfNone: newSystemCategoryNameInitialAnswer potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('refactorings' insertSuperclass removeClass renameClass renameGlobal safelyRemoveClass)
('testing' is: isEditingClass isEditingExistingClass isEditingMethod isEditingNewClass)
!


!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context: resetToSelectedContextWith:)
('auto complete' classOfThisContext computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('breakpoints' toggleBreakOnEntry)
('class list' selectedClass)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('contents' currentMethodRefactored)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack handleLabelUpdatesIn:whenExecuting: peelToFirst proceed restart returnValue: selectPC send stepIntoBlock up where)
('dependents access' updateInspectors wantsSteps)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:withCaption:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUnderstand implement:inCategory:fromClass:context: implement:inClass:context: overrideMethodOnSubclass shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnOverridableMethod wasInterruptedOnSubclassResponsibility)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('testing' is: isEditingMethod)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: fixReceiverInspector lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
!


!MethodCategoriesPrompter reorganize!
('initialization' initializeStaringFrom:rejectingFirst:prompting:)
('value' valueIfNone:)
('categories initialization - private' addCategories addCategoriesOf: createCategories initializeCategories initializeCategoriesWhenMeta:)
('prompting - private' promptCategory requestNewCategory)
!

