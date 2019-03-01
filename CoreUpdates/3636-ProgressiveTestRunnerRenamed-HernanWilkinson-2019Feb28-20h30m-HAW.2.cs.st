'From Cuis 5.0 of 7 November 2016 [latest update: #3634] on 28 February 2019 at 8:31:40 pm'!
Smalltalk renameClassNamed: #ProgessiveTestRunner as: #ProgressiveTestRunner!

!Browser methodsFor: 'tdd' stamp: 'HAW 2/28/2019 20:30:14'!
runClassTests
	
	self selectedClassName ifNotNil: [ :aClassName | | selectedClass |
		selectedClass _ Smalltalk classNamed: aClassName.
		(ProgressiveTestRunner for: (TestSuite forClass: selectedClass)) value ]! !

!Browser methodsFor: 'tdd' stamp: 'HAW 2/28/2019 20:30:14'!
runMessageCategoryTests
	
	selectedMessageCategory ifNotNil: [ | selectedClass suite |
		selectedClass _ Smalltalk classNamed: selectedClassName.
		suite _ TestSuite forMessageCategoryNamed: selectedMessageCategory of: selectedClass categorizedWith: classOrganizer.
		(ProgressiveTestRunner for: suite) value ]
	
	! !

!Browser methodsFor: 'tdd' stamp: 'HAW 2/28/2019 20:30:14'!
runMethodTest
	
	| suite |
	
	suite _ TestSuite forCompiledMethod: currentCompiledMethod.
	(ProgressiveTestRunner for: suite) value 
	! !

!Browser methodsFor: 'tdd' stamp: 'HAW 2/28/2019 20:30:14'!
runSystemCategoryTests
	
	selectedSystemCategory ifNotNil: [ | suite |
		suite _ TestSuite forSystemCategoryNamed: selectedSystemCategory using: systemOrganizer.
		(ProgressiveTestRunner for: suite) value ]
	
	! !


!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 2/28/2019 20:30:14'!
runTestSuite: aTestSuite

	(ProgressiveTestRunner for: aTestSuite) value
	
	! !


!TestResultWindow methodsFor: 'actions' stamp: 'HAW 2/28/2019 20:30:14'!
runSuite

	| suite |
	 
	suite := TestSuite new.
	suite addTests: testResult tests.
	self delete.
	(ProgressiveTestRunner for: suite) value.
	! !


!ProgressiveTestRunner methodsFor: 'show result - private' stamp: 'HAW 2/28/2019 20:30:59'!
showDeffects
	
	 | defects |
			
	defects _ testResult defects.
	defects size = 1
		ifTrue: [ defects anyOne debug ] 
		ifFalse: [ self openTestResultWindow]! !


!Browser reorganize!
('accessing' acceptedStringOrText contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit removeClass renameClass)
('class list' classList classListIndex classListIndex: classListIndexOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically listClassesHierarchically: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying: contents:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categorizeUnderCategoryAt:selector: categorizeUnderNewCategorySelector: categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories newMethodCategoryNameIfNone: removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories categorizeUnderCategoryAt:class: categorizeUnderNewCategoryClass: changeSystemCategories: editSystemCategories fileOutSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp newSystemCategoryNameIfNone: potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
!


!SmalltalkEditor reorganize!
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt printIt profileIt)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorUnderCursorDo:ifErrorsParsing: withSelectorUnderCursorDo:ifErrorsParsing:ifNoSelector: withSelectorUnderCursorDo:otherwise:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier selectWord)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('menu' createMenuCollectingOptionsWith: getMenu getMenu2 openMenu openMenu2 openMenu:)
!


!TestResultWindow reorganize!
('actions' debug runSuite)
('button creation' createDebugButton createReRunButton)
('GUI building' addButton:to:color: addButtonsTo:color: buildLowerPanes buttonsRow)
('initialization' initializeFor:)
('testing' isMessageSelected)
!

