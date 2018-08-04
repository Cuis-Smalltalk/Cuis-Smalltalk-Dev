'From Cuis 5.0 of 7 November 2016 [latest update: #3389] on 3 August 2018 at 2:54:59 pm'!
!classDefinition: #Browser category: #'Tools-Browser'!
CodeProvider subclass: #Browser
	instanceVariableNames: 'systemOrganizer classOrganizer metaClassOrganizer selectedSystemCategory selectedClassName selectedMessageCategory selectedMessage editSelection metaClassIndicated listClassesHierarchically '
	classVariableNames: 'RecentClasses '
	poolDictionaries: ''
	category: 'Tools-Browser'!

!Browser methodsFor: 'class list' stamp: 'HAW 8/3/2018 12:29:41'!
enableListClassesAlphabetically

	self listClassesHierarchically: false! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/3/2018 12:29:30'!
enableListClassesHierarchically

	self listClassesHierarchically: true! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/3/2018 14:49:36'!
listClassesHierarchically: aBoolean

	listClassesHierarchically := aBoolean.
	
	self changed: #classList.
	self changed: #classListIndex.
! !

!Browser methodsFor: 'initialization' stamp: 'HAW 8/3/2018 14:51:55'!
initialize

	super initialize.
	self initializeListClassesHierachically! !

!Browser methodsFor: 'initialization' stamp: 'HAW 8/3/2018 14:51:47'!
initializeListClassesHierachically

	listClassesHierarchically _ self class listClassesHierarchically ! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'HAW 8/3/2018 11:59:30'!
createClassButton

	| classSwitch |
	
	classSwitch := PluggableButtonMorph 
		model: model
		stateGetter: #classMessagesIndicated
		action: #indicateClassMessages.
	
	classSwitch
		label: 'class';
		setBalloonText: 'show class methods'.
		
	^classSwitch! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'HAW 8/3/2018 11:59:41'!
createCommentButton

	|  commentSwitch  |
	
	commentSwitch := PluggableButtonMorph 
		model: model
		stateGetter: #classCommentIndicated
		action: #plusButtonHit.
				
	commentSwitch 
		label: '?';
		setBalloonText: 'show class comment'.
		
	^commentSwitch ! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'HAW 8/3/2018 11:59:22'!
createInstanceButton

	| instanceSwitch |
	
	instanceSwitch := PluggableButtonMorph 
		model: model
		stateGetter: #instanceMessagesIndicated
		action: #indicateInstanceMessages.

	instanceSwitch 
		label: 'instance';
		setBalloonText: 'show instance methods'.

	^instanceSwitch ! !


!Browser methodsFor: 'class list' stamp: 'HAW 8/3/2018 14:52:35'!
listClassesHierarchically 

	"I check for nil to support migration on already opened browser when the change is loaded in image - Hernan"
	^listClassesHierarchically ifNil: [ self initializeListClassesHierachically]! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'HAW 8/3/2018 14:54:25'!
buildMorphicSwitches

	| instanceSwitch commentSwitch classSwitch row buttonColor |
	
	instanceSwitch _ self createInstanceButton.
	commentSwitch _ self createCommentButton.
	classSwitch _ self createClassButton.
	
	row _ LayoutMorph newRow.
	row
		doAdoptWidgetsColor;
		addMorph: instanceSwitch proportionalWidth: 0.45;
		addMorph: commentSwitch proportionalWidth: 0.22;
		addMorph: classSwitch proportionalWidth: 0.33.
	buttonColor _ self buttonColor.
	row color: buttonColor.

	{ 
		instanceSwitch.
		commentSwitch.
		classSwitch} do: [:m |  m color: buttonColor ].
		
	^row! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'HAW 8/3/2018 14:47:08'!
classListMenuOptions

	^ `{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 	-> 		20.
				#itemOrder 		-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutClass.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		5.
				#label 			-> 		'show classes hierarchically'.
				#object 			-> 		#model.
				#selector 		-> 		#enableListClassesHierarchically.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		6.
				#label 			-> 		'show classes alphabetically'.
				#object 			-> 		#model.
				#selector 		-> 		#enableListClassesAlphabetically.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		10.
				#label 			-> 		'show hierarchy'.
				#object 			-> 		#model.
				#selector 		-> 		#hierarchy.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		20.
				#label 			-> 		'show definition'.
				#object 			-> 		#model.
				#selector 		-> 		#editClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 	-> 		30.
				#itemOrder 		-> 		30.
				#label 			-> 		'show comment'.
				#object 			-> 		#model.
				#selector 		-> 		#editComment.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 		-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		40.
				#itemOrder 		-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 		-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		10.
				#label 			-> 		'class var refs...'.
				#selector 		-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		20.
				#label 			-> 		'class vars'.
				#selector 		-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		50.
				#itemOrder 		-> 		30.
				#label 			-> 		'class refs (N)'.
				#selector 		-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename class ...'.
				#object 			-> 		#model.
				#selector 		-> 		#renameClass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		20.
				#label 			-> 		'copy class...'.
				#object 			-> 		#model.
				#selector 		-> 		#copyClass.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#itemGroup 	-> 		60.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove class (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 	-> 		70.
				#itemOrder 		-> 		10.
				#label 			-> 		'run tests (t)'.
				#object 			-> 		#model.
				#selector 		-> 		#runClassTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		70.
				#itemOrder 		-> 		20.
				#label 			-> 		'more...'.
				#selector 		-> 		#offerClassListMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	! !

!methodRemoval: HierarchyBrowserWindow #buildMorphicSwitches!
HierarchyBrowserWindow removeSelector: #buildMorphicSwitches!
!methodRemoval: BrowserWindow #createListClassesAlphabetically!
BrowserWindow removeSelector: #createListClassesAlphabetically!
!methodRemoval: BrowserWindow #createListClassesAlphabeticallyButton!
BrowserWindow removeSelector: #createListClassesAlphabeticallyButton!
!methodRemoval: BrowserWindow #createListClassesHierarchically!
BrowserWindow removeSelector: #createListClassesHierarchically!
!methodRemoval: BrowserWindow #createListClassesHierarchicallyButton!
BrowserWindow removeSelector: #createListClassesHierarchicallyButton!
!methodRemoval: Browser #listClassesAlphabetically!
Browser removeSelector: #listClassesAlphabetically!
!classDefinition: #Browser category: #'Tools-Browser'!
CodeProvider subclass: #Browser
	instanceVariableNames: 'systemOrganizer classOrganizer metaClassOrganizer selectedSystemCategory selectedClassName selectedMessageCategory selectedMessage editSelection metaClassIndicated listClassesHierarchically'
	classVariableNames: 'RecentClasses'
	poolDictionaries: ''
	category: 'Tools-Browser'!

!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit)
('class list' classList classListIndex classListIndex: classListIndexOf: classListIndexWhenShowingHierarchicallyOf: createHierarchyTreeOf: defaultClassList enableListClassesAlphabetically enableListClassesHierarchically flattenHierarchyTree:on:indent: flattenHierarchyTree:on:indent:by: hierarchicalClassList listClassesHierarchically listClassesHierarchically: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories changeSystemCategories: editSystemCategories fileOutSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shouldStyle:with:)
('initialization' defaultBrowserTitle initialize initializeListClassesHierachically labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
('*Refactoring' isEditingMethod removeClass renameClass safelyRemoveClass)
('*DenotativeObject' compile:receiver:in: debug debug:receiver:in:withResultDo: debugAndInspect debugAndWithResultDo: debugDenotativeObjectMethodTest isTest: runDenotativeObjectMessageCategoryTests runDenotativeObjectMethodTest runDenotativeObjectSystemCategoryTests runDenotativeObjectTests send sendAndInspect sendAndWithResultDo: stringToEvalSending:to:onCancel:)
!


!HierarchyBrowserWindow reorganize!
('keyboard shortcuts' systemCatSingletonKey:from:)
('GUI building')
!

