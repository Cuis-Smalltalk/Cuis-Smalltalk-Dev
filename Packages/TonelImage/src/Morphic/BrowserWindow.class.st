"
A morphic window for Browser models. See category 'GUI building'.

(BrowserWindow new model: Browser new; buildMorphicWindow) openInWorld

instead of

Browser openBrowser


----
No dejar en clases de modelo comportamiento que se haya implementado aca!!!!
el unico protocolo repetido deberia ser delegacion
"
Class {
	#name : 'BrowserWindow',
	#superclass : 'CodeWindow',
	#instVars : [
		'codePane'
	],
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'services' }
BrowserWindow class >> classNameRequester  [

	^ClassNameRequestMorph
]

{ #category : 'services' }
BrowserWindow class >> classNamesFrom: potentialClassNames with: pattern matching: toMatch  [

	^ (pattern last == $. or: [pattern last == $ ])
		ifTrue: [potentialClassNames select: [:className | className asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: [:className | className includesSubstring: toMatch caseSensitive: false]]
]

{ #category : 'instance creation' }
BrowserWindow class >> findAndBrowseClass [

	| allClassNames |
	allClassNames := Smalltalk classNames asOrderedCollection.
	self findClassFrom: allClassNames ifFound: [ :foundClass | self fullOnClass: foundClass selector: nil ].
 	
	
]

{ #category : 'services' }
BrowserWindow class >> findClassFrom: potentialClassNames ifFound: aBlock [

	self classNameRequester 
		request: 'Class name or fragment?'
		initialAnswer: '' 
		do: [ :pattern | self findClassFrom: potentialClassNames ifFound: aBlock with: pattern ]
]

{ #category : 'services' }
BrowserWindow class >> findClassFrom: potentialClassNames ifFound: aBlock with: pattern [

	| exactMatch foundClass index  classNames toMatch |
	pattern isEmpty ifTrue: [^self flash].

	"If the user entered an exact class name (including case), just use it."
	(potentialClassNames includes: pattern) ifTrue: [
		foundClass := Smalltalk at: pattern asSymbol.
		aBlock value: foundClass.
		^self ].

	toMatch := (pattern copyWithout: $.) asLowercase withBlanksTrimmed.
	classNames := self classNamesFrom: potentialClassNames with: pattern matching: toMatch.
	classNames isEmpty ifTrue: [^self flash].

	exactMatch := classNames detect: [ :each | each asLowercase = toMatch] ifNone: nil.
	index := self indexOfClassFrom: classNames exactMatch: exactMatch.
	index = 0 ifTrue: [^self flash].

	foundClass := Smalltalk at: (classNames at: index) asSymbol.
	aBlock value: foundClass.
]

{ #category : 'instance creation' }
BrowserWindow class >> fullOnClass: aClass selector: aSelector [
	"Open a new full browser set to class."

	| browser |
	browser := Browser new.
	browser setClass: aClass selector: aSelector.
	^ self open: browser label: browser labelString
]

{ #category : 'instance creation' }
BrowserWindow class >> hierarchyOnClass: aClass selector: aSymbol [
	"Open a new full browser set to class."

	^HierarchyBrowserWindow onClass: aClass selector: aSymbol
]

{ #category : 'services' }
BrowserWindow class >> indexOfClassFrom: classNames exactMatch: exactMatch [

	| options |
	
	classNames size = 1 ifTrue: [ ^1 ].
	
	options := exactMatch
		ifNil: [ PopUpMenu labelArray: classNames lines: #()]
		ifNotNil: [
			classNames remove: exactMatch ifAbsent: nil.
			classNames addFirst: exactMatch.
			PopUpMenu labelArray: classNames lines: #(1)].
		
	^options startUpMenu
]

{ #category : 'instance creation' }
BrowserWindow class >> openNoSysCat: model label: aString [

	^ self new
		model: model;
		buildNoSysCatMorphicWindow;
		setLabel: aString;
		openInWorld
]

{ #category : 'menu-world' }
BrowserWindow class >> worldMenuOptions [
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		30.
			#label 			-> 		'Browser'.
			#object 		-> 		Smalltalk.
			#selector 		-> 		#browse.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 	-> 		'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary}`
]

{ #category : 'menu commands' }
BrowserWindow >> browseAllClasses [
	Smalltalk browseAllClasses
]

{ #category : 'menu commands' }
BrowserWindow >> browsePackage [

	| package browser |
	package := nil.
	"Try to get package of the selected method"
	model currentCompiledMethod ifNotNil: [ :method |
		package := CodePackage packageOfMethod: model currentCompiledMethod asMethodReference ifNone: nil ].
	"No selected method, try to get package for method category."
	package ifNil: [
		model selectedClass ifNotNil: [ :selectedClass |
			model selectedMessageCategoryName ifNotNil: [ :selectedMessageCategoryName |
				package := CodePackage packageOfMethodCategory: selectedMessageCategoryName ofClass: selectedClass ifNone: nil ]]].
	"No luck. Try to get package for class."
	package ifNil: [
		model selectedClass ifNotNil: [ :selectedClass |
			package := CodePackage packageOfClass: selectedClass ifNone: nil ]].
	"Last resort. Try to get package for system category"
	package ifNil: [
		package := CodePackage packageOfSystemCategory: model selectedSystemCategory ifNone: nil ].

	package ifNotNil: [
		browser := SinglePackageBrowser new.
		browser package: package.
		BrowserWindow open: browser label: browser labelString ].
]

{ #category : 'GUI building' }
BrowserWindow >> buildLowerPanes [
	| codeAndButtons codeButtonsAndAnnotations code comment separator |
	code := self buildMorphicCodePane.
	comment := self buildMorphicCommentPane.
	separator := LayoutAdjustingMorph new.
	comment separator: separator code: code.
	codeAndButtons := LinearLayoutMorph newColumn.
	(Preferences at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: code proportionalHeight: 0.5;
		addMorph: separator fixedHeight: Theme current layoutAdjusterThickness;
		addMorph: comment proportionalHeight: 0.5.
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations := LinearLayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations
]

{ #category : 'GUI building' }
BrowserWindow >> buildMorphicClassColumnWith: classList [
	| column |
	column := LinearLayoutMorph newColumn.
	column
		addMorphUseAll: classList;
		addAdjusterAndMorph: self buildMorphicSwitches fixedHeight: Theme current buttonPaneHeight.
	^column
]

{ #category : 'GUI building' }
BrowserWindow >> buildMorphicClassList [

	^(PluggableListMorph
		withModel: model
		listGetter: #classList
		indexGetter: #classListIndex
		indexSetter: #classListIndex:)
			menuNshortcutsKey: #classListMenuSpec;
			doubleClickSelector: #browseMethodFull.
]

{ #category : 'GUI building' }
BrowserWindow >> buildMorphicCommentPane [
	"Construct the pane that shows the class comment."
	 ^ (BrowserCommentTextMorph
		textProvider: model
		textGetter: #classCommentText
		textSetter: #newClassComment:) emptyTextDisplayMessage: 'Please enter a comment for this class'.
]

{ #category : 'GUI building' }
BrowserWindow >> buildMorphicMessageCatList [

	^(PluggableListMorph
		withModel: model
		listGetter: #messageCategoryList
		indexGetter: #messageCategoryListIndex
		indexSetter: #messageCategoryListIndex:)
			menuNshortcutsKey: #messageCatListMenuSpec.

]

{ #category : 'GUI building' }
BrowserWindow >> buildMorphicMessageList [
	"Build a morphic message list, with #messageList as its list-getter"

	^(PluggableListMorph
		withModel: model
		listGetter: #messageList
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:)
			menuNshortcutsKey: #messageListMenuSpec;
			doubleClickSelector: #browseMethodFull.
]

{ #category : 'GUI building' }
BrowserWindow >> buildMorphicSwitches [

	| instanceSwitch commentSwitch classSwitch row buttonColor |
	
	instanceSwitch := self createInstanceButton.
	commentSwitch := self createCommentButton.
	classSwitch := self createClassButton.
	
	row := LinearLayoutMorph newRow.
	row
		doAdoptWidgetsColor;
		addMorph: instanceSwitch proportionalWidth: 0.45;
		addMorph: commentSwitch proportionalWidth: 0.22;
		addMorph: classSwitch proportionalWidth: 0.33.
	buttonColor := self buttonColor.
	row color: buttonColor.

	{ 
		instanceSwitch.
		commentSwitch.
		classSwitch} do: [:m |  m color: buttonColor ].
		
	^row
]

{ #category : 'GUI building' }
BrowserWindow >> buildMorphicSystemCategoryList [
	^(HierarchicalListMorph
		withModel: model
		listGetter: #systemCategoryRoots
		indexGetter: #selectedSystemCategory
		indexSetter: #setSelectedSystemCategoryTreeItem:)
			menuNshortcutsKey: #systemCatListMenuSpec;
			autoDeselect: true;
			autoExpand: true;
			yourself
]

{ #category : 'GUI building' }
BrowserWindow >> buildMorphicWindow [
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes messageCatList messageList systemCatList classList |

	systemCatList := self buildMorphicSystemCategoryList.
	classList := self buildMorphicClassList.
	messageCatList := self buildMorphicMessageCatList.
	messageList := self buildMorphicMessageList.
	
	classList allowItemDrag: true.
	systemCatList 
		acceptDropsFrom: classList 
		performing: #categorizeUnderCategory:class:.
		
	messageList allowItemDrag: true.
	messageCatList
		acceptDropsFrom: messageList
		performing: #categorizeUnderCategoryAt:selector:.
	
	upperPanes := LinearLayoutMorph newRow.
	upperPanes
		addMorph: systemCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: (self buildMorphicClassColumnWith: classList) proportionalWidth: 0.2;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection
]

{ #category : 'GUI building' }
BrowserWindow >> buildNoSysCatMorphicWindow [
	"A Browser without the class categories list"

	| mySingletonList upperPanes messageCatList messageList classColumn classList |
	mySingletonList := (PluggableListMorph
				withModel: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:)
					menuNshortcutsKey: #systemCatSingletonMenuSpec.
	mySingletonList hideScrollBarsIndefinitely.

	classList := self buildMorphicClassList.
	classColumn := self buildMorphicClassColumnWith: classList.
	messageCatList := self buildMorphicMessageCatList.
	messageList := self buildMorphicMessageList.
	
	upperPanes := LinearLayoutMorph newRow.
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

	model changed: #editSelection
]

{ #category : 'updating' }
BrowserWindow >> classAdded: addedClass [

	owner ifNotNil: [
		model selectedSystemCategory = addedClass category 
			ifTrue: [ model changed: #classList ]].
]

{ #category : 'menu & kbd shortcuts' }
BrowserWindow >> classListMenuSpec [
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class List' 					false)
		#(10	'New Subclass' 				nil 	model 		makeNewSubclass 					classIcon 		'put a template into the code pane for defining of a subclass of this class')
		#(20	'Copy Class...' 				nil 	model 		copyClass 							copyIcon)
		#(30	'Delete (x)' 					$x 	model 		removeClass 						deleteIcon)
		#(40 	'-----')
		#(50	'Browse Hierarchy (h)' 		$h 	window 	browseHierarchy 					goTopIcon)
		#(60	'Full Browse (b)' 				$b 	window 	browseMethodFull 					editFindReplaceIcon)
		#(70	'Browse Protocol (p)' 			$p 	window 	browseFullProtocol 				spreadsheetIcon)
		#(80	'Browse Package' 			nil 	window 	browsePackage 					packageIcon)
		#(90 	'-----')
		#(100	'File Out' 					nil 	model 		fileOutClass 						fileOutIcon)
		#(110	'File Out Alphabetically (o)' 	$o 	model 		fileOutClassAlphabetically 			fileOutIcon)
		#(120	'Run Tests (t)' 				$t 	model 		runClassTests 						weatherFewCloudsIcon)
		#(130 	'-----')
		#(140	'Refactorings' 				nil 	nil 			1000 								listAddIcon)
		#(150 	'-----')
		#(160	'Show classes Hierarchically' 	nil model 		enableListClassesHierarchically 	goTopIcon)
		#(170	'Show classes Alphabetically' 	nil model 		enableListClassesAlphabetically 	sendReceiveIcon)
		#(180	'Show Definition' 				nil 	model 		editClass 							editFindReplaceIcon)
		#(190	'Show Comment' 			nil 	model 		editComment 						editFindReplaceIcon)
		#(200 	'-----')
		#(210	'Class References (N)' 		$N 	window 	browseClassRefs 					classIcon)
		#(220 	'-----')
		#(230	'More...' 						nil 	nil 			2000 								listAddIcon)

		"Keyboard shortcut only. No menu entry."
		#(991	nil 							$r 	model 		recent)

		" 'refactorings' SubMenu"
		#(1010	'Rename Class... (R)' 			$R 	model 		renameClass 						saveAsIcon)
		#(1020	'Insert Superclass...' 			nil 	model 		insertSuperclass 					saveAsIcon)
		#(1030 	'-----')
		#(1040	'Add Inst Var...' 				nil 	model 		addInstVar 							listAddIcon)
		#(1050	'Rename Inst Var...' 			nil 	model 		renameInstVar 						saveAsIcon)
		#(1060	'Remove Inst Var...' 			nil 	model 		removeInstVar 						deleteIcon)
		#(1070	'Remove all Unreferenced Inst Var...' nil model 	removeAllUnreferencedInstVar 	deleteIcon)
		#(1080	'Push Up Inst Var...' 			nil 	model 		pushUpInstanceVariable 			goTopIcon)
		#(1090	'Push Down Inst Var...' 		nil 	model 		pushDownInstanceVariable 		goBottomIcon)
		#(1100 	'-----')
		#(1110	'Rename Global...' 			nil 	model 		renameGlobal 						saveAsIcon)

		" 'more' SubMenu"
		#(2010	'Unsent Methods' 			nil 	window 	browseUnusedMethods 			junkIcon 			'browse all methods defined by this class that have no senders')
		#(2020	'Unreferenced inst Vars' 		nil 	window 	showUnreferencedInstVars 		junkIcon 			'show a list of all instance variables that are not referenced in methods')
		#(2030	'Unreferenced class Vars' 		nil 	window 	showUnreferencedClassVars 		junkIcon 			'show a list of all class variables that are not referenced in methods')
		#(2040 	'-----')
		#(2050	'Sample instance' 			nil 	window 	makeSampleInstance 				instanceIcon 		'give me a sample instance of this class, if possible')
		#(2060	'Inspect instances' 			nil 	window 	inspectInstances 					inspectIcon 		'open an inspector on all the extant instances of this class')
		#(2070	'Inspect subinstances' 		nil 	window 	inspectSubInstances 				inspectIcon 		'open an inspector on all the extant instances of this class and of all of its subclasses')
		#(2080 	'-----')
		#(2090	'Create inst var accessors' 	nil 	model 		createInstVarAccessors 			sendReceiveIcon 	'compile instance-variable access methods for any instance variables that do not yet have them')
	)
]

{ #category : 'updating' }
BrowserWindow >> classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName [

	owner ifNotNil: [
		self canDiscardEdits ifTrue: [
			model classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName ]].
]

{ #category : 'GUI building' }
BrowserWindow >> createClassButton [

	| classSwitch |
	
	classSwitch := PluggableButtonMorph 
		model: model
		stateGetter: #classMessagesIndicated
		action: #indicateClassMessages.
	
	classSwitch
		label: 'class';
		setBalloonText: 'show class methods'.
		
	^classSwitch
]

{ #category : 'GUI building' }
BrowserWindow >> createCodePaneMorph [
	
	codePane := super createCodePaneMorph.
	^codePane
]

{ #category : 'GUI building' }
BrowserWindow >> createCommentButton [

	|  commentSwitch  |
	
	commentSwitch := PluggableButtonMorph 
		model: model
		stateGetter: #classCommentIndicated
		action: #plusButtonHit.
				
	commentSwitch 
		label: '?';
		setBalloonText: 'show class comment'.
		
	^commentSwitch 
]

{ #category : 'GUI building' }
BrowserWindow >> createInstanceButton [

	| instanceSwitch |
	
	instanceSwitch := PluggableButtonMorph 
		model: model
		stateGetter: #instanceMessagesIndicated
		action: #indicateInstanceMessages.

	instanceSwitch 
		label: 'instance';
		setBalloonText: 'show instance methods'.

	^instanceSwitch 
]

{ #category : 'updating' }
BrowserWindow >> disableCodePaneEditing [

	codePane ifNotNil: [ codePane disableEditing ]
]

{ #category : 'updating' }
BrowserWindow >> editSelectionChanged [
	
	model isEditingNone
		ifTrue: [ self disableCodePaneEditing ]
		ifFalse: [ self enableCodePaneEditing]
]

{ #category : 'updating' }
BrowserWindow >> enableCodePaneEditing [

	codePane ifNotNil: [ codePane enableEditing ]
]

{ #category : 'menu commands' }
BrowserWindow >> findClass [

	| scopedClassNames |
	
	scopedClassNames := model potentialClassNames asOrderedCollection.
	
	self class 
		findClassFrom: scopedClassNames 
		ifFound: [:foundClass |
			model setSelectedSystemCategory: foundClass category.
			model selectClass: foundClass ]
]

{ #category : 'menu & kbd shortcuts' }
BrowserWindow >> messageCatListMenuSpec [
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Categories' 			false)
		#(5		'Browse Package (p)' 				$p 	window 	browsePackage 						packageIcon)
		#(6		'-----')
		#(10	'File Out (o)' 						$o 	model 		fileOutMessageCategories 				fileOutIcon)
		#(20 	'-----')
		#(30	'Reorganize (r)' 					$r 	model 		editMessageCategories 				sendReceiveIcon)
		#(40	'Alphabetize (a)' 					$a 	model 		alphabetizeMessageCategories 		fontXGenericIcon)
		#(50	'Remove empty Categories (e)' 	$e 	model 		removeEmptyCategories 				listRemoveIcon)
		#(60	'Categorize all Uncategorized (c)' 	$c 	model 		categorizeAllUncategorizedMethods 	packageIcon)
		#(70	'New Category... (n)' 				$n 	model 		addCategory 							newIcon)
		#(80	'New extension Category... (N)' 	$N 	model 		addExtensionCategory 					listAddIcon)
		#(90 	'-----')
		#(100	'Refactorings' 					nil 	nil 			1000 									listAddIcon)
		#(110	'Rename... (R)' 					$R 	model 		renameCategory 						saveAsIcon)
		#(120	'Remove (x)' 					$x 	model 		removeMessageCategory 				deleteIcon)
		#(130 	'-----')
		#(140	'Run Tests (t)' 					$t 	model 		runMessageCategoryTests 			weatherFewCloudsIcon)

		" 'Refactorings' SubMenu"
		#(1010	'Push Up' 									nil 	model 	pushUpCategorySelectors 							goTopIcon)
		#(1020	'Add in Superclass as subclassResponsibility' 	nil	model 	addCategoryAsSubclassResponsibilitySelectors 	goTopIcon)
		#(1030	'Push Down to Subclasses' 					nil 	model 	pushDownCategorySelectorsToSubclasses 		goBottomIcon)
		#(1040	'Push Down to one Subclass' 					nil 	model 	pushDownCategorySelectorsToOneSubclass 		goBottomIcon)
	)
]

{ #category : 'menu & kbd shortcuts' }
BrowserWindow >> messageListMenuSpec [
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message List' 						false)
		#(10	'Browse Full (b)' 							$b 	window 	browseMethodFull 							editFindReplaceIcon)
		#(20	'Browse Hierarchy (h)' 					$h 	window 	browseHierarchy 							goTopIcon)
		#(30	'Browse Method (O)' 						$O 	window 	openSingleMessageBrowser 				scriptIcon)
		#(35	'Browse Package (p)' 						$p 	window 	browsePackage 							packageIcon)
		#(40 	'-----')
		#(50	'File Out (o)' 								$o 	model 		fileOutMessage 							fileOutIcon)
		#(60	'Run Test (t)' 							$t 	model 		runMethodTest 							weatherFewCloudsIcon)
		#(70	'Debug Test (r)' 							$r 	model 		debugMethodTest 							weatherFewCloudsIcon)
		#(80	'Inspect CompiledMethod' 				nil 	model 		inspectCompiledMethod 					exploreIcon)
		#(90 	'-----')
		#(100	'Refactorings' 							nil 	nil 			1000 										listAddIcon)
		#(110	'Senders of... (n)' 						nil 	window 	browseSendersOfMessages 				mailForwardIcon)
		#(120	'Implementors of... (m)' 					nil 	window 	browseImplementorsOfMessages 			developmentIcon)
		#(130	'Inheritance (i)' 							$i 	window 	methodInheritance 						goDownIcon)
		#(140	'Versions (v)' 							$v 	window 	browseVersions 							clockIcon)
		#(150 	'-----')
		#(160	'Remove Method (x)' 						$x 	model 		removeMessage 							deleteIcon)
		#(170	'Remove Method in Hierarchy' 			nil 	model 		removeMessageInHierarchy 				deleteIcon)
		#(180	'More...' 									nil 	nil 			2000 										listAddIcon)

		"Keyboard shortcut only. No menu entry."
		#(991	nil 										$m 	window 	browseImplementors)
		#(992	nil 										$n 	window 	browseSenders)
		#(993	nil 										$c 	model 		copySelector)

		" 'refactorings' SubMenu"
		#(1010	'Rename... (R)' 							$R 	model 		renameSelector 							saveAsIcon)
		#(1020	'Change keyword Order...' 				nil 	model 		changeKeywordOrder 						sendReceiveIcon)
		#(1030	'Add Parameter... (A)' 					$A 	model 		addParameter 								listAddIcon)
		#(1040	'Remove Parameter... (S)' 				$S 	model 		removeParameter 							listRemoveIcon)
		#(1050 	'-----')
		#(1060	'Inline Method... (F)' 						$F 	model 		inlineMethod 								saveAsIcon)
		#(1070	'Move to Instance/Class Method' 			nil 	model 		moveToInstanceOrClassMethod 			changesIcon)
		#(1080	'Push Up' 								nil 	model 		pushUpSelector 							goTopIcon)
		#(1090	'Add in Superclass as subclassResponsibility' nil model 		addAsSubclassResponsibilitySelector 		goTopIcon)
		#(1100	'Push Down to Subclasses' 				nil 	model 		pushDownSelectorToSubclasses 			goBottomIcon)
		#(1110	'Push Down to One Subclass' 				nil 	model 		pushDownSelectorToOneSubclass 			goBottomIcon)

		" 'more' SubMenu"
		#(2010	'Local Senders of...' 						nil 	window 	browseLocalSendersOfMessages 			mailForwardIcon)
		#(2020	'Local Implementors of...' 					nil 	window 	browseLocalImplementorsOfMessages 	classIcon)
		#(2030 	'-----')
		#(2040	'Sample Instance' 						nil 	window 	makeSampleInstance 						instanceIcon)
		#(2050	'Inspect Instances' 						nil 	window 	inspectInstances 							inspectIcon)
		#(2060	'Inspect Subinstances' 					nil 	window 	inspectSubInstances 						inspectIcon)
		#(2070 	'-----')
		#(2080	'Change Category...' 						nil 	model 		changeCategory 							saveAsIcon)
		#(2090	'Select method Category (C)' 				$C 	model 		showHomeCategory 						editFindReplaceIcon)
		#(2100	'Change sets with this Method' 			nil 	window 	findMethodInChangeSets 					changesIcon)
		#(2110	'Revert to previous Version' 				nil 	model 		revertToPreviousVersion 					undoIcon)
	)
]

{ #category : 'menu commands' }
BrowserWindow >> openSystemCategoryBrowser [
	"Create and schedule a new system category browser with initial textual
	contents set to aString."

	model systemCategoryBrowser ifNotNil: [ :newBrowser |
		newBrowser selectedSystemCategory ifNotNil: [ :category |
			BrowserWindow
				openNoSysCat: newBrowser
				label: 'Classes in category ', category ]]
]

{ #category : 'menu & kbd shortcuts' }
BrowserWindow >> systemCatListMenuSpec [
	"Spec for the menu and shortcut keys for the system categories list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class Categories' 	false)
		#(10	'Find Class... (f)' 		$f 	window 	findClass 								findIcon)
		#(20	'Recent Classes... (r)' 	$r 	model 		recent 									clockIcon)
		#(30 	'-----')
		#(40	'Browse All (B)' 		$B 	window 	browseAllClasses 						editFindReplaceIcon)
		#(50	'Browse (b)' 			$b 	window 	openSystemCategoryBrowser 			packageIcon)
		#(55	'Browse Package (p)' 	$p 	window 	browsePackage 						packageIcon)
		#(60 	'-----')
		#(70	'File Out (o)' 			$o 	model 		fileOutSystemCategory 				fileOutIcon)
		#(80 	'-----')
		#(90	'Reorganize' 			nil 	model 		editSystemCategories 					sendReceiveIcon)
		#(100	'Alphabetize (A)' 		$A 	model 		alphabetizeSystemCategories 			fontXGenericIcon)
		#(110 	'-----')
		#(120	'Update (u)' 			$u 	model 		updateSystemCategories 				updateIcon)
		#(130	'Add Category... (a)' 	$a 	model 		addSystemCategory 					newIcon)
		#(140	'Rename... (R)' 		$R 	model 		renameSystemCategory 				saveAsIcon)
		#(150	'Move to... (m)' 		$m 	model 		moveAllToOtherSystemCategory 		saveAsIcon)
		#(160	'Remove (x)' 		$x 	model 		removeSystemCategory 				deleteIcon)
		#(170 	'-----')
		#(180	'Move to Top' 		nil 	model 		moveSystemCategoryTop 				goTopIcon)
		#(190	'Move Up' 			nil 	model 		moveSystemCategoryUp 				goUpIcon)
		#(200	'Move Down' 		nil 	model 		moveSystemCategoryDown 			goDownIcon)
		#(210	'Move to Bottom' 	nil 	model 		moveSystemCategoryBottom 			fileOutIcon)
		#(220 	'-----')
		#(230	'Run Tests (t)' 		$t 	model 		runSystemCategoryTests 				weatherFewCloudsIcon)
	)
]

{ #category : 'menu & kbd shortcuts' }
BrowserWindow >> systemCatSingletonMenuSpec [
	"Spec for the menu and shortcut keys for the one-item system category list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class category' 					false)
		#(10	'Find Class... (f)' 	$f 	window 	findClass 						findIcon)
		#(20 	'-----')
		#(30	'Browse All (B)' 	$B 	window 	browseAllClasses 				editFindReplaceIcon)
		#(40	'Browse (b)' 		$b 	window 	openSystemCategoryBrowser 	packageIcon)
		#(50 	'-----')
		#(60	'File Out (o)' 		$o 	model 		fileOutSystemCategory 		fileOutIcon)
		#(70 	'-----')
		#(80	'Update (u)' 		$u 	model 		updateSystemCategories 		updateIcon)
		#(90	'Rename... (R)' 	$R 	model 		renameSystemCategory 		saveAsIcon)
		#(100	'Remove (x)' 	$x 	model 		removeSystemCategory 		deleteIcon)
	)
]

{ #category : 'updating' }
BrowserWindow >> update: anEvent [
	super update: anEvent.
	anEvent = #editSelection ifTrue: [self editSelectionChanged ] 
]

{ #category : 'GUI building' }
BrowserWindow >> windowColor [
	^ Theme current browser
]
