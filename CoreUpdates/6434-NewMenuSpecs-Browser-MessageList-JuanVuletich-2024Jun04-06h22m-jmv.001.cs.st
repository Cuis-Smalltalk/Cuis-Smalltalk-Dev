'From Cuis7.1 [latest update: #6433] on 4 June 2024 at 8:24:58 am'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 08:20:59'!
messageListMenuSpec
	"Spec for the menu and shortcut keys the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message List' 					false)
		#(10		'what to show...' 							nil 	window 			offerWhatToShowMenu 									preferencesIcon)
		#(20		'toggle break on entry' 							nil 	model 			toggleBreakOnEntry 									debugIcon)
		#(30 		'-----')
		#(40		'browse full (b)' 							$b 	window 			browseMethodFull 									editFindReplaceIcon)
		#(50		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 									goTopIcon)
		#(60		'browse method (O)' 							$O 	window 			openSingleMessageBrowser 									scriptIcon)
		#(70		'browse protocol (p)' 							$p 	window 			browseFullProtocol 									spreadsheetIcon)
		#(80		'browse method category (c)' 							$c 	window 			browseCategoryOfCurrentMethod 									editFindReplaceIcon)
		#(90 		'-----')
		#(100		'fileOut' 							nil 	model 			fileOutClass 									fileOutIcon)
		#(110		'run test (t)' 							$t 	model 			runMethodTest 									weatherFewCloudsIcon)
		#(120		'debug test (r)' 							$r 	model 			debugMethodTest 									weatherFewCloudsIcon)
		#(130		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 									exploreIcon)
		#(140 		'-----')
		#(150		'refactorings' 							nil 	nil 			1000 									listAddIcon)
		#(160		'senders of... (n)' 							$n 	window 			browseSendersOfMessages 									mailForwardIcon)
		#(170		'implementors of... (m)' 							$m 	window 			browseMessages 									developmentIcon)
		#(180		'inheritance (i)' 							$i 	window 			methodInheritance 									goDownIcon)
		#(190		'versions (v)' 							$v 	window 			browseVersions 									clockIcon)
		#(200 		'-----')
		#(210		'inst var refs...' 							nil 	window 			browseInstVarRefs 									instanceIcon)
		#(220		'inst var defs...' 							nil 	window 			browseInstVarDefs 									instanceIcon)
		#(230		'class var refs...' 							nil 	window 			browseClassVarRefs 									classIcon)
		#(240		'class variabless' 							nil 	window 			browseClassVariables 									classIcon)
		#(250		'class refs (N)' 							$N 	window 			browseClassRefs 									classIcon)
		#(260 		'-----')
		#(270		'remove method (x)' 							$x 	model 			removeMessage 									deleteIcon)
		#(280		'remove method in hierarchy' 							nil 	model 			removeMessageInHierarchy 									deleteIcon)
		#(290		'more' 							nil 	nil 			2000 									listAddIcon)

		" 'refactorings' SubMenu"
		#(1010		'rename... (R)' 											$R 	model 			renameSelector 											saveAsIcon)
		#(1020		'change keyword order...' 											nil 	model 			changeKeywordOrder 											sendReceiveIcon)
		#(1030		'add parameter... (A)' 											$A 	model 			addParameter 											listAddIcon)
		#(1040		'remove parameter... (S)' 											$S 	model 			removeParameter 											listRemoveIcon)
		#(1050 		'-----')
		#(1060		'inline method... (F)' 											$F 	model 			inlineMethod 											saveAsIcon)
		#(1070		'move to instance/class method' 											nil 	model 			moveToInstanceOrClassMethod 											changesIcon)
		#(1080		'push up' 											nil 	model 			pushUpSelector 											goTopIcon)
		#(1090		'add in superclass as subclassResponsability' 											nil 	model 			addAsSubclassResponsabilitySelector 											goTopIcon)
		#(1100		'push down to subclasses' 											nil 	model 			pushDownSelectorToSubclasses 											goBottomIcon)
		#(1110		'push down to one subclass' 											nil 	model 			pushDownSelectorToOneSubclass 											goBottomIcon)

		" 'more' SubMenu"
		#(2010		'toggle diffing (D)' 								$D 	model 			toggleDiffing 									switchIcon)
		#(2020		'implementors of sent messages' 								nil 	window 			browseAllMessages 									developmentIcon)
		#(2030 		'-----')
		#(2040		'local senders of...' 								nil 	window 			browseLocalSendersOfMessages 									mailForwardIcon)
		#(2050		'local implementors of...' 								nil 	window 			browseLocalImplementors 									classIcon)
		#(2060 		'-----')
		#(2070		'sample instance' 								nil 	window 			makeSampleInstance 									instanceIcon)
		#(2080		'inspect instances' 								nil 	window 			inspectInstances 									inspectIcon)
		#(2090		'inspect subinstances' 								nil 	window 			inspectSubInstances 									inspectIcon)
		#(2100		'change category...' 								nil 	model 			changeCategory 									saveAsIcon)
		#(2110		'show category (C)' 								$C 	model 			showHomeCategory 									packageIcon)
		#(2120		'change sets with this method' 								nil 	window 			findMethodInChangeSets 									changesIcon)
		#(2130		'revert to previous version' 								nil 	model 			revertToPreviousVersion 									undoIcon)
	)! !


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/4/2024 08:12:54'!
messageListMenuSpec
	"Spec for the menu and shortcut keys the messages list.
	See coment at Morph >> #getMenu
	"
	^ self model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Messages' 			false)
				#(10		'fileIn' 						nil 	model 			fileInMessage 							updateIcon)
				#(20		'fileOut' 						nil 	model 			fileOutMessage 							fileOutIcon)
				#(30 		'-----')
				#(40		'senders (n)' 						$n 	window 			browseSenders 							mailForwardIcon)
				#(50		'implementors (m)' 						$m 	window 			browseImplementors 							developmentIcon)
				#(60		'method inheritance (h)' 						$h 	window 			methodInheritance 							goDownIcon)
				#(70		'browse full (b)' 						$b 	window 			browseMethodFull 							editFindReplaceIcon)
				#(80		'versions (v)' 						nil 	window 			browseVersions 							clockIcon)
				#(90 		'-----')
				#(100		'remove method (x)' 						$x 	model 			removeMessage 							deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Messages' 			false)
				#(10		'fileOut' 						nil 	model 			fileOutMessage 							fileOutIcon)
				#(20 		'-----')
				#(30		'remove method (x)' 						$x 	model 			removeMessage 							deleteIcon)
			)].! !


!MethodSetWindow methodsFor: 'menu building' stamp: 'jmv 6/4/2024 08:16:12'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel class |
	self flag: #definesKeyboardShortcuts.
	aChar = $D ifTrue: [^ model toggleDiffing].

	sel := model selectedMessageName.
	aChar = $m ifTrue: [  "These next two put up a type in if no message selected"
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk].
	aChar = $n ifTrue: [
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllCallsOn: to: Smalltalk].

	"The following require a class selection"
	(class := model selectedClassOrMetaClass) ifNil: [^ self ].
	aChar = $b ifTrue: [^ BrowserWindow fullOnClass: class selector: sel].
	aChar = $N ifTrue: [^ self browseClassRefs].
	aChar = $h ifTrue: [^ self browseHierarchy].
	aChar = $p ifTrue: [^ self browseFullProtocol].

	sel
		ifNotNil: [
			"The following require a method selection"
			aChar = $i ifTrue: [^ self methodInheritance].
			aChar = $o ifTrue: [^ model fileOutMessage].
			aChar = $c ifTrue: [^ model copySelector].
			aChar = $v ifTrue: [^ self browseVersions].
			aChar = $C ifTrue: [^ model showHomeCategory].
			aChar = $O ifTrue: [^ self openSingleMessageBrowser].
			aChar = $x ifTrue: [^ model removeMessage].
			aChar = $t ifTrue: [^ model runMethodTest].
			aChar = $r ifTrue: [^ model debugMethodTest]]
		ifNil: [
			aChar = $R ifTrue: [^ model renameClass]]! !


!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 6/4/2024 06:31:57'!
addItemTo: topMenu fromSpec: itemSpec submenus: submenusDictionary
	"
	See coment at Morph >> #getMenu
	"
	| submenuId menu label target selector iconSymbol helpText menuItem |
	label := itemSpec second.
	label ifNil: [ ^self ]. "Only a keyboard shortcut, not a real menu item."
	submenuId := itemSpec first // 1000.
	menu := submenuId = 0 ifTrue: [ topMenu ] ifFalse: [ submenusDictionary at: submenuId ].
	target := itemSpec fourth
		ifNotNil: [ :targetGetter | self perform: targetGetter ]
		ifNil: [ self ].
	selector := itemSpec fifth.
	menuItem := label isSymbol
		ifTrue: [ menu addUpdating: label target: target action: selector argumentList: #() ]
		ifFalse: [ menu add: label target: target action: selector argumentList: #() ].
	selector ifNil: [
		menuItem isEnabled: false ].
	iconSymbol := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
	iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
	helpText := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
	helpText ifNotNil: [ menuItem setBalloonText: helpText ].! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 6/4/2024 06:32:29'!
addSubmenuTo: menu fromSpec: itemSpec submenus: submenusDictionary
	| submenu label submenuId iconSymbol helpText menuItem |
	submenu := MenuMorph new.
	label := itemSpec second.
	submenuId := itemSpec fifth // 1000.
	menuItem := menu add: label subMenu: submenu.
	iconSymbol := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
	iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
	helpText := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
	helpText ifNotNil: [ menuItem setBalloonText: helpText ].
	submenusDictionary at: submenuId put: submenu! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 6/4/2024 08:10:12'!
buildMorphicMessageList
	"Build a morphic message list, with #messageList as its list-getter"

	^(PluggableListMorph
		withModel: model
		listGetter: #messageList
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:)
			menuNshortcutsKey: #messageListMenuSpec.! !


!MethodSetWindow methodsFor: 'menu building' stamp: 'jmv 6/4/2024 08:09:12'!
messageListMenu2
	"Fill aMenu with the items appropriate when the shift key is held down"

	self flag: #assumesKeyboardShortcuts.
	^DynamicMenuBuilder 
		buildTitled: 'Message Set' 
		targeting: self 
		collectingMenuOptionsWith: #messageSetMenu2Options 
		changingThemWith: [ :options |
			self addExtraMenu2ItemsTo: options ].! !

!methodRemoval: RefactoringMenues class #messageListMenuOptions stamp: 'jmv 6/4/2024 08:06:37'!
RefactoringMenues class removeSelector: #messageListMenuOptions!
!methodRemoval: CodeFileBrowserWindow #messageListMenu stamp: 'jmv 6/4/2024 08:06:24'!
CodeFileBrowserWindow removeSelector: #messageListMenu!
!methodRemoval: CodeFileBrowserWindow #messageListKey:from: stamp: 'jmv 6/4/2024 08:13:01'!
CodeFileBrowserWindow removeSelector: #messageListKey:from:!
!methodRemoval: BrowserWindow class #messageListMenu2Options stamp: 'jmv 6/4/2024 08:08:22'!
BrowserWindow class removeSelector: #messageListMenu2Options!
!methodRemoval: BrowserWindow class #messageListMenuOptions stamp: 'jmv 6/4/2024 08:06:44'!
BrowserWindow class removeSelector: #messageListMenuOptions!
!methodRemoval: BrowserWindow #openMessageListMenu stamp: 'jmv 6/4/2024 08:06:04'!
BrowserWindow removeSelector: #openMessageListMenu!
!methodRemoval: BrowserWindow #messageListMenu2 stamp: 'jmv 6/4/2024 08:07:53'!
BrowserWindow removeSelector: #messageListMenu2!
!methodRemoval: BrowserWindow #messageListMenu stamp: 'jmv 6/4/2024 08:06:27'!
BrowserWindow removeSelector: #messageListMenu!
!methodRemoval: BrowserWindow #messageListKey:from: stamp: 'jmv 6/4/2024 08:14:14'!
BrowserWindow removeSelector: #messageListKey:from:!
!methodRemoval: BrowserWindow #addExtraMenu2ItemsTo: stamp: 'jmv 6/4/2024 08:08:09'!
BrowserWindow removeSelector: #addExtraMenu2ItemsTo:!
!methodRemoval: BrowserWindow #openMessageListMenu2 stamp: 'jmv 6/4/2024 08:07:17'!
BrowserWindow removeSelector: #openMessageListMenu2!
!methodRemoval: CodeWindow #messageListKey:from: stamp: 'jmv 6/4/2024 08:16:25'!
CodeWindow removeSelector: #messageListKey:from:!
!methodRemoval: Browser #canShowMultipleMessageCategories stamp: 'jmv 6/4/2024 08:09:19'!
Browser removeSelector: #canShowMultipleMessageCategories!
!methodRemoval: CodeProvider #canShowMultipleMessageCategories stamp: 'jmv 6/4/2024 08:09:21'!
CodeProvider removeSelector: #canShowMultipleMessageCategories!

!CodeProvider reorganize!
('annotation' addPriorVersionsCountForSelector:ofClass:to: annotation annotationForClassCommentFor: annotationForClassDefinitionFor: annotationForMessageCategory:ofClass: annotationForSelector:ofClass: annotationForSystemCategory: annotationSeparator)
('categories' categoryFromUserWithPrompt:for: categoryOfCurrentMethod changeCategory letUserReclassify:in: methodCategoryChanged selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText commentContents contentsSymbol contentsSymbol: currentMethodRefactored methodNodeOf:ifErrorsParsing:)
('commands' hierarchyBrowser removeClass)
('controls' contentsSymbolQuints sourceAndDiffsQuintsOnly)
('diffs' defaultDiffsSymbol diffFromPriorSourceFor: shouldDiffWords showDiffs: showLineDiffs: showPrettyLineDiffs: showPrettyWordDiffs: showWordDiffs: showingAnyKindOfDiffs showingAnyKindOfPrettyDiffs showingLineDiffs showingLineDiffsString showingPrettyLineDiffs showingPrettyLineDiffsString showingPrettyWordDiffs showingPrettyWordDiffsString showingWordDiffs showingWordDiffsString toggleDiffing toggleLineDiffing togglePlainSource togglePrettyLineDiffing togglePrettyPrint togglePrettyWordDiffing toggleWordDiffing)
('misc' isThereAnOverride isThisAnOverride okayToAccept priorSourceOrNil releaseCachedState)
('self-updating' didCodeChangeElsewhere updateIfNeeded)
('what to show' prettyPrintString showByteCodes: showComment showDecompile: showDocumentation: showingByteCodes showingByteCodesString showingDecompile showingDecompileString showingDocumentation showingDocumentationString showingPlainSource showingPlainSourceString showingPrettyPrint showingSource toggleDecompile toggleShowDocumentation toggleShowingByteCodes)
('message list' decompiledSource selectedBytecodes selectedMessage selector:refactoredTo: sourceStringPrettifiedAndDiffed)
('message list menu' copySelector fileOutMessage inspectCompiledMethod revertToPreviousVersion timeStamp unusedMethods)
('testing' is: isEditingMethod)
('accessing' currentCompiledMethod selectedClass selectedClassOrMetaClass selectedMessageName)
('evaluation' doItContext)
('shout styling' isModeStyleable shouldStyle:with: styleByParagraphs)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
!


!CodeWindow reorganize!
('GUI building' addShiftClickEventHandlerFor: buildLowerPanes buildMorphicAnnotationsPane buildMorphicCodePane createCodePaneMorph defaultAnnotationPaneHeight lineDiffButton lineDiffButtonHelp optionalButtonRow optionalButtonTuples prettyLineDiffButton prettyLineDiffButtonHelp prettyWordDiffButton prettyWordDiffButtonHelp wordDiffButton wordDiffButtonHelp)
('accessing' buttonWithSelector: inheritanceButton)
('updating' classAdded: classRenamed:from:to:inCategory: decorateButtons decorateForInheritance updateListsAndCode updateListsAndCodeNow)
('top window' activateAndSendTopToBack:)
('menu commands' browseAllMessages browseClassRefs browseClassVarRefs browseClassVariables browseFullProtocol browseHierarchy browseHierarchyAndReplaceMe browseImplementors browseInstVarDefs browseInstVarRefs browseLocalImplementors browseLocalSendersOfMessages browseMessages browseMethodFull browseMethodFullAndReplaceMe browseSenders browseSendersOfMessages browseSendersOfMethod browseUnusedMethods browseVersions findMethodInChangeSets inspectInstances inspectSubInstances makeSampleInstance methodInheritance methodInheritanceAndReplaceMe openSingleMessageBrowser shiftClickPressedOn:at: showUnreferencedClassVars showUnreferencedInstVars)
('menu building' addContentsTogglesTo: offerWhatToShowMenu)
('misc' getSelectorAndSendQuery:to: getSelectorAndSendQuery:to:with: selectMessageAndEvaluate: sendQuery:to: useSelector:orGetSelectorAndSendQuery:to:)
('initialization' model:)
('notification actions' initializeNotificationActions registerNotificationActions registerNotificationActionsIfModelNotNil removeNotificationActions)
!


!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumnWith: buildMorphicClassList buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCategoryList buildMorphicWindow buildNoSysCatMorphicWindow createClassButton createCodePaneMorph createCommentButton createInstanceButton windowColor)
('menu commands' browseAllClasses browseCategoryOfCurrentMethod findClass openSystemCategoryBrowser)
('updating' classAdded: classRenamed:from:to:inCategory: disableCodePaneEditing editSelectionChanged enableCodePaneEditing update:)
('menu & kbd shortcuts' classListMenuSpec messageListMenuSpec methodCategoriesListMenuSpec systemCatSingletonMenuSpec systemCategoriesListMenuSpec)
!


!BrowserWindow class reorganize!
('GUI building' classNameRequester classNamesFrom:with:matching: findClass findClassDialogTitle findClassFrom:ifFound: findClassFrom:ifFound:with: fullOnClass: fullOnClass:selector: indexOfClassFrom:exactMatch:)
('menu-world' worldMenuOptions)
('instance creation' openNoSysCat:label:)
!


!CodeFileBrowserWindow reorganize!
('GUI building' buildMorphicWindow optionalButtonTuples windowColor)
('menu building' codeFileListMenu)
('menu commands' browseMethodFull browseVersions findClass methodInheritance)
('keyboard shortcuts' codeFileListKey:from:)
('menu & kbd shortcuts' classListMenuSpec messageListMenuSpec methodCategoriesListMenuSpec)
!

