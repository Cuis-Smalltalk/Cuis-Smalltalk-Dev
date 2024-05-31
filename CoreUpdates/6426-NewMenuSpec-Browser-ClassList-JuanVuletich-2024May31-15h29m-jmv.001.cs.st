'From Cuis7.1 [latest update: #6421] on 31 May 2024 at 4:20:25 pm'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/31/2024 16:04:28'!
classListMenuSpec
	"Spec for the menu and shortcut keys the class list.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments. If a Number (multiple of 1000), indicates a subMenu. Indexes of items must be in range.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	^#(
		#(0 		'Class List' 					false)
		#(10		'browse full (b)' 							$b 	window 			browseMethodFull 									nil 		editFindReplaceIcon)
		#(20		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 									nil 		goTopIcon)
		#(30		'browse protocol (p)' 							$p 	window 			browseFullProtocol 									nil 		spreadsheetIcon)
		#(40 		'-----')
		#(50		'fileOut' 							nil 	model 			fileOutClass 									nil 		fileOutIcon)
		#(60		'run tests (t)' 							$t 	model 			runClassTests 									nil 		weatherFewCloudsIcon)
		#(70 		'-----')
		#(80		'rename class... (R)' 							$R 	model 			renameClass 									nil 		saveAsIcon)
		#(90		'refactorings' 							nil 	nil 			1000 									nil 		listAddIcon)
		#(100		'copy class...' 							nil 	model 			copyClass 									nil 		copyIcon)
		#(110		'remove class (x)' 							$x 	model 			removeClass 									nil 		deleteIcon)
		#(120 		'-----')
		#(130		'show classes hierarchically' 							nil 	model 			enableListClassesHierarchically 									nil 		goTopIcon)
		#(140		'show classes alphabetically' 							nil 	model 			enableListClassesAlphabetically 									nil 		sendReceiveIcon)
		#(150		'show definition' 							nil 	model 			editClass 									nil 		editFindReplaceIcon)
		#(160		'show comment' 							nil 	model 			editComment 									nil 		editFindReplaceIcon)
		#(170 		'-----')
		#(180		'inst var refs...' 							nil 	window 			browseInstVarRefs 									nil 		instanceIcon)
		#(190		'inst var defs...' 							nil 	window 			browseInstVarDefs 									nil 		instanceIcon)
		#(200 		'-----')
		#(210		'class var refs...' 							nil 	window 			browseClassVarRefs 									nil 		classIcon)
		#(220		'class vars' 							nil 	window 			browseClassVariables 									nil 		classIcon)
		#(230		'class refs (N)' 							$N 	window 			browseClassRefs 									nil 		classIcon)
		#(240 		'-----')
		#(250		'more' 							nil 	nil 			2000 									nil 		listAddIcon)
		#(260		nil 							$r 	model 			recent) "Shortcut only. No menu entry."

		" 'refactorings' SubMenu"
		#(1010		'insert superclass...' 							nil 	model 			insertSuperclass 									nil 		saveAsIcon)
		#(1020 		'-----')
		#(1030		'add inst var...' 							nil 	model 			addInstVar 									nil 		listAddIcon)
		#(1040		'rename inst var...' 							nil 	model 			renameInstVar 									nil 		saveAsIcon)
		#(1050		'remove inst var...' 							nil 	model 			removeInstVar 									nil 		deleteIcon)
		#(1060		'remove all unreferenced inst var...' 						
											nil 	model 			removeAllUnreferencedInstVar 									nil 		deleteIcon)
		#(1070		'push up inst var...' 							nil 	model 			pushUpInstanceVariable 									nil 		goTopIcon)
		#(1080		'push down inst var...' 							nil 	model 			pushDownInstanceVariable 									nil 		goBottomIcon)
		#(1090 		'-----')
		#(1100		'add inst var...' 							nil 	model 			addInstVar 									nil 		listAddIcon)

		" 'more' SubMenu"
		#(2010		'unsent methods' 							nil 	window 			browseUnusedMethods 									nil 		junkIcon
					'browse all methods defined by this class that have no senders')
		#(2020		'unreferenced inst vars' 							nil 	window 			showUnreferencedInstVars 									nil 		junkIcon
					'show a list of all instance variables that are not referenced in methods')
		#(2030		'unreferenced class vars' 							nil 	window 			showUnreferencedClassVars 									nil 		junkIcon
					'show a list of all class variables that are not referenced in methods')
		#(2040		'subclass template' 							nil 	model 			makeNewSubclass 									nil 		classIcon
					'put a template into the code pane for defining of a subclass of this class')
		#(2050 		'-----')
		#(2060		'sample instance' 							nil 	window 			makeSampleInstance 									nil 		instanceIcon
					'give me a sample instance of this class, if possible')
		#(2070		'inspect instances' 							nil 	window 			inspectInstances 									nil 		inspectIcon
					'open an inspector on all the extant instances of this class')
		#(2080		'inspect subinstances' 							nil 	window 			inspectSubInstances 									nil 		inspectIcon
					'open an inspector on all the extant instances of this class and of all of its subclasses')
		#(2090 		'-----')
		#(2100		'create inst var accessors' 							nil 	model 			createInstVarAccessors 									nil 		sendReceiveIcon
					'compile instance-variable access methods for any instance variables that do not yet have them')
	)! !


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/31/2024 15:59:56'!
classListMenuSpec
	"Spec for the menu and shortcut keys the class list.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	^self model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Class List' 			false)
				#(10		'definition' 					nil 	model 			editClass 						nil 		editFindReplaceIcon)
				#(20		'comment' 					nil 	model 			editComment 						nil 		editFindReplaceIcon)
				#(30 		'-----')
				#(40		'browse full (b)' 					$b 	window 			browseMethodFull 						nil 		editFindReplaceIcon)
				#(50		'class refs (N)' 					$N 	window 			browseClassRefs 						nil 		classIcon)
				#(60 		'-----')
				#(70		'fileIn' 					nil 	model 			fileInClass 						nil 		updateIcon)
				#(80		'fileOut' 					nil 	model 			fileOutClass 						nil 		fileOutIcon)
				#(90 		'-----')
				#(100		'rename...' 					nil 	model 			renameClass 						nil 		saveAsIcon)
				#(110		'remove' 					nil 	model 			removeClass 						nil 		listRemoveIcon)
				#(120 		'-----')
				#(130		'remove existing' 					nil 	model 			removeClass 						nil 		deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Class List' 			false)
				#(10		'definition' 					nil 	model 			editClass 						nil 		editFindReplaceIcon)
				#(20		'comment' 					nil 	model 			editComment 						nil 		editFindReplaceIcon)
				#(30 		'-----')
				#(80		'fileOut' 					nil 	model 			fileOutClass 						nil 		fileOutIcon)
				#(90 		'-----')
				#(100		'rename...' 					nil 	model 			renameClass 						nil 		saveAsIcon)
				#(110		'remove' 					nil 	model 			removeClass 						nil 		listRemoveIcon)
				#(120 		'-----')
				#(130		'remove existing' 					nil 	model 			removeClass 						nil 		deleteIcon)
			) ].! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/31/2024 16:07:19'!
buildMorphicClassList

	^(PluggableListMorph
		withModel: model
		listGetter: #classList
		indexGetter: #classListIndex
		indexSetter: #classListIndex:)
			menuNshortcutsKey: #classListMenuSpec.! !

!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/31/2024 16:06:37'!
systemCatListKey: aChar from: view

	self flag: #definesKeyboardShortcuts.
	aChar = $r ifTrue: [^ model recent ].
	aChar = $f ifTrue: [ ^ self findClass ].
	aChar = $x ifTrue: [ ^ model removeSystemCategory ].
	aChar = $t ifTrue: [ ^ model runSystemCategoryTests ].
	aChar = $a ifTrue: [ ^ model addSystemCategory ].
	aChar = $A ifTrue: [ ^ model alphabetizeSystemCategories ].
	aChar = $b ifTrue: [ ^ self openSystemCategoryBrowser ].
	aChar = $B ifTrue: [ ^ self browseAllClasses ].
	aChar = $o ifTrue: [ ^ model fileOutSystemCategory ].
	aChar = $u ifTrue: [ ^ model updateSystemCategories ].
	aChar = $R ifTrue: [ ^ model renameSystemCategory ].
! !

!methodRemoval: RefactoringMenues class #classListMenuOptions stamp: 'jmv 5/31/2024 16:14:15'!
RefactoringMenues class removeSelector: #classListMenuOptions!
!methodRemoval: CodeFileBrowserWindow #classListMenu stamp: 'jmv 5/31/2024 16:09:05'!
CodeFileBrowserWindow removeSelector: #classListMenu!
!methodRemoval: CodeFileBrowserWindow #classListKey:from: stamp: 'jmv 5/31/2024 16:10:52'!
CodeFileBrowserWindow removeSelector: #classListKey:from:!
!methodRemoval: BrowserWindow class #classListMenuOptions stamp: 'jmv 5/31/2024 16:14:14'!
BrowserWindow class removeSelector: #classListMenuOptions!
!methodRemoval: BrowserWindow class #classListMoreMenuOptions stamp: 'jmv 5/31/2024 16:14:30'!
BrowserWindow class removeSelector: #classListMoreMenuOptions!
!methodRemoval: BrowserWindow class #moreMenuLabel stamp: 'jmv 5/31/2024 16:14:32'!
BrowserWindow class removeSelector: #moreMenuLabel!
!methodRemoval: BrowserWindow #classListKey:from: stamp: 'jmv 5/31/2024 16:10:52'!
BrowserWindow removeSelector: #classListKey:from:!
!methodRemoval: BrowserWindow #classListMenu stamp: 'jmv 5/31/2024 16:09:28'!
BrowserWindow removeSelector: #classListMenu!
