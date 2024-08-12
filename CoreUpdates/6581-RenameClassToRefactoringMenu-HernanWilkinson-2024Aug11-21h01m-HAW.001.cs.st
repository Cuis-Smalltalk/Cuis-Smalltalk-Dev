'From Cuis7.1 [latest update: #6580] on 11 August 2024 at 9:05:10 pm'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'HAW 8/11/2024 21:01:04'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class List' 					false)
		#(10		'browse full (b)' 							$b 	window 			browseMethodFull 									editFindReplaceIcon)
		#(20		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 									goTopIcon)
		#(30		'browse protocol (p)' 							$p 	window 			browseFullProtocol 									spreadsheetIcon)
		#(40 		'-----')
		#(50		'fileOut' 							nil 	model 			fileOutClass 									fileOutIcon)
		#(60		'run tests (t)' 							$t 	model 			runClassTests 									weatherFewCloudsIcon)
		#(70 		'-----')
		#(90		'refactorings' 							nil 	nil 			1000 									listAddIcon)
		#(100		'copy class...' 							nil 	model 			copyClass 									copyIcon)
		#(110		'remove class (x)' 							$x 	model 			removeClass 									deleteIcon)
		#(120 		'-----')
		#(130		'show classes hierarchically' 							nil 	model 			enableListClassesHierarchically 									goTopIcon)
		#(140		'show classes alphabetically' 							nil 	model 			enableListClassesAlphabetically 									sendReceiveIcon)
		#(150		'show definition' 							nil 	model 			editClass 									editFindReplaceIcon)
		#(160		'show comment' 							nil 	model 			editComment 									editFindReplaceIcon)
		#(170 		'-----')
		#(180		'inst var refs...' 							nil 	window 			browseInstVarRefs 									instanceIcon)
		#(190		'inst var defs...' 							nil 	window 			browseInstVarDefs 									instanceIcon)
		#(200 		'-----')
		#(210		'class var refs...' 							nil 	window 			browseClassVarRefs 									classIcon)
		#(220		'class vars' 							nil 	window 			browseClassVariables 									classIcon)
		#(230		'class refs (N)' 							$N 	window 			browseClassRefs 									classIcon)
		#(240 		'-----')
		#(250		'more' 							nil 	nil 			2000 									listAddIcon)

		"Shortcut only. No menu entry."
		#(991		nil 							$r 	model 			recent)

		" 'refactorings' SubMenu"
		#(1005		'rename class... (R)' 							$R 	model 			renameClass 									saveAsIcon)
		#(1010		'insert superclass...' 							nil 	model 			insertSuperclass 									saveAsIcon)
		#(1020 		'-----')
		#(1030		'add inst var...' 							nil 	model 			addInstVar 									listAddIcon)
		#(1040		'rename inst var...' 							nil 	model 			renameInstVar 									saveAsIcon)
		#(1050		'remove inst var...' 							nil 	model 			removeInstVar 									deleteIcon)
		#(1060		'remove all unreferenced inst var...' 						
											nil 	model 			removeAllUnreferencedInstVar 									deleteIcon)
		#(1070		'push up inst var...' 							nil 	model 			pushUpInstanceVariable 									goTopIcon)
		#(1080		'push down inst var...' 							nil 	model 			pushDownInstanceVariable 									goBottomIcon)
		#(1090 		'-----')
		#(1100		'rename global...' 							nil 	model 			renameGlobal 									saveAsIcon)

		" 'more' SubMenu"
		#(2010		'unsent methods' 							nil 	window 			browseUnusedMethods 									junkIcon
					'browse all methods defined by this class that have no senders')
		#(2020		'unreferenced inst vars' 							nil 	window 			showUnreferencedInstVars 									junkIcon
					'show a list of all instance variables that are not referenced in methods')
		#(2030		'unreferenced class vars' 							nil 	window 			showUnreferencedClassVars 									junkIcon
					'show a list of all class variables that are not referenced in methods')
		#(2040		'subclass template' 							nil 	model 			makeNewSubclass 									classIcon
					'put a template into the code pane for defining of a subclass of this class')
		#(2050 		'-----')
		#(2060		'sample instance' 							nil 	window 			makeSampleInstance 									instanceIcon
					'give me a sample instance of this class, if possible')
		#(2070		'inspect instances' 							nil 	window 			inspectInstances 									inspectIcon
					'open an inspector on all the extant instances of this class')
		#(2080		'inspect subinstances' 							nil 	window 			inspectSubInstances 									inspectIcon
					'open an inspector on all the extant instances of this class and of all of its subclasses')
		#(2090 		'-----')
		#(2100		'create inst var accessors' 							nil 	model 			createInstVarAccessors 									sendReceiveIcon
					'compile instance-variable access methods for any instance variables that do not yet have them')
	)! !

