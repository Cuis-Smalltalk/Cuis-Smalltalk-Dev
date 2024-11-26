'From Cuis7.1 [latest update: #6848] on 25 November 2024 at 4:03:35 pm'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/25/2024 16:02:07'!
classListMenuSpec
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
		#(110	'File Out (alphabetically)' 		nil 	model 		fileOutClassAlphabetically 			fileOutIcon)
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
		#(230	'more...' 					nil 	nil 			2000 								listAddIcon)

		"Shortcut only. No menu entry."
		#(991	nil 							$r 	model 		recent)

		" 'refactorings' SubMenu"
		#(1010	'Rename Class... (R)' 			$R 	model 		renameClass 						saveAsIcon)
		#(1020	'Insert Superclass...' 			nil 	model 		insertSuperclass 					saveAsIcon)
		#(1030 	'-----')
		#(1040	'Add inst var...' 				nil 	model 		addInstVar 							listAddIcon)
		#(1050	'Rename inst var...' 			nil 	model 		renameInstVar 						saveAsIcon)
		#(1060	'Remove inst var...' 			nil 	model 		removeInstVar 						deleteIcon)
		#(1070	'Remove all unreferenced inst var...' nil model 	removeAllUnreferencedInstVar 	deleteIcon)
		#(1080	'Push up inst var...' 			nil 	model 		pushUpInstanceVariable 			goTopIcon)
		#(1090	'Push down inst var...' 		nil 	model 		pushDownInstanceVariable 		goBottomIcon)
		#(1100 	'-----')
		#(1110	'rename global...' 			nil 	model 		renameGlobal 						saveAsIcon)

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
	)! !

