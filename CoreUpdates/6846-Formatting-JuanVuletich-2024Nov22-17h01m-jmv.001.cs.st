'From Cuis7.1 [latest update: #6845] on 22 November 2024 at 5:02:09 pm'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:28:20'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class List' 					false)
		#(10	'browse full (b)' 				$b 	window 	browseMethodFull 					editFindReplaceIcon)
		#(20	'browse hierarchy (h)' 		$h 	window 	browseHierarchy 					goTopIcon)
		#(30	'browse protocol (p)' 			$p 	window 	browseFullProtocol 				spreadsheetIcon)
		#(35	'browse package' 			nil 	window 	browsePackage 					packageIcon)
		#(40 	'-----')
		#(50	'fileOut' 						nil 	model 		fileOutClass 						fileOutIcon)
		#(51	'fileOut (alphabetically)' 		nil 	model 		fileOutClassAlphabetically 			fileOutIcon)
		#(60	'run tests (t)' 				$t 	model 		runClassTests 						weatherFewCloudsIcon)
		#(70 	'-----')
		#(80	'refactorings' 				nil 	nil 			1000 								listAddIcon)
		#(90	'copy class...' 				nil 	model 		copyClass 							copyIcon)
		#(100	'remove class (x)' 			$x 	model 		removeClass 						deleteIcon)
		#(110 	'-----')
		#(120	'show classes hierarchically' 	nil model 		enableListClassesHierarchically 	goTopIcon)
		#(130	'show classes alphabetically' 	nil model 		enableListClassesAlphabetically 	sendReceiveIcon)
		#(140	'show definition' 				nil 	model 		editClass 							editFindReplaceIcon)
		#(150	'show comment' 				nil 	model 		editComment 						editFindReplaceIcon)
		#(160 	'-----')
		#(170	'class refs (N)' 				$N 	window 	browseClassRefs 					classIcon)
		#(180 	'-----')
		#(190	'more' 						nil 	nil 			2000 								listAddIcon)

		"Shortcut only. No menu entry."
		#(991	nil 							$r 	model 		recent)

		" 'refactorings' SubMenu"
		#(1010	'rename class... (R)' 			$R 	model 		renameClass 						saveAsIcon)
		#(1020	'insert superclass...' 			nil 	model 		insertSuperclass 					saveAsIcon)
		#(1030 	'-----')
		#(1040	'add inst var...' 				nil 	model 		addInstVar 							listAddIcon)
		#(1050	'rename inst var...' 			nil 	model 		renameInstVar 						saveAsIcon)
		#(1060	'remove inst var...' 			nil 	model 		removeInstVar 						deleteIcon)
		#(1070	'remove all unreferenced inst var...' 						
											nil 	model 		removeAllUnreferencedInstVar 	deleteIcon)
		#(1080	'push up inst var...' 			nil 	model 		pushUpInstanceVariable 			goTopIcon)
		#(1090	'push down inst var...' 		nil 	model 		pushDownInstanceVariable 		goBottomIcon)
		#(1100 	'-----')
		#(1110	'rename global...' 			nil 	model 		renameGlobal 						saveAsIcon)

		" 'more' SubMenu"
		#(2010	'unsent methods' 			nil 	window 	browseUnusedMethods 			junkIcon
						'browse all methods defined by this class that have no senders')
		#(2020	'unreferenced inst vars' 		nil 	window 	showUnreferencedInstVars 		junkIcon
						'show a list of all instance variables that are not referenced in methods')
		#(2030	'unreferenced class vars' 		nil 	window 	showUnreferencedClassVars 		junkIcon
						'show a list of all class variables that are not referenced in methods')
		#(2040	'subclass template' 			nil 	model 		makeNewSubclass 					classIcon
						'put a template into the code pane for defining of a subclass of this class')
		#(2050 	'-----')
		#(2060	'sample instance' 			nil 	window 	makeSampleInstance 				instanceIcon
						'give me a sample instance of this class, if possible')
		#(2070	'inspect instances' 			nil 	window 	inspectInstances 					inspectIcon
						'open an inspector on all the extant instances of this class')
		#(2080	'inspect subinstances' 		nil 	window 	inspectSubInstances 				inspectIcon
						'open an inspector on all the extant instances of this class and of all of its subclasses')
		#(2090 	'-----')
		#(2100	'create inst var accessors' 	nil 	model 		createInstVarAccessors 			sendReceiveIcon
					'compile instance-variable access methods for any instance variables that do not yet have them')
	)! !


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:19:03'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^ model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Class List' 			false)
				#(10	'definition' 				nil 	model 		editClass 							editFindReplaceIcon)
				#(20	'comment' 				nil 	model 		editComment 						editFindReplaceIcon)
				#(30      'show hierarchically' 		nil   model          enableListClassesHierarchically 	 goTopIcon)		
				#(40 	'show alphabetically' 		nil 	model 		enableListClassesAlphabetically 	sendReceiveIcon)
				#(50 	'-----')
				#(60	'browse full (b)' 			$b 	window 	browseMethodFull 					editFindReplaceIcon)
				#(70	'class refs (N)' 			$N 	window 	browseClassRefs 					classIcon)
				#(80 	'-----')
				#(90	'fileIn' 					nil 	model 		fileInClass 							updateIcon)
				#(100	'fileOut' 					nil 	model 		fileOutClass 						fileOutIcon)
				#(110    'browse changes'			nil   model          browseCodeFileChanges  			changesIcon)
				#(120 	'-----')
				#(130	'rename...' 				nil 	model 		renameClass 						saveAsIcon)
				#(140	'remove' 				nil 	model 		removeClass 						listRemoveIcon)
				#(150 	'-----')
				#(160	'remove existing' 		nil 	model 		removeClass 						deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Class List' 			false)
				#(10	'definition' 				nil 	model 		editClass 							editFindReplaceIcon)
				#(20	'comment' 				nil 	model 		editComment 						editFindReplaceIcon)
				#(30      'show hierarchically' 		nil   model          enableListClassesHierarchically 	goTopIcon)		
				#(40 	'show alphabetically' 		nil 	model 		enableListClassesAlphabetically 	 sendReceiveIcon)
				#(50 	'-----')
				#(80	'fileOut' 					nil 	model 		fileOutClass 						fileOutIcon)
				#(90 	'-----')
				#(100	'rename...' 				nil 	model 		renameClass 						saveAsIcon)
				#(110	'remove' 				nil 	model 		removeClass 						listRemoveIcon)
				#(120 	'-----')
				#(130	'remove existing' 		nil 	model 		removeClass 						deleteIcon)
			) ].! !


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:20:23'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Class List' 						false)
			#(10 	'Delete class from change set (d)' 	$d 	model 		forgetClass 			warningIcon)
			#(20 	'Remove class from system (x)' 	$x 	model 		removeClass 			deleteIcon)
			#(30 	'-----')
			#(40 	'Browse full (b)' 					$b 	window 	browseMethodFull 		editFindReplaceIcon)
			#(50 	'browse hierarchy (h)' 			$h 	window 	browseHierarchy 		goTopIcon)
			#(60 	'browse protocol (p)' 				$p 	window 	browseFullProtocol 	spreadsheetIcon)
			#(70 	'-----')		
			#(80 	'Class refs (N)' 					$N 	window 	browseClassRefs 		classIcon)
	).
! !


!ProtocolBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:19:36'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class List' 					false)
		#(10	'browse full (b)' 			$b 	window 	browseMethodFull 		editFindReplaceIcon)
		#(20	'browse hierarchy (h)' 	$h 	window 	browseHierarchy 		goTopIcon)
	)! !

