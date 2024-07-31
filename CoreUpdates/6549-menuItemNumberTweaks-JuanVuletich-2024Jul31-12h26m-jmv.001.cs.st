'From Cuis7.1 [latest update: #6548] on 31 July 2024 at 12:35:21 pm'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 7/31/2024 12:27:53'!
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
		#(80		'rename class... (R)' 							$R 	model 			renameClass 									saveAsIcon)
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

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 7/31/2024 12:28:47'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
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
		#(90 		'-----')
		#(100		'fileOut' 							nil 	model 			fileOutMessage 									fileOutIcon)
		#(110		'run test (t)' 							$t 	model 			runMethodTest 									weatherFewCloudsIcon)
		#(120		'debug test (r)' 							$r 	model 			debugMethodTest 									weatherFewCloudsIcon)
		#(130		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 									exploreIcon)
		#(140 		'-----')
		#(150		'refactorings' 							nil 	nil 			1000 									listAddIcon)
		#(160		'senders of... (n)' 							nil 	window 			browseSendersOfMessages 									mailForwardIcon)
		#(170		'implementors of... (m)' 							nil 	window 			browseMessages 									developmentIcon)
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

		"Shortcut only. No menu entry."
		#(991		nil 							$m 	window 			browseImplementors)
		#(992		nil 							$n 	window 			browseSenders)
		#(993		nil 							$c 	model 			copySelector)

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
		#(2110		'select method category (C)' 								$C 	model 			showHomeCategory 									editFindReplaceIcon)
		#(2120		'change sets with this method' 								nil 	window 			findMethodInChangeSets 									changesIcon)
		#(2130		'revert to previous version' 								nil 	model 			revertToPreviousVersion 									undoIcon)
	)! !


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 7/31/2024 12:29:46'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the message list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Message List' 						true)
			#(10 		'delete method from change set (d)' 									$d 	model 			forget 								warningIcon)
			#(20 		'remove method from system (x)' 									$x 	model 			removeMessage 								deleteIcon)
			#(30 		'-----')
			#(40 		'browse full (b)' 									$b 	window 			browseMethodFull 								editFindReplaceIcon)
			#(50 		'browse hierarchy (h)' 									$h 	window 			browseHierarchy 								goTopIcon)
			#(60 		'browse method (O)' 									$O 	window 			openSingleMessageBrowser 								scriptIcon)
			#(70 		'browse protocol (p)' 									$p 	window 			browseFullProtocol 								spreadsheetIcon)
			#(80 		'-----')		
			#(90 		'fileOut (o)' 									$o 	model 			fileOutMessage 								fileOutIcon)
			#(100 		'-----')		
			#(110		'senders of... (n)' 									nil 	window 			browseSendersOfMessages 								mailForwardIcon)
			#(120		'implementors of... (m)' 									nil 	window 			browseMessages 								developmentIcon)
			#(130 		'inheritance (i)' 									$i 	window 			methodInheritance 								goDownIcon)
			#(140 		'versions (v)' 									$v 	window 			browseVersions 								clockIcon)

			"Shortcut only. No menu entry."
			#(991		nil 									$m 	window 			browseImplementors)
			#(992		nil 									$n 	window 			browseSenders)
	).
! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 7/31/2024 12:31:48'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Set' 					false)
		#(10		'what to show...' 							nil 	window 			offerWhatToShowMenu 									preferencesIcon)
		#(20		'toggle break on entry' 							nil 	model 			toggleBreakOnEntry 									debugIcon)
		#(30 		'-----')
		#(40		'browse full (b)' 							$b 	window 			browseMethodFull 									editFindReplaceIcon)
		#(50		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 									goTopIcon)
		#(60		'browse method (O)' 							$O 	window 			openSingleMessageBrowser 									scriptIcon)
		#(70		'browse protocol (p)' 							$p 	window 			browseFullProtocol 									spreadsheetIcon)
		#(90 		'-----')
		#(100		'fileOut' 							nil 	model 			fileOutMessage 									fileOutIcon)
		#(110		'run test (t)' 							$t 	model 			runMethodTest 									weatherFewCloudsIcon)
		#(120		'debug test (r)' 							$r 	model 			debugMethodTest 									weatherFewCloudsIcon)
		#(130		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 									exploreIcon)
		#(140 		'-----')
		#(160		'senders of... (n)' 							nil 	window 			browseSendersOfMessages 									mailForwardIcon)
		#(170		'implementors of... (m)' 							nil 	window 			browseMessages 									developmentIcon)
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
		#(290		'more' 							nil 	nil 			2000 									listAddIcon)

		"Shortcut only. No menu entry."
		#(991		nil 							$m 	window 			browseImplementors)
		#(992		nil 							$n 	window 			browseSenders)
		#(993		nil 							$c 	model 			copySelector)

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
		#(2100 		'remove from this browser' 								nil 	model 			removeMessageFromBrowser 									listRemoveIcon)
		#(2110 		'filter message list...' 								nil 	window 			filterMessageList 									findIcon)
		#(2120		'change category...' 								nil 	model 			changeCategory 									saveAsIcon)
		#(2130		'change sets with this method' 								nil 	window 			findMethodInChangeSets 									changesIcon)
		#(2140		'revert to previous version' 								nil 	model 			revertToPreviousVersion 									undoIcon)
		#(2150 		'-----')
		#(2160 		'sort by class hierarchy' 								nil 	model 			sortByClassHierarchy 									classIcon)
		#(2170 		'sort by class name' 								nil 	model 			sortByClassName 									classIcon)
		#(2180 		'sort by selector' 								nil 	model 			sortBySelector 									scriptIcon)
		#(2190 		'sort by date' 								nil 	model 			sortByDate 									dateIcon)
		#(2200 		'reverse sort' 								nil 	model 			sortReverse 									redoIcon)
	).
! !

