'From Cuis7.1 [latest update: #6828] on 21 November 2024 at 11:52:13 am'!

!BrowserWindow methodsFor: 'menu commands' stamp: 'jmv 11/20/2024 17:11:04'!
browsePackage

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
		BrowserWindow open: browser label: browser labelString ].! !


!MethodSetWindow methodsFor: 'menu commands' stamp: 'jmv 11/21/2024 11:51:35'!
browsePackage

	| package browser |
	"Try to get package of the selected method"
	model currentCompiledMethod ifNotNil: [ :method |
		package := CodePackage packageOfMethod: model currentCompiledMethod asMethodReference ifNone: nil.
		browser := SinglePackageBrowser new.
		browser package: package.
		BrowserWindow open: browser label: browser labelString ].! !


!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/20/2024 17:12:31'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class List' 					false)
		#(10		'browse full (b)' 							$b 	window 			browseMethodFull 									editFindReplaceIcon)
		#(20		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 									goTopIcon)
		#(30		'browse protocol (p)' 							$p 	window 			browseFullProtocol 									spreadsheetIcon)
		#(35		'browse package' 							nil 	window 			browsePackage 									packageIcon)
		#(40 		'-----')
		#(50		'fileOut' 							nil 	model 			fileOutClass 									fileOutIcon)
		#(51		'fileOut (alphabetically)' 							nil 	model 			fileOutClassAlphabetically 									fileOutIcon)
		#(60		'run tests (t)' 							$t 	model 			runClassTests 									weatherFewCloudsIcon)
		#(70 		'-----')
		#(80		'refactorings' 							nil 	nil 			1000 									listAddIcon)
		#(90		'copy class...' 							nil 	model 			copyClass 									copyIcon)
		#(100		'remove class (x)' 							$x 	model 			removeClass 									deleteIcon)
		#(110 		'-----')
		#(120		'show classes hierarchically' 							nil 	model 			enableListClassesHierarchically 									goTopIcon)
		#(130		'show classes alphabetically' 							nil 	model 			enableListClassesAlphabetically 									sendReceiveIcon)
		#(140		'show definition' 							nil 	model 			editClass 									editFindReplaceIcon)
		#(150		'show comment' 							nil 	model 			editComment 									editFindReplaceIcon)
		#(160 		'-----')
		#(170		'class refs (N)' 							$N 	window 			browseClassRefs 									classIcon)
		#(180 		'-----')
		#(190		'more' 							nil 	nil 			2000 									listAddIcon)

		"Shortcut only. No menu entry."
		#(991		nil 							$r 	model 			recent)

		" 'refactorings' SubMenu"
		#(1010		'rename class... (R)' 							$R 	model 			renameClass 									saveAsIcon)
		#(1020		'insert superclass...' 							nil 	model 			insertSuperclass 									saveAsIcon)
		#(1030 		'-----')
		#(1040		'add inst var...' 							nil 	model 			addInstVar 									listAddIcon)
		#(1050		'rename inst var...' 							nil 	model 			renameInstVar 									saveAsIcon)
		#(1060		'remove inst var...' 							nil 	model 			removeInstVar 									deleteIcon)
		#(1070		'remove all unreferenced inst var...' 						
											nil 	model 			removeAllUnreferencedInstVar 									deleteIcon)
		#(1080		'push up inst var...' 							nil 	model 			pushUpInstanceVariable 									goTopIcon)
		#(1090		'push down inst var...' 							nil 	model 			pushDownInstanceVariable 									goBottomIcon)
		#(1100 		'-----')
		#(1110		'rename global...' 							nil 	model 			renameGlobal 									saveAsIcon)

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

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/20/2024 16:31:05'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message List' 					false)
		#(10		'browse full (b)' 							$b 	window 			browseMethodFull 									editFindReplaceIcon)
		#(20		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 									goTopIcon)
		#(30		'browse method (O)' 							$O 	window 			openSingleMessageBrowser 									scriptIcon)
		#(35		'browse package (p)' 							$p 	window 			browsePackage 									packageIcon)
		#(40 		'-----')
		#(50		'fileOut' 							nil 	model 			fileOutMessage 									fileOutIcon)
		#(60		'run test (t)' 							$t 	model 			runMethodTest 									weatherFewCloudsIcon)
		#(70		'debug test (r)' 							$r 	model 			debugMethodTest 									weatherFewCloudsIcon)
		#(80		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 									exploreIcon)
		#(90 		'-----')
		#(100		'refactorings' 							nil 	nil 			1000 									listAddIcon)
		#(110		'senders of... (n)' 							nil 	window 			browseSendersOfMessages 									mailForwardIcon)
		#(120		'implementors of... (m)' 							nil 	window 			browseImplementorsOfMessages 									developmentIcon)
		#(130		'inheritance (i)' 							$i 	window 			methodInheritance 									goDownIcon)
		#(140		'versions (v)' 							$v 	window 			browseVersions 									clockIcon)
		#(150 		'-----')
		#(160		'remove method (x)' 							$x 	model 			removeMessage 									deleteIcon)
		#(170		'remove method in hierarchy' 							nil 	model 			removeMessageInHierarchy 									deleteIcon)
		#(180		'more' 							nil 	nil 			2000 									listAddIcon)

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
		#(2010		'local senders of...' 								nil 	window 			browseLocalSendersOfMessages 											mailForwardIcon)
		#(2020		'local implementors of...' 								nil 	window 			browseLocalImplementorsOfMessages 											classIcon)
		#(2030 		'-----')
		#(2040		'sample instance' 								nil 	window 			makeSampleInstance 											instanceIcon)
		#(2050		'inspect instances' 								nil 	window 			inspectInstances 											inspectIcon)
		#(2060		'inspect subinstances' 								nil 	window 			inspectSubInstances 											inspectIcon)
		#(2070 		'-----')
		#(2080		'change category...' 								nil 	model 			changeCategory 											saveAsIcon)
		#(2090		'select method category (C)' 								$C 	model 			showHomeCategory 											editFindReplaceIcon)
		#(2100		'change sets with this method' 								nil 	window 			findMethodInChangeSets 											changesIcon)
		#(2110		'revert to previous version' 								nil 	model 			revertToPreviousVersion 											undoIcon)
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/21/2024 11:34:15'!
methodCategoriesListMenuSpec
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Categories' 					false)
		#(5		'browse package (p)' 								$p 	window 			browsePackage 										packageIcon)
		#(6		'-----')
		#(10		'fileOut (o)' 								$o 	model 			fileOutMessageCategories 										fileOutIcon)
		#(20 		'-----')
		#(30		'reorganize (r)' 								$r 	model 			editMessageCategories 										sendReceiveIcon)
		#(40		'alphabetize (a)' 								$a 	model 			alphabetizeMessageCategories 										fontXGenericIcon)
		#(50		'remove empty categories (e)' 								$e 	model 			removeEmptyCategories 										listRemoveIcon)
		#(60		'categorize all uncategorized (c)' 								$c 	model 			categorizeAllUncategorizedMethods 										packageIcon)
		#(70		'new category... (n)' 								$n 	model 			addCategory 										newIcon)
		#(80		'new extension category... (N)' 								$N 	model 			addExtensionCategory 										listAddIcon)
		#(90 		'-----')
		#(100		'rename... (R)' 								$R 	model 			renameCategory 										saveAsIcon)
		#(110		'remove (x)' 								$x 	model 			removeMessageCategory 										deleteIcon)
		#(120 		'-----')
		#(130		'run tests (t)' 								$t 	model 			runMessageCategoryTests 										weatherFewCloudsIcon)
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/21/2024 11:43:37'!
systemCategoriesListMenuSpec
	"Spec for the menu and shortcut keys for the system categories list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class Categories' 					false)
		#(10		'find class... (f)' 						$f 	window 			findClass 									findIcon)
		#(20		'recent classes... (r)' 						$r 	model 			recent 									clockIcon)
		#(30 		'-----')
		#(40		'browse all (B)' 						$B 	window 			browseAllClasses 									editFindReplaceIcon)
		#(50		'browse (b)' 						$b 	window 			openSystemCategoryBrowser 									packageIcon)
		#(55		'browse package (p)' 						$p 	window 			browsePackage 									packageIcon)
		#(60 		'-----')
		#(70		'fileOut (o)' 						$o 	model 			fileOutSystemCategory 									fileOutIcon)
		#(80 		'-----')
		#(90		'reorganize' 						nil 	model 			editSystemCategories 									sendReceiveIcon)
		#(100		'alphabetize (A)' 						$A 	model 			alphabetizeSystemCategories 									fontXGenericIcon)
		#(110 		'-----')
		#(120		'update (u)' 						$u 	model 			updateSystemCategories 									updateIcon)
		#(130		'add item... (a)' 						$a 	model 			addSystemCategory 									newIcon)
		#(140		'rename... (R)' 						$R 	model 			renameSystemCategory 									saveAsIcon)
		#(150		'move to... (m)' 						$m 	model 			moveAllToOtherSystemCategory 									saveAsIcon)
		#(160		'remove (x)' 						$x 	model 			removeSystemCategory 									deleteIcon)
		#(170 		'-----')
		#(180		'move to top' 						nil 	model 			moveSystemCategoryTop 									goTopIcon)
		#(190		'move up' 						nil 	model 			moveSystemCategoryUp 									goUpIcon)
		#(200		'move down' 						nil 	model 			moveSystemCategoryDown 									goDownIcon)
		#(210		'move to bottom' 						nil 	model 			moveSystemCategoryBottom 									fileOutIcon)
		#(220 		'-----')
		#(230		'run tests (t)' 						$t 	model 			runSystemCategoryTests 									weatherFewCloudsIcon)
	)! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/21/2024 11:48:43'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Set' 					false)
		#(10		'browse full (b)' 							$b 	window 			browseMethodFull 											editFindReplaceIcon)
		#(20		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 											goTopIcon)
		#(30		'browse method (O)' 							$O 	window 			openSingleMessageBrowser 											scriptIcon)
		#(40		'browse protocol (p)' 							$p 	window 			browseFullProtocol 											spreadsheetIcon)
		#(45		'browse package' 							nil 	window 			browsePackage 											packageIcon)
		#(50 		'-----')
		#(60		'fileOut' 							nil 	model 			fileOutMessage 											fileOutIcon)
		#(70		'run test (t)' 							$t 	model 			runMethodTest 											weatherFewCloudsIcon)
		#(80		'debug test (r)' 							$r 	model 			debugMethodTest 											weatherFewCloudsIcon)
		#(90		'inspect CompiledMethod' 							nil 	model 			inspectCompiledMethod 											exploreIcon)
		#(100 		'-----')
		#(110		'senders of... (n)' 							nil 	window 			browseSendersOfMessages 											mailForwardIcon)
		#(120		'local senders of...' 							nil 	window 			browseLocalSendersOfMessages 											mailForwardIcon)
		#(130		'implementors of... (m)' 							nil 	window 			browseImplementorsOfMessages 											developmentIcon)
		#(140		'local implementors of...' 							nil 	window 			browseLocalImplementorsOfMessages 											developmentIcon)
		#(150		'inheritance (i)' 							$i 	window 			methodInheritance 											goDownIcon)
		#(160		'versions (v)' 							$v 	window 			browseVersions 											clockIcon)
		#(170 		'-----')
		#(180		'remove method (x)' 							$x 	model 			removeMessage 											deleteIcon)
		#(190		'more' 							nil 	nil 			2000 											listAddIcon)

		"Shortcut only. No menu entry."
		#(991		nil 							$m 	window 			browseImplementors)
		#(992		nil 							$n 	window 			browseSenders)
		#(993		nil 							$c 	model 			copySelector)
		#(994		nil 							$N 	window 			browseClassRefs)

		" 'more' SubMenu"
		#(2010		'sample instance' 								nil 	window 			makeSampleInstance 										instanceIcon)
		#(2020		'inspect instances' 								nil 	window 			inspectInstances 										inspectIcon)
		#(2030		'inspect subinstances' 								nil 	window 			inspectSubInstances 										inspectIcon)
		#(2040 		'-----')
		#(2050 		'remove from this browser' 								nil 	model 			removeMessageFromBrowser 										listRemoveIcon)
		#(2060 		'filter message list...' 								nil 	window 			filterMessageList 										findIcon)
		#(2070		'change category...' 								nil 	model 			changeCategory 										saveAsIcon)
		#(2080		'change sets with this method' 								nil 	window 			findMethodInChangeSets 										changesIcon)
		#(2090		'revert to previous version' 								nil 	model 			revertToPreviousVersion 										undoIcon)
		#(2100 		'-----')
		#(2110 		'sort by class hierarchy' 								nil 	model 			sortByClassHierarchy 										classIcon)
		#(2120 		'sort by class name' 								nil 	model 			sortByClassName 										classIcon)
		#(2130 		'sort by selector' 								nil 	model 			sortBySelector 										scriptIcon)
		#(2140 		'sort by date' 								nil 	model 			sortByDate 										dateIcon)
		#(2150 		'reverse sort' 								nil 	model 			sortReverse 										redoIcon)
	).
! !

