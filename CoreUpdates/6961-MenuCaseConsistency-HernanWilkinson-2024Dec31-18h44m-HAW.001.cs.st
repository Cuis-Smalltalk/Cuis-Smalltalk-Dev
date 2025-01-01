'From Cuis7.3 [latest update: #6960] on 31 December 2024 at 7:01:56 pm'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'HAW 12/31/2024 19:00:05'!
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
		#(230	'More...' 					nil 	nil 			2000 								listAddIcon)

		"Shortcut only. No menu entry."
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
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'HAW 12/31/2024 18:52:17'!
messageCatListMenuSpec
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
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'HAW 12/31/2024 19:00:12'!
messageListMenuSpec
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
		#(50	'File Out' 								nil 	model 		fileOutMessage 							fileOutIcon)
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
		#(180	'More...' 								nil 	nil 			2000 										listAddIcon)

		"Shortcut only. No menu entry."
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
	)! !

