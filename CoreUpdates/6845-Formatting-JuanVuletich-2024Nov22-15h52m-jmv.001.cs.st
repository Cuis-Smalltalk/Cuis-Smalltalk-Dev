'From Cuis7.1 [latest update: #6844] on 22 November 2024 at 3:54:46 pm'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 14:51:43'!
messageCatListMenuSpec
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Categories' 			false)
		#(5		'browse package (p)' 				$p 	window 	browsePackage 						packageIcon)
		#(6		'-----')
		#(10	'fileOut (o)' 						$o 	model 		fileOutMessageCategories 				fileOutIcon)
		#(20 	'-----')
		#(30	'reorganize (r)' 					$r 	model 		editMessageCategories 				sendReceiveIcon)
		#(40	'alphabetize (a)' 					$a 	model 		alphabetizeMessageCategories 		fontXGenericIcon)
		#(50	'remove empty categories (e)' 	$e 	model 		removeEmptyCategories 				listRemoveIcon)
		#(60	'categorize all uncategorized (c)' 	$c 	model 		categorizeAllUncategorizedMethods 	packageIcon)
		#(70	'new category... (n)' 				$n 	model 		addCategory 							newIcon)
		#(80	'new extension category... (N)' 	$N 	model 		addExtensionCategory 					listAddIcon)
		#(90 	'-----')
		#(100	'rename... (R)' 					$R 	model 		renameCategory 						saveAsIcon)
		#(110	'remove (x)' 						$x 	model 		removeMessageCategory 				deleteIcon)
		#(120 	'-----')
		#(130	'run tests (t)' 					$t 	model 		runMessageCategoryTests 			weatherFewCloudsIcon)
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 14:28:41'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message List' 						false)
		#(10	'browse full (b)' 							$b 	window 	browseMethodFull 							editFindReplaceIcon)
		#(20	'browse hierarchy (h)' 					$h 	window 	browseHierarchy 							goTopIcon)
		#(30	'browse method (O)' 						$O 	window 	openSingleMessageBrowser 				scriptIcon)
		#(35	'browse package (p)' 						$p 	window 	browsePackage 							packageIcon)
		#(40 	'-----')
		#(50	'fileOut' 									nil 	model 		fileOutMessage 							fileOutIcon)
		#(60	'run test (t)' 								$t 	model 		runMethodTest 							weatherFewCloudsIcon)
		#(70	'debug test (r)' 							$r 	model 		debugMethodTest 							weatherFewCloudsIcon)
		#(80	'inspect CompiledMethod' 				nil 	model 		inspectCompiledMethod 					exploreIcon)
		#(90 	'-----')
		#(100	'refactorings' 							nil 	nil 			1000 										listAddIcon)
		#(110	'senders of... (n)' 						nil 	window 	browseSendersOfMessages 				mailForwardIcon)
		#(120	'implementors of... (m)' 					nil 	window 	browseImplementorsOfMessages 			developmentIcon)
		#(130	'inheritance (i)' 							$i 	window 	methodInheritance 						goDownIcon)
		#(140	'versions (v)' 							$v 	window 	browseVersions 							clockIcon)
		#(150 	'-----')
		#(160	'remove method (x)' 						$x 	model 		removeMessage 							deleteIcon)
		#(170	'remove method in hierarchy' 				nil 	model 		removeMessageInHierarchy 				deleteIcon)
		#(180	'more' 									nil 	nil 			2000 										listAddIcon)

		"Shortcut only. No menu entry."
		#(991	nil 										$m 	window 	browseImplementors)
		#(992	nil 										$n 	window 	browseSenders)
		#(993	nil 										$c 	model 		copySelector)

		" 'refactorings' SubMenu"
		#(1010	'rename... (R)' 							$R 	model 		renameSelector 							saveAsIcon)
		#(1020	'change keyword order...' 					nil 	model 		changeKeywordOrder 						sendReceiveIcon)
		#(1030	'add parameter... (A)' 					$A 	model 		addParameter 								listAddIcon)
		#(1040	'remove parameter... (S)' 					$S 	model 		removeParameter 							listRemoveIcon)
		#(1050 	'-----')
		#(1060	'inline method... (F)' 						$F 	model 		inlineMethod 								saveAsIcon)
		#(1070	'move to instance/class method' 			nil 	model 		moveToInstanceOrClassMethod 			changesIcon)
		#(1080	'push up' 								nil 	model 		pushUpSelector 							goTopIcon)
		#(1090	'add in superclass as subclassResponsability' nil model 		addAsSubclassResponsabilitySelector 		goTopIcon)
		#(1100	'push down to subclasses' 				nil 	model 		pushDownSelectorToSubclasses 			goBottomIcon)
		#(1110	'push down to one subclass' 				nil 	model 		pushDownSelectorToOneSubclass 			goBottomIcon)

		" 'more' SubMenu"
		#(2010	'local senders of...' 						nil 	window 	browseLocalSendersOfMessages 			mailForwardIcon)
		#(2020	'local implementors of...' 					nil 	window 	browseLocalImplementorsOfMessages 	classIcon)
		#(2030 	'-----')
		#(2040	'sample instance' 						nil 	window 	makeSampleInstance 						instanceIcon)
		#(2050	'inspect instances' 						nil 	window 	inspectInstances 							inspectIcon)
		#(2060	'inspect subinstances' 					nil 	window 	inspectSubInstances 						inspectIcon)
		#(2070 	'-----')
		#(2080	'change category...' 						nil 	model 		changeCategory 							saveAsIcon)
		#(2090	'select method category (C)' 				$C 	model 		showHomeCategory 						editFindReplaceIcon)
		#(2100	'change sets with this method' 			nil 	window 	findMethodInChangeSets 					changesIcon)
		#(2110	'revert to previous version' 				nil 	model 		revertToPreviousVersion 					undoIcon)
	)! !


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 14:59:08'!
codeFileListMenuSpec
	"Spec for the menu and shortcut keys for the single entry filename list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := 
			#(
				#(0 		'Code File' 			false)
				#(10	'find class... (f)' 			$f 	window 	findClass)
				#(20 	'-----') ).
	model baseCodeSource isLiveSmalltalkImage ifTrue: [
			answer := answer, #(
				#(30	'fileIn' 					nil 	model 		fileIn) )].
	answer := answer, 
			#(
				#(40	'fileOut' 					nil 	model 		fileOut) ).
	model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(50	'remove existing' 		nil 	model 		removeUnmodifiedClasses) )].
	^answer! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 14:52:42'!
messageCatListMenuSpec
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Message Categories' false)
				#(10	'fileIn' 				nil 	model 		fileInMessageCategories 		updateIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Message Categories' false)
			)].
	answer := answer, 
			#(
				#(20	'fileOut' 				nil 	model 		fileOutMessageCategories 		fileOutIcon)
				#(30 	'-----')
				#(40	'reorganize' 			nil 	model 		editMessageCategories 		sendReceiveIcon)
				#(50 	'-----')
				#(60	'add item...' 			nil 	model 		addCategory 					newIcon)
				#(70 	'-----')
				#(80	'rename...' 			nil 	model 		renameCategory 				saveAsIcon)
				#(90	'remove' 			nil 	model 		removeMessageCategory 		listRemoveIcon)
			).
	model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(100 	'-----')
				#(100	'remove existing' 	nil 	model 		removeUnmodifiedMethods 	deleteIcon)
		) ].
	^answer! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 14:21:46'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^ model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Messages' 				false)
				#(10	'fileIn' 					nil 	model 		fileInMessage 			updateIcon)
				#(20	'fileOut' 					nil 	model 		fileOutMessage 		fileOutIcon)
				#(30 	'-----')
				#(40	'senders (n)' 				$n 	window 	browseSenders 		mailForwardIcon)
				#(50	'implementors (m)' 		$m 	window 	browseImplementors 	developmentIcon)
				#(60	'method inheritance (h)' 	$h 	window 	methodInheritance 	goDownIcon)
				#(70	'browse full (b)' 			$b 	window 	browseMethodFull 		editFindReplaceIcon)
				#(80	'versions (v)' 			nil 	window 	browseVersions 		clockIcon)
				#(90 	'-----')
				#(100	'remove method (x)' 		$x 	model 		removeMessage 		deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Messages' 				false)
				#(10	'fileOut' 					nil 	model 		fileOutMessage 		fileOutIcon)
				#(20 	'-----')
				#(30	'remove method (x)' 		$x 	model 		removeMessage 		deleteIcon)
			)].! !


!ChangeListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:08:05'!
listMenuSpec
	"Spec for the menu and shortcut keys for the changes list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Change List' 									true)
		#(10	'fileIn selections' 									nil 	model 	fileInSelections 						nil
					'import the selected items into the image')
		#(20	'fileOut selections...	' 							nil 	model 	fileOutSelections 						nil
		 			'create a new file containing the selected items')
		#(30	'fileOut current version of selections...' 			nil 	model 	fileOutCurrentVersionsOfSelections 	nil
 					'create a new file containing the current (in-image) counterparts of the selected methods')
		#(40 	'-----')
		#(50	'compare to current' 								nil 	model 	compareToCurrentVersion 				nil
 					'open a separate window which shows the text differences between the on-file version and the in-image version.')
		#(60 	'-----')
		#(70	'select new methods' 								nil 	model 	selectNewMethods 					nil
 					'select methods in the file that do not currently exist in the image')
		#(80	'select changes for absent classes' 				nil 	model 	selectAllForAbsentClasses 				nil
					'select methods in the file for classes that are not defined in the image')
		#(90	'select all changes for this class' 					nil 	model 	selectAllForThisClass 					nil
					'select all methods in the file that belong to the currently-selected class')
		#(100	'select unchanged methods' 						nil 	model 	selectUnchangedMethods 				nil
					'select methods in the file whose in-image versions are the same as their in-file counterparts')
		#(110	'select methods equivalent to current' 				nil 	model 	selectEquivalentMethods 				nil
					'select methods in the file whose in-image versions have the same behavior as their in-file counterparts')
		#(120	'select methods older than current' 				nil 	model 	selectMethodsOlderThanCurrent 		nil
					'select methods in the file that are older than the one currently in the image')
		#(130	'select removals of sent methods' 					nil 	model 	selectRemovalsOfSent 					nil
					'select all method removals of methods that have some sender in the image')
		#(140 	'-----')
		#(150	'select all (a)' 									$a 	model 	selectAll 								nil
					'select all the items in the list')
		#(160	'deselect all (A)' 									$A 	model 	deselectAll 								nil
					'deselect all the items in the list')
		#(170	'invert selections' 								nil 	model 	invertSelections 						nil
					'select every item that is not currently selected, and deselect every item that *is* currently selected')
		#(180 	'-----')
		#(190	'browse class and method' 						nil 	window browseMethodFull 					nil
					'open a full browser showing the selected method')
		#(200	'browse all versions of single selection' 			nil 	window browseVersions 						nil
					'open a version browser showing the versions of the currently selected method')
		#(210	'browse current versions of selections' 				nil 	window browseCurrentVersionsOfSelections 	nil
					'open a message-list browser showing the current (in-image) counterparts of the selected methods')
		#(220	'destroy current methods of selections' 			nil 	model 	destroyCurrentCodeOfSelections 		nil
					'remove (*destroy*) the in-image counterparts of all selected methods')
		#(230 	'-----')
		#(240	'remove doIts' 									nil 	model 	removeDoIts 							nil
					'remove all items that are doIts rather than definitions')
		#(250	'remove older versions' 							nil 	model 	removeOlderMethodVersions 			nil
					'remove all but the most recent versions of methods in the list')
		#(260	'remove up-to-date versions' 					nil 	model 	removeUpToDate 						nil
					'remove all items whose code is the same as the counterpart in-image code')
		#(270	'remove empty class comments' 					nil 	model 	removeEmptyClassComments 			nil
					'remove all empty class comments')
		#(280	'remove selected items' 							nil 	model 	removeSelections 						nil
					'remove the selected items from the change-list')
		#(290	'remove unselected items' 						nil 	model 	removeNonSelections 					nil
					'remove all the items not currently selected from the change-list')
	)! !


!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:14:25'!
methodVersionsMenuSpec
	"Spec for the menu and shortcut keys for a method version in the versions list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
				#(0 		'Versions' 									true)
	).
	model listIndex > 0 ifTrue: [
		(model list size > 1 ) ifTrue: [
			answer := answer, #(
				#(10		'compare to current' 									nil 	model 		compareToCurrentVersion 		nil
							'compare selected version to the current version')
				#(20		'compare to version...' 								nil 	model 		compareToOtherVersion 		nil
							'compare selected version to another selected version'))].
		answer := answer, #(
				#(30		'revert to selected version (z)' 						$z 	model 		fileInSelections 				nil
							'resubmit the selected version, so that it becomes the current version'))].
	answer := answer, #(
				#(40		'edit current method (O)' 								$O 	window 	openSingleMessageBrowser 	nil
							'open a single-message browser on the current version of this method')
				#(50 		'-----')
				#(60		'update list' 											nil 	model 		reformulateList 				nil
							'reformulate the list of versions, in case it somehow got out of synch with reality')
				#(70 		'-----')
				#(80		'senders (n)' 											$n 	window 	browseSenders 				nil
							'browse all senders of this selector')
				#(90		'implementors (m)' 									$m 	window 	browseImplementors 			nil
							'browse all implementors of this selector')
				#(100 		'-----')
				#(110		'help...' 												nil 	model 		offerVersionsHelp 				nil
							'provide an explanation of the use of this tool')
	).
	^answer! !


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 14:19:27'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the message list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Message List' 						true)
			#(10 	'Delete method from change set (d)' 	$d 	model 		forget 								warningIcon)
			#(20 	'Remove method from system (x)' 		$x 	model 		removeMessage 					deleteIcon)
			#(30 	'-----')
			#(40 	'Browse full (b)' 						$b 	window 	browseMethodFull 					editFindReplaceIcon)
			#(50 	'Browse hierarchy (h)' 				$h 	window 	browseHierarchy 					goTopIcon)
			#(60 	'Browse method (O)' 					$O 	window 	openSingleMessageBrowser 		scriptIcon)
			#(80 	'-----')
			#(90 	'FileOut (o)' 							$o 	model 		fileOutMessage 					fileOutIcon)
			#(100 	'-----')		
			#(110	'Senders of... (n)' 					nil 	window 	browseSendersOfMessages 		mailForwardIcon)
			#(120	'Implementors of... (m)' 				nil 	window 	browseImplementorsOfMessages 	developmentIcon)
			#(130 	'Inheritance (i)' 						$i 	window 	methodInheritance 				goDownIcon)
			#(140 	'Versions (v)' 						$v 	window 	browseVersions 					clockIcon)

			"Shortcut only. No menu entry."
			#(991	nil 									$m 	window 	browseImplementors)
			#(992	nil 									$n 	window 	browseSenders)
	).
! !


!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:00:35'!
contextStackMenuSpec
	"Spec for the menu and shortcut keys for call stack list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10	'fullStack (f)' 			$f 	window 	fullStack 				nil 	'show the complete call stack')
		#(20	'proceed (p)' 			$p 	window 	proceed 				nil 	'close the debugger and proceed')
		#(30	'restart (r)' 				$r 	window 	restart 					nil 	'reset this context to its start')
		#(40	'step into send (e)' 		$e 	window 	send 					nil 	'step into message sends')
		#(50	'step over send (t)' 		$t 	window 	doStep 					nil 	'step over message sends')
		#(60	'step into block (T)' 		$T 	window 	stepIntoBlock 			nil 	'step into a code block')
		#(70	'run to cursor' 			nil 	window 	runToCursor 			nil 	'run to cursor')
		#(80	'where (w)' 				$w 	window 	where 					nil 	'select current pc range')
		#(90	'peel to first like this' 		nil 	window 	peelToFirst 			nil 	'peel stack back to the second occurance of selected message')
		#(100 	'-----')
		#(110	'return entered value' 	nil 	window 	returnValue 			nil 	'lets you enter an expression and return method answering result')
		#(120	'toggle break on entry' 	nil 	model 		toggleBreakOnEntry 	nil 	'install or uninstall a breakpoint on method entry')
		
		#(991	nil 						$m 	window 	browseImplementors)
		#(992	nil 						$n 	window 	browseSenders)
		#(993	nil 						$i 	window 	methodInheritance)
		#(994	nil 						$v 	window 	browseVersions)
		#(995	nil 						$N 	window 	browseClassRefs)
	)! !

!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:01:16'!
contextVariablesListMenuSpec
	"Spec for the menu and shortcut keys for the list of variables in this context.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10	'inspect (i)' 				$i 	window 	inspectContextSelection 		inspectIcon)
		#(20	'explore (I)' 				$I 	window 	exploreContextSelection 		exploreIcon)
		#(30 	'-----')
		#(40	'browser full (b)' 			$b 	window 	contextClassBrowse 			editFindReplaceIcon)
		#(50	'browser hierarch (h)' 	$h 	window 	contextClassHierarchy 			goTopIcon)
		#(60	'browser protocol (p)' 	$p 	window 	contextClassProtocol 			spreadsheetIcon)
	)! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 14:15:43'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Set' 				false)
		#(10	'browse full (b)' 				$b 	window browseMethodFull 						editFindReplaceIcon)
		#(20	'browse hierarchy (h)' 		$h 	window browseHierarchy 							goTopIcon)
		#(30	'browse method (O)' 			$O 	window openSingleMessageBrowser 				scriptIcon)
		#(40	'browse protocol (p)' 			$p 	window browseFullProtocol 						spreadsheetIcon)
		#(45	'browse package' 			nil 	window browsePackage 							packageIcon)
		#(50 	'-----')
		#(60	'fileOut' 						nil 	model 	fileOutMessage 							fileOutIcon)
		#(70	'run test (t)' 					$t 	model 	runMethodTest 							weatherFewCloudsIcon)
		#(80	'debug test (r)' 				$r 	model 	debugMethodTest 							weatherFewCloudsIcon)
		#(90	'inspect CompiledMethod' 	nil 	model 	inspectCompiledMethod 					exploreIcon)
		#(100 	'-----')
		#(110	'senders of... (n)' 			nil 	window browseSendersOfMessages 				mailForwardIcon)
		#(120	'local senders of...' 			nil 	window browseLocalSendersOfMessages 			mailForwardIcon)
		#(130	'implementors of... (m)' 		nil 	window browseImplementorsOfMessages 			developmentIcon)
		#(140	'local implementors of...' 		nil 	window browseLocalImplementorsOfMessages 	developmentIcon)
		#(150	'inheritance (i)' 				$i 	window methodInheritance 						goDownIcon)
		#(160	'versions (v)' 				$v 	window browseVersions 							clockIcon)
		#(170 	'-----')
		#(180	'remove method (x)' 			$x 	model 	removeMessage 							deleteIcon)
		#(190	'more' 						nil 	nil 		2000 										listAddIcon)

		"Shortcut only. No menu entry."
		#(991	nil 							$m 	window browseImplementors)
		#(992	nil 							$n 	window browseSenders)
		#(993	nil 							$c 	model 	copySelector)
		#(994	nil 							$N 	window browseClassRefs)

		" 'more' SubMenu"
		#(2010	'sample instance' 			nil 	window makeSampleInstance 						instanceIcon)
		#(2020	'inspect instances' 			nil 	window inspectInstances 							inspectIcon)
		#(2030	'inspect subinstances' 		nil 	window inspectSubInstances 						inspectIcon)
		#(2040 	'-----')
		#(2050 	'remove from this browser' 	nil 	model 	removeMessageFromBrowser 				listRemoveIcon)
		#(2060 	'filter message list...' 			nil 	window filterMessageList 							findIcon)
		#(2070	'change category...' 			nil 	model 	changeCategory 							saveAsIcon)
		#(2080	'change sets with this method' nil 	window findMethodInChangeSets 					changesIcon)
		#(2090	'revert to previous version' 	nil 	model 	revertToPreviousVersion 					undoIcon)
		#(2100 	'-----')
		#(2110 	'sort by class hierarchy' 		nil 	model 	sortByClassHierarchy 						classIcon)
		#(2120 	'sort by class name' 			nil 	model 	sortByClassName 							classIcon)
		#(2130 	'sort by selector' 				nil 	model 	sortBySelector 								scriptIcon)
		#(2140 	'sort by date' 				nil 	model 	sortByDate 								dateIcon)
		#(2150 	'reverse sort' 				nil 	model 	sortReverse 								redoIcon)
	).
! !


!ProtocolBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 14:17:22'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^ super messageListMenuSpec, #(
		#(81		'select method category (C)' 			$C 	model 		showHomeCategory 	editFindReplaceIcon)
		#(2050 		'XXremove from this browser')
		#(2060 		'XXfilter message list...'))! !


!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:03:29'!
fieldListMenuSpec
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
		#(10	'inspect (i)' 				$i 	window 	inspectSelection 			inspectIcon)
		#(20	'explore (I)' 				$I 	window 	exploreSelection 			exploreIcon)
		#(30	'copy to clipboard (c)' 	$c 	window 	copySelectionToClipboard 	copyIcon)
		#(40	'basic inspect' 			nil 	window 	inspectBasic 				inspectIcon)
		#(50	'references finder' 		nil 	window 	openReferencesFinder 		exploreIcon)
		#(60	'weight explorer' 			nil 	window 	openWeightExplorer 		exploreIcon)
	).
	(model objectSpecificMenuSpecFrom: self) ifNotNil: [ :spec |
		answer := answer, spec ].
	answer := answer, #(
		#(210 	'-----')
		#(220	'browser full (b)' 			$b 	window 	browseMethodFull 			editFindReplaceIcon)
		#(230	'browser hierarch (h)' 	$h 	window 	browseHierarchy 			goTopIcon)
		#(240	'browser protocol (p)' 	$p 	window 	browseFullProtocol 		spreadsheetIcon)
	).
	^answer! !

!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:11:37'!
menuSpecForDictionary
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	^#(
		#(100 		'-----')
		#(110		'inspect key' 		nil 	window 	inspectSelectedKey 		editFindReplaceIcon)
		#(120		'senders of this key' 	nil 	window 	sendersOfSelectedKey 		mailForwardIcon)
		#(130		'add key' 			nil 	window 	addEntry 					listAddIcon)
		#(140		'rename key' 		nil 	window 	renameEntry 				saveAsIcon)
		#(150		'remove' 			nil 	model 		removeSelection 			listRemoveIcon)
	)! !


!ObjectExplorerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:09:33'!
listMenuSpec
	"Spec for the menu and shortcut keys for our tree nodes.
	See coment at Morph >> #getMenu
	"
	| answer |
	model getCurrentSelection
		ifNil: [^#()]
		ifNotNil: [ :currSel |
			answer := #(
				#(10		'inspect (i)' 				$i 	window 	inspectSelection 			inspectIcon)
				#(20		'explore (I)' 				$I 	window 	exploreSelection 			exploreIcon)
				#(30		'copy to clipboard (c)' 	$c 	window 	copySelectionToClipboard 	copyIcon)
				#(40		'basic inspect' 			nil 	window 	inspectBasic 				inspectIcon)
				#(50		'references finder' 		nil 	window 	openReferencesFinder 		exploreIcon)
				#(60		'weight explorer' 			nil 	window 	openWeightExplorer 		exploreIcon)
				#(70 		'-----')
				#(80		'browser full (b)' 			$b 	window 	browseMethodFull 			editFindReplaceIcon)
				#(90		'browser hierarch (h)' 	$h 	window 	browseHierarchy 			goTopIcon)
				#(100		'browser protocol (p)' 	$p 	window 	browseFullProtocol 		spreadsheetIcon)).
			(currSel item is: #Morph) ifTrue: [ answer := answer, #(
				#(110 		'-----')
				#(120		'show morph halo' 		nil 	window 	addHaloToSelection 		editFindReplaceIcon)) ].
			model class == ReferencesExplorer ifTrue: [ answer := answer, #(
				#(210 		'-----')
				#(220		'rescan' 					nil 	window 	rescan 						editFindReplaceIcon)) ].
			answer := answer, #(
				#(310 		'-----')
				#(320		'monitor changes' 		nil 	window 	monitorChanges 			editFindReplaceIcon)).
			model isMonitoring ifTrue: [ answer := answer, #(
				#(410 		'-----')
				#(420		'stop monitoring all' 		nil 	window 	stopMonitoring 			editFindReplaceIcon)) ].
			^answer ].! !


!ProcessBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 14:33:32'!
processListMenuSpec
	"Spec for the menu and shortcut keys for the list of processes.
	See coment at Morph >> #getMenu
	"
	| rules answer |
	answer := #().
	model selectedProcess ifNotNil: [ :selectedProcess |
		rules := model class rulesFor: model selectedProcess.
		answer := answer, #(
				#(10		'inspect (i)' 						$i 	window 	inspectProcess 			inspectIcon)
				#(20		'explore (I)' 						$I 	window 	exploreProcess 			exploreIcon)
				#(30		'references finder (P)' 			$P 	window 	openReferencesFinder 		exploreIcon)).
		rules first ifTrue: [
			answer := answer, 
				#((40		'terminate (t)' 					$t 	model 		terminateProcess 			cancelIcon)).
			selectedProcess isSuspended ifTrue: [ answer := answer,
				#((50		'resume (r)' 						$r 	model 		resumeProcess 			mediaPlaybackStartIcon))]
			ifFalse: [ answer := answer, 
				#((50 		'suspend (s)' 					$s 	model 		suspendProcess 			chatIcon))]].
		rules second ifTrue: [ answer := answer, #(
				#(60 		'change priority (p)' 				$p 	window 	changePriority 				systemMonitorIcon)
				#(70 		'debug (d)' 						$d 	window 	debugProcess 				debugIcon))].
		(selectedProcess suspendingList isKindOf: Semaphore) ifTrue: [ answer := answer,
				#((80 		'signal Semaphore (S)' 			$S 	model 		signalSemaphore 			haloHelpIcon))].
		answer := answer, #(
				#(90		'full stack (k)' 					$k 	model 		moreStack 					systemIcon)
				#(100 		'-----'))].
	answer := answer, #(
				#(110		'find context... (f)' 				$f 	window 	findContext 				findIcon)
				#(120		'find again (g)' 					$g 	model 		nextContext 				systemIcon)
				#(130 		'-----')).
	answer := answer, (isStepping ifTrue: [
				#((140		'turn off auto-update (a)' 		$a 	window 	toggleAutoUpdate 			cancelIcon)) ]
			ifFalse: [
				#((140		'turn on auto-update (a)' 		$a 	window 	toggleAutoUpdate 			updateIcon)) ]).
	answer := answer, #(
				#(150		'update list (u)' 					$u 	model 		updateProcessList 			updateIcon)
				#(160 		'-----')).
	answer := answer, (CPUWatcher isMonitoring ifTrue: [
				#((170		'stop CPUWatcher' 				nil 	model 		stopCPUWatcher 			inspectIcon)) ]
			ifFalse: [
				#((170		'start CPUWatcher' 				nil 	model 		startCPUWatcher 			inspectIcon)) ]).
	answer := answer, (StackSizeWatcher isWatching ifTrue: [
				#((180		'stop StackSizeWatcher' 			nil 	model 		stopStackSizeWatcher 		inspectIcon)) ]
			ifFalse: [
				#((180		'start StackSizeWatcher' 			nil 	model 		startStackSizeWatcher 	inspectIcon)) ]).
	^answer! !


!TestRunnerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:10:55'!
listMenuSpec
	"Spec for the menu and shortcut keys for the list of test classes.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
		#(0 		'Test Cases Set' 		false)
		#(10	'select all (a)' 		$a 	model 		selectAll 			selectAllIcon)
		#(20	'deselect all (A)' 		$A 	model 		deselectAll 			selectAllIcon)
		#(30	'invert selections' 	nil 	model 		invertSelections 	switchIcon)
		#(40	'filter (f)' 			$f 	model 		setFilter 			findIcon)
		#(50	'refresh (u)' 			$u 	model 		refreshTests 		refreshIcon)
		#(60 	'-----')).
	model isRunning ifTrue: [ answer := answer, #(
		#(70	'stop (l)' 			$l 	model 		interruptRun 		stopIcon))]
	ifFalse: [ answer := answer, #(
		#(70	'run (r)' 				$r 	model 		runTests 			playIcon)
		#(80	'run one (o)' 			$o 	model 		runOneTest 		playIcon)
		#(90	'run profiled (p)' 		$p 	model 		runProfiledTests 	playIcon))].
	model selectedSuite > 0 ifTrue: [ answer := answer, #(
		#(100 	'-----')
		#(110	'browse' 			nil 	window 	browseSelected 	editFindReplaceIcon))].
	answer := answer, #(
		#(100 	'-----')
		#(110	'log to Transcript' 	nil 	model 		showResult 		printerIcon)).
	^answer! !


!SimpleEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 15:01:54'!
defaultMenuSpec
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	^
	#(
		#(120	'Copy (c)' 	$c 	editor 	copySelection 	copyIcon 	'Copy selection to the Clipboard')
		#(130	'Cut (x)' 		$x 	editor 	cut 				cutIcon 	'Cut selection and store it in the Clipboard')
		#(140	'Paste (v)' 	$v 	editor 	paste 			pasteIcon 	'Paste Clipboard contents')
		"Keyboard shortcuts only"
		#(999 	nil 			$a 	editor 	selectAll 		nil 			'Select all')
	).
! !

