'From Cuis7.1 [latest update: #6845] on 22 November 2024 at 5:03:30 pm'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:11:09'!
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
		#(100	'Rename... (R)' 					$R 	model 		renameCategory 						saveAsIcon)
		#(110	'Remove (x)' 					$x 	model 		removeMessageCategory 				deleteIcon)
		#(120 	'-----')
		#(130	'Run Tests (t)' 					$t 	model 		runMessageCategoryTests 			weatherFewCloudsIcon)
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:38:25'!
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
		#(180	'more...' 								nil 	nil 			2000 										listAddIcon)

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
		#(1070	'Move to instance/class Method' 			nil 	model 		moveToInstanceOrClassMethod 			changesIcon)
		#(1080	'Push Up' 								nil 	model 		pushUpSelector 							goTopIcon)
		#(1090	'Add in Superclass as subclassResponsability' nil model 		addAsSubclassResponsabilitySelector 		goTopIcon)
		#(1100	'Push down to subclasses' 				nil 	model 		pushDownSelectorToSubclasses 			goBottomIcon)
		#(1110	'Push down to one subclass' 				nil 	model 		pushDownSelectorToOneSubclass 			goBottomIcon)

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

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:00:10'!
systemCatListMenuSpec
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
	)! !


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:32:14'!
codeFileListMenuSpec
	"Spec for the menu and shortcut keys for the single entry filename list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := 
			#(
				#(0 		'Code File' 			false)
				#(10	'Find Class... (f)' 			$f 	window 	findClass)
				#(20 	'-----') ).
	model baseCodeSource isLiveSmalltalkImage ifTrue: [
			answer := answer, #(
				#(30	'File In' 					nil 	model 		fileIn) )].
	answer := answer, 
			#(
				#(40	'File Out' 					nil 	model 		fileOut) ).
	model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(50	'Remove Existing' 		nil 	model 		removeUnmodifiedClasses) )].
	^answer! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:14:32'!
messageCatListMenuSpec
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Message Categories' false)
				#(10	'File In' 				nil 	model 		fileInMessageCategories 		updateIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Message Categories' false)
			)].
	answer := answer, 
			#(
				#(20	'File Out' 			nil 	model 		fileOutMessageCategories 		fileOutIcon)
			).
	model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(100 	'-----')
				#(100	'Remove Existing' 	nil 	model 		removeUnmodifiedMethods 	deleteIcon)
		) ].
	^answer! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:39:44'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^ model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Messages' 				false)
				#(10	'File In' 					nil 	model 		fileInMessage 			updateIcon)
				#(20	'File Out' 				nil 	model 		fileOutMessage 		fileOutIcon)
				#(30 	'-----')
				#(40	'Senders (n)' 			$n 	window 	browseSenders 		mailForwardIcon)
				#(50	'Implementors (m)' 		$m 	window 	browseImplementors 	developmentIcon)
				#(60	'Method Inheritance (h)' 	$h 	window 	methodInheritance 	goDownIcon)
				#(70	'Browse Full (b)' 			$b 	window 	browseMethodFull 		editFindReplaceIcon)
				#(80	'Versions (v)' 			nil 	window 	browseVersions 		clockIcon)
				#(90 	'-----')
				#(100	'Remove Method (x)' 		$x 	model 		removeMessage 		deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Messages' 				false)
				#(10	'File Out' 				nil 	model 		fileOutMessage 		fileOutIcon)
				#(20 	'-----')
				#(30	'Remove Method (x)' 		$x 	model 		removeMessage 		deleteIcon)
			)].! !


!ChangeListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:42:19'!
listMenuSpec
	"Spec for the menu and shortcut keys for the changes list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Change List' 									true)
		#(10	'File In selections' 									nil 	model 	fileInSelections 						nil
					'import the selected items into the image')
		#(20	'File Out selections...	' 							nil 	model 	fileOutSelections 						nil
		 			'create a new file containing the selected items')
		#(30	'File Out current version of selections...' 			nil 	model 	fileOutCurrentVersionsOfSelections 	nil
 					'create a new file containing the current (in-image) counterparts of the selected methods')
		#(40 	'-----')
		#(50	'Compare to Current' 								nil 	model 	compareToCurrentVersion 				nil
 					'open a separate window which shows the text differences between the on-file version and the in-image version.')
		#(60 	'-----')
		#(70	'Select new methods' 								nil 	model 	selectNewMethods 					nil
 					'select methods in the file that do not currently exist in the image')
		#(80	'Select changes for Absent Classes' 				nil 	model 	selectAllForAbsentClasses 				nil
					'select methods in the file for classes that are not defined in the image')
		#(90	'Select all changes for this Class' 					nil 	model 	selectAllForThisClass 					nil
					'select all methods in the file that belong to the currently-selected class')
		#(100	'Select Unchanged methods' 						nil 	model 	selectUnchangedMethods 				nil
					'select methods in the file whose in-image versions are the same as their in-file counterparts')
		#(110	'Select methods Equivalent to current' 				nil 	model 	selectEquivalentMethods 				nil
					'select methods in the file whose in-image versions have the same behavior as their in-file counterparts')
		#(120	'Select methods Older than current' 				nil 	model 	selectMethodsOlderThanCurrent 		nil
					'select methods in the file that are older than the one currently in the image')
		#(130	'Select removals of Sent methods' 					nil 	model 	selectRemovalsOfSent 					nil
					'select all method removals of methods that have some sender in the image')
		#(140 	'-----')
		#(150	'Select All (a)' 									$a 	model 	selectAll 								nil
					'select all the items in the list')
		#(160	'Deselect All (A)' 									$A 	model 	deselectAll 								nil
					'deselect all the items in the list')
		#(170	'Invert Selections' 								nil 	model 	invertSelections 						nil
					'select every item that is not currently selected, and deselect every item that *is* currently selected')
		#(180 	'-----')
		#(190	'Browse class and method' 						nil 	window browseMethodFull 					nil
					'open a full browser showing the selected method')
		#(200	'Browse all versions of single selection' 			nil 	window browseVersions 						nil
					'open a version browser showing the versions of the currently selected method')
		#(210	'Browse current versions of selections' 				nil 	window browseCurrentVersionsOfSelections 	nil
					'open a message-list browser showing the current (in-image) counterparts of the selected methods')
		#(220	'Destroy current methods of selections' 			nil 	model 	destroyCurrentCodeOfSelections 		nil
					'remove (*destroy*) the in-image counterparts of all selected methods')
		#(230 	'-----')
		#(240	'Remove doIts' 									nil 	model 	removeDoIts 							nil
					'remove all items that are doIts rather than definitions')
		#(250	'Remove older versions' 							nil 	model 	removeOlderMethodVersions 			nil
					'remove all but the most recent versions of methods in the list')
		#(260	'Remove up-to-date versions' 					nil 	model 	removeUpToDate 						nil
					'remove all items whose code is the same as the counterpart in-image code')
		#(270	'Remove empty class comments' 					nil 	model 	removeEmptyClassComments 			nil
					'remove all empty class comments')
		#(280	'Remove selected items' 							nil 	model 	removeSelections 						nil
					'remove the selected items from the change-list')
		#(290	'Remove unselected items' 						nil 	model 	removeNonSelections 					nil
					'remove all the items not currently selected from the change-list')
	)! !


!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:43:12'!
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
				#(10		'Compare to current' 									nil 	model 		compareToCurrentVersion 		nil
							'Compare selected version to the current version')
				#(20		'Compare to version...' 								nil 	model 		compareToOtherVersion 		nil
							'Compare selected version to another selected version'))].
		answer := answer, #(
				#(30		'Revert to selected version (z)' 						$z 	model 		fileInSelections 				nil
							'Resubmit the selected version, so that it becomes the current version'))].
	answer := answer, #(
				#(40		'Edit current method (O)' 								$O 	window 	openSingleMessageBrowser 	nil
							'Open a single-message browser on the current version of this method')
				#(50 		'-----')
				#(60		'Update List' 											nil 	model 		reformulateList 				nil
							'Reformulate the list of versions, in case it somehow got out of synch with reality')
				#(70 		'-----')
				#(80		'Senders (n)' 											$n 	window 	browseSenders 				nil
							'Browse all Senders of this selector')
				#(90		'Implementors (m)' 									$m 	window 	browseImplementors 			nil
							'Browse all Implementors of this selector')
				#(100 		'-----')
				#(110		'Help...' 												nil 	model 		offerVersionsHelp 				nil
							'provide an explanation of the use of this tool')
	).
	^answer! !


!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:44:15'!
contextStackMenuSpec
	"Spec for the menu and shortcut keys for call stack list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10	'Full Stack (f)' 			$f 	window 	fullStack 				nil 	'show the complete call stack')
		#(20	'Proceed (p)' 			$p 	window 	proceed 				nil 	'close the debugger and proceed')
		#(30	'Restart (r)' 				$r 	window 	restart 					nil 	'reset this context to its start')
		#(40	'Step Into send (e)' 		$e 	window 	send 					nil 	'step into message sends')
		#(50	'Step Over send (t)' 		$t 	window 	doStep 					nil 	'step over message sends')
		#(60	'Step into Block (T)' 		$T 	window 	stepIntoBlock 			nil 	'step into a code block')
		#(70	'Run to Cursor' 			nil 	window 	runToCursor 			nil 	'run to cursor')
		#(80	'Where (w)' 				$w 	window 	where 					nil 	'select current pc range')
		#(90	'Peel to First like this' 		nil 	window 	peelToFirst 			nil 	'peel stack back to the second occurance of selected message')
		#(100 	'-----')
		#(110	'Return entered Value' 	nil 	window 	returnValue 			nil 	'lets you enter an expression and return method answering result')
		#(120	'Toggle break on Entry' 	nil 	model 		toggleBreakOnEntry 	nil 	'install or uninstall a breakpoint on method entry')
		
		#(991	nil 						$m 	window 	browseImplementors)
		#(992	nil 						$n 	window 	browseSenders)
		#(993	nil 						$i 	window 	methodInheritance)
		#(994	nil 						$v 	window 	browseVersions)
		#(995	nil 						$N 	window 	browseClassRefs)
	)! !

!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:50:48'!
contextVariablesListMenuSpec
	"Spec for the menu and shortcut keys for the list of variables in this context.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10	'Inspect (i)' 			$i 	window 	inspectContextSelection 		inspectIcon)
		#(20	'Explore (I)' 			$I 	window 	exploreContextSelection 		exploreIcon)
		#(30 	'-----')
		#(40	'Browse Full (b)' 		$b 	window 	contextClassBrowse 			editFindReplaceIcon)
		#(50	'Browse Hierarch (h)' 	$h 	window 	contextClassHierarchy 			goTopIcon)
		#(60	'Browse Protocol (p)' 	$p 	window 	contextClassProtocol 			spreadsheetIcon)
	)! !

!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:51:03'!
receiverVariablesListMenuSpec
	"Spec for the menu and shortcut keys for the list of instance variables in the receiver.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10	'Inspect (i)' 			$i 	window 	inspectReceiverSelection 		inspectIcon)
		#(20	'Explore (I)' 			$I 	window 	exploreReceiverSelection 		exploreIcon)
		#(30 	'-----')
		#(40	'Browse Full (b)' 		$b 	window 	receiverClassBrowse 			editFindReplaceIcon)
		#(50	'Browse Hierarch (h)' 	$h 	window 	receiverClassHierarchy 		goTopIcon)
		#(60	'Browse Protocol (p)' 	$p 	window 	receiverClassProtocol 			spreadsheetIcon)
	)! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:46:50'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Set' 				false)
		#(10	'Browse Full (b)' 				$b 	window browseMethodFull 						editFindReplaceIcon)
		#(20	'Browse Hierarchy (h)' 		$h 	window browseHierarchy 							goTopIcon)
		#(30	'Browse Method (O)' 			$O 	window openSingleMessageBrowser 				scriptIcon)
		#(40	'Browse Protocol (p)' 			$p 	window browseFullProtocol 						spreadsheetIcon)
		#(45	'Browse Package' 			nil 	window browsePackage 							packageIcon)
		#(50 	'-----')
		#(60	'File Out' 						nil 	model 	fileOutMessage 							fileOutIcon)
		#(70	'Run Test (t)' 					$t 	model 	runMethodTest 							weatherFewCloudsIcon)
		#(80	'Debug Test (r)' 				$r 	model 	debugMethodTest 							weatherFewCloudsIcon)
		#(90	'Inspect CompiledMethod' 	nil 	model 	inspectCompiledMethod 					exploreIcon)
		#(100 	'-----')
		#(110	'Senders of... (n)' 			nil 	window browseSendersOfMessages 				mailForwardIcon)
		#(120	'Local Senders of...' 			nil 	window browseLocalSendersOfMessages 			mailForwardIcon)
		#(130	'Implementors of... (m)' 		nil 	window browseImplementorsOfMessages 			developmentIcon)
		#(140	'Local Implementors of...' 		nil 	window browseLocalImplementorsOfMessages 	developmentIcon)
		#(150	'Inheritance (i)' 				$i 	window methodInheritance 						goDownIcon)
		#(160	'Versions (v)' 				$v 	window browseVersions 							clockIcon)
		#(170 	'-----')
		#(180	'Remove Method (x)' 			$x 	model 	removeMessage 							deleteIcon)
		#(190	'more' 						nil 	nil 		2000 										listAddIcon)

		"Shortcut only. No menu entry."
		#(991	nil 							$m 	window browseImplementors)
		#(992	nil 							$n 	window browseSenders)
		#(993	nil 							$c 	model 	copySelector)
		#(994	nil 							$N 	window browseClassRefs)

		" 'more' SubMenu"
		#(2010	'Sample instance' 			nil 	window makeSampleInstance 						instanceIcon)
		#(2020	'Inspect instances' 			nil 	window inspectInstances 							inspectIcon)
		#(2030	'Inspect subinstances' 		nil 	window inspectSubInstances 						inspectIcon)
		#(2040 	'-----')
		#(2050 	'Remove from this Browser' 	nil 	model 	removeMessageFromBrowser 				listRemoveIcon)
		#(2060 	'Filter message list...' 			nil 	window filterMessageList 							findIcon)
		#(2070	'Change category...' 			nil 	model 	changeCategory 							saveAsIcon)
		#(2080	'Change sets with this method' nil 	window findMethodInChangeSets 					changesIcon)
		#(2090	'Revert to previous version' 	nil 	model 	revertToPreviousVersion 					undoIcon)
		#(2100 	'-----')
		#(2110 	'Sort by class Hierarchy' 		nil 	model 	sortByClassHierarchy 						classIcon)
		#(2120 	'Sort by class Name' 			nil 	model 	sortByClassName 							classIcon)
		#(2130 	'Sort by Selector' 				nil 	model 	sortBySelector 								scriptIcon)
		#(2140 	'Sort by Date' 				nil 	model 	sortByDate 								dateIcon)
		#(2150 	'Reverse sort' 				nil 	model 	sortReverse 								redoIcon)
	).
! !


!ProtocolBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:47:58'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^ super messageListMenuSpec, #(
		#(81		'Select Method Category (C)' 		$C 	model 		showHomeCategory 	editFindReplaceIcon)
		#(2050 		'XXX Remove from this Browser')
		#(2060 		'XXX Filter message list...'))! !


!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:51:32'!
fieldListMenuSpec
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
		#(10	'Inspect (i)' 				$i 	window 	inspectSelection 			inspectIcon)
		#(20	'Explore (I)' 				$I 	window 	exploreSelection 			exploreIcon)
		#(30	'Copy to clipboard (c)' 	$c 	window 	copySelectionToClipboard 	copyIcon)
		#(40	'Basic Inspect' 			nil 	window 	inspectBasic 				inspectIcon)
		#(50	'References finder' 		nil 	window 	openReferencesFinder 		exploreIcon)
		#(60	'Weight explorer' 		nil 	window 	openWeightExplorer 		exploreIcon)
	).
	(model objectSpecificMenuSpecFrom: self) ifNotNil: [ :spec |
		answer := answer, spec ].
	answer := answer, #(
		#(210 	'-----')
		#(220	'Browse Full (b)' 			$b 	window 	browseMethodFull 			editFindReplaceIcon)
		#(230	'Browse Hierarch (h)' 		$h 	window 	browseHierarchy 			goTopIcon)
		#(240	'Browse Protocol (p)' 		$p 	window 	browseFullProtocol 		spreadsheetIcon)
	).
	^answer! !

!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:49:12'!
menuSpecForDictionary
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	^#(
		#(100 		'-----')
		#(110		'Inspect Key' 		nil 	window 	inspectSelectedKey 		editFindReplaceIcon)
		#(120		'Senders of this Key' 	nil 	window 	sendersOfSelectedKey 		mailForwardIcon)
		#(130		'Add Key' 			nil 	window 	addEntry 					listAddIcon)
		#(140		'Rename Key' 		nil 	window 	renameEntry 				saveAsIcon)
		#(150		'Remove' 			nil 	model 		removeSelection 			listRemoveIcon)
	)! !


!ObjectExplorerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:52:20'!
listMenuSpec
	"Spec for the menu and shortcut keys for our tree nodes.
	See coment at Morph >> #getMenu
	"
	| answer |
	model getCurrentSelection
		ifNil: [^#()]
		ifNotNil: [ :currSel |
			answer := #(
				#(10		'Inspect (i)' 				$i 	window 	inspectSelection 			inspectIcon)
				#(20		'Explore (I)' 				$I 	window 	exploreSelection 			exploreIcon)
				#(30		'Copy to clipboard (c)' 	$c 	window 	copySelectionToClipboard 	copyIcon)
				#(40		'Basic inspect' 			nil 	window 	inspectBasic 				inspectIcon)
				#(50		'References finder' 		nil 	window 	openReferencesFinder 		exploreIcon)
				#(60		'Weight explorer' 		nil 	window 	openWeightExplorer 		exploreIcon)
				#(70 		'-----')
				#(80		'Browse Full (b)' 			$b 	window 	browseMethodFull 			editFindReplaceIcon)
				#(90		'Browse Hierarch (h)' 		$h 	window 	browseHierarchy 			goTopIcon)
				#(100		'Browse Protocol (p)' 		$p 	window 	browseFullProtocol 		spreadsheetIcon)).
			(currSel item is: #Morph) ifTrue: [ answer := answer, #(
				#(110 		'-----')
				#(120		'Show morph Halo' 		nil 	window 	addHaloToSelection 		editFindReplaceIcon)) ].
			model class == ReferencesExplorer ifTrue: [ answer := answer, #(
				#(210 		'-----')
				#(220		'Rescan' 				nil 	window 	rescan 						editFindReplaceIcon)) ].
			answer := answer, #(
				#(310 		'-----')
				#(320		'Monitor changes' 		nil 	window 	monitorChanges 			editFindReplaceIcon)).
			model isMonitoring ifTrue: [ answer := answer, #(
				#(410 		'-----')
				#(420		'stop monitoring all' 		nil 	window 	stopMonitoring 			editFindReplaceIcon)) ].
			^answer ].! !


!ProcessBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:53:54'!
processListMenuSpec
	"Spec for the menu and shortcut keys for the list of processes.
	See coment at Morph >> #getMenu
	"
	| rules answer |
	answer := #().
	model selectedProcess ifNotNil: [ :selectedProcess |
		rules := model class rulesFor: model selectedProcess.
		answer := answer, #(
				#(10		'Inspect (i)' 						$i 	window 	inspectProcess 			inspectIcon)
				#(20		'Explore (I)' 						$I 	window 	exploreProcess 			exploreIcon)
				#(30		'References Finder (P)' 			$P 	window 	openReferencesFinder 		exploreIcon)).
		rules first ifTrue: [
			answer := answer, 
				#((40		'Terminate (t)' 					$t 	model 		terminateProcess 			cancelIcon)).
			selectedProcess isSuspended ifTrue: [ answer := answer,
				#((50		'Resume (r)' 						$r 	model 		resumeProcess 			mediaPlaybackStartIcon))]
			ifFalse: [ answer := answer, 
				#((50 		'Suspend (s)' 					$s 	model 		suspendProcess 			chatIcon))]].
		rules second ifTrue: [ answer := answer, #(
				#(60 		'Change priority (p)' 				$p 	window 	changePriority 				systemMonitorIcon)
				#(70 		'Debug (d)' 						$d 	window 	debugProcess 				debugIcon))].
		(selectedProcess suspendingList isKindOf: Semaphore) ifTrue: [ answer := answer,
				#((80 		'Signal Semaphore (S)' 			$S 	model 		signalSemaphore 			haloHelpIcon))].
		answer := answer, #(
				#(90		'Full stack (k)' 					$k 	model 		moreStack 					systemIcon)
				#(100 		'-----'))].
	answer := answer, #(
				#(110		'Find context... (f)' 				$f 	window 	findContext 				findIcon)
				#(120		'Find again (g)' 					$g 	model 		nextContext 				systemIcon)
				#(130 		'-----')).
	answer := answer, (isStepping ifTrue: [
				#((140		'Turn off auto-update (a)' 		$a 	window 	toggleAutoUpdate 			cancelIcon)) ]
			ifFalse: [
				#((140		'Turn on auto-update (a)' 		$a 	window 	toggleAutoUpdate 			updateIcon)) ]).
	answer := answer, #(
				#(150		'Update list (u)' 					$u 	model 		updateProcessList 			updateIcon)
				#(160 		'-----')).
	answer := answer, (CPUWatcher isMonitoring ifTrue: [
				#((170		'Stop CPUWatcher' 				nil 	model 		stopCPUWatcher 			inspectIcon)) ]
			ifFalse: [
				#((170		'Start CPUWatcher' 				nil 	model 		startCPUWatcher 			inspectIcon)) ]).
	answer := answer, (StackSizeWatcher isWatching ifTrue: [
				#((180		'Stop StackSizeWatcher' 			nil 	model 		stopStackSizeWatcher 		inspectIcon)) ]
			ifFalse: [
				#((180		'Start StackSizeWatcher' 			nil 	model 		startStackSizeWatcher 	inspectIcon)) ]).
	^answer! !


!TestRunnerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 16:54:33'!
listMenuSpec
	"Spec for the menu and shortcut keys for the list of test classes.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
		#(0 		'Test Cases Set' 		false)
		#(10	'Select all (a)' 		$a 	model 		selectAll 			selectAllIcon)
		#(20	'Deselect all (A)' 		$A 	model 		deselectAll 			selectAllIcon)
		#(30	'Invert selections' 	nil 	model 		invertSelections 	switchIcon)
		#(40	'Filter (f)' 			$f 	model 		setFilter 			findIcon)
		#(50	'Refresh (u)' 			$u 	model 		refreshTests 		refreshIcon)
		#(60 	'-----')).
	model isRunning ifTrue: [ answer := answer, #(
		#(70	'Stop (l)' 			$l 	model 		interruptRun 		stopIcon))]
	ifFalse: [ answer := answer, #(
		#(70	'Run (r)' 				$r 	model 		runTests 			playIcon)
		#(80	'Run One (o)' 		$o 	model 		runOneTest 		playIcon)
		#(90	'Run Profiled (p)' 		$p 	model 		runProfiledTests 	playIcon))].
	model selectedSuite > 0 ifTrue: [ answer := answer, #(
		#(100 	'-----')
		#(110	'Browse' 			nil 	window 	browseSelected 	editFindReplaceIcon))].
	answer := answer, #(
		#(100 	'-----')
		#(110	'Log to Transcript' 	nil 	model 		showResult 		printerIcon)).
	^answer! !

