'From Cuis7.1 [latest update: #6847] on 22 November 2024 at 5:36:52 pm'!

!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 11/22/2024 17:28:24'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList textConstructor |
	textConstructor := [ :string :backgroundColor | 
		string asText addAttribute: (TextBackgroundColor color: backgroundColor) ].

	aList := #(
		(10	'browse'				browseMethodFull								'view this method in a browser'	browseMethodFullAndReplaceMe)
		(11	'senders' 			browseSendersOfMessages					'browse senders of...'				browseSenders)
		(16	'implementors'		browseImplementorsOfMessages				'browse implementors of...' 		browseImplementors)
		(12	'versions'			browseVersions								'browse versions')), 

		((Preferences at: #decorateBrowserButtons)
			ifTrue: [
				{{13	. 'inheritance'.	 #methodInheritance. 	'Browse Method Inheritance

', (textConstructor value: 'green' value: `Color green muchLighter`),': sends to super
', (textConstructor value: 'tan' value: `Color tan`), ': has override(s)
', (textConstructor value: 'mauve' value: `Color blue muchLighter`), ': both of the above
', (textConstructor value: 'pink' value: `Color red muchLighter`), ': is an override but doesn''t call super
', (textConstructor value: 'pinkish tan' value: `Color r: 0.94 g: 0.823 b: 0.673`), ': has override(s), also is an override but doesn''t call super
'. #methodInheritanceAndReplaceMe }}]
			ifFalse: [
				{#(13	'inheritance'	methodInheritance	 'browse method inheritance'. #methodInheritanceAndReplaceMe)}]),

		#(
		(12	'hierarchy'			browseHierarchy								'browse class hierarchy'			browseHierarchyAndReplaceMe)
		(10	'inst vars'			browseInstVarRefs								'instance variable references...')
		(11	'class vars'			browseClassVarRefs							'class variable references...')).

	^ aList! !


!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:20:56'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class List' 					false)
		#(10	'Browse Full (b)' 				$b 	window 	browseMethodFull 					editFindReplaceIcon)
		#(20	'Browse Hierarchy (h)' 		$h 	window 	browseHierarchy 					goTopIcon)
		#(30	'Browse Protocol (p)' 			$p 	window 	browseFullProtocol 				spreadsheetIcon)
		#(35	'Browse Package' 			nil 	window 	browsePackage 					packageIcon)
		#(40 	'-----')
		#(50	'File Out' 					nil 	model 		fileOutClass 						fileOutIcon)
		#(51	'File Out (alphabetically)' 		nil 	model 		fileOutClassAlphabetically 			fileOutIcon)
		#(60	'Run Tests (t)' 				$t 	model 		runClassTests 						weatherFewCloudsIcon)
		#(70 	'-----')
		#(80	'Refactorings' 				nil 	nil 			1000 								listAddIcon)
		#(90	'Copy Class...' 				nil 	model 		copyClass 							copyIcon)
		#(100	'Remove Class (x)' 			$x 	model 		removeClass 						deleteIcon)
		#(110 	'-----')
		#(120	'Show classes Hierarchically' 	nil model 		enableListClassesHierarchically 	goTopIcon)
		#(130	'Show classes Alphabetically' 	nil model 		enableListClassesAlphabetically 	sendReceiveIcon)
		#(140	'Show Definition' 				nil 	model 		editClass 							editFindReplaceIcon)
		#(150	'Show Comment' 			nil 	model 		editComment 						editFindReplaceIcon)
		#(160 	'-----')
		#(170	'Class References (N)' 		$N 	window 	browseClassRefs 					classIcon)
		#(180 	'-----')
		#(190	'more...' 					nil 	nil 			2000 								listAddIcon)

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
		#(2040	'Subclass Template' 			nil 	model 		makeNewSubclass 					classIcon 			'put a template into the code pane for defining of a subclass of this class')
		#(2050 	'-----')
		#(2060	'Sample instance' 			nil 	window 	makeSampleInstance 				instanceIcon 		'give me a sample instance of this class, if possible')
		#(2070	'Inspect instances' 			nil 	window 	inspectInstances 					inspectIcon 		'open an inspector on all the extant instances of this class')
		#(2080	'Inspect subinstances' 		nil 	window 	inspectSubInstances 				inspectIcon 		'open an inspector on all the extant instances of this class and of all of its subclasses')
		#(2090 	'-----')
		#(2100	'Create inst var accessors' 	nil 	model 		createInstVarAccessors 			sendReceiveIcon 	'compile instance-variable access methods for any instance variables that do not yet have them')
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:25:00'!
systemCatSingletonMenuSpec
	"Spec for the menu and shortcut keys for the one-item system category list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class category' 					false)
		#(10	'Find Class... (f)' 	$f 	window 	findClass 						findIcon)
		#(20 	'-----')
		#(30	'Browse All (B)' 	$B 	window 	browseAllClasses 				editFindReplaceIcon)
		#(40	'Browse (b)' 		$b 	window 	openSystemCategoryBrowser 	packageIcon)
		#(50 	'-----')
		#(60	'File Out (o)' 		$o 	model 		fileOutSystemCategory 		fileOutIcon)
		#(70 	'-----')
		#(80	'Update (u)' 		$u 	model 		updateSystemCategories 		updateIcon)
		#(90	'Rename... (R)' 	$R 	model 		renameSystemCategory 		saveAsIcon)
		#(100	'Remove (x)' 	$x 	model 		removeSystemCategory 		deleteIcon)
	)! !


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:23:14'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^ model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Class List' 			false)
				#(10	'Definition' 				nil 	model 		editClass 							editFindReplaceIcon)
				#(20	'Comment' 				nil 	model 		editComment 						editFindReplaceIcon)
				#(30      'Show Hierarchically' 		nil   model          enableListClassesHierarchically 	goTopIcon)		
				#(40 	'Show Alphabetically' 	nil 	model 		enableListClassesAlphabetically 	sendReceiveIcon)
				#(50 	'-----')
				#(60	'Browse Full (b)' 			$b 	window 	browseMethodFull 					editFindReplaceIcon)
				#(70	'Class References (N)' 	$N 	window 	browseClassRefs 					classIcon)
				#(80 	'-----')
				#(90	'File In' 					nil 	model 		fileInClass 							updateIcon)
				#(100	'File Out' 				nil 	model 		fileOutClass 						fileOutIcon)
				#(110    'Browse Changes'		nil   model          browseCodeFileChanges  			changesIcon)
				#(120 	'-----')
				#(130	'Rename...' 				nil 	model 		renameClass 						saveAsIcon)
				#(140	'Remove' 				nil 	model 		removeClass 						listRemoveIcon)
				#(150 	'-----')
				#(160	'Remove Existing' 		nil 	model 		removeClass 						deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Class List' 			false)
				#(10	'Definition' 				nil 	model 		editClass 							editFindReplaceIcon)
				#(20	'Comment' 				nil 	model 		editComment 						editFindReplaceIcon)
				#(30      'Show Hierarchically' 		nil   model          enableListClassesHierarchically 	goTopIcon)		
				#(40 	'Show Alphabetically' 	nil 	model 		enableListClassesAlphabetically 	sendReceiveIcon)
				#(50 	'-----')
				#(80	'File Out' 				nil 	model 		fileOutClass 						fileOutIcon)
				#(90 	'-----')
				#(100	'Rename...' 				nil 	model 		renameClass 						saveAsIcon)
				#(110	'Remove' 				nil 	model 		removeClass 						listRemoveIcon)
				#(120 	'-----')
				#(130	'Remove Existing' 		nil 	model 		removeClass 						deleteIcon)
			) ].! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:30:33'!
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
				#(40	'File Out' 				nil 	model 		fileOut) ).
	model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(50	'Remove Existing' 		nil 	model 		removeUnmodifiedClasses) )].
	^answer! !


!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:15:47'!
classCommentVersionsMenuSpec
	"Spec for the menu and shortcut keys for a class comment version in the versions list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Versions' 					true)
		#(10	'Compare to Current' 			nil 	model 	compareToCurrentVersion 	nil 	'compare selected version to the current version')
		#(20	'Revert to selected Version' 	nil 	model 	fileInSelections 			nil 	'resubmit the selected version, so that it becomes the current version')
		#(30 	'-----')
		#(40	'Update list' 					nil 	model 	reformulateList 			nil 	'reformulate the list of versions, in case it somehow got out of synch with reality')
		#(50 	'-----')
		#(60	'Help...' 						nil 	model 	offerVersionsHelp 			nil 	'provide an explanation of the use of this tool')
	)! !

!VersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:34:01'!
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
				#(80		'Senders (n)' 										$n 	window 	browseSenders 				nil
							'Browse all Senders of this selector')
				#(90		'Implementors (m)' 									$m 	window 	browseImplementors 			nil
							'Browse all Implementors of this selector')
				#(100 		'-----')
				#(110		'Help...' 												nil 	model 		offerVersionsHelp 				nil
							'provide an explanation of the use of this tool')
	).
	^answer! !


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:14:06'!
changeSetListMenuSpec
	"Spec for the menu and shortcut keys for the changesets list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
			#(0 			'Change Set' 						true)
			#(10 		'File out + Start an empty one (o)' 		$o 	model 	fileOutAndRemove 	fileOutIcon)
			#(20 		'File out + keep adding to same (k)' 	$k 	model 	fileOutAndKeep 	fileOutIcon)
			#(30 		'-----')
			#(40 		'Rename change set (R)' 				$R 	model 	rename 			saveAsIcon)
			#(50 		'Destroy change set (x)' 				$x 	model 	remove 			warningIcon)
	).
	model currentCanHavePreambleAndPostscript ifTrue: [
		model currentHasPreamble
			ifTrue: [ answer  := answer, #(
				#(60 	'-----')
				#(70 	'Edit preamble (p)' 		$p 	model 			addPreamble 			textEditorIcon)
				#(80 	'Remove preamble' 		nil 	model 			removePreamble 		listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(60 	'-----')
				#(70 	'Add preamble (p)' 		$p 	model 			addPreamble 			listAddIcon) )].
		model currentHasPostscript
			ifTrue: [ answer  := answer, #(
				#(90 	'Edit postscript...' 		nil 	model 			editPostscript 			textEditorIcon)
				#(100 	'Remove postscript' 		nil 	model 			removePostscript 		listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(90 	'Add postscript...' 		nil 	model 			editPostscript 			listAddIcon) )].
	].

	answer  := answer, #(
		#(110 	'-----')
		#(120 	'Conflicts with other change sets' 	nil 	window		browseMethodConflicts emblemImportantIcon 	'Browse all methods that occur both in this change set and in at least one other change set.')
		#(130 	'-----')
		#(140 	'Trim history' 					nil 	model		trimHistory 			clockIcon 					'Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes.
 NOTE: can cause confusion if later filed in over an earlier version of these changes.')
		#(150 	'View affected class categories' 	nil 	model 		viewAffectedClassCategories 	packageIcon 	'Show class categories affected by any contained change')
	).
	model csIsForBase ifFalse: [ "Maybe disable a few: Copy them the same as above, but without selector."
		answer := answer, #(
			#(40 	'Rename change set (R)' 		$R 	model 		nil 							saveAsIcon)
			#(50 	'Destroy change set (x)' 		$x 	model 		nil 							warningIcon)
			#(140 	'Trim history' 				nil 	model		nil 							clockIcon)) ].
	^answer! !

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:29:48'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Class List' 						false)
			#(10 	'Delete class from Change Set (d)' 	$d 	model 		forgetClass 			warningIcon)
			#(20 	'Remove class from System (x)' 	$x 	model 		removeClass 			deleteIcon)
			#(30 	'-----')
			#(40 	'Browse Full (b)' 					$b 	window 	browseMethodFull 		editFindReplaceIcon)
			#(50 	'Browse Hierarchy (h)' 			$h 	window 	browseHierarchy 		goTopIcon)
			#(60 	'Browse Protocol (p)' 				$p 	window 	browseFullProtocol 	spreadsheetIcon)
			#(70 	'-----')		
			#(80 	'Class References (N)' 			$N 	window 	browseClassRefs 		classIcon)
	).
! !

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:25:43'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the message list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Message List' 						true)
			#(10 	'Delete method from Change Set (d)' 	$d 	model 		forget 								warningIcon)
			#(20 	'Remove method from System (x)' 	$x 	model 		removeMessage 					deleteIcon)
			#(30 	'-----')
			#(40 	'Browse Full (b)' 						$b 	window 	browseMethodFull 					editFindReplaceIcon)
			#(50 	'Browse Hierarchy (h)' 				$h 	window 	browseHierarchy 					goTopIcon)
			#(60 	'Browse Method (O)' 					$O 	window 	openSingleMessageBrowser 		scriptIcon)
			#(80 	'-----')
			#(90 	'File Out (o)' 							$o 	model 		fileOutMessage 					fileOutIcon)
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


!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:30:59'!
contextStackMenuSpec
	"Spec for the menu and shortcut keys for call stack list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10	'Full Stack (f)' 			$f 	window 	fullStack 				nil 	'show the complete call stack')
		#(20	'Proceed (p)' 			$p 	window 	proceed 				nil 	'close the debugger and proceed')
		#(30	'Restart (r)' 				$r 	window 	restart 					nil 	'reset this context to its start')
		#(40	'Step Into Send (e)' 		$e 	window 	send 					nil 	'step into message sends')
		#(50	'Step Over Send (t)' 		$t 	window 	doStep 					nil 	'step over message sends')
		#(60	'Step into Block (T)' 		$T 	window 	stepIntoBlock 			nil 	'step into a code block')
		#(70	'Run to Cursor' 			nil 	window 	runToCursor 			nil 	'run to cursor')
		#(80	'Where (w)' 				$w 	window 	where 					nil 	'select current pc range')
		#(90	'Peel to First like this' 	nil 	window 	peelToFirst 			nil 	'peel stack back to the second occurance of selected message')
		#(100 	'-----')
		#(110	'Return entered Value' 	nil 	window 	returnValue 			nil 	'lets you enter an expression and return method answering result')
		#(120	'Toggle break on Entry' 	nil 	model 		toggleBreakOnEntry 	nil 	'install or uninstall a breakpoint on method entry')
		
		#(991	nil 						$m 	window 	browseImplementors)
		#(992	nil 						$n 	window 	browseSenders)
		#(993	nil 						$i 	window 	methodInheritance)
		#(994	nil 						$v 	window 	browseVersions)
		#(995	nil 						$N 	window 	browseClassRefs)
	)! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:26:30'!
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
		#(2080	'Change sets with this Method' nil 	window findMethodInChangeSets 					changesIcon)
		#(2090	'Revert to previous Version' 	nil 	model 	revertToPreviousVersion 					undoIcon)
		#(2100 	'-----')
		#(2110 	'Sort by class Hierarchy' 		nil 	model 	sortByClassHierarchy 						classIcon)
		#(2120 	'Sort by class Name' 			nil 	model 	sortByClassName 							classIcon)
		#(2130 	'Sort by Selector' 				nil 	model 	sortBySelector 								scriptIcon)
		#(2140 	'Sort by Date' 				nil 	model 	sortByDate 								dateIcon)
		#(2150 	'Reverse sort' 				nil 	model 	sortReverse 								redoIcon)
	).
! !


!MessageNamesWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:34:41'!
selectorListMenuSpec
	"Spec for the menu and shortcut keys for the selectors list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Selectors' 					false)
		#(10	'Senders (n)' 				$n 	window 	browseSenders 			mailForwardIcon)
		#(20	'Implementors of... (m)' 		$m 	window 	browseImplementors 		developmentIcon)
	).
! !


!ProtocolBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:24:16'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class List' 					false)
		#(10	'Browse Full (b)' 			$b 	window 	browseMethodFull 		editFindReplaceIcon)
		#(20	'Browse Hierarchy (h)' 	$h 	window 	browseHierarchy 		goTopIcon)
	)! !


!FileListWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:32:30'!
directoryTreeMenuSpec
	"Spec for the menu and shortcut keys for our tree nodes.
	See coment at Morph >> #getMenu
	"
	| showHiddenTogleAndLabel |
	showHiddenTogleAndLabel := model showHidden asMenuItemTextPrefix, 'show hidden folders and files'.
	^{
		{10. 'Rename Directory...'. 						nil. 	#model. 		#renameDirectory. 			#saveAsIcon.
			'Rename the selected directory'}.
		{20. 'Delete Directory...'. 							nil. 	#model. 		#deleteDirectory. 				#warningIcon.
			'Delete the selected directory'}.
		{30. showHiddenTogleAndLabel. 					nil. 	#model. 		#toggleShowHidden. 			nil.
			'Show or Hide files and directories that begin with a dot'}.
		{40. 'show Cuis top as tree root'. 					nil. 	#window. 		#useCuisTopAsTreeRoot. 		nil.
			'Use the default Cuis top directory'}.
		{50. 'show OS roots as tree root'. 					nil. 	#window. 		#useOSrootsAsTreeRoot. 		nil.
			'Use the host OS root directories. Drives on Windows; "/" on Unix'}
	}! !


!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:33:29'!
menuSpecForSet
	"Spec for the menu and shortcut keys for the list of variables in our object.
	See coment at Morph >> #getMenu
	"
	^#(
		#(100 	'-----')
		#(110	'Remove' 		nil 	model 		removeSelection 		listRemoveIcon)
	)! !


!ProcessBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 17:35:23'!
stackMenuSpec
	"Spec for the menu and shortcut keys for the list of stack frames.
	See coment at Morph >> #getMenu
	"
	^ #(
		#(10	'Inspect Context (c)' 		$c 	window 	inspectContext 	inspectIcon)
		#(20	'Explore Context (C)' 		$C 	window 	exploreContext 	exploreIcon)
		#(30	'Inspect Receiver (i)' 		$i 	window 	inspectReceiver 	inspectIcon)
		#(40	'Explore Receiver (I)' 		$I 	window 	exploreReceiver 	exploreIcon)
		#(50	'Browse Context (b)' 		$b 	window 	browseContext 	editFindReplaceIcon)
	).! !

