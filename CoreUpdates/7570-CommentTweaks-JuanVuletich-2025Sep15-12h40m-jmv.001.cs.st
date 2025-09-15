'From Cuis7.5 [latest update: #7570] on 15 September 2025 at 12:40:31 pm'!

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'HAW 4/28/2025 20:41:47'!
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
		#(110	'File Out Alphabetically (o)' 	$o 	model 		fileOutClassAlphabetically 			fileOutIcon)
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
		#(230	'More...' 						nil 	nil 			2000 								listAddIcon)

		"Keyboard shortcut only. No menu entry."
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

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'HAW 4/28/2025 20:42:07'!
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
		#(50	'File Out (o)' 								$o 	model 		fileOutMessage 							fileOutIcon)
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
		#(180	'More...' 									nil 	nil 			2000 										listAddIcon)

		"Keyboard shortcut only. No menu entry."
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

			"Keyboard shortcut only. No menu entry."
			#(991	nil 									$m 	window 	browseImplementors)
			#(992	nil 									$n 	window 	browseSenders)
	).
! !


!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 12/4/2024 16:42:26'!
contextStackMenuSpec
	"Spec for the menu and shortcut keys for call stack list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10	'Browse (b)' 				$b 	window 	browseMethodFull 		nil 	'open a Browser on method')
		#(20	'Full Stack (f)' 			$f 	window 	fullStack 				nil 	'show the complete call stack')
		#(30	'Proceed (p)' 			$p 	window 	proceed 				nil 	'close the debugger and proceed')
		#(40	'Restart (r)' 				$r 	window 	restart 					nil 	'reset this context to its start')
		#(50	'Step Into Send (e)' 		$e 	window 	send 					nil 	'step into message sends')
		#(60	'Step Over Send (t)' 		$t 	window 	doStep 					nil 	'step over message sends')
		#(70	'Step into Block (T)' 		$T 	window 	stepIntoBlock 			nil 	'step into a code block')
		#(80	'Run to Cursor' 			nil 	window 	runToCursor 			nil 	'run to cursor')
		#(90	'Where (w)' 				$w 	window 	where 					nil 	'select current pc range')
		#(100	'Peel to First like this' 	nil 	window 	peelToFirst 			nil 	'peel stack back to the second occurance of selected message')
		#(110 	'-----')
		#(120	'Return entered Value' 	nil 	window 	returnValue 			nil 	'lets you enter an expression and return method answering result')
		#(130	'Toggle break on Entry' 	nil 	model 		toggleBreakOnEntry 	nil 	'install or uninstall a breakpoint on method entry')
		
		"Keyboard shortcut only. No menu entry."
		#(991	nil 						$m 	window 	browseImplementors)
		#(992	nil 						$n 	window 	browseSenders)
		#(993	nil 						$i 	window 	methodInheritance)
		#(994	nil 						$v 	window 	browseVersions)
		#(995	nil 						$N 	window 	browseClassRefs)
	)! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'HAW 3/22/2025 19:00:22'!
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
		#(60	'File Out' 					nil 	model 	fileOutMessage 							fileOutIcon)
		#(61	'File Out All' 					nil 	model 	fileOutAllMethods 							fileOutIcon)
		#(70	'Run Test (t)' 				$t 	model 	runMethodTest 							weatherFewCloudsIcon)
		#(80	'Debug Test (r)' 				$r 	model 	debugMethodTest 							weatherFewCloudsIcon)
		#(90	'Inspect CompiledMethod' 	nil 	model 	inspectCompiledMethod 					exploreIcon)
		#(100 	'-----')
		#(103	'Refactorings' 				nil 	nil 		1000 										listAddIcon)
		#(110	'Senders of... (n)' 			nil 	window browseSendersOfMessages 				mailForwardIcon)
		#(120	'Local Senders of...' 			nil 	window browseLocalSendersOfMessages 			mailForwardIcon)
		#(130	'Implementors of... (m)' 		nil 	window browseImplementorsOfMessages 			developmentIcon)
		#(140	'Local Implementors of...' 		nil 	window browseLocalImplementorsOfMessages 	developmentIcon)
		#(150	'Inheritance (i)' 				$i 	window methodInheritance 						goDownIcon)
		#(160	'Versions (v)' 				$v 	window browseVersions 							clockIcon)
		#(170 	'-----')
		#(180	'Remove Method (x)' 			$x 	model 	removeMessage 							deleteIcon)
		#(190	'more' 						nil 	nil 		2000 										listAddIcon)

		"Keyboard shortcut only. No menu entry."
		#(991	nil 							$m 	window browseImplementors)
		#(992	nil 							$n 	window browseSenders)
		#(993	nil 							$c 	model 	copySelector)
		#(994	nil 							$N 	window browseClassRefs)

		" 'Refactoring' SubMenu"
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
		"Keyboard shortcut only. No menu entry."
		#(999 	nil 			$a 	editor 	selectAll 		nil 			'Select all')
	).
! !


!TextEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 11:44:31'!
defaultMenuSpec
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	| acceptEntry |
	acceptEntry := self hasUnacceptedEdits
		ifTrue: [ #((110	'Save (s)' 		$s 	editor 	acceptContents 					acceptIcon 						'Save')) ]
		ifFalse: [ #((110 	'Save' 			nil 	editor 	nil 									acceptIcon 						'Save')) ].
	^#(
		#(0 		'Text Editor' 				true)
		#(10	'Help... (h)' 				$h 	editor 	openHelp 							helpIcon 						'Open Help')
		#(20 	'-----')
		#(30	'Find...(f)' 				$f 	editor 	find 								findIcon 						'Find')
		#(40	'Find Again (g)' 			$g 	editor 	findAgain 							systemIcon 					'Find again')
		#(50	'Use Selection for Find (j)'	$j 	editor 	setSearchString 					saveAsIcon 					'Set selection as search string for find again')
		#(60 	'-----')
		#(70	'Undo - multiple (z)' 		$z 	editor 	undo 								undoIcon 						'Undo (multiple levels)')
		#(80	'Redo - multiple (Z)' 		$Z 	editor 	redo 								redoIcon 						'Redo (multiple levels)')
		#(90	'Undo / Redo history' 		nil 	editor 	offerUndoHistory 					changesIcon)
		#(100 	'-----')
	),
	acceptEntry,
	#(
		#(120	'Copy (c)' 				$c 	editor 	copySelection 						copyIcon 						'Copy selection to the Clipboard')
		#(130	'Cut (x)' 					$x 	editor 	cut 									cutIcon 						'Cut selection and store it in the Clipboard')
		#(140 	'Compare to Clipboard (C)' $C 	editor 	compareToClipboard 				changesIcon 					'Compare selection to clipboard')
		#(150	'Paste (v)' 				$v 	editor 	paste 								pasteIcon 						'Paste Clipboard contents')
		#(160	'Paste without Format' 	nil 	editor 	pasteString 						pasteIcon)
		#(170	'Paste...' 				nil 	editor 	pasteRecent 						worldIcon)
		#(180 	'-----')
		#(190	'Toggle WordWrap' 		nil 	editor 	wrapOnOff 							genericTextIcon)
		#(200	'Clear Font' 				nil 	editor 	clearFont 							newIcon)
		#(210	'Set Font... (k)' 			$k 	editor 	offerFontMenu 						preferencesDesktopFontIcon 	'Set font')
		#(220	'Set Font Size' 			nil 	editor 	offerFontSizeMenu 				preferencesDesktopFontIcon)
		#(230	'Set Style' 				nil 	editor 	offerFontStyleMenu 				preferencesDesktopFontIcon)
		#(240	'Set Text Color (8)' 		$8 	editor 	offerColorMenu 					graphicsIcon)
		#(250	'Set Alignment... (u)' 		$u 	editor 	chooseAlignment 					formatJustifyLeftIcon 			'Toggle alignment')

		"Keyboard shortcut only. No menu entry."
		#(500	nil 						$0 	editor 	changeEmphasisNormal 			nil 			'Normal')
		#(501	nil 						$1 	editor 	changeEmphasisBold 				nil 			'Bold')
		#(502	nil 						$2 	editor 	changeEmphasisItalic 				nil 			'Italic')
		#(503	nil 						$3 	editor 	changeEmphasisUnderlined 		nil 			'Underlined')
		#(504	nil 						$4 	editor 	changeEmphasisStruckThrough 	nil 			'Struck Through')
		#(505	nil 						$5 	editor 	changeEmphasisSuperscript 		nil 			'Superscript')
		#(506	nil 						$6 	editor 	changeEmphasisSubscript 			nil 			'Subscript')
		#(601 	nil 						$a 	editor 	selectAll 							nil 			'Select all')
		#(602 	nil						$U 	editor 	changeLineEndsToLf 				nil 			'Convert line endings to LF characters (Cuis convention) in selection')
		#(603 	nil 						$( 	editor 	encloseWithParenthesis 			nil 			'Enclose within ( and ), or remove enclosing ( and )')
		#(604 	nil 						$[ 	editor 	encloseWithSquareBrackets 		nil 			'Enclose within [ and ], or remove enclosing [ and ]')
		#(605 	nil 						${ 	editor 	encloseWithCurlyBrackets 			nil 			'Enclose within { and }, or remove enclosing { and }')
		#(606 	nil 						$' 	editor 	encloseWithSingleQuotes 			nil 			'Enclose within single quotes, or remove enclosing single quotes')
		#(607 	nil 						$" 	editor 	encloseWithDoubleQuotes 			nil 			'Enclose within double quotes, or remove enclosing double quotes')
		#(608 	nil 						$` 	editor 	encloseWithBackTicks 				nil 			'Enclose within backticks, or remove enclosing backticks')
		"#(609 	nil 						$< 	editor 	encloseWithAngleBrackets 			nil 			'Enclose within < and >, or remove enclosing ( and )')"
		#(610 	nil 						$H 	editor 	cursorTopHome 					nil 			'Move cursor to start of text')
	).
! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 10:28:48'!
keyboardShortuctsSubmenuSpec
	"Keyboard shortcuts only"

	^#(
		"Keyboard shortcut only. No menu entry."
		#(1500	nil 	$0 	editor 	changeEmphasisNormal 			nil 	'Normal')
		#(1501	nil 	$1 	editor 	changeEmphasisBold 				nil 	'Bold')
		#(1502	nil 	$2 	editor 	changeEmphasisItalic 				nil 	'Italic')
		#(1503	nil 	$3 	editor 	changeEmphasisUnderlined 		nil 	'Underlined')
		#(1504	nil 	$4 	editor 	changeEmphasisStruckThrough 	nil 	'Struck Through')
		#(1505	nil 	$5 	editor 	changeEmphasisSuperscript 		nil 	'Superscript')
		#(1506	nil 	$6 	editor 	changeEmphasisSubscript 			nil 	'Subscript')
		#(1507	nil 	$l 	editor 	cancel 								nil 	'Cancel all edits')
		#(1508	nil 	$Q 	editor 	argPrev 							nil 	'Previous argument')
		#(1509	nil 	$W 	editor 	argNext 							nil 	'Next argument')
		#(1510	nil 	$T 	editor 	displayIfTrue)
		#(1511	nil 	$F 	editor 	displayIfFalse)
		#(1601 	nil 	$a 	editor 	selectAll 							nil 	'Select all')
		#(1603 	nil 	$U 	editor 	changeLineEndsToLf 				nil 	'Convert line endings to LF characters (Cuis convention) in selection')
		#(1604 	nil 	$( 	editor 	encloseWithParenthesis 			nil 	'Enclose within ( and ), or remove enclosing ( and )')
		#(1605 	nil 	$[ 	editor 	encloseWithSquareBrackets 		nil 	'Enclose within [ and ], or remove enclosing [ and ]')
		#(1606 	nil 	${ 	editor 	encloseWithCurlyBrackets 			nil 	'Enclose within { and }, or remove enclosing { and }')
		#(1607 	nil 	$' 	editor 	encloseWithSingleQuotes 			nil 	'Enclose within single quotes, or remove enclosing single quotes')
		#(1608 	nil 	$" 	editor 	encloseWithDoubleQuotes 			nil 	'Enclose within double quotes, or remove enclosing double quotes')
		#(1609 	nil 	$` 	editor 	encloseWithBackTicks 				nil 	'Enclose within backticks, or remove enclosing backticks')
		"#(1610 nil 	$< 	editor 	encloseWithAngleBrackets 			nil 	'Enclose within < and >, or remove enclosing ( and )')"
		#(1611 	nil 	$H 	editor 	cursorTopHome 					nil 	'Move cursor to start of text')
	).! !

