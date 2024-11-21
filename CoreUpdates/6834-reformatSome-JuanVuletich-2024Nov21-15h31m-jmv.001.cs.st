'From Cuis7.1 [latest update: #6833] on 21 November 2024 at 3:32:54 pm'!

!CodeProvider methodsFor: 'controls' stamp: 'jmv 11/21/2024 15:15:44'!
whatToShowSubmenuSpec
	" 'What to Show' SubMenu"
	^{
		{3010. 	'-----'}.
		{3020.	 self showingSourceString. 			nil. 	#codeProvider. 	#showSource. 				nil. 'the textual source code as written'}.
		{3030. 	'-----'}.
		{3040.	self showingPrettyPrintString. 			nil. 	#codeProvider. 	#showPrettyPrint. 			nil. 'the method source presented in a standard text format'}.
		{3050. 	'-----'}.
		{3060.	self showingLineDiffsString. 			nil. 	#codeProvider. 	#showLineDiffs. 			nil. 'the textual source lines diffed from its prior version'}.
		{3070.	self showingWordDiffsString. 			nil. 	#codeProvider. 	#showWordDiffs. 			nil. 'the textual source words diffed from its prior version'}.
		{3080.	self showingPrettyLineDiffsString. 		nil. 	#codeProvider. 	#showPrettyLineDiffs. 		nil. 'formatted source lines diffed from formatted prior version'}.
		{3090.	self showingPrettyWordDiffsString. 	nil. 	#codeProvider. 	#showPrettyWordDiffs. 	nil. 'formatted source words diffed from formatted prior version'}.
		{3100. 	'-----'}.
		{3110.	self showingDecompileString. 			nil. 	#codeProvider. 	#showDecompile. 			nil. 'source code decompiled from byteCodes'}.
		{3120.	self showingByteCodesString. 			nil. 	#codeProvider. 	#showByteCodes. 			nil. 'the bytecodes that comprise the compiled method'}.
	}.
! !


!CodeFileBrowser methodsFor: 'controls' stamp: 'jmv 11/21/2024 15:16:52'!
whatToShowSubmenuSpec
	" 'What to Show' SubMenu"
	^{
		{3010. 	'-----'}.
		{3020.	 self showingSourceString. 		nil. 	#codeProvider. 	#showSource. 				nil. 'the textual source code as written'}.
		{3030. 	'-----'}.
		{3040.	self showingLineDiffsString. 		nil. 	#codeProvider. 	#showLineDiffs. 			nil. 'the textual source lines diffed from its prior version'}.
		{3050.	self showingWordDiffsString. 		nil. 	#codeProvider. 	#showWordDiffs. 			nil. 'the textual source words diffed from its prior version'}.
		{3060.	self showingPrettyLineDiffsString. 	nil. 	#codeProvider. 	#showPrettyLineDiffs. 		nil. 'formatted source lines diffed from formatted prior version'}.
		{3070.	self showingPrettyWordDiffsString. nil. 	#codeProvider. 	#showPrettyWordDiffs.	nil. 'formatted source words diffed from formatted prior version'}.
	}.
! !


!ChangeList methodsFor: 'controls' stamp: 'jmv 11/21/2024 15:18:33'!
whatToShowSubmenuSpec
	" 'What to Show' SubMenu"
	^{
		{3010. 	'-----'}.
		{3020.	 self showingSourceString. 			nil. 	#codeProvider. 	#showSource. 				nil. 'the textual source code as written'}.
		{3030. 	'-----'}.
		{3040.	self showingLineDiffsString. 			nil. 	#codeProvider. 	#showLineDiffs. 			nil. 'the textual source lines diffed from its prior version'}.
		{3050.	self showingWordDiffsString. 			nil. 	#codeProvider. 	#showWordDiffs. 			nil. 'the textual source words diffed from its prior version'}.
		{3060.	self showingPrettyLineDiffsString. 		nil. 	#codeProvider. 	#showPrettyLineDiffs. 		nil. 'formatted source lines diffed from formatted prior version'}.
		{3070.	self showingPrettyWordDiffsString. 	nil. 	#codeProvider. 	#showPrettyWordDiffs. 	nil. 'formatted source words diffed from formatted prior version'}.
	}.
! !


!ClassCommentVersionsBrowser methodsFor: 'controls' stamp: 'jmv 11/21/2024 15:18:37'!
whatToShowSubmenuSpec
	" 'What to Show' SubMenu"
	^{
		{3010. 	'-----'}.
		{3020.	 self showingSourceString. 	nil. 	#codeProvider. 	#showSource. 		nil. 'the textual source code as written'}.
		{3030. 	'-----'}.
		{3040.	self showingLineDiffsString. 	nil. 	#codeProvider. 	#showLineDiffs. 	nil. 'the textual source lines diffed from its prior version'}.
	}.
! !


!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/21/2024 15:19:57'!
systemCatSingletonMenuSpec
	"Spec for the menu and shortcut keys for the one-item system category list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class category' 					false)
		#(10	'find class... (f)' 	$f 	window 	findClass 						findIcon)
		#(20 	'-----')
		#(30	'browse all (B)' 	$B 	window 	browseAllClasses 				editFindReplaceIcon)
		#(40	'browse (b)' 		$b 	window 	openSystemCategoryBrowser 	packageIcon)
		#(50 	'-----')
		#(60	'fileOut (o)' 		$o 	model 		fileOutSystemCategory 		fileOutIcon)
		#(70 	'-----')
		#(80	'update (u)' 		$u 	model 		updateSystemCategories 		updateIcon)
		#(90	'rename... (R)' 	$R 	model 		renameSystemCategory 		saveAsIcon)
		#(100	'remove (x)' 		$x 	model 		removeSystemCategory 		deleteIcon)
	)! !

!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/21/2024 15:32:25'!
systemCategoriesListMenuSpec
	"Spec for the menu and shortcut keys for the system categories list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Class Categories' 	false)
		#(10	'find class... (f)' 		$f 	window 	findClass 								findIcon)
		#(20	'recent classes... (r)' 	$r 	model 		recent 									clockIcon)
		#(30 	'-----')
		#(40	'browse all (B)' 		$B 	window 	browseAllClasses 						editFindReplaceIcon)
		#(50	'browse (b)' 			$b 	window 	openSystemCategoryBrowser 			packageIcon)
		#(55	'browse package (p)' 	$p 	window 	browsePackage 						packageIcon)
		#(60 	'-----')
		#(70	'fileOut (o)' 			$o 	model 		fileOutSystemCategory 				fileOutIcon)
		#(80 	'-----')
		#(90	'reorganize' 			nil 	model 		editSystemCategories 					sendReceiveIcon)
		#(100	'alphabetize (A)' 		$A 	model 		alphabetizeSystemCategories 			fontXGenericIcon)
		#(110 	'-----')
		#(120	'update (u)' 			$u 	model 		updateSystemCategories 				updateIcon)
		#(130	'add item... (a)' 		$a 	model 		addSystemCategory 					newIcon)
		#(140	'rename... (R)' 		$R 	model 		renameSystemCategory 				saveAsIcon)
		#(150	'move to... (m)' 		$m 	model 		moveAllToOtherSystemCategory 		saveAsIcon)
		#(160	'remove (x)' 			$x 	model 		removeSystemCategory 				deleteIcon)
		#(170 	'-----')
		#(180	'move to top' 		nil 	model 		moveSystemCategoryTop 				goTopIcon)
		#(190	'move up' 			nil 	model 		moveSystemCategoryUp 				goUpIcon)
		#(200	'move down' 		nil 	model 		moveSystemCategoryDown 			goDownIcon)
		#(210	'move to bottom' 	nil 	model 		moveSystemCategoryBottom 			fileOutIcon)
		#(220 	'-----')
		#(230	'run tests (t)' 		$t 	model 		runSystemCategoryTests 				weatherFewCloudsIcon)
	)! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 11/21/2024 15:30:42'!
customButtonSpecs
	"Answer an array of elements of the form wording, selector, help-message, that characterize the custom button row of a debugger."

	^#(
		('Restart'		restart				'reset this context to its start.')
		('Into'			send				'step Into message sends')
		('Over'			doStep				'step Over message sends')
		('Through'		stepIntoBlock		'step into a block')
		('Run to Cursor'	runToCursor		'run to cursor')
		('Full Stack'		fullStack			'show full stack')
		('Where'			where				'select current pc range')
		('Create'			createMethod		'create method'))! !

!DebuggerWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/21/2024 15:11:29'!
receiverVariablesListMenuSpec
	"Spec for the menu and shortcut keys for the list of instance variables in the receiver.
	See coment at Morph >> #getMenu
	"
	^#(
		#(10		'inspect (i)' 				$i 	window 	inspectReceiverSelection 		inspectIcon)
		#(20		'explore (I)' 				$I 	window 	exploreReceiverSelection 		exploreIcon)
		#(30 		'-----')
		#(40		'browser full (b)' 			$b 	window 	receiverClassBrowse 			editFindReplaceIcon)
		#(50		'browser hierarch (h)' 	$h 	window 	receiverClassHierarchy 		goTopIcon)
		#(60		'browser protocol (p)' 	$p 	window 	receiverClassProtocol 			spreadsheetIcon)
	)! !


!MessageNamesWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/21/2024 15:12:10'!
selectorListMenuSpec
	"Spec for the menu and shortcut keys for the selectors list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Selectors' 					false)
		#(10	'senders (n)' 					$n 	window 	browseSenders 			mailForwardIcon)
		#(20	'implementors of... (m)' 		$m 	window 	browseImplementors 		developmentIcon)
	).
! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 11/21/2024 15:10:30'!
preDebugButtonSpec

	^{
		{'Proceed'.		#proceed. 	'continue execution' }.
		{'Abandon'.		#abandon. 	'abandon this execution by closing this window' }.
		{'Debug'.		#debug.	'bring up a debugger' }
	}! !


!ProcessBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/21/2024 15:13:06'!
stackMenuSpec
	"Spec for the menu and shortcut keys for the list of stack frames.
	See coment at Morph >> #getMenu
	"
	^ #(
		#(10	'inspect context (c)' 		$c 	window 	inspectContext 	inspectIcon)
		#(20	'explore context (C)' 		$C 	window 	exploreContext 	exploreIcon)
		#(30	'inspect receiver (i)' 		$i 	window 	inspectReceiver 	inspectIcon)
		#(40	'explore receiver (I)' 		$I 	window 	exploreReceiver 	exploreIcon)
		#(50	'browse context (b)' 		$b 	window 	browseContext 	editFindReplaceIcon)
	).! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 11/21/2024 15:30:29'!
iconicHaloSpecifications
"Answer an array that characterizes the locations, colors, icons, and selectors of the halo handles 
that may be used in the iconic halo scheme"

	^ #("selector					horiz			vert				color info			icon key"
		(addDismissHandle:		left				top				(red)				haloDismissIcon 		'Remove')
		(addMenuHandle:			leftCenter		top				(blue lighter)		haloMenuIcon 			'Menu')
		(addGrabHandle:			center			top				(black)				haloGrabIcon 			'Pick up')
		(addDragHandle:			rightCenter		top				(brown)				haloDragIcon 			'Move')
		(addDupHandle:			right			top				(green)				haloDuplicateIcon 		'Duplicate')	
		(addExploreHandle:		left				topCenter		(orange)			haloDebugIcon 		'Explore')
		(addDebugHandle:			right			topCenter		(orange)			haloDebugIcon 		'Debug')
		(addCollapseHandle:		left				center			(tan)				haloCollapseIcon 		'Collapse')
		(addScaleHandle:			right			center			(blue)				haloScaleIcon 			'Change scale')
		(addRotateHandle:			left				bottom			(blue)				haloRotateIcon 		'Rotate')
		(addHelpHandle:			center			bottom			(lightBlue)			haloHelpIcon 			'Help')
		(addResizeHandle:			right			bottom			(yellow)			haloResizeIcon 			'Change size')
		"FIXME - Currently non-functional...
		(addRecolorHandle:			right			bottomCenter	(magenta darker)	haloColorIcon 			'Change color')"
	)! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/21/2024 15:25:29'!
refactoringsSubmenuSpec
	" 'Refactorings' SubMenu"

	^#(
		#(2010 		'-----')
		#(2020		'Rename... (R)' 							$R 	editor 	contextualRename 							saveAsIcon 		'Renames what is under cursor')
		#(2030		'Extract as Parameter... (V)' 				$V 	editor 	contextualExtractAsParameter 			listAddIcon 		'Extracts the selected code as parameter')
		#(2040		'Add Parameter... (A)' 					$A 	editor 	contextualAddParameter 					listAddIcon 		'Adds parameter to message that is under cursor')
		#(2050		'Remove Parameter... (S)' 				$S 	editor 	contextualRemoveParameter 				listRemoveIcon 	'Removes parameter to message that is under cursor')
		#(2060 		'-----')
		#(2070		'Extract Temporary... (J)' 					$J 	editor 	extractToTemporary 						saveAsIcon 		'Extracts the selected code into a temporary variable')
		#(2080		'Extract Method... (K)' 					$K 	editor 	extractMethod 								saveAsIcon 		'Extracts the selected code into a separate method')
		#(2090 		'-----')
		#(2100		'Inline Temporary... (O)' 					$O 	editor 	inlineTemporaryVariable 					saveAsIcon 		'Inlines the selected temporary variable into each usage')
		#(2110		'Inline Method... (F)' 						$F 	editor 	contextualInlineMethod 					saveAsIcon  		'Inlines the selected message send into desired colaborations')
		#(2120 		'-----')
		#(2130		'Temporary to Instance Variable (G)' 		$G 	editor 	temporaryToInstanceVariable 				saveAsIcon 		'Extracts temporary variable to instance variable')
		#(2140		'Push Up Instance Variable' 				nil 	editor 	contextualPushUpInClassDefinition 		goTopIcon)
		#(2150		'Push Down Instance Variable' 			nil 	editor 	contextualPushDownInClassDefinition 		goBottomIcon)
	).! !

