'From Cuis7.1 [latest update: #6744] on 24 September 2024 at 4:24:11 pm'!

!CodeProvider methodsFor: 'controls' stamp: 'jmv 9/24/2024 15:48:03'!
whatToShowSubmenuSpec
	" 'What to Show' SubMenu"
	^{
		{3010. 		'-----'}.
		{3020.	 	self showingSourceString. 								nil. 	#codeProvider. 					#showSource. 							nil. 'the textual source code as written'}.
		{3030. 		'-----'}.
		{3040.		self showingPrettyPrintString. 								nil. 	#codeProvider. 					#showPrettyPrint. 							nil. 'the method source presented in a standard text format'}.
		{3050. 		'-----'}.
		{3060.		self showingLineDiffsString. 								nil. 	#codeProvider. 					#showLineDiffs. 			 				nil. 'the textual source lines diffed from its prior version'}.
		{3070.		self showingWordDiffsString. 								nil. 	#codeProvider. 					#showWordDiffs. 			 				nil. 'the textual source words diffed from its prior version'}.
		{3080.		self showingPrettyLineDiffsString. 								nil. 	#codeProvider. 					#showPrettyLineDiffs. 			 				nil. 'formatted source lines diffed from formatted prior version'}.
		{3090.		self showingPrettyWordDiffsString. 								nil. 	#codeProvider. 					#showPrettyWordDiffs. 			 				nil. 'formatted source words diffed from formatted prior version'}.
		{3100. 		'-----'}.
		{3110.		self showingDecompileString. 								nil. 	#codeProvider. 					#showDecompile. 			 				nil. 'source code decompiled from byteCodes'}.
		{3120.		self showingByteCodesString. 								nil. 	#codeProvider. 					#showByteCodes. 			 				nil. 'the bytecodes that comprise the compiled method'}.
	}.
! !


!CodeFileBrowser methodsFor: 'controls' stamp: 'jmv 9/24/2024 16:10:45'!
whatToShowSubmenuSpec
	" 'What to Show' SubMenu"
	^{
		{3010. 		'-----'}.
		{3020.	 	self showingSourceString. 								nil. 	#codeProvider. 					#showSource. 							nil. 'the textual source code as written'}.
		{3030. 		'-----'}.
		{3040.		self showingLineDiffsString. 								nil. 	#codeProvider. 					#showLineDiffs. 			 				nil. 'the textual source lines diffed from its prior version'}.
		{3050.		self showingWordDiffsString. 								nil. 	#codeProvider. 					#showWordDiffs. 			 				nil. 'the textual source words diffed from its prior version'}.
		{3060.		self showingPrettyLineDiffsString. 								nil. 	#codeProvider. 					#showPrettyLineDiffs. 			 				nil. 'formatted source lines diffed from formatted prior version'}.
		{3070.		self showingPrettyWordDiffsString. 								nil. 	#codeProvider. 					#showPrettyWordDiffs. 			 				nil. 'formatted source words diffed from formatted prior version'}.
	}.
! !


!ChangeList methodsFor: 'controls' stamp: 'jmv 9/24/2024 16:10:58'!
whatToShowSubmenuSpec
	" 'What to Show' SubMenu"
	^{
		{3010. 		'-----'}.
		{3020.	 	self showingSourceString. 								nil. 	#codeProvider. 					#showSource. 							nil. 'the textual source code as written'}.
		{3030. 		'-----'}.
		{3040.		self showingLineDiffsString. 								nil. 	#codeProvider. 					#showLineDiffs. 			 				nil. 'the textual source lines diffed from its prior version'}.
		{3050.		self showingWordDiffsString. 								nil. 	#codeProvider. 					#showWordDiffs. 			 				nil. 'the textual source words diffed from its prior version'}.
		{3060.		self showingPrettyLineDiffsString. 								nil. 	#codeProvider. 					#showPrettyLineDiffs. 			 				nil. 'formatted source lines diffed from formatted prior version'}.
		{3070.		self showingPrettyWordDiffsString. 								nil. 	#codeProvider. 					#showPrettyWordDiffs. 			 				nil. 'formatted source words diffed from formatted prior version'}.
	}.
! !


!ClassCommentVersionsBrowser methodsFor: 'controls' stamp: 'jmv 9/24/2024 16:10:19'!
whatToShowSubmenuSpec
	" 'What to Show' SubMenu"
	^{
		{3010. 		'-----'}.
		{3020.	 	self showingSourceString. 								nil. 	#codeProvider. 					#showSource. 							nil. 'the textual source code as written'}.
		{3030. 		'-----'}.
		{3040.		self showingLineDiffsString. 								nil. 	#codeProvider. 					#showLineDiffs. 			 				nil. 'the textual source lines diffed from its prior version'}.
	}.
! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 9/24/2024 10:25:58'!
codeProvider
	"Return my Text Provider."
	editor ifNil: [ self installEditorAndTextComposition ].
	^editor codeProvider! !

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 9/24/2024 10:15:18'!
model
	^model! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 9/24/2024 15:57:15'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList textConstructor |
	textConstructor := [ :string :backgroundColor | 
		string asText addAttribute: (TextBackgroundColor color: backgroundColor) ].

	aList := #(
		(10	'browse'				browseMethodFull									'view this method in a browser'								browseMethodFullAndReplaceMe)
		(11	'senders' 				browseSendersOfMessages									'browse senders of...'								browseSenders)
		(16	'implementors'				browseImplementorsOfMessages									'browse implementors of...' 								browseImplementors)
		(12	'versions'				browseVersions									'browse versions')), 

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
				{#(13	'inheritance'		 methodInheritance	 'browse method inheritance'. #methodInheritanceAndReplaceMe)}]),

		#(
		(12	'hierarchy'				browseHierarchy									'browse class hierarchy'								browseHierarchyAndReplaceMe)
		(10	'inst vars'				browseInstVarRefs									'inst var refs...')
		(11	'class vars'				browseClassVarRefs									'class var refs...')).

	^ aList! !


!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 16:18:17'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message List' 					false)
		#(10		'browse full (b)' 							$b 	window 			browseMethodFull 									editFindReplaceIcon)
		#(20		'browse hierarchy (h)' 							$h 	window 			browseHierarchy 									goTopIcon)
		#(30		'browse method (O)' 							$O 	window 			openSingleMessageBrowser 									scriptIcon)
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


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 9/24/2024 15:57:51'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"
	^ (model baseCodeSource isLiveSmalltalkImage and: [ model caseCodeSource isLiveSmalltalkImage not ])
		ifTrue: [ super optionalButtonTuples ]
		ifFalse: [
			"For non-standard browser configurations assume most of the default buttons are invalid"
			#(
			) ].! !


!MethodSetWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 16:18:51'!
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


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 16:23:52'!
defaultMenuSpec
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	| codeProvider acceptEntry methodEntries isEditingAMethod |
	isEditingAMethod := false.
	codeProvider := self codeProvider.
	((codeProvider is: #CodeProvider) and: [ codeProvider isEditingMethod ])
		ifTrue: [
			isEditingAMethod := true.
			acceptEntry := self hasUnacceptedEdits
				ifTrue: [ #(250				'Accept (s)' 							$s 	editor 			acceptContents 						acceptIcon 							'Save') ]
				ifFalse: [ #(250 				'Accept' 							nil 	editor 			nil 						acceptIcon 							'Save') ].
			methodEntries := #(
				#(20 		'-----')
				#(30		'What to Show' 									nil 	nil 			3000 						preferencesIcon 							'Select alternative views of the method')
				#(40		'Toggle Break' 									nil 	model 			toggleBreakOnEntry 						debugIcon 							'Add o remove a Breakpoint')
				#(170 		'-----')
				#(180		'Refactorings' 									nil 	nil 			2000 						listAddIcon)
				#(260		'Accept & Run Test in Class (t)' 									$t 	editor 			acceptAndTest 						weatherFewCloudsIcon 							'Saves method, runs it as test and then all tests in class. Opens debugger if error.')
				#(270		'Accept & Run Test in Category (y)' 									$y 	editor 			acceptAndTestAll 						weatherFewCloudsIcon 							'Saves method, runs it as test and then all tests in class category. Opens debugger if error.')
				#(280		'Accept & Debug Test (r)' 									$r 	editor 			acceptAndDebugTest 						weatherFewCloudsIcon 							'Saves method and debugs it')
			) ]
		ifFalse: [
			acceptEntry := self hasUnacceptedEdits
				ifTrue: [ #(250				'Save to file (s)' 							$s 	editor 			acceptContents 						acceptIcon 							'Save contents to a file') ]
				ifFalse: [ #(250 				'Save to file' 							nil 	editor 			nil 						acceptIcon 							'Save contents to a file') ]].
	^Array streamContents: [ :strm |
		strm nextPutAll:
			#(
				#(0 		'Smalltalk Editor' 					true)
				#(10		'Help... (h)' 							$h 	editor 			openHelp 						helpIcon 							'Open Help')
				#(50 		'-----')
				#(60		'Do it (d)' 							$d 	editor 			doIt 						doItIcon 							'Do it (selection is a valid expression)')
				#(70		'Print it (p)' 							$p 	editor 			printIt 						printIcon 							'Print it (selection is a valid expression)')
				#(80		'Inspect it (i)' 							$i 	editor 			inspectIt 						inspectIcon 							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
				#(90		'Explore it (I)' 							$I 	editor 			exploreIt 						exploreIcon							'Inspect via Object Explorer')
				#(100		'Debug it (D)' 							$D 	editor 			debugIt 						debugIcon							'Debug it')
				#(110		'Profile it' 							nil 	editor 			profileIt 						clockIcon)
				#(120 		'-----')
				#(130		'Edit' 							nil 	nil 			1000 						textEditorIcon)
				#(140 		'-----')
				#(150		'Explain' 							nil 	editor 			explain 						helpIcon)
				#(160		'Browse it (b)' 							$b 	editor 			browseIt 						editFindReplaceIcon 							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
				#(170		'Senders of it (n)' 							$n 	editor 			sendersOfIt 						mailForwardIcon 							'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(180		'Implementors of it (m)' 							$m 	editor 			implementorsOfIt 						developmentIcon 							'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(190		'References to it (N)' 							$N 	editor 			referencesToIt 						addressBookIcon 							'References to it (selection is a class name, or cursor is over a class-list or message-list)')
				#(200 		'-----')
				#(210		'Method Strings with it (E)' 							$E	editor 			methodStringsContainingIt 								genericTextIcon 					'Method strings containing it')
				#(220		'Method Source with it (e)' 							$e 	editor 			methodSourceContainingIt 								scriptIcon 					'Method source containing it')
				#(230		'Class Comments with it' 							nil 	editor 			classCommentsContainingIt 								chatIcon)
				#(240 		'-----')
			);
			nextPut: acceptEntry.
		isEditingAMethod ifTrue: [
			strm nextPutAll: methodEntries ].
		strm
			nextPutAll: self editSubmenuSpec;
			nextPutAll: self keyboardShortuctsSubmenuSpec.
		isEditingAMethod ifTrue: [
			strm nextPutAll: self refactoringsSubmenuSpec ].
		isEditingAMethod ifTrue: [
			strm nextPutAll: self codeProvider whatToShowSubmenuSpec ].
	].! !

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 16:15:05'!
keyboardShortuctsSubmenuSpec
	"Keyboard shortcuts only"

	^#(
		#(1500		nil 	$0 	editor 			changeEmphasisNormal 									nil 		'Normal')
		#(1501		nil 	$1 	editor 			changeEmphasisBold 									nil 		'Bold')
		#(1502		nil 	$2 	editor 			changeEmphasisItalic 									nil 		'Italic')
		#(1503		nil 	$3 	editor 			changeEmphasisUnderlined 									nil 		'Underlined')
		#(1504		nil 	$4 	editor 			changeEmphasisStruckThrough 									nil 		'Struck Through')
		#(1505		nil 	$5 	editor 			changeEmphasisSuperscript 									nil 		'Superscript')
		#(1506		nil 	$6 	editor 			changeEmphasisSubscript 									nil 		'Subscript')
		#(1507		nil 	$l 	editor 			cancel 									nil 		'Cancel all edits')
		#(1508		nil 	$Q 	editor 			argPrev 									nil 		'Previous argument')
		#(1509		nil 	$W 	editor 			argNext 									nil 		'Next argument')
		#(1510		nil 	$T 	editor 			displayIfTrue)
		#(1511		nil 	$F 	editor 			displayIfFalse)
		#(1601 		nil 	$a 	editor 			selectAll 									nil 		'Select all')
		#(1602 		nil 	$C 	editor 			compareToClipboard 									nil 		'Compare argument to clipboard')
		#(1603 		nil 	$U 	editor 			changeLineEndsToLf 									nil 		'Convert line endings to LF characters (Cuis convention) in selection')
		#(1604 		nil 	$( 	editor 			encloseWithParenthesis 									nil 		'Enclose within ( and ), or remove enclosing ( and )')
		#(1605 		nil 	$[ 	editor 			encloseWithSquareBrackets 									nil 		'Enclose within [ and ], or remove enclosing [ and ]')
		#(1606 		nil 	${ 	editor 			encloseWithCurlyBrackets 									nil 		'Enclose within { and }, or remove enclosing { and }')
		#(1607 		nil 	$' 	editor 			encloseWithSingleQuotes 									nil 		'Enclose within single quotes, or remove enclosing single quotes')
		#(1608 		nil 	$" 	editor 			encloseWithDoubleQuotes 									nil 		'Enclose within double quotes, or remove enclosing double quotes')
		#(1609 		nil 	$` 	editor 			encloseWithBackTicks 									nil 		'Enclose within backticks, or remove enclosing backticks')
		"#(1610 		nil 	$< 	editor 			encloseWithAngleBrackets 									nil 		'Enclose within < and >, or remove enclosing ( and )')"
		#(1611 		nil 	$H 	editor 			cursorTopHome 									nil 		'Move cursor to start of text')
	).! !

!methodRemoval: DebuggerWindow #optionalButtonTuples stamp: 'jmv 9/24/2024 16:16:34'!
DebuggerWindow removeSelector: #optionalButtonTuples!
!methodRemoval: CodeWindow #addContentsTogglesTo: stamp: 'jmv 9/24/2024 16:16:44'!
CodeWindow removeSelector: #addContentsTogglesTo:!
!methodRemoval: CodeWindow #offerWhatToShowMenu stamp: 'jmv 9/24/2024 16:16:44'!
CodeWindow removeSelector: #offerWhatToShowMenu!
!methodRemoval: ClassCommentVersionsBrowser #contentsSymbolQuints stamp: 'jmv 9/24/2024 16:16:44'!
ClassCommentVersionsBrowser removeSelector: #contentsSymbolQuints!
!methodRemoval: ChangeList #contentsSymbolQuints stamp: 'jmv 9/24/2024 16:16:44'!
ChangeList removeSelector: #contentsSymbolQuints!
!methodRemoval: CodeFileBrowser #contentsSymbolQuints stamp: 'jmv 9/24/2024 16:16:44'!
CodeFileBrowser removeSelector: #contentsSymbolQuints!
!methodRemoval: CodeProvider #contentsSymbolQuints stamp: 'jmv 9/24/2024 16:16:44'!
CodeProvider removeSelector: #contentsSymbolQuints!
!methodRemoval: CodeProvider #sourceAndDiffsQuintsOnly stamp: 'jmv 9/24/2024 16:16:44'!
CodeProvider removeSelector: #sourceAndDiffsQuintsOnly!
