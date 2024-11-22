'From Cuis7.1 [latest update: #6840] on 22 November 2024 at 11:47:33 am'!

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

		"Keyboard shortcuts only"
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


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 11:36:13'!
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
				ifTrue: [ #(270	'Save (s)' 				$s 	editor 	acceptContents 		acceptIcon 				'Save') ]
				ifFalse: [ #(270 	'Save' 					nil 	editor 	nil 						acceptIcon 				'Save') ].
			methodEntries := #(
				#(20 	'-----')
				#(30	'What to Show' 					nil 	nil 		3000 					preferencesIcon 		'Select alternative views of the method')
				#(40	'Toggle Break' 					nil 	editor 	toggleBreakOnEntry 	debugIcon 				'Add or remove a Breakpoint')
				#(200 	'-----')
				#(210	'Refactorings' 					nil 	nil 		2000 					listAddIcon)
				#(280	'Save & Run Test in Class (t)' 		$t 	editor 	acceptAndTest 			weatherFewCloudsIcon 'Saves method, runs it as test and then all tests in class. Opens debugger if error.')
				#(290	'Save & Run Test in Category (y)' 	$y 	editor 	acceptAndTestAll 		weatherFewCloudsIcon 'Saves method, runs it as test and then all tests in class category. Opens debugger if error.')
				#(300	'Save & Debug Test (r)' 			$r 	editor 	acceptAndDebugTest 	weatherFewCloudsIcon 'Saves method and debugs it')
			) ]
		ifFalse: [
			acceptEntry := self hasUnacceptedEdits
				ifTrue: [ #(270	'Save to file (s)' 			$s 	editor 	acceptContents 		acceptIcon 				'Save contents to a file') ]
				ifFalse: [ #(270 	'Save to file' 				nil 	editor 	nil 						acceptIcon 				'Save contents to a file') ]].
	^Array streamContents: [ :strm |
		strm nextPutAll:
			#(
				#(0 		'Smalltalk Editor' 				true)
				#(10	'Help... (h)' 					$h 	editor 	openHelp 				helpIcon 				'Open Help')
				#(50 	'-----')
				#(60	'Do it (d)' 					$d 	editor 	doIt 					doItIcon 				'Do it (selection is a valid expression)')
				#(70	'Print it (p)' 					$p 	editor 	printIt 					printIcon 				'Print it (selection is a valid expression)')
				#(80	'Inspect it (i)' 				$i 	editor 	inspectIt 				inspectIcon 			'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
				#(90	'Explore it (I)' 				$I 	editor 	exploreIt 				exploreIcon			'Inspect via Object Explorer')
				#(100	'Debug it (D)' 				$D 	editor 	debugIt 				debugIcon				'Debug it')
				#(110	'Profile it' 					nil 	editor 	profileIt 				clockIcon)
				#(120 	'-----')
				#(130	'Edit' 						nil 	nil 		1000 					textEditorIcon)
				#(140 	'-----')
				#(150	'Explain' 					nil 	editor 	explain 				helpIcon)
				#(160	'Browse it (b)' 				$b 	editor 	browseIt 				editFindReplaceIcon 	'Browse it (selection is a class name or cursor is over a class-list or message-list)')
				#(170	'Senders (n)' 				$n 	editor 	sendersOfIt 			mailForwardIcon 		'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(180	'Implementors (m)' 			$m 	editor 	implementorsOfIt 		developmentIcon 		'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(190	'References (N)' 				$N 	editor 	referencesToIt 			addressBookIcon 		'References to it (selection is a class name, or cursor is over a class-list or message-list)')
				#(220 	'-----')
				#(230	'Find with Scope (e)' 			$e 	editor 	findMethodSourceContainingIt scriptIcon 		'Find with scope')
				#(240	'Method Strings with it (E)' 	$E	editor 	methodStringsContainingIt 	genericTextIcon 'Method strings containing it')
				#(250	'Find in class comments' 		nil 	editor 	classCommentsContainingIt 	chatIcon)
				#(260 	'-----')
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

