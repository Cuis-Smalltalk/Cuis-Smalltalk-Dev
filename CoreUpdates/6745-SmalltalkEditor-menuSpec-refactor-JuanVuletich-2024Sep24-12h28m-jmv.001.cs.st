'From Cuis7.1 [latest update: #6742] on 24 September 2024 at 12:46:42 pm'!

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 12:29:02'!
editSubmenuSpec
	" 'Edit' SubMenu"

	^#(
		#(1010 		'-----')
		#(1020		'Find...(f)' 							$f 	editor 			find 						findIcon 									'Find')
		#(1030		'Find Again (g)' 							$g 	editor 			findAgain 						systemIcon 									'Find again')
		#(1040		'Use Selection for Find (j)' 							$j 	editor 			setSearchString 						saveAsIcon 									'Set selection as search string for find again')
		#(1050 		'-----')
		#(1060		'Undo - multiple (z)' 							$z 	editor 			undo 						undoIcon 									'Undo (multiple levels)')
		#(1070		'Redo - multiple (Z)' 							$Z 	editor 			redo 						redoIcon 									'Redo (multiple levels)')
		#(1080		'Undo / Redo history' 							nil 	editor 			offerUndoHistory 						changesIcon)
		#(1090 		'-----')
		#(1100		'Copy (c)' 							$c 	editor 			copySelection 						copyIcon 									'Copy selection to the Clipboard')
		#(1110		'Cut (x)' 							$x 	editor 			cut 						cutIcon 									'Cut selection and store it in the Clipboard')
		#(1120		'Paste (v)' 							$v 	editor 			paste 						pasteIcon 									'Paste Clipboard contents')
		#(1140		'Paste...' 							nil 	editor 			pasteRecent 						worldIcon)
		#(1150 		'-----')
		#(1160		'Toggle WordWrap' 							nil 	editor 			wrapOnOff 						genericTextIcon)
		#(1170		'Clear Font' 							nil 	editor 			clearFont 						newIcon)
		#(1180		'Set Font... (k)' 							$k 	editor 			offerFontMenu 						preferencesDesktopFontIcon 									'Set font')
		#(1190		'Set Font Size' 							nil 	editor 			offerFontSizeMenu 						preferencesDesktopFontIcon)
	).! !

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 12:31:45'!
keyboardShortuctsSubmenuSpec
	"Keyboard shortcuts only"

	^#(
		#(1500		nil 							$0 	editor 			changeEmphasisNormal 									nil 		'Normal')
		#(1501		nil 							$1 	editor 			changeEmphasisBold 									nil 		'Bold')
		#(1502		nil 							$2 	editor 			changeEmphasisItalic 									nil 		'Italic')
		#(1503		nil 							$3 	editor 			changeEmphasisUnderlined 									nil 		'Underlined')
		#(1504		nil 							$4 	editor 			changeEmphasisStruckThrough 									nil 		'Struck Through')
		#(1505		nil 							$5 	editor 			changeEmphasisSuperscript 									nil 		'Superscript')
		#(1506		nil 							$6 	editor 			changeEmphasisSubscript 									nil 		'Subscript')
		#(1507		nil 							$l 	editor 			cancel 									nil 		'Cancel all edits')
		#(1508		nil 							$Q 	editor 			argPrev 									nil 		'Previous argument')
		#(1509		nil 							$W 	editor 			argNext 									nil 		'Next argument')
		#(1510		nil 							$T 	editor 			displayIfTrue)
		#(1511		nil 							$F 	editor 			displayIfFalse)
		#(1601 		nil 							$a 	editor 			selectAll 									nil 		'Select all')
		#(1602 		nil 							$C 	editor 			compareToClipboard 									nil 		'Compare argument to clipboard')
		#(1603 		nil 							$U 	editor 			changeLineEndsToLf 									nil 		'Convert line endings to LF characters (Cuis convention) in selection')
		#(1604 		nil 							$( 	editor 			encloseWithParenthesis 									nil 		'Enclose within ( and ), or remove enclosing ( and )')
		#(1605 		nil 							$[ 	editor 			encloseWithSquareBrackets 									nil 		'Enclose within [ and ], or remove enclosing [ and ]')
		#(1606 		nil 							${ 	editor 			encloseWithCurlyBrackets 									nil 		'Enclose within { and }, or remove enclosing { and }')
		#(1607 		nil 							$' 	editor 			encloseWithSingleQuotes 									nil 		'Enclose within single quotes, or remove enclosing single quotes')
		#(1608 		nil 							$" 	editor 			encloseWithDoubleQuotes 									nil 		'Enclose within double quotes, or remove enclosing double quotes')
		#(1609 		nil 							$` 	editor 			encloseWithBackTicks 									nil 		'Enclose within backticks, or remove enclosing backticks')
		"#(1610 		nil 							$< 	editor 			encloseWithAngleBrackets 									nil 		'Enclose within < and >, or remove enclosing ( and )')"
		#(1611 		nil 							$H 	editor 			cursorTopHome 									nil 		'Move cursor to start of text')
	).! !

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 12:30:40'!
refactoringsSubmenuSpec
	" 'Refactorings' SubMenu"

	^#(
		#(2010 		'-----')
		#(2020		'Rename... (R)' 									$R 	editor 			contextualRename 										saveAsIcon 				'Renames what is under cursor')
		#(2030		'Extract as Parameter... (V)' 									$V 	editor 			contextualExtractAsParameter 										listAddIcon 				'Extracts the selected code as parameter')
		#(2040		'Add Parameter... (A)' 									$A 	editor 			contextualAddParameter 										listAddIcon 				'Adds parameter to message that is under cursor')
		#(2050		'Remove Parameter... (S)' 									$S 	editor 			contextualRemoveParameter 										listRemoveIcon 'Removes parameter to message that is under cursor')
		#(2060 		'-----')
		#(2070		'Extract Temporary... (J)' 									$J 	editor 			extractToTemporary 										saveAsIcon 				'Extracts the selected code into a temporary variable')
		#(2080		'Extract Method... (K)' 									$K 	editor 			extractMethod 										saveAsIcon 				'Extracts the selected code into a separate method')
		#(2090 		'-----')
		#(2100		'Inline Temporary... (O)' 									$O 	editor 			inlineTemporaryVariable 										saveAsIcon 				'Inlines the selected temporary variable into each usage')
		#(2110		'Inline Method... (F)' 									$F 	editor 			contextualInlineMethod 										saveAsIcon  				'Inlines the selected message send into desired colaborations')
		#(2120 		'-----')
		#(2130		'Temporary to Instance Variable (G)' 									$G 	editor 			temporaryToInstanceVariable 										saveAsIcon 				'Extracts temporary variable to instance variable')
		#(2140		'Push Up Instance Variable' 									nil 	editor 			contextualPushUpInClassDefinition 										goTopIcon)
		#(2150		'Push Down Instance Variable' 									nil 	editor 			contextualPushDownInClassDefinition 										goBottomIcon)
	).! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 9/24/2024 12:45:55'!
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
				ifTrue: [ #(240				'Accept (s)' 							$s 	editor 			acceptContents 						acceptIcon 							'Save') ]
				ifFalse: [ #(240 				'Accept' 							nil 	editor 			nil 						acceptIcon 							'Save') ].
			methodEntries := #(
				#(170 		'-----')
				#(180		'Refactorings' 									nil 	nil 			2000 						listAddIcon)
				#(250		'Accept & Run Test in Class (t)' 									$t 	editor 			acceptAndTest 						weatherFewCloudsIcon 							'Saves method, runs it as test and then all tests in class. Opens debugger if error.')
				#(260		'Accept & Run Test in Category (y)' 									$y 	editor 			acceptAndTestAll 						weatherFewCloudsIcon 							'Saves method, runs it as test and then all tests in class category. Opens debugger if error.')
				#(270		'Accept & Debug Test (r)' 									$r 	editor 			acceptAndDebugTest 						weatherFewCloudsIcon 							'Saves method and debugs it')
			) ]
		ifFalse: [
			acceptEntry := self hasUnacceptedEdits
				ifTrue: [ #(240				'Save to file (s)' 							$s 	editor 			acceptContents 						acceptIcon 							'Save contents to a file') ]
				ifFalse: [ #(240 				'Save to file' 							nil 	editor 			nil 						acceptIcon 							'Save contents to a file') ]].
	^Array streamContents: [ :strm |
		strm nextPutAll:
			#(
				#(0 		'Smalltalk Editor' 					true)
				#(10		'Help... (h)' 							$h 	editor 			openHelp 						helpIcon 							'Open Help')
				#(20 		'-----')
				#(30		'Do it (d)' 							$d 	editor 			doIt 						doItIcon 							'Do it (selection is a valid expression)')
				#(40		'Print it (p)' 							$p 	editor 			printIt 						printIcon 							'Print it (selection is a valid expression)')
				#(50		'Inspect it (i)' 							$i 	editor 			inspectIt 						inspectIcon 							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
				#(60		'Explore it (I)' 							$I 	editor 			exploreIt 						exploreIcon							'Inspect via Object Explorer')
				#(70		'Debug it (D)' 							$D 	editor 			debugIt 						debugIcon							'Debug it')
				#(80		'Profile it' 							nil 	editor 			profileIt 						clockIcon)
				#(90 		'-----')
				#(100		'Edit' 							nil 	nil 			1000 						textEditorIcon)
				#(110 		'-----')
				#(120		'Explain' 							nil 	editor 			explain 						helpIcon)
				#(130		'Browse it (b)' 							$b 	editor 			browseIt 						editFindReplaceIcon 							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
				#(140		'Senders of it (n)' 							$n 	editor 			sendersOfIt 						mailForwardIcon 							'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(150		'Implementors of it (m)' 							$m 	editor 			implementorsOfIt 						developmentIcon 							'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')
				#(160		'References to it (N)' 							$N 	editor 			referencesToIt 						addressBookIcon 							'References to it (selection is a class name, or cursor is over a class-list or message-list)')
				#(190 		'-----')
				#(200		'Method Strings with it (E)' 							$E	editor 			methodStringsContainingIt 								genericTextIcon 					'Method strings containing it')
				#(210		'Method Source with it (e)' 							$e 	editor 			methodSourceContainingIt 								scriptIcon 					'Method source containing it')
				#(220		'Class Comments with it' 							nil 	editor 			classCommentsContainingIt 								chatIcon)
				#(230 		'-----')
			);
			nextPut: acceptEntry.
		isEditingAMethod ifTrue: [
			strm nextPutAll: methodEntries ].
		strm
			nextPutAll: self editSubmenuSpec;
			nextPutAll: self keyboardShortuctsSubmenuSpec.
		isEditingAMethod ifTrue: [
			strm nextPutAll: self refactoringsSubmenuSpec ].
	].! !

