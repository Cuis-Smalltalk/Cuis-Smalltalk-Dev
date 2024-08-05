'From Cuis7.1 [latest update: #6461] on 11 June 2024 at 9:35:37 am'!

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 6/10/2024 19:07:06'!
cursorTopHome
	"Put cursor at beginning of text -- invoked from cmd-H shortcut, useful for keyboards that have no home key."

	self deselectAndPlaceCursorAt: 1.! !

!Editor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/10/2024 18:14:19'!
defaultMenuSpec
	#()! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 19:03:41'!
changeLineEndsToLf
	"Replace all CRs and CrLfs by LFs.
	Triggered by Cmd-U -- useful when getting code from FTP sites"
	"This is a user command, and generates undo"
	
	self replaceSelectionWith: self selectedString withCuisLineEndings.! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 17:25:05'!
encloseWith: aCharacter
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| left right startIndex stopIndex oldSelection which |
	startIndex := self startIndex.
	stopIndex := self stopIndex.
	oldSelection := self selection.
	which := '([<{"''`' indexOf: aCharacter ifAbsent: [ ^true ].
	left := '([<{"''`' at: which.
	right := ')]>}"''`' at: which.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) = left
				and: [(model actualContents at: stopIndex) = right]])
		ifTrue: [
			"already enclosed; strip off brackets"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection]
		ifFalse: [
			"not enclosed; enclose by matching brackets"
			self replaceSelectionWith:
				(Text string: (UnicodeString with: left) attributes: emphasisHere),
				oldSelection,
				(Text string: (UnicodeString with: right) attributes: emphasisHere).
			self selectFrom: startIndex+1 to: stopIndex].
	^ true.! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 17:27:22'!
encloseWithAngleBrackets
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $<! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 17:27:52'!
encloseWithBackTicks
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $`! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 17:27:12'!
encloseWithCurlyBrackets
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: ${! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 17:27:44'!
encloseWithDoubleQuotes
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $"! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 17:26:18'!
encloseWithParenthesis
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $(! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 17:27:38'!
encloseWithSingleQuotes
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $'! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 17:26:59'!
encloseWithSquareBrackets
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $[! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 19:18:11'!
makeCapitalized
	"Force the current selection to uppercase.  Triggered by Cmd-X."
	"This is a user command, and generates undo"

	| prev |
	prev := $-.  "not a letter"
	self replaceSelectionWith: 
		(self selectedString collect:
			[:c | prev := prev isLetter ifTrue: [c asLowercase] ifFalse: [c asUppercase]]).! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 19:18:06'!
makeLowercase
	"Force the current selection to lowercase.  Triggered by Cmd-X."
	"This is a user command, and generates undo"

	self replaceSelectionWith: self selectedString asLowercase.! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 19:18:03'!
makeUppercase
	"Force the current selection to uppercase.  Triggered by Cmd-Y."
	"This is a user command, and generates undo"

	self replaceSelectionWith: self selectedString asUppercase.! !


!SmalltalkEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 17:24:53'!
encloseWith: aCharacter
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| startIndex stopIndex oldSelection new enlargedBy |
	"This implementation is only for commenting and uncommenting code.
	Other cases are handled by the super implementation.
	This implementation only handles String and discards Text attributes."
	aCharacter = $" ifFalse: [
		^super encloseWith: aCharacter ].

	startIndex := self startIndex.
	stopIndex := self stopIndex.
	oldSelection := self selection string.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) = $"
				and: [(model actualContents at: stopIndex) = $"]])
		ifTrue: [
			"already enclosed in double colons; make selection no longer a comment"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection withDoubleColonsUndoubled ]
		ifFalse: [
			"not enclosed in double colons; make selection a comment"
			new := oldSelection withDoubleColonsDoubled.
			enlargedBy := new size - oldSelection size.
			self replaceSelectionWith: $" asString, new, $" asString.
			self selectFrom: startIndex+1 to: stopIndex + enlargedBy ].
	^ true.! !

!SmalltalkEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 19:26:54'!
pasteInitials
	"Replace the current text selection by an authorship name/date stamp; invoked by cmd-shift-v, easy way to put an authorship stamp in the comments of an editor."
	"This is a user command, and generates undo"

	| i |
	i := self stopIndex.
	self replaceSelectionWith: (Text fromString: Utilities changeStamp).
	self deselectAndPlaceCursorAt: i.! !


!Editor methodsFor: 'accessing' stamp: 'jmv 6/10/2024 18:34:25'!
help
	"
	TextEditor new help edit
	SmalltalkEditor new help edit
	"
	| tabs |
	^ String streamContents: [ :strm |
		tabs := String tab, String tab, String tab, String tab.
		self class allShiftShortcutsSpec do: [ :triplet | | c |
			c := triplet first = Character space
				ifFalse: [ triplet first asString, '   	' ]
				ifTrue: [ 'Space'].
			strm nextPutAll: ('Shift', '-', c, tabs, triplet third).
			strm newLine ].
		tabs := String tab, String tab, String tab, String tab, String tab.
		self defaultMenuSpec do: [ :menuItemNshortcutSpec |
			menuItemNshortcutSpec size > 6 ifTrue: [
				menuItemNshortcutSpec third ifNotNil: [ :shortcutCharacter |
					strm nextPutAll: ('Command', '-', shortcutCharacter asString, tabs, menuItemNshortcutSpec seventh).
				strm newLine ].
			].
		].
	]! !


!TextEditor methodsFor: 'typing support' stamp: 'jmv 6/11/2024 09:30:04'!
dispatchOn: aKeyboardEvent
	"Carry out the action associated with this character, if any."

	| codePoint asciiValue c |
	self clearParens.
  	codePoint := aKeyboardEvent keyValue.
	
	codePoint > 255 ifTrue: [
		^self normalCharacter: aKeyboardEvent ].

	c := aKeyboardEvent keyCharacter.
	(')]}' includes: c)
		ifTrue: [ self blinkPrevParen: c ].

	asciiValue := codePoint.
	^ self perform: (self shortcuts at: asciiValue + 1) with: aKeyboardEvent.! !

!TextEditor methodsFor: 'testing' stamp: 'jmv 6/10/2024 18:26:16'!
hasUnacceptedEdits

	^morph notNil and: [ morph hasUnacceptedEdits ].! !

!TextEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/10/2024 19:11:17'!
defaultMenuSpec
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	| acceptEntry |
	acceptEntry := self hasUnacceptedEdits
		ifTrue: [ #((110				'Accept (s)' 					$s 	editor 			acceptContents 						acceptIcon 								'Save')) ]
		ifFalse: [ #((110 				'Accept' 					nil 	editor 			nil 						acceptIcon 								'Save')) ].
	^#(
		#(0 		'Text Editor' 					true)
		#(10		'Help... (h)' 							$h 	editor 			openHelp 						helpIcon 								'Open Help')
		#(20 		'-----')
		#(30		'Find...(f)' 							$f 	editor 			find 						findIcon 								'Find')
		#(40		'Find Again (g)' 							$g 	editor 			findAgain 						systemIcon 								'Find again')
		#(50		'Use Selection for Find (j)' 							$j 	editor 			setSearchString 						saveAsIcon 								'Set selection as search string for find again')
		#(60 		'-----')
		#(70		'Undo - multiple (z)' 							$z 	editor 			undo 						undoIcon 								'Undo (multiple levels)')
		#(80		'Redo - multiple (Z)' 							$Z 	editor 			redo 						redoIcon 								'Redo (multiple levels)')
		#(90		'Undo / Redo history' 							nil 	editor 			offerUndoHistory 						changesIcon)
		#(100 		'-----')
	),
	acceptEntry,
	#(
		#(120		'Copy (c)' 							$c 	editor 			copySelection 						copyIcon 								'Copy selection to the Clipboard')
		#(130		'Cut (x)' 							$x 	editor 			cut 						cutIcon 								'Cut selection and store it in the Clipboard')
		#(140		'Paste (v)' 							$v 	editor 			paste 						pasteIcon 								'Paste Clipboard contents')
		#(150		'Paste without Format' 							nil 	editor 			pasteString 						pasteIcon)
		#(160		'Paste...' 							nil 	editor 			pasteRecent 						worldIcon)
		#(180 		'-----')
		#(190		'Toggle WordWrap' 							nil 	editor 			wrapOnOff 						genericTextIcon)
		#(200		'Clear Font' 							nil 	editor 			clearFont 						newIcon)
		#(210		'Set Font... (k)' 							$k 	editor 			offerFontMenu 						preferencesDesktopFontIcon 								'Set font')
		#(220		'Set Font Size' 							nil 	editor 			offerFontSizeMenu 						preferencesDesktopFontIcon)
		#(230		'Set Style' 							nil 	editor 			offerFontStyleMenu 						preferencesDesktopFontIcon)
		#(240		'Set Text Color (8)' 							$8 	editor 			offerColorMenu 						graphicsIcon)
		#(250		'Set Alignment... (u)' 							$u 	editor 			chooseAlignment 						formatJustifyLeftIcon 								'Toggle alignment')

		"Keyboard shortcuts only"
		#(500		nil 							$0 	editor 			changeEmphasisNormal 									nil 			'Normal')
		#(501		nil 							$1 	editor 			changeEmphasisBold 									nil 			'Bold')
		#(502		nil 							$2 	editor 			changeEmphasisItalic 									nil 			'Italic')
		#(503		nil 							$3 	editor 			changeEmphasisUnderlined 									nil 			'Underlined')
		#(504		nil 							$4 	editor 			changeEmphasisStruckThrough 									nil 			'Struck Through')
		#(505		nil 							$5 	editor 			changeEmphasisSuperscript 									nil 			'Superscript')
		#(506		nil 							$6 	editor 			changeEmphasisSubscript 									nil 			'Subscript')
		#(601 		nil 							$a 	editor 			selectAll 									nil 			'Select all')
		#(602 		nil 							$C 	editor 			compareToClipboard 									nil 			'Compare argument to clipboard')
		#(603 		nil 							$U 	editor 			changeLineEndsToLf 									nil 			'Convert line endings to LF characters (Cuis convention) in selection')
		#(604 		nil 							$( 	editor 			encloseWithParenthesis 									nil 			'Enclose within ( and ), or remove enclosing ( and )')
		#(605 		nil 							$[ 	editor 			encloseWithSquareBrackets 									nil 			'Enclose within [ and ], or remove enclosing [ and ]')
		#(606 		nil 							${ 	editor 			encloseWithCurlyBrackets 									nil 			'Enclose within { and }, or remove enclosing { and }')
		#(607 		nil 							$' 	editor 			encloseWithSingleQuotes 									nil 			'Enclose within single quotes, or remove enclosing single quotes')
		#(608 		nil 							$" 	editor 			encloseWithDoubleQuotes 									nil 			'Enclose within double quotes, or remove enclosing double quotes')
		#(609 		nil 							$` 	editor 			encloseWithBackTicks 									nil 			'Enclose within backticks, or remove enclosing backticks')
		"#(610 		nil 							$< 	editor 			encloseWithAngleBrackets 									nil 			'Enclose within < and >, or remove enclosing ( and )')"
		#(611 		nil 							$H 	editor 			cursorTopHome 									nil 			'Move cursor to start of text')
	).
! !


!SmalltalkEditor methodsFor: 'tdd' stamp: 'jmv 6/10/2024 19:22:14'!
acceptAndTest
	
	self acceptThenTestMethodAndSuite: [ :aMethod | self testSuiteOf: aMethod methodClass ].! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'jmv 6/10/2024 19:22:36'!
acceptAndTestAll
	
	self acceptThenTestMethodAndSuite: [ :aMethod | self testSuiteForCategoryOf: aMethod methodClass ].! !

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/11/2024 09:31:44'!
defaultMenuSpec
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	| acceptEntry |
	acceptEntry := self hasUnacceptedEdits
		ifTrue: [ #((240				'Accept (s)' 					$s 	editor 			acceptContents 						acceptIcon 							'Save')) ]
		ifFalse: [ #((240 				'Accept' 					nil 	editor 			nil 						acceptIcon 							'Save')) ].
	^#(
		#(0 		'Smalltalk Editor' 					true)
		#(10		'Help... (h)' 							$h 	editor 			openHelp 						helpIcon 							'Open Help')
		#(20 		'-----')
		#(30		'Do it (d)' 							$d 	editor 			doIt 						doItIcon 							'Do it (selection is a valid expression)')
		#(40		'Print it (p)' 							$p 	editor 			printIt 						printIcon 							'Print it (selection is a valid expression)')
		#(50		'Inspect it (i)' 							$i 	editor 			inspectIt 						inspectIcon 							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(60		'Explore it (I)' 							$I 	editor 			exploreIt 						exploreIcon
					'Inspect via Object Explorer')
		#(70		'Debug it (D)' 							$D 	editor 			debugIt 						debugIcon
					'Debug it')
		#(80		'Profile it' 							nil 	editor 			profileIt 						clockIcon)
		#(90 		'-----')
		#(100		'Edit' 							nil 	nil 			1000 						textEditorIcon)
		#(110 		'-----')
		#(120		'Explain' 							nil 	editor 			explain 						helpIcon)
		#(130		'Browse it (b)' 							$b 	editor 			browseIt 						editFindReplaceIcon 							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(140		'Senders of it (n)' 							$n 	editor 			sendersOfIt 						mailForwardIcon 							'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(150		'Implementors of it (m)' 							$m 	editor 			implementorsOfIt 						developmentIcon 							'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(160		'References to it (N)' 							$N 	editor 			referencesToIt 						addressBookIcon 							'References to it (selection is a class name, or cursor is over a class-list or message-list)')
		#(170 		'-----')
		#(180		'Refactorings' 							nil 	nil 			2000 						listAddIcon)
		#(190 		'-----')
		#(200		'Method Strings with it (E)' 							$E	editor 			methodStringsContainingIt 								genericTextIcon 					'Method strings containing it')
		#(210		'Method Source with it (e)' 							$e 	editor 			methodSourceContainingIt 								scriptIcon 					'Method source containing it')
		#(220		'Class Comments with it' 							nil 	editor 			classCommentsContainingIt 								chatIcon)
		#(230 		'-----')
	),
	acceptEntry,
	#(
		#(250		'Accept & Run Test in Class (t)' 									$t 	editor 			acceptAndTest 						weatherFewCloudsIcon 							'Saves method, runs it as test and then all tests in class. Opens debugger if error.')
		#(260		'Accept & Run Test in Category (y)' 									$y 	editor 			acceptAndTestAll 						weatherFewCloudsIcon 							'Saves method, runs it as test and then all tests in class category. Opens debugger if error.')
		#(270		'Accept & Debug Test (r)' 									$r 	editor 			acceptAndDebugTest 						weatherFewCloudsIcon 							'Saves method and debugs it')

		" 'Edit' SubMenu"
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
		#(1130		'Paste without Format' 							nil 	editor 			pasteString 						pasteIcon)
		#(1140		'Paste...' 							nil 	editor 			pasteRecent 						worldIcon)
		#(1150 		'-----')
		#(1160		'Toggle WordWrap' 							nil 	editor 			wrapOnOff 						genericTextIcon)
		#(1170		'Clear Font' 							nil 	editor 			clearFont 						newIcon)
		#(1180		'Set Font... (k)' 							$k 	editor 			offerFontMenu 						preferencesDesktopFontIcon 									'Set font')
		#(1190		'Set Font Size' 							nil 	editor 			offerFontSizeMenu 						preferencesDesktopFontIcon)

		"Keyboard shortcuts only"
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

		" 'Refactorings' SubMenu"
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
	).
! !

!methodRemoval: TextEditor #shouldHandleUsingCmdShortcuts: stamp: 'jmv 6/11/2024 09:31:58'!
TextEditor removeSelector: #shouldHandleUsingCmdShortcuts:!
