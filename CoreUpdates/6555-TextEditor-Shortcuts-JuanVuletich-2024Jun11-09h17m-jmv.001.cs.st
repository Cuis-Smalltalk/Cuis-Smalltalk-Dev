'From Cuis7.1 [latest update: #6461] on 11 June 2024 at 9:20:33 am'!

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 16:16:39'!
changeEmphasisBold
	^self changeEmphasisTo: 1! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 16:16:58'!
changeEmphasisItalic
	^self changeEmphasisTo: 2! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 16:56:05'!
changeEmphasisNormal
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"This is a user command, and generates undo"

	| attributesToRemove |

	"Remove all TextEmphasis"
	attributesToRemove := emphasisHere.
	attributesToRemove ifNotNil: [ self unapplyAttributes: attributesToRemove ].
	^ true! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 16:17:21'!
changeEmphasisStruckThrough
	^self changeEmphasisTo: 4! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 16:17:40'!
changeEmphasisSubscript
	^self changeEmphasisTo: 6! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 16:17:29'!
changeEmphasisSuperscript
	^self changeEmphasisTo: 5! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 16:57:01'!
changeEmphasisTo: emphasisCode
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"This is a user command, and generates undo"

	| attributeToAdd attributesToRemove oldAttributes |
	oldAttributes := emphasisHere.

	"Toggle the indicated TextEmphasis"
	(emphasisCode between: 1 and: 6) ifTrue: [
		attributeToAdd := TextEmphasis perform: (
			#(#bold #italic #underlined #struckThrough #superscript #subscript)
				at: emphasisCode).
		oldAttributes do: [ :oldAtt |
			oldAtt = attributeToAdd ifTrue: [
				attributeToAdd := nil.
				attributesToRemove := {oldAtt} ]]].

	attributeToAdd ifNotNil: [ self applyAttribute: attributeToAdd ].
	attributesToRemove ifNotNil: [ self unapplyAttributes: attributesToRemove ].
	^ true! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 16:17:11'!
changeEmphasisUnderlined
	^self changeEmphasisTo: 3! !


!SmalltalkEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2024 16:44:41'!
cancel
	"Cancel all edits done. Revert to saved version."
	model refetch.! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 6/10/2024 16:45:46'!
argNext
	"Invoked by cmd-A.
	 Search forward from the end of the selection for a colon followed by
		a space.  Place the text cursor after the space.  If none are found, place the
		text cursor at the end of the text.."

	| start t |
	t := model actualContents.
	start := t findString: ': ' startingAt: self startIndex.
	start = 0 ifTrue: [ start := t size + 1].
	self deselectAndPlaceCursorAt: start + 2.
	^true! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 6/10/2024 16:45:42'!
argPrev
	"Invoked by cmd-Q.
	 Search backwards from the start of the selection for a colon followed by
		a space.  Place the text cursor after the space.  If none are found, place the
		text cursor at the start of the text.."

	| t i |
	t := model actualContents.
	i := self stopIndex.
	i > 1 ifTrue: [
		i := i -2.
		[i > 0 and: [ (t at: i) ~= $  or: [(t at: i-1) ~= $: ]]] whileTrue: [
			i := i -1 ].
		self deselectAndPlaceCursorAt: i + 1.
	].
	^true! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 6/10/2024 16:50:31'!
displayIfFalse
	"Replace the current text selection with the text 'ifFalse:'--initiated by 
	cmd-F."

	self addString: 'ifFalse:'.! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 6/10/2024 16:50:23'!
displayIfTrue
	"Replace the current text selection with the text 'ifTrue:'--initiated by 
	cmd-T."

	self addString: 'ifTrue:'.! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 6/10/2024 15:56:50'!
keyboardShortcut: aKeyboardEvent
	"Handle a keyboard shortcut.
	See coment at Morph >> #getMenu
	"
	"ctrl and option (if on Mac) are used to enter #normalCharacter:"
	aKeyboardEvent commandAltKeyPressed ifFalse: [ ^false ].
	(aKeyboardEvent keyValue between: 32 and: 126) ifFalse: [ ^false ].
	^super keyboardShortcut: aKeyboardEvent! !


!TextEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/10/2024 16:48:55'!
defaultMenuSpec
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	| acceptEntry |
	acceptEntry := self hasUnacceptedEdits
		ifTrue: [ #((110				'Accept (s)' 					$s 	editor 			acceptContents 						acceptIcon)) ]
		ifFalse: [ #((110 				'Accept (s)' 					nil 	editor 			nil 						acceptIcon)) ].
	^#(
		#(0 		'Text Editor' 					true)
		#(10		'Help... (h)' 							$h 	editor 			openHelp 						helpIcon)
		#(20 		'-----')
		#(30		'Find...(f)' 							$f 	editor 			find 						findIcon)
		#(40		'Find Again (g)' 							$g 	editor 			findAgain 						systemIcon)
		#(50		'Use Selection for Find (j)' 							$j 	editor 			setSearchString 						saveAsIcon)
		#(60 		'-----')
		#(70		'Undo - multiple (z)' 							$z 	editor 			undo 						undoIcon)
		#(80		'Redo - multiple (Z)' 							$Z 	editor 			redo 						redoIcon)
		#(90		'Undo / Redo history' 							nil 	editor 			offerUndoHistory 						changesIcon)
		#(100 		'-----')
	),
	acceptEntry,
	#(
		#(120		'Copy (c)' 							$c 	editor 			copySelection 						copyIcon)
		#(130		'Cut (x)' 							$x 	editor 			cut 						cutIcon)
		#(140		'Paste (v)' 							$v 	editor 			paste 						pasteIcon)
		#(150		'Paste without Format' 							nil 	editor 			pasteString 						pasteIcon)
		#(160		'Paste...' 							nil 	editor 			pasteRecent 						worldIcon)
		#(180 		'-----')
		#(190		'Toggle WordWrap' 							nil 	editor 			wrapOnOff 						genericTextIcon)
		#(200		'Clear Font' 							nil 	editor 			clearFont 						newIcon)
		#(210		'Set Font... (k)' 							$k 	editor 			offerFontMenu 						preferencesDesktopFontIcon)
		#(220		'Set Font Size' 							nil 	editor 			offerFontSizeMenu 						preferencesDesktopFontIcon)
		#(230		'Set Style' 							nil 	editor 			offerFontStyleMenu 						preferencesDesktopFontIcon)
		#(240		'Set Text Color (8)' 							$8 	editor 			offerColorMenu 						graphicsIcon)
		#(250		'Set Alignment... (u)' 							$u 	editor 			chooseAlignment 						formatJustifyLeftIcon)

		"Keyboard shortcuts only"
		#(500		nil 							$0 	editor 			changeEmphasisNormal)
		#(501		nil 							$1 	editor 			changeEmphasisBold)
		#(502		nil 							$2 	editor 			changeEmphasisItalic)
		#(503		nil 							$3 	editor 			changeEmphasisUnderlined)
		#(504		nil 							$4 	editor 			changeEmphasisStruckThrough)
		#(505		nil 							$5 	editor 			changeEmphasisSuperscript)
		#(506		nil 							$6 	editor 			changeEmphasisSubscript)
	).! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/10/2024 16:52:56'!
defaultMenuSpec
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	| acceptEntry |
	acceptEntry := self hasUnacceptedEdits
		ifTrue: [ #((240				'Accept (s)' 					$s 	editor 			acceptContents 						acceptIcon)) ]
		ifFalse: [ #((240 				'Accept (s)' 					nil 	editor 			nil 						acceptIcon)) ].
	^#(
		#(0 		'Smalltalk Editor' 					true)
		#(10		'Help... (h)' 							$h 	editor 			openHelp 						helpIcon)
		#(20 		'-----')
		#(30		'Do it (d)' 							$d 	editor 			doIt 						doItIcon
					'Do it (selection is a valid expression)')
		#(40		'Print it (p)' 							$p 	editor 			printIt 						printIcon
					'Print it (selection is a valid expression)')
		#(50		'Inspect it (i)' 							$i 	editor 			inspectIt 						inspectIcon
					'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(60		'Explore it (I)' 							$I 	editor 			exploreIt 						exploreIcon
					'Inspect via Object Explorer')
		#(70		'Debug it (D)' 							$D 	editor 			debugIt 						debugIcon
					'Debug it')
		#(80		'Profile it' 							nil 	editor 			profileIt 						clockIcon)
		#(90 		'-----')
		#(100		'Edit' 							nil 	nil 			1000 						textEditorIcon)
		#(110 		'-----')
		#(120		'Explain' 							nil 	editor 			explain 						helpIcon)
		#(130		'Browse it (b)' 							$b 	editor 			browseIt 						editFindReplaceIcon
					'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(140		'Senders of it (n)' 							$n 	editor 			sendersOfIt 						mailForwardIcon
					'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(150		'Implementors of it (m)' 							$m 	editor 			implementorsOfIt 						developmentIcon
					'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(160		'References to it (N)' 							$N 	editor 			referencesToIt 						addressBookIcon
					'References to it (selection is a class name, or cursor is over a class-list or message-list)')
		#(170 		'-----')
		#(180		'Refactorings' 							nil 	nil 			2000 						listAddIcon)
		#(190 		'-----')
		#(200		'Method Strings with it (E)' 							$E	editor 			methodStringsContainingIt 								genericTextIcon
					'Method strings containing it')
		#(210		'Method Source with it (e)' 							$e 	editor 			methodSourceContainingIt 								scriptIcon
					'Method source containing it')
		#(220		'Class Comments with it' 							nil 	editor 			classCommentsContainingIt 								chatIcon)
		#(230 		'-----')
	),
	acceptEntry,
	#(
		#(250		'Accept & Run Test in Class (t)' 									$t 	editor 			acceptAndTest 						weatherFewCloudsIcon
					'Saves method, runs it as test and then all tests in class. Opens debugger if error.')
		#(260		'Accept & Run Test in Category (y)' 									$y 	editor 			acceptAndTestAll 						weatherFewCloudsIcon
					'Saves method, runs it as test and then all tests in class category. Opens debugger if error.')
		#(270		'Accept & Debug Test (r)' 									$r 	editor 			acceptAndDebugTest 						weatherFewCloudsIcon
					'Saves method and debugs it')

		"Keyboard shortcuts only"
		#(500		nil 							$0 	editor 			changeEmphasisNormal)
		#(501		nil 							$1 	editor 			changeEmphasisBold)
		#(502		nil 							$2 	editor 			changeEmphasisItalic)
		#(503		nil 							$3 	editor 			changeEmphasisUnderlined)
		#(504		nil 							$4 	editor 			changeEmphasisStruckThrough)
		#(505		nil 							$5 	editor 			changeEmphasisSuperscript)
		#(506		nil 							$6 	editor 			changeEmphasisSubscript)
		#(507		nil 							$l 	editor 			cancel)
		#(508		nil 							$Q 	editor 			argPrev)
		#(509		nil 							$W 	editor 			argNext)
		#(510		nil 							$T 	editor 			displayIfTrue)
		#(511		nil 							$F 	editor 			displayIfFalse)


		" 'Edit' SubMenu"
		#(1010 		'-----')
		#(1020		'Find...(f)' 							$f 	editor 			find 						findIcon)
		#(1030		'Find Again (g)' 							$g 	editor 			findAgain 						systemIcon)
		#(1040		'Use Selection for Find (j)' 							$j 	editor 			setSearchString 						saveAsIcon)
		#(1050 		'-----')
		#(1060		'Undo - multiple (z)' 							$z 	editor 			undo 						undoIcon)
		#(1070		'Redo - multiple (Z)' 							$Z 	editor 			redo 						redoIcon)
		#(1080		'Undo / Redo history' 							nil 	editor 			offerUndoHistory 						changesIcon)
		#(1090 		'-----')
		#(1100		'Copy (c)' 							$c 	editor 			copySelection 						copyIcon)
		#(1110		'Cut (x)' 							$x 	editor 			cut 						cutIcon)
		#(1120		'Paste (v)' 							$v 	editor 			paste 						pasteIcon)
		#(1130		'Paste without Format' 							nil 	editor 			pasteString 						pasteIcon)
		#(1140		'Paste...' 							nil 	editor 			pasteRecent 						worldIcon)
		#(1150 		'-----')
		#(1160		'Toggle WordWrap' 							nil 	editor 			wrapOnOff 						genericTextIcon)
		#(1170		'Clear Font' 							nil 	editor 			clearFont 						newIcon)
		#(1180		'Set Font... (k)' 							$k 	editor 			offerFontMenu 						preferencesDesktopFontIcon)
		#(1190		'Set Font Size' 							nil 	editor 			offerFontSizeMenu 						preferencesDesktopFontIcon)

		" 'Refactorings' SubMenu"
		#(2010 		'-----')
		#(2020		'Rename... (R)' 									$R 	editor 			contextualRename 										saveAsIcon)
		#(2030		'Extract as Parameter... (V)' 									$V 	editor 			contextualExtractAsParameter 										listAddIcon)
		#(2040		'Add Parameter... (A)' 									$A 	editor 			contextualAddParameter 										listAddIcon)
		#(2050		'Remove Parameter... (S)' 									$S 	editor 			contextualRemoveParameter 										listRemoveIcon)
		#(2060 		'-----')
		#(2070		'Extract Temporary... (J)' 									$J 	editor 			extractToTemporary 										saveAsIcon)
		#(2080		'Extract Method... (K)' 									$K 	editor 			extractMethod 										saveAsIcon)
		#(2090 		'-----')
		#(2100		'Inline Temporary... (O)' 									$O 	editor 			inlineTemporaryVariable 										saveAsIcon)
		#(2110		'Inline Method... (F)' 									$F 	editor 			contextualInlineMethod 										saveAsIcon)
		#(2120 		'-----')
		#(2130		'Temporary to Instance Variable (G)' 									$G 	editor 			temporaryToInstanceVariable 										saveAsIcon)
		#(2140		'Push Up Instance Variable' 									nil 	editor 			contextualPushUpInClassDefinition 										goTopIcon)
		#(2150		'Push Down Instance Variable' 									nil 	editor 			contextualPushDownInClassDefinition 										goBottomIcon)
	).
! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 6/10/2024 16:31:01'!
keyStroke: aKeyboardEvent

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ 
		self selectionChanged.  "Note new selection"
		self updateFromTextComposition.
		^self ].

	autoCompleter 
		ifNil: [ self processKeystrokeEvent: aKeyboardEvent ]
		ifNotNil: [
			autoCompleter
				autoCompletionAround: [ self processKeystrokeEvent: aKeyboardEvent ]
				keyStroke: aKeyboardEvent ].

	aKeyboardEvent wasHandled: true.
! !

