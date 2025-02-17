'From Cuis7.1 [latest update: #6837] on 22 November 2024 at 10:31:19 am'!

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 10:28:19'!
editSubmenuSpec
	" 'Edit' SubMenu"

	^#(
		#(1010 	'-----')
		#(1020	'Find...(f)' 					$f 	editor 	find 					findIcon 			'Find')
		#(1030	'Find Again (g)' 				$g 	editor 	findAgain 				systemIcon 		'Find again')
		#(1040	'Use Selection for Find (j)'	 	$j 	editor 	setSearchString 		saveAsIcon 		'Set selection as search string for find again')
		#(1050 	'-----')
		#(1060	'Undo - multiple (z)' 			$z 	editor 	undo 					undoIcon 			'Undo (multiple levels)')
		#(1070	'Redo - multiple (Z)' 			$Z 	editor 	redo 					redoIcon 			'Redo (multiple levels)')
		#(1080	'Undo / Redo history' 			nil 	editor 	offerUndoHistory 		changesIcon)
		#(1090 	'-----')
		#(1100	'Copy (c)' 					$c 	editor 	copySelection 			copyIcon 			'Copy selection to the Clipboard')
		#(1110	'Cut (x)' 						$x 	editor 	cut 						cutIcon 			'Cut selection and store it in the Clipboard')
		#(1120 	'Compare to Clipboard (C)' 	$C 	editor 	compareToClipboard 	changesIcon 		'Compare selection to clipboard')
		#(1130	'Paste (v)' 					$v 	editor 	paste 					pasteIcon 			'Paste Clipboard contents')
		#(1140	'Paste...' 					nil 	editor 	pasteRecent 			worldIcon)
		#(1150 	'-----')
		#(1160	'Toggle WordWrap' 			nil 	editor 	wrapOnOff 				genericTextIcon)
		#(1170	'Clear Font' 					nil 	editor 	clearFont 				newIcon)
		#(1180	'Set Font... (k)' 				$k 	editor 	offerFontMenu 			preferencesDesktopFontIcon 	'Set font')
		#(1190	'Set Font Size' 				nil 	editor 	offerFontSizeMenu 	preferencesDesktopFontIcon)
	).! !

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 10:28:48'!
keyboardShortuctsSubmenuSpec
	"Keyboard shortcuts only"

	^#(
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

