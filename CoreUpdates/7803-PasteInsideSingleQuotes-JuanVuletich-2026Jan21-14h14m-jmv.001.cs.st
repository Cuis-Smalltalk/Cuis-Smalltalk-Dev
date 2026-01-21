'From Cuis7.7 [latest update: #7802] on 21 January 2026 at 2:14:44 pm'!

!Editor methodsFor: 'menu messages' stamp: 'jmv 21/Jan/2026 14:09:49'!
pasteSingleQuoted
	"Paste the text's string from the shared buffer over the current selection and 
	redisplay if necessary.
	Pasting a string means using destination current attributes if appropriate.
	Paste inside single quotes, doubling inbedded quotes.
	"

	self replaceSelectionWith: self clipboardStringOrText asPlainString storeString.! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 21/Jan/2026 14:12:56'!
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
		#(1131	'Paste inside single quotes'	nil 	editor 	pasteSingleQuoted 	pasteIcon 			'Paste Clipboard contents')
		#(1140	'Paste...' 					nil 	editor 	pasteRecent 			worldIcon)
		#(1150 	'-----')
		#(1160	'Toggle WordWrap' 			nil 	editor 	wrapOnOff 				genericTextIcon)
		#(1170	'Clear Font' 					nil 	editor 	clearFont 				newIcon)
		#(1180	'Set Font... (k)' 				$k 	editor 	offerFontMenu 			preferencesDesktopFontIcon 	'Set font')
		#(1190	'Set Font Size' 				nil 	editor 	offerFontSizeMenu 	preferencesDesktopFontIcon)
	).! !

