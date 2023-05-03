'From Haver 6.0 [latest update: #5780] on 28 April 2023 at 3:10:32 pm'!

!TextEditor methodsFor: 'editing keys' stamp: 'KLG 4/28/2023 15:03:41'!
save: aKeyboardEvent
	"Submit the current text.  Equivalent to 'accept' 1/18/96 sw"

	self acceptContents.
	^ true! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'KLG 4/28/2023 15:03:59'!
basicCmdShortcutsSpec
	"
	Editor initialize
	"

	"arranged in QWERTY keyboard order"
	^#(
		#(		$( 	#enclose:					'Enclose within ( and ), or remove enclosing ( and )')
		#(		$[ 	#enclose:					'Enclose within [ and ], or remove enclosing [ and ]')
		#(		${ 	#enclose:					'Enclose within { and }, or remove enclosing { and }')
		#(		$' 	#enclose:					'Enclose within single quotes, or remove enclosing single quotes')
		#(		$" 	#enclose:					'Enclose within double quotes, or remove enclosing double quotes')
		#(		$` 	#enclose:					'Enclose within backticks, or remove enclosing backticks')
		#(		$< 	#enclose:					'Enclose within < and >, or remove enclosing < and >')

		#(		$a 	#selectAll:					'Select all')
		#(		$s 	#save:					'Accept')
		#(		$f 	#find:						'Find')
		#(		$g 	#findAgain:					'Find again')
		#(		$h 	#help:						'Open this help')
		#(		$j 	#setSearchString:			'Set selection as search string for find again')

		#(		$z 	#undo:						'Undo (multiple levels)')
		#(		$x 	#cut:							'Cut selection and store it in the Clipboard')
		#(		$c 	#copySelection:			'Copy selection to the Clipboard')
		#(		$v 	#paste:						'Paste Clipboard contents')

		#(		$Y		#makeUppercase:			'Force selection to uppercase')
		#(		$U		#changeLineEndsToLf:		'Convert line endings to LF characters (Cuis convention) in selection')

		#(		$H		#cursorTopHome:			'Move cursor to start of text')

		#(		$Z		#redo:						'Redo (multiple levels)')
		#(		$X		#makeLowercase:			'Force selection to lowercase')
		#(		$C		#compareToClipboard:		'Compare argument to clipboard')
	)! !

!methodRemoval: SmalltalkEditor #save: stamp: 'KLG 4/28/2023 15:03:41'!
SmalltalkEditor removeSelector: #save:!
"Postscript:
Initialize the changed command shortcuts in the TextEditor"

TextEditor initializeCmdShortcuts!

