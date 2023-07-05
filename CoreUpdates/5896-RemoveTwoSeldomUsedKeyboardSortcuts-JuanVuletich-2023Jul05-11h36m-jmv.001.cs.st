'From Cuis 6.0 [latest update: #5895] on 5 July 2023 at 11:37:15 am'!

!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 7/5/2023 11:36:24'!
basicCmdShortcutsSpec
	"
	Editor initialize
	"

	"arranged in QWERTY keyboard order"
	self flag: #definesKeyboardShortcuts.
	^#(
		#(		$( 	#enclose:					'Enclose within ( and ), or remove enclosing ( and )')
		#(		$[ 	#enclose:					'Enclose within [ and ], or remove enclosing [ and ]')
		#(		${ 	#enclose:					'Enclose within { and }, or remove enclosing { and }')
		#(		$' 	#enclose:					'Enclose within single quotes, or remove enclosing single quotes')
		#(		$" 	#enclose:					'Enclose within double quotes, or remove enclosing double quotes')
		#(		$` 	#enclose:					'Enclose within backticks, or remove enclosing backticks')
		#(		$< 	#enclose:					'Enclose within < and >, or remove enclosing < and >')

		#(		$a 	#selectAll:					'Select all')
		#(		$s 	#save:					'Save')
		#(		$f 	#find:					'Find')
		#(		$g 	#findAgain:					'Find again')
		#(		$h 	#help:					'Open this help')
		#(		$j 	#setSearchString:					'Set selection as search string for find again')

		#(		$z 	#undo:					'Undo (multiple levels)')
		#(		$x 	#cut:					'Cut selection and store it in the Clipboard')
		#(		$c 	#copySelection:					'Copy selection to the Clipboard')
		#(		$v 	#paste:					'Paste Clipboard contents')

		#(		$U	#changeLineEndsToLf:		'Convert line endings to LF characters (Cuis convention) in selection')

		#(		$H	#cursorTopHome:					'Move cursor to start of text')

		#(		$Z	#redo:					'Redo (multiple levels)')
		#(		$C	#compareToClipboard:		'Compare argument to clipboard')
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Editor initialize!

