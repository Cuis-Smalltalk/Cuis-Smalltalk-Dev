'From Cuis 6.0 [latest update: #5785] on 4 May 2023 at 9:39:48 am'!

!Editor class methodsFor: 'class initialization' stamp: 'jmv 5/4/2023 09:38:09'!
putIntoCmdShortcuts: shortcutsSpec

	shortcutsSpec do: [ :ary | | i previous |
		i := ary first numericValue + 1.
		previous := cmdShortcuts at: i.
		previous = #noop:
			ifTrue: [
				cmdShortcuts at: i put: ary second ]
			ifFalse: [ 
				('Editor shortcut: ', ary first printString, ' already taken for: ', previous, 
				'. Override request for: ', ary second, '  ignored') print ]].! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 5/4/2023 09:37:35'!
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
		#(		$s 	#save:					'Save')
		#(		$f 	#find:					'Find')
		#(		$g 	#findAgain:					'Find again')
		#(		$h 	#help:					'Open this help')
		#(		$j 	#setSearchString:					'Set selection as search string for find again')

		#(		$z 	#undo:					'Undo (multiple levels)')
		#(		$x 	#cut:					'Cut selection and store it in the Clipboard')
		#(		$c 	#copySelection:					'Copy selection to the Clipboard')
		#(		$v 	#paste:					'Paste Clipboard contents')

		#(		$Y	#makeUppercase:					'Force selection to uppercase')
		#(		$U	#changeLineEndsToLf:		'Convert line endings to LF characters (Cuis convention) in selection')

		#(		$H	#cursorTopHome:					'Move cursor to start of text')

		#(		$Z	#redo:					'Redo (multiple levels)')
		#(		$X	#makeLowercase:					'Force selection to lowercase')
		#(		$C	#compareToClipboard:		'Compare argument to clipboard')
	)! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 5/4/2023 09:37:29'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$r	#acceptAndDebugTest:							'Saves method and debugs it')
		#(		$t	#acceptAndTest:							'Saves method, runs it as test and then all tests in class. Opens debugger if error')
		#(		$y	#acceptAndTestAll:							'Saves method, runs it as test and then all tests in class category. Opens debugger if error')
		#(		$i	#inspectIt:							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(		$p	#printIt:							'Print it (selection is a valid expression)')

		#(		$d	#doIt:							'Do it (selection is a valid expression)')
		#(		$l	#cancel:							'Cancel all edits')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:							'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:							'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$E	#methodStringsContainingit:							'Method strings containing it')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$Q	#argPrev:							'Previous argument')
		#(		$W	#argNext:							'Next argument')
		#(		$D	#debugIt:							'Debug it')

		#(		$N	#referencesToIt:							'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Editor initialize
!

