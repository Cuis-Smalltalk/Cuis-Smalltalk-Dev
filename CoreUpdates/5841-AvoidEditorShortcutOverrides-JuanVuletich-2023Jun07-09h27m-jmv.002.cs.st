'From Cuis 6.0 [latest update: #5840] on 7 June 2023 at 9:49:04 am'!

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/7/2023 09:31:45'!
shiftShortcutsSpec
	"Only for help. See senders."

	^#(
		#(		'Enter' 	#globalFindClass: 'Global Find class name or fragment')
	)! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/7/2023 09:33:31'!
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

!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/7/2023 09:38:04'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$0 	changeEmphasis:			'Normal')
		#(		$1 	changeEmphasis:			'Bold')
		#(		$2 	changeEmphasis:			'Italic')
		#(		$3 	changeEmphasis:			'Underline')
		#(		$4 	changeEmphasis:			'Strikeout')
		#(		$5 	changeEmphasis:			'Superscript')
		#(		$6 	changeEmphasis:			'Subscript')
		#(		$8	#offerColorMenu:		'Change color')

		#(		$u	#align:				'Toggle alignment')

		#(		$k	#offerFontMenu:			'Set font')
	)! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/7/2023 09:37:29'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$0 	changeEmphasis:							'Normal')
		#(		$1 	changeEmphasis:							'Bold')
		#(		$2 	changeEmphasis:							'Italic')
		#(		$3 	changeEmphasis:							'Underline')
		#(		$4 	changeEmphasis:							'Strikeout')
		#(		$5 	changeEmphasis:							'Superscript')
		#(		$6 	changeEmphasis:							'Subscript')
		#(		$8	#offerColorMenu:							'Change color')
	
		#(		$e	#methodSourceContainingIt:							'Method source containing it')
		#(		$r	#acceptAndDebugTest:							'Saves method and debugs it')
		#(		$t	#acceptAndTest:							'Saves method, runs it as test and then all tests in class. Opens debugger if error')
		#(		$y	#acceptAndTestAll:							'Saves method, runs it as test and then all tests in class category. Opens debugger if error')
		#(		$u	#align:							'Toggle alignment')
		#(		$i	#inspectIt:							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(		$p	#printIt:							'Print it (selection is a valid expression)')

		#(		$d	#doIt:							'Do it (selection is a valid expression)')
		#(		$k	#offerFontMenu:							'Set font')
		#(		$l	#cancel:							'Cancel all edits')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:							'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:							'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$Q	#argPrev:							'Previous argument')
		#(		$W	#argNext:							'Next argument')
		#(		$E	#methodStringsContainingIt:							'Method strings containing it')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$D	#debugIt:							'Debug it')

		#(		$N	#referencesToIt:							'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !


!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'jmv 6/7/2023 09:48:00'!
smalltalkEditorCmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(	$R	#contextualRename:								'Renames what is under cursor')
		#(	$T	#temporaryToInstanceVariable:								'Extracts temporary variable to instance variable')
		#(	$O	#inlineTemporaryVariable:								'Inlines the selected temporary variable into each usage')
		#(	$P	#contextualInlineMethod:								'Inlines the selected message send into desired colaborations')

		#(	$A	#contextualAddParameter:								'Adds parameter to message that is under cursor')
		#(	$S	#contextualRemoveParameter:								'Removes parameter to message that is under cursor')
		#(	$J	#extractToTemporary:								'Extracts the selected code into a temporary variable')
		#(	$K	#extractMethod:								'Extracts the selected code into a separate method')		
		#(	$L 	#contextualExtractAsParameter: 								'Extracts the selected code as parameter')
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Editor initialize
!

