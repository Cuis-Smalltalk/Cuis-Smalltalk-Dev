'From Cuis 5.0 [latest update: #3869] on 12 September 2019 at 12:53:24 pm'!

!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 9/12/2019 11:05:51'!
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


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 9/12/2019 11:10:04'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$r	#acceptAndDebugTest:		'Saves method and debugs it')
		#(		$t	#acceptAndTest:				'Saves method, runs it as test and then all tests in class. Opens debugger if error')
		#(		$y	#acceptAndTestAll:				'Saves method, runs it as test and then all tests in class category. Opens debugger if error')
		#(		$i	#inspectIt:							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(		$p	#printIt:								'Print it (selection is a valid expression)')

		#(		$s	#save:								'Save (i.e. accept)')
		#(		$d	#doIt:								'Do it (selection is a valid expression)')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:						'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:				'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$E	#methodStringsContainingit:'Method strings containing it')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$Q	#argPrev:							'Previous argument')
		#(		$W	#argNext:							'Next argument')
		#(		$D	#debugIt:							'Debug it')

		#(		$N	#referencesToIt:					'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !


!RefactoringMenues class methodsFor: 'editor menus' stamp: 'jmv 9/12/2019 11:19:24'!
smalltalkEditorMenuOptions

	^`{
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		10.
				#label 			-> 		'Rename... (R)'.
				#selector 		-> 		#contextualRename.
				#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		20.
				#label 			-> 		'Add Parameter... (A)'.
				#selector 		-> 		#contextualAddParameter.
				#icon 			-> 		#listAddIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		30.
				#label 			-> 		'Remove Parameter... (S)'.
				#selector 		-> 		#contextualRemoveParameter.
				#icon 			-> 		#listRemoveIcon
		} asDictionary.
		{
				#itemGroup 		-> 		35.
				#itemOrder 		-> 		40.
				#label 			-> 		'Temporary to Instance Variable (O)'.
				#selector 		-> 		#temporaryToInstanceVariable.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
		
	}`! !

!RefactoringMenues class methodsFor: 'shortcuts' stamp: 'jmv 9/12/2019 11:15:55'!
smalltalkEditorCmdShortcutsSpec

	^#(
		#(		$R	#contextualRename:		'Renames what is under cursor')
		#(		$A	#contextualAddParameter:		'Adds parameter to message that is under cursor')
		#(		$S	#contextualRemoveParameter:		'Removes parameter to message that is under cursor')
		#(		$O	#temporaryToInstanceVariable:		'Extracts temporary variable to instance variable'))
! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Editor initialize!

