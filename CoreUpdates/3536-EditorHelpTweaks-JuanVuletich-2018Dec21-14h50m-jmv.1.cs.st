'From Cuis 5.0 of 7 November 2016 [latest update: #3537] on 21 December 2018 at 2:50:39 pm'!

!Editor class methodsFor: 'help' stamp: 'jmv 12/21/2018 10:14:26'!
help
	"
	TextEditor help edit
	SmalltalkEditor help edit
	"
	| allSpecs |
	allSpecs _ self cmdShortcutsSpec, self basicCmdShortcutsSpec.
	^String streamContents: [ :strm |
		allSpecs do: [ :triplet | | c |
			c _ triplet first = Character space
				ifFalse: [ triplet first asString, '   	' ]
				ifTrue: [ 'Space'].
			strm nextPutAll: ('Cmd-', c, String tab, String tab, triplet third).
			strm newLine ]]! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 12/21/2018 14:43:04'!
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

		#(		$R		#indent:						'Indent (same as [Tab], move selection one tab-stap right)')
		#(		$Y		#makeUppercase:			'Force selection to uppercase')
		#(		$U		#changeLineEndsToLf:		'Convert line endings to LF characters (Cuis convention) in selection')

		#(		$H		#cursorTopHome:			'Move cursor to start of text')
		#(		$L		#outdent:					'Outdent (same as [Shift][Tab], move selection one tab-stop left)')

		#(		$Z		#redo:						'Redo (multiple levels)')
		#(		$X		#makeLowercase:			'Force selection to lowercase')
		#(		$C		#compareToClipboard:		'Compare argument to clipboard')

		#(		$ 		#selectWord:				'Select the current word as with double clicking')
	)! !

