'From Cuis 5.0 of 7 November 2016 [latest update: #3016] on 30 December 2016 at 11:00:38 am'!

!Editor class methodsFor: 'class initialization' stamp: 'jmv 12/30/2016 11:00:14'!
initialize
	"
	Editor initialize
	"
	self withAllSubclassesDo: [ :c |
		c basicInitialize ]! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 12/30/2016 10:34:01'!
enclose: aKeyboardEvent
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| left right startIndex stopIndex oldSelection which |
	startIndex _ self startIndex.
	stopIndex _ self stopIndex.
	oldSelection _ self selection.
	which _ '([<{"''`' indexOf: aKeyboardEvent keyCharacter ifAbsent: [ ^true ].
	left _ '([<{"''`' at: which.
	right _ ')]>}"''`' at: which.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) = left and: [(model actualContents at: stopIndex) = right]])
		ifTrue: [
			"already enclosed; strip off brackets"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection]
		ifFalse: [
			"not enclosed; enclose by matching brackets"
			self replaceSelectionWith:
				(Text string: (String with: left) attributes: emphasisHere),
				oldSelection,
				(Text string: (String with: right) attributes: emphasisHere).
			self selectFrom: startIndex+1 to: stopIndex].
	^true! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 12/30/2016 10:33:45'!
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
		#(		$< #enclose:					'Enclose within < and >, or remove enclosing < and >')

		#(		$a 	#selectAll:					'Select all')
		#(		$f 	#find:						'Find')
		#(		$g 	#findAgain:				'Find again')
		#(		$h 	#help:						'Open this help')
		#(		$j 	#setSearchString:		'Set selection as search string for find again')

		#(		$z 	#undo:						'Undo (multiple levels)')
		#(		$x 	#cut:							'Cut selection and store it in the Clipboard')
		#(		$c 	#copySelection:			'Copy selection to the Clipboard')
		#(		$v 	#paste:						'Paste Clipboard contents')

		#(		$R	#indent:						'Indent (move selection one tab-stap right)')
		#(		$Y	#makeUppercase:		'Force selection to uppercase')
		#(		$U	#changeLineEndsToLf:	'Convert line endings to LF characters (Cuis convention) in selection')

		#(		$H	#cursorTopHome:		'Move cursor to start of text')
		#(		$L	#outdent:					'Outdent (move selection one tab-stop left)')

		#(		$Z	#redo:						'Redo (multiple levels)')
		#(		$X	#makeLowercase:		'Force selection to lowercase')
		#(		$C	#compareToClipboard:	'Compare argument to clipboard')

		#(		$ 	#selectWord:				'Select the current word as with double clicking')
	)! !


!SmalltalkEditor methodsFor: 'new selection' stamp: 'jmv 12/30/2016 10:36:10'!
selectWord
	"Select delimited text or word--the result of double-clicking."

	| leftDelimiters rightDelimiters |
	"Warning. Once me (jmv) added Character crCharacter to the delimiters, to make double-click at and of line select whole line.
	This had the bad effect that if a class name is the last word of a line, double-click would correctly select it, but after that,
	doing ctrl-b to browse it would select the whole line..."
	leftDelimiters _ '([{<|''"`'.
	rightDelimiters _ ')]}>|''"`'.
	^self selectWordLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters! !

Editor initialize!
