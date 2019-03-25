'From Cuis 5.0 of 7 November 2016 [latest update: #3685] on 25 March 2019 at 4:08:19 pm'!

!Editor class methodsFor: 'class initialization' stamp: 'HAW 3/25/2019 16:06:05'!
collectCmdShortcutsSpecUsing: anInitializationMessage

	| shortcutsSpec dynamicInitializationMessage |
	
	shortcutsSpec := self perform: anInitializationMessage.

	dynamicInitializationMessage := (self name asString uncapitalized, anInitializationMessage asString capitalized) asSymbol.
	(Smalltalk allClassesImplementing: dynamicInitializationMessage) do: [ :aClass | 
		shortcutsSpec := shortcutsSpec, (aClass soleInstance perform: dynamicInitializationMessage) ].

	^shortcutsSpec

! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 3/25/2019 16:02:43'!
allCmdShortcutsSpec

	^ (self collectCmdShortcutsSpecUsing: #basicCmdShortcutsSpec), (self collectCmdShortcutsSpecUsing: #cmdShortcutsSpec)! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 3/25/2019 15:57:24'!
allShiftShortcutsSpec

	^ self shiftShortcutsSpec! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 3/25/2019 15:57:38'!
shiftShortcutsSpec

	^#(
		#(		'Enter' 	#globalFindClass: 'Global Find class name or fragment')
	)! !

!Editor class methodsFor: 'help' stamp: 'HAW 3/25/2019 15:33:02'!
formatShortcutsUsingModifierKey: aModifierKey andSpecs: aSpecs
	"Format shortcuts specs with a modifier key"
	^ String streamContents: [ :strm |
		aSpecs do: [ :triplet | | c |
			c _ triplet first = Character space
				ifFalse: [ triplet first asString, '   	' ]
				ifTrue: [ 'Space'].
			strm nextPutAll: (aModifierKey, '-', c, String tab, String tab, triplet third).
			strm newLine ]]! !


!Editor class methodsFor: 'class initialization' stamp: 'HAW 3/25/2019 15:59:28'!
basicInitialize
	"
	Editor initialize
	"
	self withAllSubclassesDo: [ :c | c initializeShortcuts; initializeCmdShortcuts ]! !

!Editor class methodsFor: 'class initialization' stamp: 'HAW 3/25/2019 15:59:22'!
initialize
	"
	Editor initialize
	"
	self withAllSubclassesDo: [ :c | c basicInitialize ]! !

!Editor class methodsFor: 'class initialization' stamp: 'HAW 3/25/2019 16:04:41'!
initializeCmdShortcuts
	"Initialize the (unshifted) command-key (or alt-key if not on Mac) shortcut table.
	If you want to add a new shortcut for an specific editor, for example SmalltalkEditor, you should
	define the message #smalltalkEditorCmdShortcutsSpec in a class of your category and it will
	be dynamically send"

	"NOTE: if you don't know what your keyboard generates, use Sensor test"

	"
	Editor initialize
	"

	cmdShortcuts _ Array new: 256 withAll: #noop:.

	self putIntoCmdShortcuts: self allCmdShortcutsSpec
! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 3/25/2019 15:57:29'!
basicCmdShortcutsSpec

	^#()! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 3/25/2019 15:57:18'!
cmdShortcuts
	"Same for all instances.
	A subclass could handle specific keyboard shortcuts for each instance, though."
	
	cmdShortcuts ifNil: [self initializeCmdShortcuts ].
	
	^cmdShortcuts! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 3/25/2019 15:57:33'!
cmdShortcutsSpec

	^#()! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 3/25/2019 15:58:00'!
shortcuts
	"Same for all instances.
	A subclass could handle specific keyboard shortcuts for each instance, though."
	
	shortcuts ifNil: [ self initializeShortcuts ].
	
	^shortcuts! !

!Editor class methodsFor: 'help' stamp: 'HAW 3/25/2019 15:33:02'!
help
	"
	TextEditor help edit
	SmalltalkEditor help edit
	"
	^ (self formatShortcutsUsingModifierKey: 'Shift' andSpecs: self allShiftShortcutsSpec) ,
		(self formatShortcutsUsingModifierKey: 'Cmd' andSpecs: self allCmdShortcutsSpec).
! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 3/25/2019 16:00:48'!
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
		#(		$L		#outdent:					'Outdent (move selection one tab-stop left)')

		#(		$Z		#redo:						'Redo (multiple levels)')
		#(		$X		#makeLowercase:			'Force selection to lowercase')
		#(		$C		#compareToClipboard:		'Compare argument to clipboard')

		#(		$ 		#selectWord:				'Select the current word as with double clicking')
	)! !

!methodRemoval: Editor class #collectCmdShortcutsSpecsUsing:!
Editor class removeSelector: #collectCmdShortcutsSpecsUsing:!
!methodRemoval: Editor class #collectCmdShortcutsUsing:!
Editor class removeSelector: #collectCmdShortcutsUsing:!
!methodRemoval: Editor class #formatShortcutsUsingModifierKey:AndSpecs:!
Editor class removeSelector: #formatShortcutsUsingModifierKey:AndSpecs:!
!methodRemoval: Editor class #initializeCmdShortcutsUsing:!
Editor class removeSelector: #initializeCmdShortcutsUsing:!
Editor initialize!

!Editor class reorganize!
('class initialization' basicInitialize collectCmdShortcutsSpecUsing: initialize initializeCmdShortcuts initializeShortcuts putIntoCmdShortcuts:)
('keyboard shortcut tables' allCmdShortcutsSpec allShiftShortcutsSpec basicCmdShortcutsSpec cmdShortcuts cmdShortcutsSpec shiftShortcutsSpec shortcuts)
('help' formatShortcutsUsingModifierKey:andSpecs: help)
('cached state access' releaseClassCachedState)
!

