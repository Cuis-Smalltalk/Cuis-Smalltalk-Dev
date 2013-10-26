'From Cuis 4.0 of 21 April 2012 [latest update: #1297] on 10 June 2012 at 11:30:09 am'!
!classDefinition: #Editor category: #'System-Text'!
Object subclass: #Editor
	instanceVariableNames: 'morph '
	classVariableNames: 'KeystrokeActions '
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: 'Editor class' category: #'System-Text'!
Editor class
	instanceVariableNames: 'shortcuts cmdShortcuts '!

!Editor commentStamp: '<historical>' prior: 0!
New text editors.
TextEditor provides most of the functionality that used to be in TextMorphEditor.
SmalltalkEditor is has Smalltalk code specific features.
SimpleEditor provides basic functionality for single line text editing. It does not handle fonts and styles, aligning and Smalltalk utilities. It handles one single line.
CellStyleEditor allows entering alphabetic characters using only number keys, like many cell phones do.!

!Editor methodsFor: 'typing support' stamp: 'jmv 6/10/2012 10:54'!
cmdShortcuts
	"Same for all instances.
	A subclass could handle specific keyboard shortcuts for each instance, though."
	^self class cmdShortcuts! !

!Editor methodsFor: 'typing support' stamp: 'jmv 6/10/2012 11:13'!
shortcuts
	"Same for all instances.
	A subclass could handle specific keyboard shortcuts for each instance, though."
	^self class shortcuts! !


!Editor class methodsFor: 'class initialization' stamp: 'jmv 6/10/2012 11:21'!
basicInitialize
	"
	Editor initialize
	"
	self withAllSubclassesDo: [ :c |
		c initializeShortcuts; initializeCmdShortcuts ]! !

!Editor class methodsFor: 'class initialization' stamp: 'jmv 6/10/2012 11:20'!
initializeCmdShortcuts
	"Initialize the (unshifted) command-key (or alt-key if not on Mac) shortcut table."
	"NOTE: if you don't know what your keyboard generates, use Sensor test"
	"
	Editor initialize
	"

	cmdShortcuts _ Array new: 256 withAll: #noop:.

	self basicCmdShortcutsSpec do: [ :ary |
		cmdShortcuts at: ary first asciiValue + 1 put: ary second ].

	self cmdShortcutsSpec do: [ :ary |
		cmdShortcuts at: ary first asciiValue + 1 put: ary second ]! !

!Editor class methodsFor: 'class initialization' stamp: 'jmv 6/10/2012 10:56'!
initializeShortcuts
	"Initialize the table for regular (i.e. non-command) keystroke dispatch"
	"
	self initializeKeystrokeActions
	"
	| actions |
	actions _ Array new: 256 withAll: #normalCharacter:.
	0 to: 31 do: [ :i | actions at: i+1 put: #noop: ].
	actions at: 1 + 1 put: #cursorHome:.				"home key"
	actions at: 3 + 1 put: #enter:.						"enter / return key"
	actions at: 4 + 1 put: #cursorEnd:.				"end key"
	actions at: 5 + 1 put: #noop:.						"insert key"
	actions at: 8 + 1 put: #backspace:.				"macDelete winBackspace key"
	actions at: 9 + 1 put: #normalCharacter:.		"tab"
	actions at: 11 + 1 put: #cursorPageUp:.			"page up key"
	actions at: 12 + 1 put: #cursorPageDown:.		"page down key"
	actions
		at:  InputSensor returnKey + 1
		put: #returnKey:.									"return (sometimes labelled enter) key"
	actions at: 27 + 1 put: #offerMenuFromEsc:.	"escape key"
	actions at: 28 + 1 put: #cursorLeft:.				"left arrow key"
	actions at: 29 + 1 put: #cursorRight:.				"right arrow key"
	actions at: 30 + 1 put: #cursorUp:.				"up arrow key"
	actions at: 31 + 1 put: #cursorDown:.			"down arrow key"
	actions at: 127 + 1 put: #forwardDelete:.		"winDelete key"
	shortcuts _ actions! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/10/2012 11:09'!
basicCmdShortcutsSpec
	^#()! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/10/2012 10:55'!
cmdShortcuts
	"Same for all instances.
	A subclass could handle specific keyboard shortcuts for each instance, though."
	^cmdShortcuts! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/10/2012 11:07'!
cmdShortcutsSpec
	^#()! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/10/2012 11:13'!
shortcuts
	"Same for all instances.
	A subclass could handle specific keyboard shortcuts for each instance, though."
	^shortcuts! !


!SimpleEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/10/2012 11:07'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$a 	#selectAll:				'Select all')

		#(		$x 	#cut:						'Cut selection and store it in the Clipboard')
		#(		$c 	#copySelection:			'Copy selection to the Clipboard')
		#(		$v 	#paste:					'Paste Clipboard contents')
		#(		$ 	#selectWord:			'Select the current word as with double clicking')
	)! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/10/2012 11:09'!
basicCmdShortcutsSpec
	"
	Editor initialize
	"

	"arranged in QWERTY keyboard order"
	^#(
		#(		$( 	#enclose:					)
		#(		$[ 	#enclose:					)
		#(		${ 	#enclose:					)
		#(		$' 	#enclose:					)
		#(		$" 	#enclose:					)
		#(		$< 	#enclose:					)

		#(		$a 	#selectAll:				'Select all')
		#(		$f 	#find:						'Find')
		#(		$g 	#findAgain:				'Find again')
		#(		$h 	#setSearchString:		'Set selection as search string for find again')

		#(		$z 	#undo:					'Undo (multiple levels)')
		#(		$x 	#cut:						'Cut selection and store it in the Clipboard')
		#(		$c 	#copySelection:			'Copy selection to the Clipboard')
		#(		$v 	#paste:					'Paste Clipboard contents')

		#(		$R	#indent:					'Indent (move selection one tab-stap right)')
		#(		$Y	#makeUppercase:		'Force selection to uppercase')
		#(		$U	#changeLineEndsToLf:	'Convert line endings to LF characters (Cuis convention) in selection')

		#(		$H	#cursorTopHome:		'Move cursor to start of text')
		#(		$L	#outdent:				'Outdent (move selection one tab-stop left)')

		#(		$Z	#redo:					'Redo (multiple levels)')
		#(		$X	#makeLowercase:		'Force selection to lowercase')
		#(		$C	#compareToClipboard:	'Compare argument to clipboard')

		#(		$ 	#selectWord:			'Select the current word as with double clicking')
	)! !

!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/10/2012 11:07'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$0 	changeEmphasis:			)
		#(		$1 	changeEmphasis:			)
		#(		$2 	changeEmphasis:			)
		#(		$3 	changeEmphasis:			)
		#(		$4 	changeEmphasis:			)
		#(		$5 	changeEmphasis:			)
		#(		$6 	changeEmphasis:			)
		#(		$8	#offerColorMenu:			)

		#(		$k	#offerFontMenu:			'Set font')

		#(		$u	#align:						'Toggle alignment')
	)! !

!TextEditor class methodsFor: 'class initialization' stamp: 'jmv 6/10/2012 11:03'!
basicInitialize 
	"Initialize the keyboard shortcut maps and the shared buffers for managing again."
	"
	Editor initialize
	"
	super basicInitialize.
	FindText _ ChangeText _ Text new.
	self initializeMenu! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/10/2012 11:07'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$i	#inspectIt:							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(		$p	#printIt:								'Print it (selection is a valid expression)')

		#(		$s	#save:								'Save (i.e. accept)')
		#(		$d	#doIt:									'Do it (selection is a valid expression)')
		#(		$l	#cancelEdits:						'Cancel')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:						'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:					'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$E	#methodStringsContainingit:		'Method strings containing it')
		#(		$T	#displayIfTrue:						'Insert #ifTrue:')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$A	#argAdvance:						'Advance argument')
		#(		$F	#displayIfFalse:						'Insert #ifFalse:')
		#(		$G	#fileItIn:								'File in selection')

		#(		$V	#pasteInitials:						'Paste author initials')
		#(		$N	#referencesToIt:					'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !


!Editor class methodsFor: 'class initialization' stamp: 'jmv 6/10/2012 11:03'!
initialize
	"
	Editor initialize
	"
	self withAllSubclassesDo: [ :c |
		c basicInitialize ]! !


!SimpleEditor methodsFor: 'typing support' stamp: 'jmv 6/10/2012 11:14'!
dispatchOn: aKeyboardEvent
	"Carry out the action associated with this character, if any."
	| asciiValue |
	asciiValue _ aKeyboardEvent keyValue.
	"Control keys are handled by #shortcuts even if they have any modifiers"
	(asciiValue >= 32 and: [
		aKeyboardEvent commandAltKeyPressed ]) ifTrue: [
		^self perform: (self cmdShortcuts at: asciiValue + 1) with: aKeyboardEvent ].

	"We don't support multiple lines. Therefore, we don't process return as a #normalCharacter:"
	aKeyboardEvent isReturnKey ifTrue: [
		^ true].

	^ self perform: (self shortcuts at: asciiValue + 1) with: aKeyboardEvent! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/10/2012 11:17'!
presumedSentMessages
	| sent |
	"
	In addition to those here, if it is desired to preserve some methods from deletion, see #nominallyUnsent:
	Smalltalk presumedSentMessages
	"

	"The following should be preserved for doIts, etc"
	sent _ IdentitySet new.
	#( rehashWithoutBecome compactSymbolTable
		browseAllSelect:  lastRemoval
		vScrollBarValue: hScrollBarValue: 
		to: removeClassNamed:
		dragon: hilberts: mandala: web test3 factorial tinyBenchmarks benchFib
		newDepth: restoreAfter: zapAllMethods obsoleteClasses
		removeAllUnSentMessages abandonSources removeUnreferencedKeys
		zapOrganization condenseChanges browseObsoleteReferences
		subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:
		startTimerEventLoop unusedClasses
		unimplemented
		reduceCuis
		variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		printSpaceAnalysis:on:) do: [ :sel |
			sent add: sel].
	"The following may be sent by perform: in dispatchOnChar..."
	Editor withAllSubclassesDo: [ :c |
		c shortcuts asSet do: [ :sel | sent add: sel ].
		c cmdShortcuts asSet do: [ :sel | sent add: sel ]].
	#(beReadOnlyBinding beReadWriteBinding) do: [ :sel |
		sent add: sel].
	^ sent! !


!TextEditor methodsFor: 'typing support' stamp: 'jmv 6/10/2012 11:15'!
dispatchOn: aKeyboardEvent
	"Carry out the action associated with this character, if any."

	| asciiValue c |
	self clearParens.
  	asciiValue _ aKeyboardEvent keyValue.
	"Control keys are handled by #shortcuts even if they have any modifiers"
	(asciiValue >= 32 and: [
		aKeyboardEvent commandAltKeyPressed ]) ifTrue: [
		^self perform: (self cmdShortcuts at: asciiValue + 1) with: aKeyboardEvent ].

	c _ aKeyboardEvent keyCharacter.
	(')]}' includes: c)
		ifTrue: [ self blinkPrevParen: c ].

	^ self perform: (self shortcuts at: asciiValue + 1) with: aKeyboardEvent! !


!Utilities class methodsFor: 'support windows' stamp: 'jmv 6/10/2012 11:30'!
commandKeyMappings
	^ self class firstCommentAt: #commandKeyMappings

"Lower-case command keys
(use with Cmd key on Mac and Alt key on other platforms)
a	Select all
b	Browse it (selection is a class name or cursor is over a class-list or message-list)
c	Copy selection
d	Do it (selection is a valid expression)
e	Exchange selection with prior selection
f	Find
g	Find again
h	Set selection as search string for find again
i	Inspect it (selection is a valid expression, or selection is over an inspect-ilst)
j	Again once (do the last text-related operation again)
k	Set font
l	Cancel
m	Implementors of it (selection is a message selector or cursor is over a class-list or message-list)
n	Senders of it (selection is a message selector or cursor is over a class-list or message-list)
o	Spawn current method
p	Print it (selection is a valid expression)
q	Query symbol (toggle all possible completion for a given prefix)
r	Recognizer
s	Save (i.e. accept)
t	Finds a Transcript (when cursor is over the desktop)
u	Toggle alignment
v	Paste
w	Delete preceding word (over text);  Close-window (over morphic desktop)
x	Cut selection
y	Swap characters
z	Undo

Note: for Do it, Senders of it, etc., a null selection will be expanded to a word or to the current line in an attempt to do what you want.  Also note that Senders/Implementors of it will find the outermost keyword selector in a large selection, as when you have selected a bracketed expression or an entire line.  Finally note that the same cmd-m and cmd-n (and cmd-v for versions) work in the message pane of most browsers.

Upper-case command keys
	(use with Shift-Cmd, or Ctrl on Mac
	or Shift-Alt on other platforms; sometimes Ctrl works too)
A	Advance argument
B	Browse it in this same browser (in System browsers only)
C	Compare argument to clipboard
D	Duplicate
E	Method strings containing it
F	Insert 'ifFalse:'
G	fileIn from it (a file name)
H	cursor TopHome:
I	Inspect via Object Explorer
J	Again many (apply the previous text command repeatedly until the end of the text)
K	Set style
L	Outdent (move selection one tab-stop left)
M	Select current type-in
N	References to it (selection is a class name, or cursor is over a class-list or message-list)
O	Open single-message browser (in message lists)
P	Make project link
R	Indent (move selection one tab-stap right)
S	Search
T	Insert 'ifTrue:'
U	Convert linefeeds to carriage returns in selection
V	Paste author's initials
W	Selectors containing it (in text); show-world-menu (when issued with cursor over desktop)
X	Force selection to lowercase
Y	Force selection to uppercase
Z	Capitalize all words in selection

Other special keys
Backspace	Backward delete character
Del			Forward delete character
Shift-Bksp	Backward delete word
Shift-Del	Forward delete word
Esc			Pop up the Desktop Menu
\			Send top window to back

Cursor keys
left, right,
up, down	Move cursor left, right, up or down
Ctrl-left		Move cursor left one word
Ctrl-right	Move cursor right one word
Home		Move cursor to begin of line or begin of text
End			Move cursor to end of line or end of text
PgUp, Ctrl-up	Move cursor up one page
PgDown, Ctrl-Dn	Move cursor down one page

Note all these keys can be used together with Shift to define or enlarge the selection. You cannot however shrink that selection again, as in some other systems.

Other Cmd-key combinations (not available on all platforms)
Return		Insert return followed by as many tabs as the previous line
			(with a further adjustment for additional brackets in that line)
Space		Select the current word as with double clicking

Enclose the selection in a kind of bracket.  Each is a toggle.
	(not available on all platforms)
Ctrl-(	Enclose within ( and ), or remove enclosing ( and )
Ctrl-[	Enclose within [ and ], or remove enclosing [ and ]
Crtl-{	Enclose within { and }, or remove enclosing { and }
Ctrl-<	Enclose within < and >, or remove enclosing < and >
Ctrl-'	Enclose within ' and ', or remove enclosing ' and '
Ctrl-""	Enclose within "" and "", or remove enclosing "" and ""
Note also that you can double-click just inside any of the above delimiters,
or at the beginning or end of a line, to select the text enclosed.

Text Emphasis
	(not available on all platforms)
Cmd-1	10 point font
Cmd-2	12 point font
Cmd-3	18 point font
Cmd-4	24 point font
Cmd-5	36 point font
Cmd-6	color, action-on-click, link to class comment, link to method, url
		Brings up a menu.  To remove these properties, select
		more than the active part and then use command-0.
Cmd-7	bold
Cmd-8	italic
Cmd-9	narrow (same as negative kern)
Cmd-0	plain text (resets all emphasis)
Cmd--	underlined (toggles it)
Cmd-=	struck out (toggles it)

Shift-Cmd--	(aka _) negative kern (letters 1 pixel closer)
Shift-Cmd-+	positive kern (letters 1 pixel larger spread)
"! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 6/8/2012 17:36'!
openCommandKeyHelp
	"Open a window giving command key help."
	"
	Utilities openCommandKeyHelp
	"

	(TextModel new contents: self commandKeyMappings)
		openLabel: 'Command Key Actions'
! !

!methodRemoval: SmalltalkEditor class #cmdKeyShortcuts!
SmalltalkEditor class removeSelector: #cmdKeyShortcuts!
!methodRemoval: SmalltalkEditor class #initializeCmdKeyShortcuts!
SmalltalkEditor class removeSelector: #initializeCmdKeyShortcuts!
!methodRemoval: TextEditor class #basicCmdKeyShortcuts!
TextEditor class removeSelector: #basicCmdKeyShortcuts!
!methodRemoval: TextEditor class #basicCmdKeyShortcutsSpec!
TextEditor class removeSelector: #basicCmdKeyShortcutsSpec!
!methodRemoval: TextEditor class #cmdActions!
TextEditor class removeSelector: #cmdActions!
!methodRemoval: TextEditor class #cmdKeyShortcuts!
TextEditor class removeSelector: #cmdKeyShortcuts!
!methodRemoval: TextEditor class #initialize!
TextEditor class removeSelector: #initialize!
!methodRemoval: TextEditor class #initializeBasicCmdKeyShortcuts!
TextEditor class removeSelector: #initializeBasicCmdKeyShortcuts!
!methodRemoval: TextEditor class #initializeCmdKeyShortcuts!
TextEditor class removeSelector: #initializeCmdKeyShortcuts!
TextEditor initialize!
!classDefinition: 'TextEditor class' category: #'System-Text'!
TextEditor class
	instanceVariableNames: 'menu'!
!methodRemoval: TextEditor #cmdActions!
TextEditor removeSelector: #cmdActions!
!methodRemoval: SimpleEditor class #cmdActions!
SimpleEditor class removeSelector: #cmdActions!
!methodRemoval: SimpleEditor class #cmdKeyShortcuts!
SimpleEditor class removeSelector: #cmdKeyShortcuts!
!methodRemoval: SimpleEditor class #initialize!
SimpleEditor class removeSelector: #initialize!
!methodRemoval: SimpleEditor class #initializeCmdKeyShortcuts!
SimpleEditor class removeSelector: #initializeCmdKeyShortcuts!
SimpleEditor initialize!
!classDefinition: 'SimpleEditor class' category: #'System-Text'!
SimpleEditor class
	instanceVariableNames: ''!
!methodRemoval: SimpleEditor #cmdActions!
SimpleEditor removeSelector: #cmdActions!
!methodRemoval: SimpleEditor #cmdShortcuts!
SimpleEditor removeSelector: #cmdShortcuts!
!methodRemoval: Editor class #basicCmdKShortcutsSpec!
Editor class removeSelector: #basicCmdKShortcutsSpec!
!methodRemoval: Editor class #basicCmdKeyShortcuts!
Editor class removeSelector: #basicCmdKeyShortcuts!
!methodRemoval: Editor class #basicCmdKeyShortcutsSpec!
Editor class removeSelector: #basicCmdKeyShortcutsSpec!
!methodRemoval: Editor class #cmdKeyShortcuts!
Editor class removeSelector: #cmdKeyShortcuts!
!methodRemoval: Editor class #initializeCmdKeyShortcuts!
Editor class removeSelector: #initializeCmdKeyShortcuts!
!methodRemoval: Editor class #initializeKeystrokeActions!
Editor class removeSelector: #initializeKeystrokeActions!
Editor initialize!
!classDefinition: 'Editor class' category: #'System-Text'!
Editor class
	instanceVariableNames: 'shortcuts cmdShortcuts'!

!Editor class reorganize!
('class initialization' basicInitialize initialize initializeCmdShortcuts initializeShortcuts)
('keyboard shortcut tables' basicCmdShortcutsSpec cmdShortcuts cmdShortcutsSpec shortcuts)
!

!classDefinition: #Editor category: #'System-Text'!
Object subclass: #Editor
	instanceVariableNames: 'morph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!Editor reorganize!
('accessing' currentAttributes morph morph: userHasEdited)
('accessing-selection' hasSelection selectionInterval selectionIntervalsDo:)
('editing keys' copySelection: cut: noop: paste:)
('menu messages' clipboardStringOrText clipboardTextPut: lineSelectAndEmptyCheck: paste pasteString wordSelectAndEmptyCheck:)
('new selection' deselectAndPlaceCursorAt: selectInterval: selectInvisiblyFrom:to: selectInvisiblyMark:point: selectMark:point: selectWord selectWordLeftDelimiters:rightDelimiters:)
('nonediting/nontyping keys' cursorDown: cursorLeft: cursorPageDown: cursorPageUp: cursorRight: cursorUp: selectWord:)
('typing/selecting keys' backspace: clearSelection cursorTopHome: enter: newLine: normalCharacter: returnKey: selectAll selectAll:)
('private' beginningOfLine: beginningOfNextParagraph: beginningOfParagraph: beginningOfText endOfLine: endOfParagraph: endOfText lines moveCursor:forward:event: nextWordEnd: nextWordStart: previousWordStart: sameColumn:newLine:forward: setIndices:forward:)
('menu commands' offerMenuFromEsc:)
('typing support' cmdShortcuts shortcuts)
!

