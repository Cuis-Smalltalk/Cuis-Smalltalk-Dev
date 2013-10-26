'From Cuis 4.0 of 21 April 2012 [latest update: #1298] on 10 June 2012 at 9:02:17 pm'!

!Editor class methodsFor: 'help' stamp: 'jmv 6/10/2012 20:50'!
help
	"
	Editor help
	SimpleEditor help
	CellStyleEditor help
	TextEditor help
	SmalltalkEditor help
	"
	| allSpecs |
	allSpecs _ self basicCmdShortcutsSpec, self cmdShortcutsSpec.
	^String streamContents: [ :strm |
		allSpecs do: [ :triplet | | c |
			c _ triplet first = Character space
				ifFalse: [ triplet first asString, String tab ]
				ifTrue: [ 'Space'].
			strm nextPutAll: ('Cmd-', c, String tab, String tab, triplet third).
			strm newLine ]]! !


!TextEditor methodsFor: 'nonediting/nontyping keys' stamp: 'jmv 6/10/2012 20:55'!
help: aKeyboardEvent
	"Show a help screen"

	self class openHelp.
	^ true! !


!TextEditor class methodsFor: 'misc' stamp: 'jmv 6/10/2012 20:55'!
openHelp

	TextModel new contents: self help; openLabel: self name, ' Help'.! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/10/2012 11:57'!
enclose: aKeyboardEvent
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| left right startIndex stopIndex oldSelection which |
	startIndex _ self startIndex.
	stopIndex _ self stopIndex.
	oldSelection _ self selection.
	which _ '([<{"''' indexOf: aKeyboardEvent keyCharacter ifAbsent: [ ^true ].
	left _ '([<{"''' at: which.
	right _ ')]>}"''' at: which.
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


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/10/2012 20:48'!
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
		#(		$< 	#enclose:					'Enclose within < and >, or remove enclosing < and >')

		#(		$a 	#selectAll:				'Select all')
		#(		$f 	#find:						'Find')
		#(		$g 	#findAgain:				'Find again')
		#(		$h 	#help:						'Open this help')
		#(		$j 	#setSearchString:		'Set selection as search string for find again')

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

!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/10/2012 12:12'!
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
		#(		$5 	changeEmphasis:			'Negative kern (letters 1 pixel closer)')
		#(		$6 	changeEmphasis:			'Positive kern (letters 1 pixel larger spread)')
		#(		$8	#offerColorMenu:			'Change color')

		#(		$k	#offerFontMenu:			'Set font')

		#(		$u	#align:						'Toggle alignment')
	)! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 6/10/2012 21:00'!
helpMenu
        "Build the help menu for the world."
        |  menu |

  	menu := self menu: 'Help...'.

        self fillIn: menu from:
        {
                {'About this System...'. {Smalltalk. #aboutThisSystem}. 'current version information.'}.
                {'Preferences...'. {Preferences. #openPreferencesInspector}. 'view and change various options.'}.
                nil.
               {'Editor keyboard shortcuts'. { SmalltalkEditor . #openHelp}. 'summary of keyboard shortcuts in editors for Smalltalk code.'}
	}.

	self addGestureHelpItemsTo: menu.

	self fillIn: menu from:
	{
                {'World menu Help'. { self . #worldMenuHelp}. 'helps find menu items buried in submenus.'}.
                {'Useful Expressions' . { Utilities . #openStandardWorkspace}. 'a window full of useful expressions.'}.
                nil.

                {'Set Author initials...' . { Utilities . #setAuthorInitials }. 'supply initials to be used to identify the author of code and other content.'}.
                {'VM Statistics' . { self . #vmStatistics}.  'obtain some intriguing data about the vm.'}.
			nil.
                {'Space Left' . { self . #garbageCollect}. 'perform a full garbage-collection and report how many bytes of space remain in the image.'}.
        }.

	^menu

! !


!Theme methodsFor: 'menus' stamp: 'jmv 6/10/2012 21:00'!
miscellaneousIcons

	"Everything else."

	^ {
		#('restore display (r)' 'set display depth...' 'move windows onscreen' 'Utilities saveDisplay.') -> #displayIcon.
		#('changes...' 'dual change sorter' 'change sets with this method' 'find a change sorter' 'recent changes in file' 'Undo / Redo history') -> #changesIcon.
		#('print PS to file...' ) -> #printIcon.
		#('find again (g)' 'full stack (k)') -> #systemIcon.
		#('print it (p)' 'check change set for slips') -> #printIcon.
		#('accept (s)' 'make changes go to me (m)') -> #acceptIcon.
		#('cancel (l)' ) -> #cancelIcon.
		#('debug...' 'debug it' 'toggle break on entry') -> #debugIcon.
		#('close' 'close all debuggers' 'close top window') -> #closeIcon.
		#('collapse' 'hide taskbar' 'collapse all windows') -> #collapseIcon.
		#('expand / contract' 'show taskbar' 'expand all windows') -> #expandIcon.
		#('menu') -> #windowMenuIcon.
		#('browse all' 'browser' 'browse it (b)' 'MessageTally UI and browse' 'browse recent submissions' 'browse full (b)' 'find changed browsers...' 'browse (b)' 'browse my changes') -> #editFindReplaceIcon.
		#('workspace' 'workspace with contents') -> #terminalIcon.
		#('styled text editor' 'text editor' 'edit this list' 'edit postscript...' 'add postscript...') -> #textEditorIcon.
		#('file list' 'find a fileList') -> #systemFileManagerIcon.
		#('transcript' 'find a transcript' 'Transcript clear.' 'log to transcript') -> #printerIcon.
		#('process browser' 'vm statistics' 'MessageTally all Processes') -> #systemMonitorIcon.
		#('emergency evaluator' 'conflicts with other change sets' 'check for slips' 'conflicts with change set opposite' 'conflicts with category opposite') -> #emblemImportantIcon.
		#('change sorter') -> #halfRefreshIcon.
		#('SUnit Test Runner') -> #weatherFewCloudsIcon.
		#('system fonts...' 'set font... (k)') -> #preferencesDesktopFontIcon.
		#('full screen on') -> #viewFullscreenIcon.
		#('full screen off') -> #exitFullscreenIcon.
		#('set desktop color...') -> #wallpaperIcon.
		#('preferences...' 'what to show...') -> #preferencesIcon.
		#('Editor keyboard shortcuts') -> #keyboardShortcutsIcon.
		#('world menu help') -> #globeIcon.
		#('useful expressions' 'class comments with it' 'check for uncommented methods' 'check for uncommented classes') -> #chatIcon.
		#('set author...' 'check for other authors' 'check for any other authors') -> #usersIcon.
		#('space left') -> #removableMediaIcon.
		#('start drawing all again' 'window color...') -> #graphicsIcon.
		#('start stepping again') -> #mediaPlaybackStartIcon.
		#('file out current change set' 'fileOut' 'File out and remove (o)' 'File out and keep (k)') -> #fileOutIcon.
		#('recently logged changes...' 'versions (v)' 'recent classes... (r)' 'trim history' 'profile messages (m)') -> #clockIcon.
		#('senders of it (n)' 'senders of... (n)' 'local senders of...' 'senders (n)') -> #mailForwardIcon.
		#('implementors of it (m)' 'implementors of... (m)' 'implementors of sent messages') -> #developmentIcon.
		#('references to it (N)') -> #addressBookIcon.
		#('class var refs...' 'class refs (N)' 'class variables' 'class vars' 'local implementors of...' 'subclass template') -> #classIcon.
		#('inst var refs...' 'inst var defs...' 'sample instance' 'inspect Pointers (P)') -> #instanceIcon.
		#('Use Selection for Find (h)' 'rename class ...' 'rename...' 'change title...') -> #saveAsIcon.
		#('method source with it' 'browse method (O)' 'check for uncategorized methods') -> #scriptIcon.
		#('method strings with it (E)') -> #genericTextIcon.
		#('browse hierarchy (h)' 'move to top' 'promote to top of list') -> #goTopIcon.
		#('move up' 'make next-to-topmost') -> #goUpIcon.
		#('move to bottom' 'send to back' 'send top window to back') -> #goBottomIcon.
		#('inheritance (i)' 'move down') -> #goDownIcon.
		#('browse protocol (p)' 'spawn sub-protocol') -> #spreadsheetIcon.
		#('spawn full protocol') -> #speadsheetTemplateIcon.
		#('alphabetize') -> #fontXGenericIcon.
		#('Installed Packages' 'browse' 'show category (C)' 'categorize all uncategorized' 'select change set...' 'view affected class categories') -> #packageIcon.
		#('remove from current change set' 'remove empty categories' 'subtract other side (-)' 'remove from this browser') -> #listRemoveIcon.
		#('add to current change set' 'add all meths to current chgs' 'add preamble (p)') -> #listAddIcon.
		#('toggle diffing (D)' 'toggle selections') -> #switchIcon.
		#('reorganize' 'create inst var accessors' 'ChangeSorter reorderChangeSets.' 'reorder all change sets' 'by name' 'by size' 'by date') -> #sendReceiveIcon.
		#('unsent methods' 'unreferenced class vars' 'unreferenced inst vars' 'Undeclared inspect.' 'Undeclared removeUnreferencedKeys; inspect.' 'ChangeSorter removeEmptyUnnamedChangeSets.' 'check for unsent messages') -> #junkIcon.
		#('update' 'turn on auto-update (a)' 'update list (u)') -> #updateIcon.
		#('find changed windows...') -> #newWindowIcon.
		#('make undraggable') -> #pushPinIcon.
		#('Utilities saveScreenshot.') -> #stillCameraIcon.
		#('add new directory') -> #newFolderIcon.
		#('select all' 'deselect all') -> #selectAllIcon.
		#('sort by date') -> #dateIcon.
		#('justified') -> #formatJustifyFillIcon.
		#('centered') -> #formatJustifyCenterIcon.
		#('set alignment...' 'leftFlush') -> #formatJustifyLeftIcon.
		#('rightFlush') -> #formatJustifyRightIcon.
		#('signal Semaphore (S)') -> #haloHelpIcon.
		#('Change Paragraph Style...' 'Change Character Style...' 'Remove Character Style' 'Replace all uses of Paragraph Style...' 'Replace all uses of Character Style...') -> #fontXGenericIcon.
	}! !

!methodRemoval: Utilities class #commandKeyMappings!
Utilities class removeSelector: #commandKeyMappings!
!methodRemoval: Utilities class #openCommandKeyHelp!
Utilities class removeSelector: #openCommandKeyHelp!

!TextEditor class reorganize!
('keyboard shortcut tables' basicCmdShortcutsSpec cmdShortcutsSpec initializeMenu)
('class initialization' abandonChangeText basicInitialize)
('accessing')
('menu' paneMenu:)
('misc' openHelp)
!


!Editor class reorganize!
('class initialization' basicInitialize initialize initializeCmdShortcuts initializeShortcuts)
('keyboard shortcut tables' basicCmdShortcutsSpec cmdShortcuts cmdShortcutsSpec shortcuts)
('help' help)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Theme current class beCurrent.
Editor initialize!

