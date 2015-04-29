'From Cuis 4.2 of 25 July 2013 [latest update: #2254] on 17 April 2015 at 4:25:54.109022 pm'!

!Editor class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 16:22'!
releaseClassCachedState

	shortcuts _ nil.
	cmdShortcuts _ nil.! !


!TextEditor class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 16:04'!
changeText
	ChangeText ifNil: [
		ChangeText _ Text new ].
	^ChangeText! !

!TextEditor class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 16:04'!
findText
	FindText ifNil: [
		FindText _ Text new ].
	^FindText! !

!TextEditor class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 16:15'!
menu
	menu ifNil: [
		self initializeMenu ].
	^menu! !

!TextEditor class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 16:24'!
releaseClassCachedState

	FindText _ nil.
	ChangeText _ nil.
	"We are not calling super to avoid cleansing class vars many times.
	So, repeat inherited class instVars!!"
	shortcuts _ nil.
	cmdShortcuts _ nil.
	menu _ nil! !


!SmalltalkEditor class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 16:18'!
menu2
	menu2 ifNil: [
		self initializeMenu ].
	^menu2! !

!SmalltalkEditor class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 16:24'!
releaseClassCachedState

	"We are not calling super to avoid cleansing class vars many times.
	So, repeat inherited class instVars!!"
	shortcuts _ nil.
	cmdShortcuts _ nil.
	menu _ nil.
	menu2 _ nil! !

!Editor class methodsFor: 'class initialization' stamp: 'jmv 4/17/2015 16:11'!
initializeShortcuts
	"Initialize the table for regular (i.e. non-command) keystroke dispatch"
	"
	self initializeShortcuts
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

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 4/17/2015 16:14'!
cmdShortcuts
	"Same for all instances.
	A subclass could handle specific keyboard shortcuts for each instance, though."
	cmdShortcuts ifNil: [
		self initializeCmdShortcuts ].
	^cmdShortcuts! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 4/17/2015 16:11'!
shortcuts
	"Same for all instances.
	A subclass could handle specific keyboard shortcuts for each instance, though."
	shortcuts ifNil: [
		self initializeShortcuts ].
	^shortcuts! !


!TextEditor methodsFor: 'accessing' stamp: 'jmv 4/17/2015 16:05'!
setSearch: aStringOrText
	"Set the FindText and ChangeText to seek aString; except if already seeking aString, leave ChangeText alone so again will repeat last replacement."

	self class findText = aStringOrText
		ifFalse: [FindText _ ChangeText _ aStringOrText]! !

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 4/17/2015 16:07'!
find
	"Prompt the user for a string to search for, and search the receiver from the current selection onward for it.  1/26/96 sw"

	| reply |
	reply _ FillInTheBlankMorph request: 'Find what? ' initialAnswer: self class findText.
	"Set focus on our text morph, so that cmd-g does the search again"
	morph world activeHand newKeyboardFocus: morph.
	reply size = 0 ifTrue: [
		^ self].
	self setSearch: reply.
	ChangeText _ self class findText.  "Implies no replacement to againOnce: method"
	(self findAndReplaceMany: false)
		ifFalse: [ self flash ].

"	morph installEditorToReplace: self"! !

!TextEditor methodsFor: 'private' stamp: 'jmv 4/17/2015 16:06'!
findAndReplaceOnce: indexStream
	"Find the next occurrence of FindText.  If none, answer false.
	Append the start index of the occurrence to the stream indices, and, if
	ChangeText is not the same object as FindText, replace the occurrence by it.
	Note that the search is case-sensitive for replacements, otherwise not."
	"This is a user command, and generates undo"

	| where |
	where _ model actualContents
		findString: self class findText
		startingAt: self stopIndex
		caseSensitive: ((self class changeText ~~ self class findText) or: [Preferences caseSensitiveFinds]).
	where = 0 ifTrue: [^ false].

	selectionStartBlocks add: (textComposition characterBlockForIndex: where).
	selectionStopBlocks add: (textComposition characterBlockForIndex: where + self class findText size).
	self selectFrom: where to: where + self class findText size - 1.	"Repeat it here. Senders beware: only one of these should last"

	self class changeText ~~ self class findText ifTrue: [ self replaceSelectionWith: self class changeText ].
	indexStream nextPut: where.
	^ true! !


!TextEditor class methodsFor: 'class initialization' stamp: 'jmv 4/17/2015 16:06'!
abandonChangeText
	"Call this to get out of the maddening situation in which the system keeps aggressively trying to do a replacement that you no longer wish to make, every time you make choose a new method in a list."
	ChangeText _ self findText

	"
	TextEditor abandonChangeText
	"! !

!TextEditor class methodsFor: 'class initialization' stamp: 'jmv 4/17/2015 16:07'!
basicInitialize 
	"Initialize the keyboard shortcut maps and the shared buffers for managing again."
	"
	Editor initialize
	"
	super basicInitialize.
	self initializeMenu! !

!TextEditor class methodsFor: 'menu' stamp: 'jmv 4/17/2015 16:16'!
paneMenu:  aMenu
	| m |
	m _ self menu.
	aMenu labels: m labelString lines: m lineArray selections: m selections.
	aMenu addTitle: self name; addStayUpIcons.
	^ aMenu! !


!SmalltalkEditor class methodsFor: 'menu' stamp: 'jmv 4/17/2015 16:19'!
paneMenu2:  aMenu

	| m |
	m _ self menu2.
	aMenu labels: m labelString lines: m lineArray selections: m selections.
	aMenu addTitle: 'Smalltalk specific menu    '; addStayUpIcons.
	^ aMenu! !


!SmalltalkEditor class reorganize!
('keyboard shortcut tables' cmdShortcutsSpec initializeMenu)
('menu' paneMenu2:)
('cached state access' menu2 releaseClassCachedState)
!


!TextEditor class reorganize!
('keyboard shortcut tables' basicCmdShortcutsSpec cmdShortcutsSpec initializeMenu)
('class initialization' abandonChangeText basicInitialize)
('menu' paneMenu:)
('misc' openHelp)
('cached state access' changeText findText menu releaseClassCachedState)
!


!Editor class reorganize!
('class initialization' basicInitialize initialize initializeCmdShortcuts initializeShortcuts)
('keyboard shortcut tables' basicCmdShortcutsSpec cmdShortcuts cmdShortcutsSpec shortcuts)
('help' help)
('cached state access' releaseClassCachedState)
!

