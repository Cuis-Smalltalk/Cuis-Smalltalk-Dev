"
An InnerTextMorph can have an autocompleter in the same way it might have a styler. My instances implement autocompletion.
"
Class {
	#name : 'AutoCompleter',
	#superclass : 'Object',
	#instVars : [
		'model',
		'textMorph',
		'menuMorph',
		'position',
		'entries',
		'prefix'
	],
	#category : 'Tools-Autocompletion',
	#package : 'Tools',
	#tag : 'Autocompletion'
}

{ #category : 'help text' }
AutoCompleter class >> helpText [
	"
	self helpText edit
	"
^'open/close menu
	ctrl-space or tab
		open the completion menu
	ESC
		close menu
	?, ctl-h
		open this help
menu navigation
	Arrows up/down
		move the selection up and down
	Page up/down
		page up and down
	Home/End
		move to first or last page of the menu
changing menu contents
	alphanumeric character
		filter the menu to the given input
	backspace
		delete an input character, adjust menu to the new input.
inserting completion
	ctrl-space or tab
		close the menu and insert selected completion. if there only one item left in the menu this done automaticaly.'
]

{ #category : 'instance creation' }
AutoCompleter class >> withModel: aStringHolder [
	^self new setModel: aStringHolder
]

{ #category : 'keyboard' }
AutoCompleter >> autoCompletionAround: aBlock keyStroke: aKeyboardEvent [

	(self handleKeystrokeBefore: aKeyboardEvent)
		ifTrue: [^ self].
	aBlock value.
	"Narrow the completion with any of the keys"
	self handleKeystrokeAfter: aKeyboardEvent
]

{ #category : 'testing' }
AutoCompleter >> canSelect: anEntry [

	^true
]

{ #category : 'testing' }
AutoCompleter >> canSelectEntryAt: anIndex [

	^self canSelect: (self entryAt: anIndex)
]

{ #category : 'menu morph' }
AutoCompleter >> closeMenu [
	menuMorph ifNotNil: [
		menuMorph delete.
		menuMorph := nil ]
]

{ #category : 'entries' }
AutoCompleter >> computeEntries [
	"This default implementation might be redefined of needed."
	| prefixStart prefixStop string |
	string := model actualContents string.
	prefixStop := position.
	prefixStart := position.
	[ prefixStart > 0 and: [ (string at: prefixStart) isSeparator not ]] whileTrue: [
		prefixStart := prefixStart - 1 ].
	prefix := string copyFrom: prefixStart+1 to: prefixStop.
	entries := Array streamContents: [ :strm |
		self addEntriesTo: strm ]
]

{ #category : 'entries' }
AutoCompleter >> entries [
	^entries
]

{ #category : 'entries' }
AutoCompleter >> entryAt: anIndex [
	
	^self entries at: anIndex
]

{ #category : 'entries' }
AutoCompleter >> entryCount [
	^ entries size
]

{ #category : 'keyboard' }
AutoCompleter >> handleKeystrokeAfter: aKeyboardEvent [
	| newPos |
	menuMorph ifNil: [^self].
	newPos := textMorph editor startIndex-1.
	newPos = position ifTrue: [^self].
	newPos < position
		ifTrue: [
			prefix := prefix copyFrom: 1 to: prefix size+(newPos-position).
			position := newPos ]
		ifFalse: [
			position := position + 1.
			prefix := prefix copyWith: (model actualContents at: position) ].
	self computeEntries.
	entries notEmpty
		ifTrue: [ menuMorph resetMenu ]
		ifFalse: [ self closeMenu ].
]

{ #category : 'keyboard' }
AutoCompleter >> handleKeystrokeBefore: kbEvent [
	"I return a boolean. true when I have handled the event and no futher processing is needed by the caller."

	"Stuff to do if the menu is not open"
	menuMorph ifNil: [
		^ self openCompletionMenuFor: kbEvent ].

	"Starting here, stuff to do if the menu is open"
	menuMorph stillActive.

	kbEvent isEsc ifTrue: [ self closeMenu. ^ true].
	kbEvent isHome ifTrue: [ menuMorph goHome. ^ true ].
	kbEvent isEnd ifTrue: [ menuMorph goToEnd. ^ true].
	kbEvent isQuestionMark ifTrue: [ menuMorph help. ^true].
	kbEvent isArrowUp ifTrue: [ menuMorph goUp. ^ true].
	kbEvent isArrowDown ifTrue: [ menuMorph goDown. ^ true].
	kbEvent isPageUp ifTrue: [ menuMorph goPageUp. ^ true].
	kbEvent isPageDown ifTrue: [ menuMorph goPageDown. ^ true].

	kbEvent isArrowRight ifTrue: [ ^self handleRightArrowKeystrokeBefore: kbEvent ].
	kbEvent isArrowLeft ifTrue: [ ^self handleLeftArrowKeystrokeBefore: kbEvent ].

	(self shouldInsertSelected: kbEvent) ifTrue: [ self insertSelected ifTrue: [^ true]].
	(self shouldCloseMenu: kbEvent) ifTrue: [ self closeMenu ].

	"Further processing needed"
	^false
]

{ #category : 'keyboard' }
AutoCompleter >> handleLeftArrowKeystrokeBefore: kbEvent [
	"Subclasses might do something"
	^true
]

{ #category : 'keyboard' }
AutoCompleter >> handleRightArrowKeystrokeBefore: kbEvent [
	"Subclasses might do something"
	^true
]

{ #category : 'entries' }
AutoCompleter >> insertSelected [
	
	| entry editor selEnd str |
	
	textMorph ifNil: [ ^false ].
	
	entry := self selectedEntryFormatted.
	(self canSelect: entry) ifFalse: [ ^true ].
	
	editor := textMorph editor.
	str := model actualContents string.
	selEnd := position.
	[selEnd < str size and: [ (str at: selEnd+1) tokenish ]] whileTrue: [ selEnd := selEnd + 1 ].
	(selEnd < str size and: [ (str at: selEnd+1) == $ ]) ifTrue: [ selEnd := selEnd + 1].
	editor selectFrom: position-prefix size+1 to: selEnd.
	editor
		replaceSelectionWith: entry;
		deselectAndPlaceCursorAt: position - prefix size + 1 + (self newCursorPosition: entry).
	textMorph redrawNeeded.
	menuMorph delete.
	menuMorph := nil.
	
	^ true
]

{ #category : 'testing' }
AutoCompleter >> isPossibleInvalidEntry: anEntry [

	^false
]

{ #category : 'keyboard - private' }
AutoCompleter >> lastCharacterWantsAutoComplete [

	| lastCharIndex lastCharacter |
	textMorph editor hasSelection ifTrue: [ ^ false ].

	lastCharIndex := textMorph editor startIndex-1.
	lastCharIndex <= 0 ifTrue: [ ^ false ].
	lastCharacter := model actualContents at: lastCharIndex.

	^ lastCharacter isAlphaNumeric
]

{ #category : 'menu morph' }
AutoCompleter >> menuClosed [
	menuMorph := nil
]

{ #category : 'menu morph' }
AutoCompleter >> morphClass [
	^AutoCompleterMorph
]

{ #category : 'entries' }
AutoCompleter >> newCursorPosition: anEntry [
	^anEntry size
]

{ #category : 'menu morph' }
AutoCompleter >> openCompletionMenu [

	| theEditor |

	theEditor := textMorph editor.
	position := theEditor startIndex - 1.
	self closeMenu.
	self computeEntries.
	entries notEmpty
		ifTrue: [ | startIndex characterBlock cursorIndex |
			cursorIndex := theEditor pointIndex.
			startIndex := (theEditor text at: cursorIndex-1) = Character space
				ifTrue: [ cursorIndex ]
				ifFalse: [ theEditor previousWordStart: (cursorIndex >  theEditor text size ifTrue: [ cursorIndex-1 ] ifFalse: [ cursorIndex ])].
			characterBlock := theEditor characterBlockForIndex: startIndex.
			menuMorph :=  self morphClass
				completer: self
				position: characterBlock bottomLeft + textMorph positionInWorld ].

]

{ #category : 'keyboard - private' }
AutoCompleter >> openCompletionMenuFor: kbEvent [

	"Ctrl-Space (not on Mac), option-Space (on Mac) for open"
	kbEvent isCtrlSpace ifTrue: [
		self openCompletionMenu.
		^ true].

	"Tab, but not after a space."
	(kbEvent isTab and: [ self opensWithTab ]) ifTrue: [
		self lastCharacterWantsAutoComplete ifTrue: [
			self openCompletionMenu.
			^ true ]].

	"Auto-open - currently deactivated"
"	(ctrl not & cmd not & alphanum) ifTrue: [ self openCompletionMenu ]."
	
	^ false
]

{ #category : 'testing' }
AutoCompleter >> opensWithTab [
	
	"Returns wheter should open the auto completer when pressing Tab or not"
	
	^false
]

{ #category : 'entries' }
AutoCompleter >> selectedEntry [

	^self entries at: menuMorph selected
]

{ #category : 'entries' }
AutoCompleter >> selectedEntryFormatted [

	^(self entries at: menuMorph selected), ' '
]

{ #category : 'accessing' }
AutoCompleter >> setModel: aTextModel [
	model := aTextModel
]

{ #category : 'keyboard - private' }
AutoCompleter >> shouldCloseMenu: kbEvent [

	"Only alphanumeric or colon without modifiers.
	Anything else should close the Autocompletion menu."

	kbEvent controlKeyPressed ifTrue: [ ^true ].
	kbEvent commandAltKeyPressed ifTrue: [ ^true ].
	kbEvent isAlphaNumeric ifTrue: [ ^false ].
	kbEvent isColon ifTrue: [ ^false ].
	^true
]

{ #category : 'keyboard - private' }
AutoCompleter >> shouldInsertSelected: kbEvent [
	
	^ kbEvent isReturnKey 
		or: [ (kbEvent isSpace and: [ kbEvent controlKeyPressed or: [ kbEvent rawMacOptionKeyPressed ]]) 
		or: [ kbEvent isTab]]
]

{ #category : 'accessing' }
AutoCompleter >> textMorph [

	^textMorph
]

{ #category : 'accessing' }
AutoCompleter >> textMorph: aBareTextMorph [
	textMorph := aBareTextMorph
]
