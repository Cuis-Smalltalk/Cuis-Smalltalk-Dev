'From Cuis7.1 [latest update: #6791] on 22 October 2024 at 4:58:50 pm'!

!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 10/22/2024 16:36:20'!
handleKeystrokeAfter: aKeyboardEvent
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
		ifFalse: [ self closeMenu ].! !

!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 10/22/2024 16:36:26'!
handleKeystrokeBefore: kbEvent
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
	(self shouldCloseMenu: kbEvent)print ifTrue: [ self closeMenu ].

	"Further processing needed"
	^false! !

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'jmv 10/22/2024 16:38:26'!
lastCharacterWantsAutoComplete

	| lastCharIndex lastCharacter |
	textMorph editor hasSelection ifTrue: [ ^ false ].

	lastCharIndex := textMorph editor startIndex-1.
	lastCharIndex <= 0 ifTrue: [ ^ false ].
	lastCharacter := model actualContents at: lastCharIndex.

	^ lastCharacter isAlphaNumeric or: [
		lastCharacter isValidInBinarySelectors or: [ lastCharacter = $: ]]! !

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'jmv 10/22/2024 16:58:33'!
openCompletionMenuFor: kbEvent

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
	
	^ false! !

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'jmv 10/22/2024 16:30:29'!
shouldCloseMenu: kbEvent

	"Only alphanumeric or colon without modifiers.
	Anything else should close the Autocompletion menu."

	kbEvent controlKeyPressed ifTrue: [ ^true ].
	kbEvent commandAltKeyPressed ifTrue: [ ^true ].
	kbEvent isAlphaNumeric ifTrue: [ ^false ].
	kbEvent isColon ifTrue: [ ^false ].
	^true! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences allPreferences removeKey: #spaceTabOpensAutoCompletion.!

