'From Cuis6.3 [latest update: #6323] on 23 April 2024 at 9:53:29 am'!

!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 4/23/2024 09:52:08'!
handleKeystrokeAfter: aKeyboardEvent
	| newPos |
	menuMorph ifNil: [^self].
	newPos := textMorph editor startIndex-1.
	newPos = position ifTrue: [^self].
	aKeyboardEvent isBackspace ifTrue: [
		self lastCharacterWantsAutoComplete ifFalse: [ self closeMenu ].
		^ self ].
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
		ifFalse: [ self closeMenu ]! !

!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 4/23/2024 09:51:51'!
handleKeystrokeBefore: kbEvent
	"I return a boolean. true when I have handled the event and no futher processing is needed by the caller."

	"Stuff to do if the menu is not open"
	menuMorph ifNil: [
		self lastCharacterWantsAutoComplete ifFalse: [ ^false ]. "Further processing needed"
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
	^false! !

