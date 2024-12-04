'From Cuis7.1 [latest update: #6886] on 3 December 2024 at 11:15:53 am'!

!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 12/3/2024 10:19:24'!
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
	(self shouldCloseMenu: kbEvent) ifTrue: [ self closeMenu ].

	"Further processing needed"
	^false! !

