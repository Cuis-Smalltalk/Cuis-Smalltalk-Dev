'From Cuis6.3 [latest update: #6286] on 21 March 2024 at 9:46:16 am'!

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'jmv 3/21/2024 09:45:51'!
lastCharacterWantsAutoComplete

	| lastCharIndex lastCharacter |
	textMorph editor hasSelection ifTrue: [ ^ false ].

	lastCharIndex := textMorph editor startIndex-1.
	lastCharIndex <= 0 ifTrue: [ ^ false ].
	lastCharacter := model actualContents at: lastCharIndex.

	^ lastCharacter isAlphaNumeric or: [
		lastCharacter isValidInBinarySelectors or: [ lastCharacter = $: ]]! !

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'jmv 3/21/2024 09:37:20'!
openCompletionMenuFor: kbEvent

	"Ctrl-Space or Tab for open"
	kbEvent isCtrlSpace ifTrue: [
		self openCompletionMenu.
		^ true].
	(kbEvent isTab and: [ self opensWithTab ]) ifTrue: [
		self openCompletionMenu.
		^ true].
		
	"Auto-open - currently deactivated"
"	(ctrl not & cmd not & alphanum) ifTrue: [ self openCompletionMenu ]."
	
	^ false! !


!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 3/21/2024 09:42:22'!
handleKeystrokeBefore: kbEvent
	"I return a boolean. true when I have handled the event and no futher processing is needed by the caller."

	"Stuff to do if the menu is not open"
	menuMorph ifNil: [
		self lastCharacterWantsAutoComplete ifFalse: [ ^false ]. "Further processing needed"
		^ self openCompletionMenuFor: kbEvent ].

	"Starting here, stuff to do if the menu is open"
	menuMorph stillActive.

	kbEvent isEsc ifTrue: [ self closeMenu. ^ true].
	kbEvent isBackspace ifTrue: [
		self lastCharacterWantsAutoComplete ifFalse: [ self closeMenu ].
		^ false ]."Further processing needed"
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

!methodRemoval: AutoCompleter #openCompletionMenuFor:if: stamp: 'jmv 3/21/2024 09:36:27'!
AutoCompleter removeSelector: #openCompletionMenuFor:if:!
!methodRemoval: AutoCompleter #shouldOpenMorph stamp: 'jmv 3/21/2024 09:42:27'!
AutoCompleter removeSelector: #shouldOpenMorph!
!methodRemoval: AutoCompleter #shouldOpenMorphWhenPrefixAt:and: stamp: 'jmv 3/21/2024 09:20:00'!
AutoCompleter removeSelector: #shouldOpenMorphWhenPrefixAt:and:!
!methodRemoval: AutoCompleter #shouldOpenMorphWhenNoPrefixFor: stamp: 'jmv 3/21/2024 09:17:10'!
AutoCompleter removeSelector: #shouldOpenMorphWhenNoPrefixFor:!
!methodRemoval: AutoCompleter #shouldOpenMorphWhenNoPrefixAt: stamp: 'jmv 3/21/2024 09:17:08'!
AutoCompleter removeSelector: #shouldOpenMorphWhenNoPrefixAt:!
!methodRemoval: AutoCompleter #isTab:and: stamp: 'jmv 3/21/2024 09:30:43'!
AutoCompleter removeSelector: #isTab:and:!
