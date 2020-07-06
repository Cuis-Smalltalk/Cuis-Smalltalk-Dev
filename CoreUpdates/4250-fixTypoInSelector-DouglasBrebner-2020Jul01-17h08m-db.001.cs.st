'From Cuis 5.0 [latest update: #4249] on 1 July 2020 at 5:10:38 pm'!

!KeyboardEvent methodsFor: 'testing' stamp: 'db 7/1/2020 17:09:47'!
isQuestionMark
	
	^ self keyCharacter = $? ! !


!AutoCompleter methodsFor: 'keyboard' stamp: 'db 7/1/2020 17:09:47'!
handleKeystrokeBefore: kbEvent
	
	"I return a boolean. true when I have handled the event and no futher processing is needed by the caller."
	
	| shouldOpenMorph |
	  
	shouldOpenMorph _ self shouldOpenMorph.
	
	"Stuff to do if the menu is not open"
	menuMorph ifNil: [ ^ self openCompletionMenuFor: kbEvent if: shouldOpenMorph ].

	"Starting here, stuff to do if the menu is open"
	menuMorph stillActive.
	
	kbEvent isEsc ifTrue: [ self closeMenu. ^ true].
	kbEvent isBackspace ifTrue: [ shouldOpenMorph ifFalse: [ self closeMenu ]. ^ false].
	kbEvent isHome ifTrue: [ menuMorph goHome. ^ true ].
	kbEvent isEnd ifTrue: [ menuMorph goToEnd. ^ true].
	kbEvent isQuestionMark ifTrue: [ menuMorph help. ^true].
	kbEvent isArrowUp ifTrue: [ menuMorph goUp. ^ true].
	kbEvent isArrowDown ifTrue: [ menuMorph goDown. ^ true].
	kbEvent isPageUp ifTrue: [ menuMorph goPageUp. ^ true].
	kbEvent isPageDown ifTrue: [ menuMorph goPageDown. ^ true].
	
	self canShowSelectorDocumentation 
		ifTrue: [ 
			kbEvent isArrowRight ifTrue: [ menuMorph showSelectorDocumentation. ^ true ].
			kbEvent isArrowLeft ifTrue: [ menuMorph hideSelectorDocumentation. ^ true ]]
		ifFalse: [
			"If it is showing identifiers I eat the right arrow key because the user is used to it when
			showing selectors,  so to avoid an unexpected behavior I do nothing with it -Hernan"
			kbEvent isArrowRight ifTrue: [ ^ true ]].
					
	(self shouldInsertSelected: kbEvent) ifTrue: [ self insertSelected ifTrue: [^ true]].
	(self shouldCloseMenu: kbEvent) ifTrue: [ self closeMenu ]. 
	 
	^false! !

!methodRemoval: KeyboardEvent #isQuesitonMark stamp: 'db 7/1/2020 17:09:47'!
KeyboardEvent removeSelector: #isQuesitonMark!

!AutoCompleter reorganize!
('accessing' setModel: textMorph textMorph:)
('menu morph' closeMenu menuClosed openCompletionMenu)
('keyboard' autoCompletionAround:keyStroke: handleKeystrokeAfter: handleKeystrokeBefore:)
('keyboard - private' isTab:and: openCompletionMenuFor:if: shouldCloseMenu: shouldInsertSelected: shouldOpenMorph shouldOpenMorphWhenNoPrefixAt: shouldOpenMorphWhenNoPrefixFor: shouldOpenMorphWhenPrefixAt:and:)
('entries' computeEntries entries entryAt: entryCount insertSelected newCursorPosition: selectedEntry selectedEntryFormatted)
('testing' canSelect: canSelectEntryAt: canShowSelectorDocumentation isPossibleInvalidEntry: opensWithTab)
!

