'From Cuis7.3 [latest update: #7122] on 11 May 2025 at 6:54:16 pm'!

!SmalltalkCompleter methodsFor: 'keyboard - private' stamp: 'HAW 5/10/2025 18:38:05'!
lastCharacterWantsAutoComplete

	| lastCharIndex lastCharacter |
	textMorph editor hasSelection ifTrue: [ ^ false ].

	lastCharIndex := textMorph editor startIndex-1.
	lastCharIndex <= 0 ifTrue: [ ^ false ].
	lastCharacter := model actualContents at: lastCharIndex.

	^ lastCharacter isAlphaNumeric 
		or: [ lastCharacter isValidInBinarySelectors 
		or: [ lastCharacter == $: 
		or: [ lastCharacter = Character space and: [ Preferences at: #spaceTabOpensAutoCompletion ]]]]! !


!AutoCompleter methodsFor: 'keyboard - private' stamp: 'jmv 5/11/2025 18:53:24'!
lastCharacterWantsAutoComplete

	| lastCharIndex lastCharacter |
	textMorph editor hasSelection ifTrue: [ ^ false ].

	lastCharIndex := textMorph editor startIndex-1.
	lastCharIndex <= 0 ifTrue: [ ^ false ].
	lastCharacter := model actualContents at: lastCharIndex.

	^ lastCharacter isAlphaNumeric! !

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


!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntryFormatted)
('testing' canSelect: canShowSelectorDocumentation isCategoryEntry: isPossibleInvalidEntry: opensWithTab)
('message entries - private' classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfUnaryMessageReturnNamed: computeMessageEntriesFor:in:and: computeMessageEntriesForClass: computeMessageEntriesForClassOrNil: computeMessageEntriesOfCascadeReceiver computeMessageEntriesOfEnclosedExpressionReturn computeMessageEntriesWhenSendingMessageFor:in:and: computeMessageEntriesWithBinaryMessageForClass: computeMessageEntriesWithEmptyPrefixFor:in:and: computeMessageEntriesWithoutBinaryMessageForClass: ifEmptyEntriesShowAllPrefixedSelectors isPreviousMessageSendBinary lookForBinarySelectorAfter:startingAt:with: lookForBinarySelectorAfterArrayStartStartingAt:with: lookForBinarySendRange lookForNoUnaryMessageSend selectorsOf:beginningWith: unaryAndBinarySelectorsOf:beginningWith:)
('identifier entries - private' computeIdentifierEntries computeIdentifierEntriesBeginningWith: computeMethodNamesFor: computeMethodNamesFor:andMethodPrefix:)
('message entries for unknown class - private' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: computeMessageEntriesForUnknowClass computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: insert:and:at:to: sortByLastUsedTime:)
('selector documentation' documentationOf:)
('accessing' allSource possibleInvalidSelectors)
('menu morph' morphClass)
('keyboard' handleLeftArrowKeystrokeBefore: handleRightArrowKeystrokeBefore:)
('keyboard - private' lastCharacterWantsAutoComplete)
('entries - private' canComputeMessageEntriesFor: computeEntriesOfMessageOrIdentifiersFor:in:and: computeSymbols parse:in:and: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
!

