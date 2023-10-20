'From Cuis 6.0 [latest update: #6051] on 20 October 2023 at 6:54:39 pm'!

!SmalltalkCompleter methodsFor: 'identifier entries - private' stamp: 'MH-NMS 10/20/2023 01:38:33'!
computeMethodNamesFor: aContextClass

	canShowSelectorDocumentation := false.
	entries := self computeMethodNamesFor: aContextClass andMethodPrefix: prefix! !

!SmalltalkCompleter methodsFor: 'identifier entries - private' stamp: 'MH-NMS 10/16/2023 11:39:40'!
computeMethodNamesFor: aContextClass andMethodPrefix: aMethodStartingWith

	| existing |
	existing := aContextClass selectors.
	^(aContextClass superclass allSelectors 
		select: [ :aSelector | (aSelector beginsWith: aMethodStartingWith) and: [(existing includes: aSelector) not] ] 
		thenCollect: [ :aSelector | aContextClass methodHeaderFor: aSelector ]) asSortedCollection! !


!SHParserST80 methodsFor: 'testing' stamp: 'MH-NMS 10/16/2023 13:39:05'!
isMethodDefinition: aSymbol

	^#(patternUnary patternBinary patternKeyword) statePointsTo: aSymbol! !


!AutoCompleter methodsFor: 'keyboard - private' stamp: 'MH-NMS 10/20/2023 00:37:34'!
shouldOpenMorphWhenPrefixAt: currentPos and: currentChar 
	
	^ model textSize >= currentPos and: [ currentChar isAlphaNumeric or: [ currentChar isValidInBinarySelectors or: [ currentChar = $: ]]]! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'MH-NMS 10/20/2023 01:08:09'!
newCursorPosition: anEntry

	^(parser isMethodDefinition: allRanges last rangeType) 
		ifTrue: [ anEntry size - 1]
		ifFalse: [ anEntry indexOf: $ ]! !

!SmalltalkCompleter methodsFor: 'entries - private' stamp: 'MH-NMS 10/16/2023 11:48:28'!
computeEntriesOfMessageOrIdentifiersFor: allSource at: range in: contextClass and: specificModel

	prefix := allSource copyFrom: range start to: range end.
	(parser isMessage: range rangeType) ifTrue: [ 
		^self computeMessageEntriesWhenSendinMessageFor: allSource in: contextClass and: specificModel ].	
	(parser isPartialOrFullIdentifier: range rangeType) ifTrue: [ ^self computeIdentifierEntries ].
	(parser isMethodDefinition: range rangeType) ifTrue: [ ^self computeMethodNamesFor: contextClass ].

	"If we don't know what to do, do nothing"
	entries := #() ! !


!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntryFormatted)
('entries - private ' canComputeMessageEntriesFor: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
('testing' canSelect: canShowSelectorDocumentation isCategoryEntry: isPossibleInvalidEntry: opensWithTab)
('message entries - private' classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfUnaryMessageReturnNamed:at: computeMessageEntriesFor:at:in:and: computeMessageEntriesForClass: computeMessageEntriesForClassOrNil: computeMessageEntriesOfCascadeReceiverAt: computeMessageEntriesOfEnclosedExpressionReturnAt: computeMessageEntriesWhenSendinMessageFor:in:and: computeMessageEntriesWithBinaryMessageForClass: computeMessageEntriesWithEmptyPrefixFor:at:in:and: computeMessageEntriesWithoutBinaryMessageForClass: ifEmptyEntriesShowAllPrefixedSelectors isPreviousMessageSendBinary lookForBinarySelectorAfter:startingAt:with: lookForBinarySelectorAfterArrayStartStartingAt:with: lookForBinarySendRange lookForNoUnaryMessageSend selectorsOf:beginningWith: unaryAndBinarySelectorsOf:beginningWith:)
('identifier entries - private' computeIdentifierEntries computeIdentifierEntriesBeginningWith: computeMethodNamesFor: computeMethodNamesFor:andMethodPrefix:)
('message entries for unknown class - private' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: computeMessageEntriesForUnknowClass computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: insert:and:at:to: sortByLastUsedTime:)
('selector documentation' documentationOf:)
('accessing' allSource possibleInvalidSelectors)
('menu morph' morphClass)
('keyboard' handleLeftArrowKeystrokeBefore: handleRightArrowKeystrokeBefore:)
('entries - private' computeEntriesOfMessageOrIdentifiersFor:at:in:and: parse:in:and:)
!

