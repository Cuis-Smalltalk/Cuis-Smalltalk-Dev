'From Cuis 6.0 [latest update: #6068] on 7 November 2023 at 11:40:33 am'!

!SHRange methodsFor: 'testing' stamp: 'HAW 11/7/2023 11:37:17'!
correspondsToIdentifier

	^#(#incompleteIdentifier
		#blockTempVar #blockArg #tempVar #methodArg
		#instVar #classVar 
		#workspaceVar #poolConstant #globalVar ) statePointsTo: type! !

!SHRange methodsFor: 'testing' stamp: 'HAW 11/7/2023 11:35:10'!
correspondsToMessage

	^#(binary incompleteBinary keyword incompleteKeyword unary incompleteUnary) statePointsTo: type ! !

!SHRange methodsFor: 'testing' stamp: 'HAW 11/7/2023 11:35:45'!
correspondsToMethodDefinition
	^#(patternUnary patternBinary patternKeyword) statePointsTo: self rangeType! !

!SHRange methodsFor: 'testing' stamp: 'HAW 11/7/2023 11:37:32'!
correspondsToPartialOrFullIdentifier: aParser

	^self correspondsToIdentifier or: [  self correspondsToReservedNameIn: aParser ] ! !

!SHRange methodsFor: 'testing' stamp: 'HAW 11/7/2023 11:37:32'!
correspondsToReservedNameIn: aParser

	^aParser reservedNames statePointsTo: type! !

!SHRange methodsFor: 'testing' stamp: 'HAW 11/7/2023 11:34:37'!
correspondsToSymbol

	^self rangeType = #symbol

	! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 11/7/2023 11:36:25'!
newCursorPosition: anEntry

	^currentRange correspondsToMethodDefinition
		ifTrue: [ anEntry size - 1]
		ifFalse: [ 
			currentRange correspondsToSymbol 
				ifTrue: [anEntry size + 1]
				ifFalse: [ anEntry indexOf: $ ]]! !

!SmalltalkCompleter methodsFor: 'entries - private' stamp: 'HAW 11/7/2023 11:38:02'!
computeEntriesOfMessageOrIdentifiersFor: allSource in: contextClass and: specificModel

	prefix := allSource copyFrom: currentRange start to: currentRange end.
	
	currentRange correspondsToMessage ifTrue: [ 
		^self computeMessageEntriesWhenSendingMessageFor: allSource in: contextClass and: specificModel ].	
	(currentRange correspondsToPartialOrFullIdentifier: parser) ifTrue: [ ^self computeIdentifierEntries ].
	currentRange correspondsToMethodDefinition ifTrue: [ ^self computeMethodNamesFor: contextClass ].
	currentRange correspondsToSymbol ifTrue: [ ^self computeSymbols ].
	
	"If we don't know what to do, do nothing"
	entries := #() 
	! !

!methodRemoval: SHRange #isPartialOrFullIdentifier: stamp: 'HAW 11/7/2023 11:35:29'!
SHRange removeSelector: #isPartialOrFullIdentifier:!
!methodRemoval: SHRange #isMethodDefinition: stamp: 'HAW 11/7/2023 11:26:00'!
SHRange removeSelector: #isMethodDefinition:!
!methodRemoval: SHRange #isSymbol2 stamp: 'HAW 11/7/2023 11:34:37'!
SHRange removeSelector: #isSymbol2!
!methodRemoval: SHRange #isMethodDefinition stamp: 'HAW 11/7/2023 11:35:45'!
SHRange removeSelector: #isMethodDefinition!
!methodRemoval: SHRange #isReservedNameIn: stamp: 'HAW 11/7/2023 11:37:32'!
SHRange removeSelector: #isReservedNameIn:!
!methodRemoval: SHRange #isIdentifier stamp: 'HAW 11/7/2023 11:37:17'!
SHRange removeSelector: #isIdentifier!
!methodRemoval: SHRange #isMessage stamp: 'HAW 11/7/2023 11:35:10'!
SHRange removeSelector: #isMessage!
!methodRemoval: SHRange #isSymbol2: stamp: 'HAW 11/7/2023 11:32:29'!
SHRange removeSelector: #isSymbol2:!
!methodRemoval: SHRange #isMessage: stamp: 'HAW 11/7/2023 11:03:02'!
SHRange removeSelector: #isMessage:!
!methodRemoval: SHParserST80 #isPartialOrFullIdentifier: stamp: 'HAW 11/7/2023 11:20:54'!
SHParserST80 removeSelector: #isPartialOrFullIdentifier:!
!methodRemoval: SHParserST80 #isIdentifier: stamp: 'HAW 11/7/2023 11:20:19'!
SHParserST80 removeSelector: #isIdentifier:!
!methodRemoval: SHParserST80 #isReservedName: stamp: 'HAW 11/7/2023 11:18:59'!
SHParserST80 removeSelector: #isReservedName:!
!methodRemoval: SHParserST80 #isMethodDefinition: stamp: 'HAW 11/7/2023 11:25:33'!
SHParserST80 removeSelector: #isMethodDefinition:!
!methodRemoval: SHParserST80 #isMessage: stamp: 'HAW 11/7/2023 11:01:44'!
SHParserST80 removeSelector: #isMessage:!
!methodRemoval: SHParserST80 #isSymbol: stamp: 'HAW 11/7/2023 11:31:55'!
SHParserST80 removeSelector: #isSymbol:!
!methodRemoval: SmalltalkCompleter #isSymbol2 stamp: 'HAW 11/7/2023 11:32:56'!
SmalltalkCompleter removeSelector: #isSymbol2!
!methodRemoval: SmalltalkCompleter #isMethodDefinition stamp: 'HAW 11/7/2023 11:29:56'!
SmalltalkCompleter removeSelector: #isMethodDefinition!
!methodRemoval: SmalltalkCompleter #parser stamp: 'HAW 11/7/2023 11:39:19'!
SmalltalkCompleter removeSelector: #parser!
!methodRemoval: SmalltalkCompleter #isMessage stamp: 'HAW 11/7/2023 11:03:31'!
SmalltalkCompleter removeSelector: #isMessage!
!methodRemoval: SmalltalkCompleter #isPartialOrFullIdentifier stamp: 'HAW 11/7/2023 11:22:22'!
SmalltalkCompleter removeSelector: #isPartialOrFullIdentifier!

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
('entries - private' canComputeMessageEntriesFor: computeEntriesOfMessageOrIdentifiersFor:in:and: computeSymbols parse:in:and: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
!


!SHRange reorganize!
('accessing' end end: length rangeType start start: start:end:type: type:)
('printing' printOn:)
('testing' correspondsToIdentifier correspondsToMessage correspondsToMethodDefinition correspondsToPartialOrFullIdentifier: correspondsToReservedNameIn: correspondsToSymbol)
!

