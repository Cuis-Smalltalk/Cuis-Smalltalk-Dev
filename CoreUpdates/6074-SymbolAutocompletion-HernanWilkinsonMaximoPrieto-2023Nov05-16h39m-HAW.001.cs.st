'From Cuis 6.0 [latest update: #6068] on 7 November 2023 at 10:36:59 am'!

!Symbol class methodsFor: 'as yet unclassified' stamp: 'HAW 11/7/2023 09:49:29'!
allSymbolsSelect: aCondition

	^Array streamContents:
		[:stream | Symbols do: [:symbol | (aCondition value: symbol) ifTrue: [ stream nextPut: symbol]]]! !


!SmalltalkCompleter methodsFor: 'as yet unclassified' stamp: 'HAW 11/7/2023 10:35:02'!
computeSymbols
	
	| prefixWithoutNumeral |
	
	canShowSelectorDocumentation := false.
	prefixWithoutNumeral := prefix allButFirst.
	entries := (Symbol allSymbolsSelect: [:aSymbol | aSymbol beginsWith: prefixWithoutNumeral ]) sorted.! !


!SHParserST80 methodsFor: 'testing' stamp: 'HAW 11/7/2023 09:31:47'!
isSymbol: aRangeType

	^aRangeType = #symbol! !

!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 11/7/2023 10:35:53'!
newCursorPosition: anEntry

	| lastRangeType |
	
	lastRangeType := parser isMethodDefinition: allRanges last rangeType.
	
	^lastRangeType
		ifTrue: [ anEntry size - 1]
		ifFalse: [ 
			(parser isSymbol: lastRangeType) 
				ifTrue: [anEntry size + 1]
				ifFalse: [ anEntry indexOf: $ ]]! !

!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 11/7/2023 10:28:54'!
selectedEntryFormatted

	| selectedEntry |
	
	selectedEntry := self entries at: menuMorph selected.
	^(parser isSymbol: allRanges last rangeType)
		ifTrue: [ '#', selectedEntry ]
		ifFalse: [ selectedEntry separateKeywords ]! !

!SmalltalkCompleter methodsFor: 'entries - private' stamp: 'HAW 11/7/2023 09:28:29'!
computeEntriesOfMessageOrIdentifiersFor: allSource at: range in: contextClass and: specificModel

	prefix := allSource copyFrom: range start to: range end.
	(parser isMessage: range rangeType) ifTrue: [ 
		^self computeMessageEntriesWhenSendinMessageFor: allSource in: contextClass and: specificModel ].	
	(parser isPartialOrFullIdentifier: range rangeType) ifTrue: [ ^self computeIdentifierEntries ].
	(parser isMethodDefinition: range rangeType) ifTrue: [ ^self computeMethodNamesFor: contextClass ].
	(parser isSymbol: range rangeType) ifTrue: [ ^self computeSymbols ].
	
	"If we don't know what to do, do nothing"
	entries := #() ! !
