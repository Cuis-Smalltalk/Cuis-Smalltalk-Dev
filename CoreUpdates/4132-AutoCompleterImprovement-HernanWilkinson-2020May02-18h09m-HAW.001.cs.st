'From Cuis 5.0 [latest update: #4129] on 3 May 2020 at 2:02:23 am'!
!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser selectorsClasses possibleInvalidSelectors canShowSelectorDocumentation possibleBinarySendRange'
	classVariableNames: 'AccessLock EntriesLimit Selectors'
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'HAW 5/3/2020 00:48:51'!
antepenultimate
	
	^self antepenultimateIfAbsent: [ self errorCollectionTooSmall ]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'HAW 5/3/2020 00:47:42'!
antepenultimateIfAbsent: aBlock
	
	| size |
	
	size := self size.
	size >= 3 ifTrue: [ ^self at: size - 2 ].
	^aBlock value! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'HAW 5/3/2020 00:39:34'!
penultimateIfAbsent: aBlock
	
	| size |
	
	size := self size.
	size >= 2 ifTrue: [ ^self at: size-1 ].
	^aBlock value! !


!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 5/3/2020 01:42:05'!
possibleBinarySendRangeFrom: allRanges

	| penultimate antepenultimate |

	penultimate := allRanges penultimateIfAbsent: [ SHRange nilObject ].
	antepenultimate := allRanges antepenultimateIfAbsent: [ SHRange nilObject ].
	
	possibleBinarySendRange _ penultimate rangeType = #arrayStart 
		ifTrue: [ antepenultimate rangeType = #symbol
			ifTrue: [ allRanges at: allRanges size - 3 ifAbsent: [ SHRange nilObject ] ]]
		ifFalse: [ 
			({#leftBrace. #'$'. #symbol. #blockStart. } includes: penultimate rangeType)
				ifTrue: [ antepenultimate ]
				ifFalse: [ penultimate ]]! !


!SmalltalkCompleter class methodsFor: 'accessing' stamp: 'HAW 5/2/2020 20:47:22'!
changeEntriesLimitTo: aNewLimit during: aBlock

	| previousLimit |
	
	previousLimit := EntriesLimit.
	EntriesLimit := aNewLimit.
	
	^aBlock ensure: [ EntriesLimit := previousLimit ]! !


!SHParserST80 methodsFor: 'accessing' stamp: 'HAW 5/3/2020 00:14:19'!
rangesWithoutExcessCode
	
	^ranges 
		ifEmpty: [ ranges ]
		ifNotEmpty: [ 
			ranges last rangeType = #excessCode
				ifTrue: [ ranges allButLast ]
				ifFalse: [ ranges ]]! !

!SHParserST80 methodsFor: 'testing' stamp: 'HAW 5/3/2020 00:27:29'!
isIdentifier: aSymbol

	^ #(#incompleteIdentifier
		#blockTempVar #blockArg #tempVar #methodArg
		#instVar #classVar 
		#workspaceVar #poolConstant #globalVar ) statePointsTo:aSymbol! !

!SHParserST80 methodsFor: 'testing' stamp: 'HAW 5/3/2020 00:27:56'!
isReservedName: aSymbol

	^ self reservedNames statePointsTo: aSymbol! !


!SHRange class methodsFor: 'instance creation' stamp: 'HAW 5/3/2020 01:10:44'!
nilObject

	"I can not reference self inside backtick - Hernan"
	^`SHRange start: 0 end: 0 type: nil`! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'HAW 5/3/2020 00:42:09'!
penultimate
	"Answer the penultimate element of the receiver.
	Raise an error if the collection is empty or has just one element."

	^self penultimateIfAbsent: [self errorCollectionTooSmall].
! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 5/3/2020 01:41:33'!
computeEntries

	| allSource contextClass specificModel allRanges range |

	allSource _ model actualContents string.
	specificModel _ self textProviderOrModel.
	contextClass _ self selectedClassOrMetaClassIn: specificModel. 
	allRanges _ self parse: allSource in: contextClass and: specificModel.
	range _ allRanges ifEmpty: [ ^entries _ #() ] ifNotEmpty: [ allRanges last ].
	possibleInvalidSelectors _ #().
	
	range end = position
		ifTrue: [ self computeEntriesOfMessageOrIdentifiersFor: allSource using: allRanges at: range in: contextClass and: specificModel ]
	 	ifFalse: [ self computeMessageEntriesWithEmptyPrefixFor: allSource using: allRanges at: range in: contextClass and: specificModel ].

	! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 5/3/2020 01:09:40'!
canComputeMessageEntriesFor: prevRange 

	^ prevRange rangeType notNil ! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 5/3/2020 00:15:12'!
computeEntriesOfMessageOrIdentifiersFor: allSource using: allRanges at: range in: contextClass and: specificModel
	
	prefix _ allSource copyFrom: range start to: range end.
	(parser isMessage: range rangeType) ifTrue: [ 
		^self computeMessageEntriesWhenSendinMessageFor: allSource using: allRanges in: contextClass and: specificModel ].	
	(parser isPartialOrFullIdentifier: range rangeType) ifTrue: [ ^self computeIdentifierEntries ].
	
	"If we don't know what to do, do nothing"
	entries _ #() ! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 5/3/2020 00:14:30'!
parse: allSource in: contextClass and: specificModel
	
	| isMethod |
	
	parser _ SHParserST80 new.
	parser
		workspace: ((specificModel is: #providesBindings) ifTrue: [specificModel]);
		classOrMetaClass: contextClass;
		source: (allSource copyFrom: 1 to: position);
		allSource: allSource.
		
	isMethod := (specificModel is: #Browser)
		ifTrue: [ specificModel isEditingClass not ]
		ifFalse: [ specificModel is: #CodeProvider ].
	parser parse: isMethod.
	
	^ parser rangesWithoutExcessCode.
! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 5/3/2020 01:41:14'!
computeMessageEntriesWhenSendinMessageFor: allSource using: allRanges in: contextClass and: specificModel 
	
	| prevRange |
	
	"If previous range is a constant or a well known identifier, we might filter messages"
	prevRange _ allRanges penultimateIfAbsent: [ SHRange nilObject ].
	possibleBinarySendRange _ allRanges antepenultimateIfAbsent: [ SHRange nilObject ].
	
	^ (self canComputeMessageEntriesFor: prevRange) 
		ifTrue: [ self computeMessageEntriesFor: allSource at: prevRange in: contextClass and: specificModel ]
		ifFalse: [ self computeMessageEntriesForUnknowClass ]
! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 5/3/2020 00:58:01'!
computeMessageEntriesWithEmptyPrefixFor: allSource using: allRanges at: range in: contextClass and: specificModel 
	
	self possibleBinarySendRangeFrom: allRanges.
	prefix _ ''.

	self computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel .! !


!SHParserST80 methodsFor: 'testing' stamp: 'HAW 5/3/2020 00:28:13'!
isPartialOrFullIdentifier: aSymbol

	^(self isIdentifier: aSymbol) or: [ self isReservedName: aSymbol ]! !

!methodRemoval: SHParserST80 #isPartialIdentifier: stamp: 'HAW 5/3/2020 00:27:29'!
SHParserST80 removeSelector: #isPartialIdentifier:!
!methodRemoval: SmalltalkCompleter #initialize stamp: 'HAW 5/2/2020 20:47:34'!
SmalltalkCompleter removeSelector: #initialize!

!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntryFormatted)
('entries - private ' canComputeMessageEntriesFor: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: parse:in:and: selectedClassOrMetaClassIn: selectorsClasses textProviderOrModel)
('testing' canSelect: canShowSelectorDocumentation isCategoryEntry: isPossibleInvalidEntry: opensWithTab)
('message entries - private' classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfUnaryMessageReturnNamed:at: computeMessageEntriesFor:at:in:and: computeMessageEntriesForClass: computeMessageEntriesForClassOrNil: computeMessageEntriesOfCascadeReceiverAt: computeMessageEntriesOfEnclosedExpressionReturnAt: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithBinaryMessageForClass: computeMessageEntriesWithEmptyPrefixFor:using:at:in:and: computeMessageEntriesWithoutBinaryMessageForClass: ifEmptyEntriesShowAllPrefixedSelectors isPreviousMessageSendBinary possibleBinarySendRangeFrom: selectorsOf:beginningWith: unaryAndBinarySelectorsOf:beginningWith:)
('identifier entries - private' computeIdentifierEntries computeIdentifierEntriesBeginningWith:)
('message entries for unknown class - private' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: computeMessageEntriesForUnknowClass computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: insert:and:at:to: sortByLastUsedTime:)
('selector documentation' documentationOf:)
('accessing' changePositionTo: possibleInvalidSelectors)
!


!SHParserST80 reorganize!
('accessing' allSource allSource: blockDepths blockDepthsStartIndexes classOrMetaClass classOrMetaClass: last3Ranges lastRange ranges rangesWithoutExcessCode source source: workspace:)
('scan' currentChar nextChar peekChar scanBinary scanComment scanIdentifier scanNext scanNumber scanPast: scanPast:level: scanPast:start:end: scanWhitespace skipBigDigits: skipDigits)
('parse support' enterBlock initializeInstanceVariables leaveBlock pushArgument: pushTemporary:)
('error handling' error failUnless: failWhen:)
('token testing' isAnsiAssignment isAssignment isBinary isKeyword isName isTokenExternalFunctionCallingConvention)
('character testing' isBigDigit:base: isBinarySelectorCharacter:)
('identifier testing' isBlockArgName: isBlockTempName: isClassVarName: isGlobal: isIncompleteBlockArgName: isIncompleteBlockTempName: isIncompleteClassVarName: isIncompleteGlobal: isIncompleteInstVarName: isIncompleteMethodArgName: isIncompleteMethodTempName: isIncompletePoolConstantName: isIncompleteReservedName: isIncompleteWorkspaceVarName: isInstVarName: isMethodArgName: isMethodTempName: isPoolConstantName: isWorkspaceVarName: resolve: resolvePartial: resolvePartialPragmaArgument: resolvePragmaArgument:)
('parse' parse parse: parseArray parseBacktick parseBinary parseBinaryMessagePattern parseBlock parseBlockArguments parseBlockTemporaries parseBraceArray parseByteArray parseCascadeAndChain parseCharSymbol parseExpression parseExternalCall parseKeyword parseKeywordMessagePattern parseLiteral: parseLiteralArrayElement parseMessagePattern parseMethodTemporaries parsePragmaBinary parsePragmaKeyword parsePragmaSequence parsePrimitive parseStatement parseStatementList parseStatementListForBraceArray parseString parseStringOrSymbol parseSymbol parseSymbolIdentifier parseSymbolSelector parseSymbolString parseTerm parseUnary parseUnaryMessagePattern)
('recording ranges' rangeType: rangeType:start:end:)
('indentifier iteration' blockArgNamesDo: blockTempNamesDo: classVarNamesDo: instVarNamesDo: methodArgNamesDo: methodTempNamesDo: namesBeginningWith:do: nonGlobalNamesDo: poolConstantNamesDo: reservedNames workspaceNamesDo:)
('testing' isIdentifier: isMessage: isPartialOrFullIdentifier: isReservedName:)
('aux' namesBeginningWith:do:in:)
!

