'From Cuis7.7 [latest update: #7783] on 5 January 2026 at 8:11:45 pm'!

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'HAW 5/Jan/2026 19:52:35'!
undefinedIdentifiers
	
	^ranges 
		select: [ :aRange | aRange correspondsToUndefinedIdentifier ]
		thenCollect: [ :aRange | source copyFrom: aRange start to: aRange end ]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'HAW 5/Jan/2026 19:53:03'!
undefinedIdentifiersWithout: anIdentifier
	
	^self undefinedIdentifiers 
		remove: anIdentifier ifAbsent: [];
		sorted! !


!SHRange methodsFor: 'testing' stamp: 'HAW 5/Jan/2026 19:31:54'!
correspondsToUndefinedIdentifier
	
	^self rangeType = #undefinedIdentifier

	! !


!SmalltalkCompleter methodsFor: 'entries - private' stamp: 'HAW 5/Jan/2026 19:53:54'!
computeEntriesOfMessageOrIdentifiersFor: allSource in: contextClass and: specificModel

	prefix := allSource copyFrom: currentRange start to: currentRange end.
	
	currentRange correspondsToMessage ifTrue: [ 
		^self computeMessageEntriesWhenSendingMessageFor: allSource in: contextClass and: specificModel ].	
	((currentRange correspondsToPartialOrFullIdentifier: parser) or: [ currentRange correspondsToUndefinedIdentifier ]) ifTrue: [ 
		^self computeIdentifierEntries ].
	currentRange correspondsToMethodDefinition ifTrue: [ ^self computeMethodNamesFor: contextClass ].
	currentRange correspondsToSymbol ifTrue: [ ^self computeSymbols ].
	
	"If we don't know what to do, do nothing"
	entries := #() 
	! !


!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'HAW 5/Jan/2026 19:49:42'!
namesBeginningWith: aString do: aBlock
	"Evaluate aBlock for all available names that start with aString"

	self nonGlobalNamesDo: [ :name :kindOfIdentifierTitle |
		(name beginsWith: aString) ifTrue: [ aBlock value: name value: kindOfIdentifierTitle ]].
	self namesBeginningWith: aString do: [ :name | aBlock value: name value: '-- classes' ] in: Smalltalk classNames.
	self namesBeginningWith: aString do: [ :name | aBlock value: name value: '-- globals' ] in: Smalltalk nonClassNames.
	self namesBeginningWith: aString do: [ :name | aBlock value: name value: '-- undefined' ] in: (self undefinedIdentifiersWithout: aString).
	! !


!SHParserST80 reorganize!
('accessing' blockDepths blockDepthsStartIndexes classOrMetaClass classOrMetaClass: last3Ranges lastRange ranges rangesWithoutExcessCode source source: workspace:)
('scan' currentChar nextChar peekChar scanBinary scanComment scanIdentifier scanNext scanNumber scanPast: scanPast:level: scanPast:start:end: scanWhitespace skipBigDigits: skipDigits)
('parse support' enterBlock initializeInstanceVariables leaveBlock pushArgument: pushTemporary:)
('error handling' error failUnless: failWhen:)
('token testing' isAnsiAssignment isAssignment isBinary isKeyword isName isTokenExternalErrorReaper isTokenExternalFunctionCallingConvention)
('character testing' isBigDigit:base: isBinarySelectorCharacter:)
('identifier testing' isBlockArgName: isBlockTempName: isClassVarName: isGlobal: isIncompleteBlockArgName: isIncompleteBlockTempName: isIncompleteClassVarName: isIncompleteGlobal: isIncompleteInstVarName: isIncompleteMethodArgName: isIncompleteMethodTempName: isIncompletePoolConstantName: isIncompleteReservedName: isIncompleteWorkspaceVarName: isInstVarName: isMethodArgName: isMethodTempName: isPoolConstantName: isWorkspaceVarName: resolve: resolvePartial: resolvePartialPragmaArgument: resolvePragmaArgument:)
('parse' parse parse: parseArray parseBacktick parseBinary parseBinaryMessagePattern parseBlock parseBlockArguments parseBlockTemporaries parseBraceArray parseByteArray parseCascadeAndChain parseCharSymbol parseExpression parseExternalCall parseExternalErrorReaperFunction parseExternalErrorReaperVariable parseKeyword parseKeywordMessagePattern parseLiteral: parseLiteralArrayElement parseMessagePattern parseMethodTemporaries parsePragmaBinary parsePragmaKeyword parsePragmaSequence parsePrimitive parseStatement parseStatementList parseStatementListForBraceArray parseString parseStringOrSymbol parseSymbol parseSymbolIdentifier parseSymbolSelector parseSymbolString parseTerm parseUnary parseUnaryMessagePattern)
('recording ranges' rangeType: rangeType:start:end:)
('indentifier iteration' blockArgNamesDo: blockTempNamesDo: classVarNamesDo: instVarNamesDo: methodArgNamesDo: methodTempNamesDo: namesBeginningWith:do: nonGlobalNamesDo: poolConstantNamesDo: reservedNames undefinedIdentifiers undefinedIdentifiersWithout: workspaceNamesDo:)
('aux' namesBeginningWith:do:in:)
!


!SHRange reorganize!
('accessing' end end: length rangeType start start: start:end:type: type:)
('printing' printOn:)
('testing' correspondsToIdentifier correspondsToMessage correspondsToMethodDefinition correspondsToPartialOrFullIdentifier: correspondsToReservedNameIn: correspondsToSymbol correspondsToUndefinedIdentifier)
!

